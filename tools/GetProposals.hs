{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-

Usage:
  getproposals <nrows>

Aim:

Search NASA ADS for (successful) proposals from Chandra and add
them to the database.

The <start> argument gives the number to start with and <nrows>
the number of entries to retrieve (see your ADS API limits for
the maximum value allowed).

Your ADS API key must be stored in the file adskey.txt in the
current working directory when the tool is run.

Fun notes:

There can be multiple bibcodes for the same proposal:

https://ui.adsabs.harvard.edu/#abs/2000cxo..prop..448C/abstract
https://ui.adsabs.harvard.edu/#abs/2000cxo..prop..730C/abstract

are

Hrc-I Calibration Observations of Arlac
Hrc-S Calibration Observations of Arlac

both are labelled as Chandra Proposal ID #02200112
and they both have the same abstract. Following the data links
shows that they are both labelled in the archive as coming
from this proposal.

So, I ignore repeated proposal values, dropping them to
the "invalid" category.

-}

module Main where

import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import qualified System.IO.Strict as S

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.State.Strict (evalStateT)

import Data.Aeson (FromJSON(..)
                   , Value(..)
                   , (.:), (.:?))
import Data.Aeson.Parser (value')
import Data.Aeson.Types (withObject, withText)

import Data.Functor ((<$>))

import Database.Groundhog.Postgresql (PersistBackend
                                     , Cond(CondEmpty), Order(Desc)
                                     , (==.)
                                     , orderBy, limitTo
                                     , countAll
                                     , deleteAll, insert_
                                     , project, select
                                     , upper)

import Data.Either (partitionEithers)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Time (UTCTime, getCurrentTime)

import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (ResponseHeaders)

import Pipes
import Pipes.Attoparsec (parse)
import Pipes.HTTP

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, hPrint, stderr)
import System.IO.Error (catchIOError, ioeGetErrorString)

import Text.Read (readMaybe)


import Database (runDb)
import Types (ADSSearchMetaData(..)
              , BibCode
              , ProposalAbstract(..)
              , InvalidProposalAbstract(..)
              , PropNum
              , fromBibCode, toBibCode
              , fromPropNum, toPropNumStr
              , Field(AdsCheckedField, PaNumField, PropNumField, PropNameField)
              )


newtype ADSKey = ADSKey { _adsKey :: String }

-- | Location of the key file
keyFile :: FilePath
keyFile = "adskey.txt"

-- | Read the key from the file `keyFile`.
--
--   Could add in a check to see if the file is group or world readable
--   (failing to load if it is) but leave that for now.
--
getADSKey :: IO (Either String ADSKey)
getADSKey = 
  let toError ioe = "Error reading from " ++ keyFile ++ ": " ++ 
                    ioeGetErrorString ioe
  in catchIOError (do
    cts <- S.readFile keyFile
    return $ case words cts of
      [] -> Left (keyFile ++ " is empty")
      (k:_) -> Right (ADSKey k)  
    ) (return . Left . toError)


usage :: String -> IO ()
usage progName = do
  hPutStrLn stderr ("Usage: " ++ progName ++ " nrows\n")
  hPutStrLn stderr "Grab proposal abstracts and titles from ADS.\n"
  hPutStrLn stderr ("The ADS key is read from the file adskey.txt (the " ++
                    "first word in the file is used).")
  exitFailure
    
main :: IO ()
main = do
  pName <- getProgName
  args <- getArgs
  case args of
    [s] -> case readMaybe s of
      Just nrows -> runQuery nrows
      _ -> usage pName
      
    _ -> usage pName

runQuery :: Int -> IO ()
runQuery nrows = do
  ekey <- getADSKey
  case ekey of
    Left emsg -> putStrLn ("ERROR accessing key: " ++  emsg)
                 >> exitFailure
    Right key -> runSearch nrows key

-- | What is the current setting.
getCurrent :: IO (Maybe ADSSearchMetaData)
getCurrent = 
  let act = select (CondEmpty `orderBy` [Desc AdsCheckedField] `limitTo` 1)
  in listToMaybe <$> runDb act

data ADSQueryLimit = ADSQueryLimit { qlLimit :: Int
                                   , qlRemaining :: Int
                                   , qlReset :: Int }

-- | What are the query limits as reported by ADS?
getADSLimits :: ResponseHeaders -> Either String ADSQueryLimit
getADSLimits hdrs =
  let get k = maybe (Left (show k)) Right (lookup k hdrs)
              >>= eread
      eread bs =
        let s = B8.unpack bs
        in maybe (Left s) Right (readMaybe s)
  in ADSQueryLimit
     <$> get "X-RateLimit-Limit"
     <*> get "X-RateLimit-Remaining"
     <*> get "X-RateLimit-Reset"

     
runSearch :: Int -> ADSKey -> IO ()
runSearch nrows key = do

  mcurrent <- getCurrent
  let start = maybe 0 adsCurrentCount mcurrent

  putStrLn ("DBG: start = " ++ show start)
  putStrLn ("DBG: nows  = " ++ show nrows)
  
  let req = parseRequest_ "https://api.adsabs.harvard.edu/v1/search/query"

      -- add in ADS authorization
      ohdrs = requestHeaders req
      authorize = ("Authorization", "Bearer:" <> keyBS)
      keyBS = B8.pack (_adsKey key)
      nhdrs = authorize : ohdrs
      req1 = req { requestHeaders = nhdrs }

      -- Note: the keyword field is expected to contain
      -- "Chandra Proposal ID #<n>". However, not all records
      -- have a keyword field - e.g. 2000cxo..prop.3946W
      -- I have seen that some may have it in publication (although
      -- this may actually be a feature of the display - e.g.
      -- https://ui.adsabs.harvard.edu/#abs/2000cxo..prop..720C/abstract
      -- since publication doesn't seem to work as a field name
      -- here (in that it doesn't add any extra field).
      --
      -- So, stick with keyword and skip those for which it fails
      -- (note that 2000cxo..prop.3946W doesn't seem to have any
      -- data attached to it).
      --
      -- I may have to do a match on proposal title.
      --
      -- fieldNames = "title,abstract,bibcode,keyword,author"
      fieldNames = "title,abstract,bibcode,keyword"

      req2 = setQueryString qopts req1 
      qopts = [ ("q", Just bibstemBS)
              , ("start", Just startBS)
              , ("rows", Just rowBS)
              , ("fl", Just fieldNames)
              ]
      startBS = B8.pack (show start)
      rowBS = B8.pack (show nrows)
      bibstemBS = "bibstem:cxo..prop"

  -- is this sufficient to get TLS working?
  mgr <- newManager tlsManagerSettings
  withHTTP req2 mgr $ \resp -> do
    case getADSLimits (responseHeaders resp) of
      Right ADSQueryLimit {..} -> do
        putStrLn "# ADS limits"
        putStrLn ("  number of queries = " ++ show qlLimit)
        putStrLn ("  number remaining  = " ++ show qlRemaining)
        -- TODO: should convert the time to a human-readable value
        putStrLn ("  reset time        = " ++ show qlReset)
      Left emsg -> 
        putStrLn ("Unable to parse ADS limits: " ++ emsg)

    mres <- evalStateT (parse value') (responseBody resp)
    case mres of
      Just (Right json) -> storeResponses json start
      Just (Left pe) -> putStrLn ("Failed parsing: " ++ show pe)
                        >> exitFailure
      _ -> putStrLn "Failed somewhere!" >> exitFailure

-- May not need start and/or nrows in here
--
-- This does not record any invalid abstracts (i.e. those that
-- can not be parsed) since this would have caused the JSON
-- parsing to fail, so we wouldn't even have got here.
--
storeResponses :: Value -> Int -> IO ()
storeResponses json start = 
  case AT.fromJSON json of
    AT.Error emsg -> do
      hPutStrLn stderr ("Error: " ++ emsg)
      hPutStrLn stderr "JSON="
      hPrint stderr json
      exitFailure
      
    AT.Success ADSResponse {..} -> do
      let nread = length adsEntities
          newCount = start + nread
          remaining = adsCount - newCount
          
      putStrLn ("Read in " ++ show nread ++ " records")
      putStrLn ("Total number of records in ADS = " ++ show adsCount)

      -- This number should not be negative, but allow it (as a visual
      -- cue that something has gone awry). It would also be good
      -- to compare about the ADS limits, but they have already
      -- been reported, so for now leave at that.
      --
      when (remaining /= 0)
        (putStrLn (" -> remaining to query = " ++ show remaining))

      -- number the results (only needed for failure but need
      -- to do this to all results).
      let res = zip [start..] adsEntities

          split (n, Left emsg) = Left (n, emsg)
          split (n, Right x) = Right (n, x)
            
          (failures, success) = partitionEithers (map split res)

      nfail0 <- runDb (countAll (undefined :: InvalidProposalAbstract))
        
      -- Since there is a constraint that the proposal number is unique
      -- in this table in the database, the following will error out if
      -- there is any conflict. Leave for now, as it's unclear what
      -- the cause of this conflict could be, so not worth trying to
      -- handle as an error case.
      --
      -- Note that we update the metadata even if no data was added,
      -- just to get the last-modified date updated.
      --
      now <- getCurrentTime

      -- We need a mapping between proposal title and number
      --
      propInfo <- runDb (project (upper PropNameField, PropNumField)
                         CondEmpty)

      let propMap = M.fromList propInfo

          dbAct = do

            mapM_ (handleSuccess propMap now) success
            mapM_ (handleFailure now) failures

            let new = ADSSearchMetaData { adsMaxCount = adsCount
                                        , adsCurrentCount = newCount
                                        , adsChecked = now }
            deleteAll (undefined :: ADSSearchMetaData)
            insert_ new
            
      runDb dbAct

      -- Since a "successful" 
      nfail1 <- runDb (countAll (undefined :: InvalidProposalAbstract))

      let nfail = nfail1 - nfail0
          n = length failures
          
      when (nfail /= 0)
        (putStrLn (" -> there were " ++ show nfail ++
                   " failed records"))
      when (nfail /= n)
        (putStrLn (" -> of these, " ++ show (nfail - n) ++
                   " were repeat proposals"))


-- | Note: this checks to see if the proposal is already known about
--   and, if it is, marks this one as being invalid.
--
--   There are two cases:
--       have a proposal number
--       only have a title
--
handleSuccess ::
  PersistBackend m
  => M.Map T.Text PropNum
  -- ^ Map from proposal title (in upper case) to proposal number
  -> UTCTime
  -- ^ approximate time of the query
  -> (Int, ADSEntity)
  -- ^ record number and proposal
  -> m ()
handleSuccess propMap now (index, ads) =
  let findProp = M.lookup (T.toUpper title) propMap

      title = adsTitle ads
      bibStr = show (fromBibCode (adsBibCode ads))

      adsNum = fromPNW <$> adsPropNum ads
      
  in case adsNum <|> findProp of
    Just pnum -> do
      mabs <- listToMaybe <$> select (PaNumField ==. pnum)
      case mabs of
        Nothing -> insert_ (convert pnum ads)
        Just ProposalAbstract {..} ->
          let emsg = "Already exists!" ++
                     " Proposal: " ++ show (fromPropNum pnum) ++
                     " BibCode: " ++ bibStr ++
                     " Title same: " ++
                     show (adsTitle ads == paTitle) ++
                     " Abstract same: " ++
                     show (adsAbstract ads == paAbstract)
                         
          in handleFailure now (index, T.pack emsg)

    Nothing ->
      let emsg = "No match for title=" <> title <>
                 " BibCode: " <> T.pack bibStr
      in handleFailure now (index, emsg)


handleFailure ::
  PersistBackend m
  => UTCTime
  -- ^ approximate time of the query
  -> (Int, T.Text)
  -- ^ record number and reason for failure
  -> m ()
handleFailure ipChecked (ipADSIndex, ipMessage) =
  insert_ InvalidProposalAbstract {..}

  
-- Could have just used ProposalAbstract, but didn't want to have
-- a FromJSON instance for it lying around. Also, since using a
-- newtype around PropNum need to unwrap it
--
convert :: PropNum -> ADSEntity -> ProposalAbstract
convert pnum ADSEntity {..} =
  ProposalAbstract
  { paNum = pnum
  , paBibCode = adsBibCode
  , paTitle = adsTitle
  , paAbstract = adsAbstract }
  
{-

The ADS JSON response is expected to look like (fl and returned fields may be
slightly different)

{
  "responseHeader": {
    "status":0, "QTime":42, "params":{"q":"bibstem:cxo..prop","fl":"abstract,bibcode,author,data,doi,keyword,orcid_pub,orcid_user,orcid_other,title,vizier","start":"1","rows":"1","wt":"json"}
  },
  "response": {
    "numFound":4441,
    "start":1,
    "docs":[ {
      "title":["ACIS Observations of Jupiter"],
      "abstract":"The intent of these 4 ACIS observations of Jupiter is to investigate the best strategy for carrying out solar system observations which are complicated by the sensitivity of the ACIS-S array to red light. The first of these observations will aquire a bias map with Jupiter outside of the field of view of ACIS-S. The following three observations will be carried out with different instrument settings (primarly the event and split thresholds) and no bias map. A slew must be perfomed between all four observations to locate Jupiter on the detector since it will drift during the required 3 ksec exposures. The exact pointings will be filled in at a later time once the the observing date is known.",
      "data":["CXO"],
      "bibcode":"2000cxo..prop..720C",
      "keyword":["Chandra Proposal ID #02108032"],
      "author":["CXC Calibration"],
      "orcid_pub":["-"]}]}}

-}

-- | Represent a single abstract/record.
--
--   It appears that some abstracts can be missing; is this true for
--   Chandra? Hopefully not.
--
--   Note this data structure is specialized to Chandra abstracts.
--
data ADSEntity = ADSEntity {
    adsBibCode :: BibCode
    , adsPropNum :: Maybe PropNumWrapper
      -- ^ can not guarantee that the ADS response will contain this
      --   information
    , adsTitle :: T.Text
    , adsAbstract :: T.Text
    }

-- | This is a *highly specialized* instance! The data is expected to
--   be encoded in a string of the form "Chandra Proposal ID #02108032"
--   and we want the numeric value, stripping the leading zeros.
--
--   Would be nice to give a reason for failing
--

-- Use a newtype to avoid orphan instance
newtype PropNumWrapper = PNW { fromPNW :: PropNum }

instance FromJSON PropNumWrapper where
  parseJSON = withText "PropNumWrapper" $ \s ->
    case T.uncons (T.dropWhile (/= '#') s) of
      Just (_, r) | T.null r -> mzero
                  | otherwise -> case toPropNumStr r of
                    Just p -> pure (PNW p)
                    _ -> mzero
      _ -> mzero


instance FromJSON ADSEntity where
  parseJSON = withObject "ADSEntity" $ \o ->
    ADSEntity <$> (toBibCode <$> o .: "bibcode")
              <*> ((head <$>) <$> (o .:? "keyword"))
              -- assume first entry from list, but it's Maybe [a] so
              -- we need to fmap it twice
              <*> (head <$> o .: "title")
              -- assume first entry from list
              <*> o .: "abstract"
  
-- | Extract all the data from an ADS response.
--
data ADSResponse = ADSResponse
                   { adsCount :: Int
                     -- ^ The number of records found matching the
                     --   search
                     , adsEntities :: [Either T.Text ADSEntity]
                   }

-- I want to record information on why a record failed
--
instance FromJSON ADSResponse where
  parseJSON = withObject "ADSResponse" $ \o -> do
    resp <- o .: "response"
    docs <- resp .: "docs"

    nfound <- resp .: "numFound"

    let switch v = case AT.fromJSON v of
          AT.Error emsg -> Left (T.pack emsg)
          AT.Success xs -> Right xs
        responses = map switch docs

    pure (ADSResponse nfound responses)

