{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-

Usage:
  getproposals [nmax]

    nmax defaults to 20

Aim:

Use the CXC interface to get the proposal abstract:

  http://cda.cfa.harvard.edu/srservices/propAbstract.do

with obsid=xxx, propNum=xxx, or both. The response is HTML.

The earlier approach used the NASA ADS service, but it ended up being
more complicated than I wanted (getting at the data is harder since
it is based on bibcode, which is not a value tracked by ocat).

It looks like leading 0's need to be given if the propNum argument
is given, since

 http://cda.cfa.harvard.edu/srservices/propAbstract.do?propNum=2200112

fails, but

 http://cda.cfa.harvard.edu/srservices/propAbstract.do?propNum=02200112

succeeds.

Note that, unlike ADS, propNum=02200112 has the title
"HRC-I CALIBRATION OBSERVATIONS OF ARLAC", whereas ADS has both
"Hrc-I Calibration Observations of Arlac" and
"Hrc-S Calibration Observations of Arlac".

-}

module Main where

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad (forM_)

import Database.Groundhog.Postgresql (Cond(CondEmpty), Order(Asc)
                                     , orderBy
                                     , insert_
                                     , project)

import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8')
import Data.Time (getCurrentTime)

import Formatting (int, sformat)

import Network.HTTP.Conduit
import Numeric.Natural

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.HTML.TagSoup (innerText
                         , isTagOpenName
                         , parseTags
                         , partitions)
import Text.Read (readMaybe)

import Database (runDb, updateLastModified)
import Types (ProposalAbstract(..), MissingProposalAbstract(..)
              , PropNum(..)
              , fromPropNum
              , Field(PaNumField, PropNumField, MpNumField)
              )


baseLoc :: String
baseLoc = "http://cda.cfa.harvard.edu/srservices/propAbstract.do"

showInt :: Int -> T.Text
showInt = sformat int

usage :: String -> IO ()
usage progName = do
  hPutStrLn stderr ("Usage: " ++ progName ++ " [nmax]\n")
  hPutStrLn stderr "Grab proposal abstracts and titles from the CXC.\n"
  hPutStrLn stderr "The default for nmax is 20."
  exitFailure

-- What are the missing proposals?
--
missingProposals ::
  Natural
  -- ^ Maximum number of proposals to return
  -> IO [PropNum]
  -- ^ This is ordered by proposal number, but is for the
  --   highest proposal numbers present.
missingProposals nmax = do

  let dbAct = do
        -- paNum and propNum are unique, so no need to worry about
        -- duplicates in these queries
        propHave <- let p = PaNumField
                    in project p (CondEmpty `orderBy` [Asc p])
        propAll <- let p = PropNumField
                   in project p (CondEmpty `orderBy` [Asc p])
        propNoData <- let p = MpNumField
                      in project p (CondEmpty `orderBy` [Asc p])
        return (propAll, propHave, propNoData)

  (pAll, pHave, pNoData) <- runDb dbAct
  
  -- let Set sort this out
  let sAll = S.fromDistinctAscList pAll
      sHave = S.fromDistinctAscList pHave
      sNoData = S.fromDistinctAscList pNoData
      s = sHave `S.union` sNoData
      wanted = S.toAscList (sAll `S.difference` s)

  let nmiss = length wanted
      ndrop = nmiss - fromIntegral nmax
  T.putStrLn ("There are " <> showInt nmiss <> " missing abstracts.")
  return (drop ndrop wanted)

  
{-
Identify those proposals which do not have an abstract, and then
query CXC for them.
-}
runSearch :: Natural -> IO ()
runSearch nmax = do

  props <- missingProposals nmax
  let nprops = length props
  if nprops == 0
    then T.putStrLn "There are no missing proposals."
    else do
      T.putStrLn ("Found " <> showInt nprops <> " missing proposals")

      flags <- mapM addProposal props
      let nfound = length (filter id flags)

      if nfound == nprops
        then T.putStrLn "  all added"
        else do
          T.putStrLn "  The following were not found:"
          let missing = filter (not . fst) (zip flags props)
          forM_ missing (T.putStrLn . ("    " <>) .
                         showInt . fromPropNum . snd)


addProposal ::
  PropNum
  -- ^ This proposal should not exist in the database
  -> IO Bool
  -- ^ True if the proposal was added
addProposal pnum = do
  
  let req = parseRequest_ baseLoc

      -- add in an identifier
      ohdrs = requestHeaders req
      userAgent = ("User-Agent", "chandraobs dburke@cfa.harvard.edu")
      nhdrs = userAgent : ohdrs
      req1 = req { requestHeaders = nhdrs }

      -- It is easier to query by obsid, as the obsid does not
      -- have to be 0-padded (I think), but leave as the
      -- following for now
      --
      pstr = show (fromPropNum pnum)
      n0 = 8 - length pstr
      propNumBS = B8.pack (replicate n0 '0' <> pstr)
      qopts = [ ("propNum", Just propNumBS) ]
      req2 = setQueryString qopts req1 

  mgr <- newManager tlsManagerSettings
  rsp <- httpLbs req2 mgr

  let rsplbs = responseBody rsp
  
      lbsToText :: L.ByteString -> Either T.Text T.Text
      lbsToText lbs =
        either (Left . T.pack . show) Right (decodeUtf8' (L.toStrict lbs))
  
  case lbsToText rsplbs of
    Right ans -> extractAbstract pnum ans
        
    Left emsg -> do
      T.hPutStrLn stderr ("Problem querying propNum: "
                          <> showInt (fromPropNum pnum))
      T.hPutStrLn stderr emsg
      return False


-- There is no check that the database constraints hold - i.e.
-- it will error out if they do not.
--
extractAbstract ::
  PropNum
  -> T.Text
  -- ^ Assumed to be the HTML response
  -> IO Bool
  -- ^ True if the proposal was added to the database
extractAbstract pnum txt = do
  now <- getCurrentTime
  case findAbstract pnum txt of
    Left emsg -> do
      T.putStrLn ("Failed for " <> showInt (fromPropNum pnum))
      T.putStrLn emsg

      let dbAct = do
            insert_ missingAbs
            updateLastModified now

          missingAbs = MissingProposalAbstract {
            mpNum = pnum
            , mpReason = emsg
            , mpChecked = now
            }
      
      runDb dbAct
      return False

    Right (absTitle, absText) -> do
      {-
      T.putStrLn ("Proposal: " <> showInt (fromPropNum pnum))
      T.putStrLn ("Title: " <> absTitle)
      T.putStrLn absText
      T.putStrLn "----------------------------------------------"
      -}
      
      let dbAct = do
            insert_ newAbs
            updateLastModified now

          newAbs = ProposalAbstract {
            paNum = pnum
            , paTitle = absTitle
            , paAbstract = absText
            }

      runDb dbAct
      return True


-- It is possible for an abstract to be incomplete - e.g.
-- missing either or both of the title and abstract. It is considered
-- an "error" if either is missing (since it is no use here if
-- we just have the title).
--
findAbstract ::
  PropNum
  -> T.Text
  -- ^ Assumed to be the HTML response
  -> Either T.Text (T.Text, T.Text)
  -- ^ On success the return is the proposal title and abstract.
findAbstract pnum txt = do
  let tags = parseTags txt
      findRow = isTagOpenName "tr"
      findCell = isTagOpenName "td"

      rows = partitions findRow tags

      clean = T.strip . innerText
  
      getSecond row = case partitions findCell row of
        [_, td] -> Right (clean td)
        _ -> Left "Expected two cells in row"

      getOne = Right . clean
      
  -- hard code the structure of the table until we find we have to
  -- be more generic
  --
  (title, number, abstract) <- case rows of
        [titleRow, _, numberRow, _, _, _, abstractHeaderRow, abstractRow]
          -> do
          title <- getSecond titleRow
          number <- getSecond numberRow
          header <- getOne abstractHeaderRow
          abstract <- getOne abstractRow
          if header /= "Abstract:"
            then Left "Unable to find abstract header"
            else Right (title, number, abstract)
        _ -> Left "Unexpected number of rows in the table"

  let checkVal l v = if v == "null"
                     then Left ("Missing " <> l)
                     else Right v
                          
  checkAbstract <- checkVal "abstract" abstract
  checkTitle <- checkVal "title" title
  
  let checkNum n | n == fromPropNum pnum = Right (checkTitle, checkAbstract)
                 | otherwise = Left "Proposal number mismatch"
                               
  maybe (Left "Invalid proposal number") checkNum
    (readMaybe (T.unpack number))


main :: IO ()
main = do
  pName <- getProgName
  args <- getArgs
  case args of
    [] -> runSearch 20
    [s] -> case readMaybe s of
      Just nmax -> runSearch nmax
      _ -> usage pName
      
    _ -> usage pName
