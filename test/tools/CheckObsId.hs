{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-   # LANGUAGE DerivingStrategies #   -}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-   # LANGUAGE StandaloneDeriving #   -}
{-   # LANGUAGE TypeApplications #   -}
{-# LANGUAGE TypeFamilies #-}

-- | Report the currently running observation.
--
-- Usage:
--    ./checkobsid
--

{-

Access a subset of the observation fields.

-}
module Main (main) where

import qualified Data.ByteString.Char8 as B8

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Hasql.Session as S

import qualified Rel8 as R

import Control.Monad (guard, when)

import Data.Foldable (forM_)
import Data.Function (on)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int32, Int64)
import Data.List (group, sort, uncons)
import Data.Maybe (fromJust)
import Data.Time (UTCTime, getCurrentTime)

import Formatting
import Formatting.Time

import GHC.Generics (Generic)

import Hasql.Connection (Connection, Settings, acquire, release, settings)

import Network.URI (URI(..)
                   , URIAuth(..)
                   , parseAbsoluteURI
                   )
                     
import Rel8 hiding (filter)

import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (exitFailure)
import System.IO (stderr)



instance DBType ObsIdStatus where
  typeInformation = parseTypeInformation fromOIS toOIS typeInformation
    where
      fromOIS t = case toObsIdStatus t of
        Just v -> Right v
        _ -> Left ("Unknown ObsIdStatus: '" <> T.unpack t <> "'")

      toOIS = fromObsIdStatus


instance DBType Grating where
  typeInformation = parseTypeInformation fromG toG typeInformation
    where
      fromG t = case toGrating t of
        Just v -> Right v
        _ -> Left ("Unknown Grating: '" <> T.unpack t <> "'")

      toG = fromGrating


usage :: IO ()
usage = do
  pName <- T.pack <$> getProgName
  T.hPutStrLn stderr ("Usage: " <> pName)
  exitFailure


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> queryDB
    _ -> usage


-- See also Web.Heroku. Can we just not use the URL directly?
--
separateDatabase :: String -> Maybe Settings
separateDatabase = go
  where
    getPort (':':port) = Just port
    getPort _ = Nothing

    -- this is only in Data.List from base 4.19.0.0, so use
    -- the example from the documentation, as we expect the
    -- input to be finite.
    unsnoc xs = (\(hd, tl) -> (reverse tl, hd)) <$> uncons (reverse xs)

    isColon = (== ':')
    getUserInfo uinfo =
      let (user, rest) = break isColon uinfo
      in do
        guard (user /= "")
        (_, rest2) <- uncons rest
        (password, lastchar) <- unsnoc rest2
        guard (password /= "")
        guard (lastchar == '@')
        pure (user, password)

    getDb path = do
      (a, b) <- uncons path
      guard (a == '/')
      guard (b /= "")
      pure b

    go env = do
      uri <- parseAbsoluteURI env
      
      guard (uriScheme uri == "postgres:")
      auth <- uriAuthority uri
      (user, password) <- getUserInfo (uriUserInfo auth)
      port <- getPort (uriPort auth)
      db <- getDb (uriPath uri)

      let hostBS = B8.pack (uriRegName auth)
          userBS = B8.pack user
          portW = read port
          passBS = B8.pack password
          dbaseBS = B8.pack db
          
      pure (settings hostBS portW userBS passBS dbaseBS)
      

quickError :: T.Text -> IO a
quickError emsg = T.hPutStrLn stderr ("ERROR: " <> emsg) >> exitFailure


getSettings :: IO Settings
getSettings = do
  murl <- lookupEnv "DATABASE_URL"
  case murl of
    Just url -> case separateDatabase url of
      Just x -> pure x
      _ -> quickError ("invalid DATABASE_URL=" <> T.pack url)
           
    _ -> quickError "no DATABASE_URL environment variable"
    


queryDB :: IO ()
queryDB = do
  nowT <- getCurrentTime
  T.putStrLn ("The current time is: " <> T.pack (show nowT))
  dbSettings <- getSettings
  resp <- acquire dbSettings
  case resp of
    Right conn -> runDb nowT conn
    Left (Just emsg) -> quickError (T.pack (B8.unpack emsg))
    _ -> quickError "Unable to connect to database"


runDb :: UTCTime -> Connection -> IO ()
runDb nowT conn = do
  reportSize conn
  T.putStrLn "------"
  -- reportObsId conn
  -- T.putStrLn "------"
  -- reportSubArrays conn
  -- T.putStrLn "------"
  findLatestScienceObs conn nowT
  T.putStrLn "------"
  findLatestNonScienceObs conn nowT
  T.putStrLn "------"
  getNextObs conn (ObsIdVal 28846) nowT
  T.putStrLn "------"
  missingProposals conn
  release conn


-- | A valid science observation is one which has a start time.
--   There should be no observations which have no start time
--   and are not either discarded or canceled, but do not
--   rely on that for now.
--
isValidScienceObs :: ScienceObs Expr -> Expr Bool
isValidScienceObs sobs =
  isNonNull (soStartTime sobs)
  &&. not_ (soStatus sobs `in_` [lit Discarded, lit Canceled])
  
isValidNonScienceObs :: NonScienceObs Expr -> Expr Bool
isValidNonScienceObs obs =
  isNonNull (nsStartTime obs)
  &&. not_ (nsStatus obs `in_` [lit Discarded, lit Canceled])
  

showT :: Show a => a -> T.Text
showT = T.pack . show
        
        
displayScienceObs :: (ScienceObs Result, ProposalAbstract Result) -> IO ()
displayScienceObs (sobs, pabs) = 
  let tval = case soStartTime sobs of
               Just tks -> showCTime tks
               _ -> "no start date"
            
  in do
    T.putStrLn ("ObsId: "
                 <> T.pack (show (fromObsId (soObsId sobs)))
                 <> "  MultiTel: "
                 <> (if soMultiTel sobs then "T" else "F")
                 <> "  Status: "
                 <> fromObsIdStatus (soStatus sobs) 
                 <> "  Grating: "
                 <> fromGrating (soGrating sobs)
                 <> "  Target: "
                 <> fromTargetName (soTarget sobs)
                 <> "  "
                 <> tval
                 -- <> "  "
                 -- <> "'" <> fromTelescope (soMultiTelObs sobs) <> "'"
               )
      
    let mSizes = do
          sstart <- soSubArrayStart sobs
          ssize <- soSubArraySize sobs
          pure (sstart, ssize)

    case mSizes of
      Just (sstart, ssize) -> T.putStrLn ("  : start= " <> showT sstart <> " end= " <> showT ssize)
      _ -> pure ()
      
    T.putStrLn ("  : " <> paTitle pabs)
  

displayNonScienceObs :: NonScienceObs Result -> IO ()
displayNonScienceObs obs = 
  let tval = case nsStartTime obs of
               Just tks -> showCTime tks
               _ -> "no start date"
            
  in do
    T.putStrLn ("ObsId: "
                 <> T.pack (show (fromObsId (nsObsId obs)))
                 <> "  Status: "
                 <> fromObsIdStatus (nsStatus obs) 
                 <> "  RA: "
                 <> showT (fromRA (nsRA obs))
                 <> "  Dec: "
                 <> showT (fromDec (nsDec obs))
                 <> "  "
                 <> tval
               )
      

abstractForObs :: ScienceObs Expr -> Query (ProposalAbstract Expr)
abstractForObs obs =
  let f = \pa -> paNum pa ==. soProposal obs
  in each proposalAbstractSchema >>= R.filter f


findLatestScienceObs :: Connection -> UTCTime -> IO ()
findLatestScienceObs conn t = do
  let tval = Just (toChandraTime t)

  let query1 = limit 1
               $ orderBy (soStartTime >$< nullsLast desc)
               $ do
        obs <- each scienceObsSchema
        where_ $ isValidScienceObs obs
        where_ $ soStartTime obs <=. lit tval
        pure obs

      query2 obs = do
        pabs <- abstractForObs obs
        pure (obs, pabs)

      query = query1 >>= query2
      
  -- putStrLn $ "QUERY: " <> showQuery query

  res <- S.run (S.statement () (run1 (select query))) conn
  case res of
    Right ans -> displayScienceObs ans
    Left err -> quickError ("Database error: " <> T.pack (show err))


findLatestNonScienceObs :: Connection -> UTCTime -> IO ()
findLatestNonScienceObs conn t = do
  let tval = Just (toChandraTime t)

  let query = limit 1
              $ orderBy (nsStartTime >$< nullsLast desc)
              $ do
        obs <- each nonScienceObsSchema
        where_ $ isValidNonScienceObs obs
        where_ $ nsStartTime obs <=. lit tval
        pure obs

  -- putStrLn $ showQuery query

  res <- S.run (S.statement () (run1 (select query))) conn
  case res of
    Right ans -> displayNonScienceObs ans
    Left err -> quickError ("Database error: " <> T.pack (show err))


-- do some projecting or aggregating
reportSubArrays :: Connection -> IO ()
reportSubArrays conn = do

  {- simple project
  let query = do
        obs <- each scienceObsSchema
        where_ $ isNonNull (soSubArrayStart obs)
        where_ $ isNonNull (soSubArraySize obs)
        pure (unsafeUnnullify (soSubArrayStart obs)
             , unsafeUnnullify (soSubArraySize obs))

      display (a, b) = T.putStrLn $ "  " <> showT a <> " " <> showT b
  -}

  -- can we aggregate?
  let query = aggregate1 aggRows selObs
      selObs = do
        obs <- each scienceObsSchema
        where_ $ isNonNull (soSubArrayStart obs)
        where_ $ isNonNull (soSubArraySize obs)
        pure (unsafeUnnullify (soSubArrayStart obs)
             , unsafeUnnullify (soSubArraySize obs))

      -- aggRows :: _
      aggRows = orderAggregateBy (fst >$< asc) $
        do
          starts <- groupByOn fst
          -- get a list of the sizes
          sizes <- listAggExprOn snd
          -- group the sizes
          -- sizes <- groupByOn snd --- WAT
          pure (starts, sizes)

      -- don't we have this already?
      -- can we not do this in the aggregation?
      countify xs = let grps = group (sort xs)
                        f l@(x:_) = (x, length l)
                    in map f grps
      
      display (start, sizes) =
        let counts = countify sizes
        in T.putStrLn $ "  " <> showT start <> ": " <> showT counts

  res <- S.run (S.statement () (run (select query))) conn
  case res of
    Right ans -> forM_ ans display
    Left err -> quickError ("Database error: " <> T.pack (show err))


-- I wonder if this could be simplified by just using the Maybe Ord
-- instance (ie do not need to special case the options)? Does
-- Nothing behave the way we would want in this scenario?
--
identifyHelper ::
  Ord c
  => (a -> c)
  -> (b -> c)
  -> (Maybe c -> Maybe c -> Bool)
  -- ordering option; if True pick the first (a), otherwise
  -- b
  -> Maybe a
  -> Maybe b
  -> Maybe (Either b a)
identifyHelper _ _ _ Nothing Nothing = Nothing
identifyHelper _ _ _ (Just x) Nothing = Just (Right x)
identifyHelper _ _ _ Nothing (Just y) = Just (Left y)
identifyHelper px py ord mx my =
  -- rely on Ord instance of Maybe for the comparison
  let ox = px <$> mx
      oy = py <$> my
  in if ord ox oy then Right <$> mx else Left <$> my


-- | Represent an entry in the schedule; this is
--   a "catch-all" type that is used as I play around with the
--   database.
--
type Record = Either (NonScienceObs Result) (ScienceObs Result)


identifyEarliestRecord ::
  Maybe (ScienceObs Result)
  -> Maybe (NonScienceObs Result)
  -> Maybe Record
identifyEarliestRecord = identifyHelper soStartTime nsStartTime (<)


getNextObs ::
  Connection
  -> ObsIdVal
  -> UTCTime
  -> IO ()
getNextObs conn oit t = do
  let tval = Just (toChandraTime t)

  let query1 = limit 1
               $ orderBy (soStartTime >$< nullsLast asc)
               $ do
        obs <- each scienceObsSchema
        where_ $ soObsId obs /=. lit oit
        where_ $ isValidScienceObs obs
        where_ $ soStartTime obs >. lit tval
        pure obs

      query2 = limit 1
               $ orderBy (nsStartTime >$< nullsLast asc)
               $ do
        obs <- each nonScienceObsSchema
        where_ $ nsObsId obs /=. lit oit
        where_ $ isValidNonScienceObs obs
        where_ $ nsStartTime obs >. lit tval
        pure obs

  q1 <- S.run (S.statement () (runMaybe (select query1))) conn
  q2 <- S.run (S.statement () (runMaybe (select query2))) conn
  let res = do
        a <- q1
        b <- q2
        pure (a, b)
        
  case res of
    Right (mobs, mns) -> displayRecord (identifyEarliestRecord mobs mns)
    Left err -> quickError ("Database error: " <> T.pack (show err))


displayRecord :: Maybe Record -> IO ()
displayRecord Nothing = T.putStrLn "No **next** observation found"
displayRecord (Just (Left ns)) = T.putStrLn ("**next**: non science " <> showT (fromObsId (nsObsId ns)))
displayRecord (Just (Right obs)) = T.putStrLn ("**next**: science " <> showT (fromObsId (soObsId obs)))

{-

getObsInRange ::
  Connection
  -> ChandraTime
  -> ChandraTime
  -> IO ()
getObsInRange conn tStart tEnd = do

  let t0 = fromChandraTime tStart
      maxlen = 250 * 1000 :: Double
      delta = (fromRational . toRational . negate) maxlen
      minStartTime = toChandraTime (addUTCTime delta t0)

      query1 = do

argh, many moving parts here ...
        
  xs1 <- fromSL <$> fetchScienceObsBy (SoStartTimeField <. Just tEnd &&.
                                       SoStartTimeField >=. Just minStartTime)

  ys1 <- project restrictedNonScience
         (((NsStartTimeField <. Just tEnd) &&.
           (NsStartTimeField >=. Just minStartTime) &&.
           notNsDiscarded)
          `orderBy` [Asc NsStartTimeField])

-}


{-
reportObsId :: Connection -> IO ()
reportObsId conn = do
  let query = limit 5 $ do
        row <- each scienceObsSchema
        where_ $ soStatus row ==. lit Discarded
        -- where_ $ soMultiTel row
        -- where_ $ soMultiTelObs row /=. lit (Telescope "") hmmm
        pure row
  
  res <- S.run (S.statement () (run (select query))) conn
  case res of
    Right ans -> forM_ ans displayScienceObs
    Left err -> quickError ("Database error: " <> T.pack (show err))

-}


missingProposals :: Connection -> IO ()
missingProposals conn = do
  let queryAll = do
        row <- each proposalSchema
        pure (propNum row)

      queryHave = do
        row <- each proposalAbstractSchema
        pure (paNum row)

      queryNoData = do
        row <- each missingProposalAbstractSchema
        pure (mpNum row)

      query = queryAll `except` (queryHave `union` queryNoData)

  -- putStrLn $ "QUERY: " <> showQuery query
  
  res <- S.run (S.statement () (run (select query))) conn
  case res of
    Right missing -> do
      T.putStrLn ("Found " <> showT (length missing) <> " missing proposal abstracts")
    Left err -> quickError ("Database error: " <> T.pack (show err))
      

{-

showSize ::
  (DbIO m, PersistEntity v)
  => T.Text
  -> v
  -> m Int
showSize l t = do
  n <- countAll t
  putIO (sformat ("Number of " % stext % " : " % int) l n)
  pure n


reportSize :: DbIO m => m Int
reportSize = do
  -- n1 <- showSize "scheduled items  " (undefined :: ScheduleItem)
  n2 <- showSize "science obs      " (undefined :: ScienceObs)
  n3 <- showSize "non-science obs  " (undefined :: NonScienceObs)
  n4 <- showSize "proposals        " (undefined :: Proposal)
  n5 <- showSize "SIMBAD match     " (undefined :: SimbadMatch)
  n6 <- showSize "SIMBAD no match  " (undefined :: SimbadNoMatch)
  n7 <- showSize "SIMBAD info      " (undefined :: SimbadInfo)
  -- n8 <- showSize "overlap obs      " (undefined :: OverlapObs)

  let n1 = 0 -- TODO: remove this
      n8 = 0
      
  nbad <- showSize "invalid obsids   " (undefined :: InvalidObsId)

  -- break down the status field of the observations
  putIO ""
  ns <- findObsStatusTypes
  let field = right 10 ' '
  forM_ ns (\(status, n) ->
             let txt = fromObsIdStatus status
             in putIO (sformat ("  status=" % field % "  : " % int) txt n))
  putIO (sformat (" -> total = " % int) (sum (map snd ns)))
  putIO ""

  -- non-science breakdown
  -- ns1 <- count notFromObsCat
  -- putIO (sformat ("  non-science (not from obscat) = " % int) ns1)
  ns2 <- count (Not notNsDiscarded)
  putIO (sformat ("  non-science discarded         = " % int) ns2)
  
  let ntot = sum [n1, n2, n3, n4, n5, n6, n7, n8, nbad]
  putIO ""
  putIO (sformat ("Number of rows              : " % int) ntot)
  pure ntot

-}

{-

so, Query doesn't have a IO escape hatch, unlilke Groundhog.

showSize l t = do
  n <- countRows (each t)
  putIO (sformat ("Number of " % stext % " : " % int) l n)
  pure n


-}


getSize :: Query (Expr Int64, Expr Int64, Expr Int64, Expr Int64)
getSize = do
  n2 <- countRows (each scienceObsSchema)
  n4b <- countRows (each proposalAbstractSchema)

  nsub <- countRows
          $ do
    obs <- each scienceObsSchema
    where_ $ isNonNull (soSubArrayStart obs)
    where_ $ isNonNull (soSubArraySize obs)
    pure obs
  nnosub <- countRows
          $ do
    obs <- each scienceObsSchema
    where_ $ isNull (soSubArrayStart obs)
    where_ $ isNull (soSubArraySize obs)
    pure obs

  pure (n2, n4b, nsub, nnosub)

showSize :: T.Text -> Int64 -> IO ()
showSize l n = T.putStrLn (sformat ("Number of " % stext % " : " % int) l n)

reportSize :: Connection -> IO ()
reportSize conn = do
  let stmt = run1 (select getSize)
  mnum <- S.run (S.statement () stmt) conn
  case mnum of
    Right (n2, n4b, nsub, nnosub) -> do
      showSize "science obs        " n2
      showSize "proposal abstract  " n4b
      showSize "sub-arrays         " nsub
      showSize "no sub-arrays      " nnosub
      when (n2 /= (nsub + nnosub)) $
        T.putStrLn "WARNING: sub array mis-match"
      
    Left err -> quickError ("Database error: " <> T.pack (show err))
