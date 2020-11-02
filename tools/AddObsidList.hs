{-# LANGUAGE FlexibleContexts #-}
{-# Language OverloadedStrings #-}

--
-- Usage:
--    ./addobsidlist <filename>
--
-- Aim:
--
-- Add the obsids in filename to the database. This is a text file
-- with a list of obsids, one per line (blank and lines beginning with
-- # are skipped). It is assumed that the obsids are not already
-- in the database, but they are skipped if they are. The idea is that
-- this lets obsids not in a short-term schedule be added.
-- 
-- It also does *not* query Simbad for any information on the
-- observations.
--

module Main (main) where

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (liftIO)

import Data.Either (partitionEithers)
import Data.Time (getCurrentTime)

import Database.Groundhog (PersistBackend
                          , (==.)
                          , delete
                          , project)
import Database.Groundhog.Postgresql (SqlDb, Conn, in_, insert_)

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Database (runDb, getInvalidObsIds, updateLastModified
                 , insertProposal)
import OCAT (OCAT, isScienceObsE
            , queryOCAT, ocatToScience, ocatToNonScience
            , showInt)

import Types (ObsIdVal(..), InvalidObsId(..)
             , Field(NsObsIdField, SoObsIdField, IoObsIdField)
             , Proposal(propNum)
             , fromPropNum
             , toObsIdValStr)


type OCATMap = [(ObsIdVal, Maybe OCAT)]


-- | Split a list into two.
--
splitWhen_ ::
  (a -> Either b c)
  -> [a]
  -> ([b], [c])
splitWhen_ f xs = partitionEithers (map f xs)


-- | Split a list into two.
--
splitWhen ::
  (a -> Bool)
  -> [a]
  -> ([a], [a]) -- ^ Those items which fail and pass the test
splitWhen f = 
  let cond x = if f x then Right x else Left x
  in splitWhen_ cond


-- | report an obsid value.
--
showObsId :: ObsIdVal -> IO ()
showObsId = putStrLn . ("  " <> ) . show . fromObsId


-- | This does *NOT* error out if an ocat can not be processed; instead
--   we carry on, displaying a message to stderr, and dropping this ObsId.
--
processScience ::
  ObsIdVal
  -> OCAT
  -> IO Bool
  -- ^ Was the obsid added?
processScience oi ocat = 
  let act now sobs prop = do
        delete (IoObsIdField ==. oi)
        -- do not check if the observation is already known about
        -- because it shouldn't, relying on the DB constraint to
        -- let us know if something is wrong (or has changed in the
        -- database).
        insert_ sobs
        flag <- insertProposal prop
        updateLastModified now
        return flag

      obsid = showInt (fromObsId oi)

  in do
     ans <- ocatToScience ocat
     case ans of
       Right (prop, sobs) -> do
         now <- getCurrentTime
         flag <- runDb (act now sobs prop)
         T.putStrLn ("Added science observation " <> obsid)
         when flag (T.putStrLn ("Also added proposal " <>
                                showInt (fromPropNum (propNum prop))))
         return True
         
       Left emsg -> do
         T.hPutStrLn stderr ("ERROR converting science obsid " <> obsid)
         T.hPutStrLn stderr emsg
         -- exitFailure
         return False
  

-- This doesn't quite match GteSchedule, since in getschedule we have
-- the short-term schedule, so can add in the expected start time
-- from that (if it is missing)
--
processEngineering :: ObsIdVal -> OCAT -> IO Bool
processEngineering oi ocat =
  let act now ns = do
        delete (IoObsIdField ==. oi)
        -- do not check if the observation is already known about
        -- because it shouldn't, relying on the DB constraint to
        -- let us know if something is wrong (or has changed in the
        -- database).
        insert_ ns
        updateLastModified now

      obsid = showInt (fromObsId oi)
      
  in case ocatToNonScience ocat of
    Left emsg -> do
      T.hPutStrLn stderr ("ERROR converting engineering obsid " <> obsid)
      T.hPutStrLn stderr emsg
      -- exitFailure
      return False

    Right ns -> do
      now <- getCurrentTime
      runDb (act now ns)
      T.putStrLn ("Added engineering observation " <> obsid)
      return True

    
-- | Return those items that need querying from OCAT.
--
--   Screen messages (to stdout) indicate those obsids that are
--   to be skipped because they are marked as invalid (i.e. unable
--   to parse the OCAT response) or are already in the database.
--
findUnknownObs ::
  (PersistBackend m, SqlDb (Conn m))
  => [ObsIdVal]
  -- ^ The set of observations we are interested in. It is assumed that
  --   this is a "small" number (value intentionally not defined).
  -> m [ObsIdVal]
  -- ^ The observations to query
findUnknownObs obsids = do

  let put = liftIO . putStrLn
      
  -- report any "invalid" obsids
  invalids <- fmap ioObsId <$> getInvalidObsIds
  let invalidSet = Set.fromList invalids
      checkValid = (`Set.notMember` invalidSet)
      (invalid, valid) = splitWhen checkValid obsids

  case invalid of
    [] -> return ()
    _ -> do
      put "# Skipping the following invalid obsids:"
      mapM_ (liftIO . showObsId) invalid
      put ""

  -- skip observations already in the database; the assumption is that
  -- these are all science observations, but need to check the
  -- non-science list just in case.
  --
  let scField = SoObsIdField `in_` valid
      nsField = NsObsIdField `in_` valid
      
  scObs <- project SoObsIdField scField
  nsObs <- project NsObsIdField nsField

  let toRemove = Set.union (Set.fromList scObs) (Set.fromList nsObs)
      checkKnown = (`Set.notMember` toRemove)
      (skip, unknown) = splitWhen checkKnown valid

  case skip of
    [] -> return ()
    _ -> do
      put "# Skipping the following known observations:"
      mapM_ (liftIO . showObsId) skip
      put ""

  return unknown


-- | Do we need to send in the obsidval as well as OCAT?
--
--   This skips problem obsids - i.e. those that we can not add to the
--   database.
--
addFromOCAT ::
  (ObsIdVal, OCAT)
  -> IO Bool
  -- ^ Was the obsid added to the database?
addFromOCAT (oi, ocat) = 
  -- check for the type, since this key should always exist
  -- rather than a more permissive "try science, then try
  -- engineering" approach
  case isScienceObsE ocat of
    Left emsg -> T.hPutStrLn stderr emsg
                 >> exitFailure
    Right flag -> if flag
                  then processScience oi ocat
                  else processEngineering oi ocat



-- | Report obsids for which there was a problem, then process the
--   successful queries.
--
processOCAT ::
  [ObsIdVal]  -- ^ ObsIds being queried
  -> OCATMap  -- ^ The OCAT response
  -> IO ()
processOCAT input omap = do

  let todo = Set.fromList input
      found = Set.fromList (map fst omap)
      missing = todo `Set.difference` found

  -- Not sure if this is possible, but keep it in for the moment
  if Set.null missing
    then return ()
    else do
      putStrLn "# No responses for the following:"
      forM_ (Set.toList missing) showObsId
      putStrLn ""

  let isOkay (oi, Just v) = Right (oi, v)
      isOkay (oi, Nothing) = Left oi
      (failed, okay) = splitWhen_ isOkay omap
      
  -- Should these be added to the invalid ObsId table?
  case failed of
    [] -> return ()
    _ -> do
      putStrLn "# Error querying OCAT for the following:"
      forM_ failed showObsId
      putStrLn ""

  -- Should probably the errors here
  flags <- forM okay addFromOCAT
  let nfail = length (filter not flags)
      failedObsIds = map snd (filter (not . fst) (zip flags (map fst okay)))

      report (i, oi) =
        let msg = "  [" <> showInt i <> "/" <> showInt nfail <>
                  "] " <> showInt (fromObsId oi)
        in T.hPutStrLn stderr msg

      doFail = do
        let n = showInt nfail
        T.hPutStrLn stderr ("\nFAILED " <> n <> " obsid(s).\n")
        forM_ (zip [1..] failedObsIds) report
        exitFailure
        
  when (nfail > 0) doFail

  
-- | Query the database to find out which obsids are already
--   known, then OCAT for the rest, adding the results to the
--   database.
--
addObsIds :: [ObsIdVal] -> IO ()
addObsIds input = do

  todo <- runDb (findUnknownObs input)
  putStrLn ("# Found " ++ show (length todo) ++ " observations to process\n")
  
  ocat <- queryOCAT todo
  case ocat of
    Right omap -> processOCAT todo omap
    Left emsg -> T.hPutStrLn stderr emsg
                 >> exitFailure
  
  
-- | Read all the obsids in the input file - an ASCII file with
--   one obsid per line and blank lines or those starting with
--   '#' being skipped - and then, for those not in the database,
--   query OCAT and add them.
--
run ::
  FilePath
  -> IO ()
run infile = do
  cts <- T.readFile infile
  let tlines = filter notSkip (map T.strip (T.lines cts))
      notSkip t = case T.uncons t of
        Nothing -> False
        Just ('#', _) -> False
        _ -> True

      parseLine t = case toObsIdValStr t of
        Nothing -> Left ("Invalid ObsId value: " <> T.unpack t)
        Just o -> Right o

      (fails, obsids) = partitionEithers (map parseLine tlines)

  case fails of
    [] -> addObsIds obsids
    [f] -> do
      hPutStrLn stderr "Invalid line found"
      hPutStrLn stderr ("  " <> f)
      exitFailure
    _ -> do
      hPutStrLn stderr "Invalid lines found"
      forM_ fails (\f -> hPutStrLn stderr ("  " <> f))
      exitFailure
      

usage :: IO ()
usage = do
  pName <- T.pack <$> getProgName
  T.hPutStrLn stderr ("Usage: " <> pName <> " infile")
  exitFailure
  
main :: IO ()
main = do
  args <- getArgs
  case args of
    [infile] -> run infile
    _ -> usage
  
