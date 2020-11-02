{-# LANGUAGE FlexibleContexts #-}
{-# Language OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

{-

Usage:
    ./updateschedule [obsid]

Aim:

Review the current database to see if there are any ObsIds
that may need updating and, if there are, query OCAT for
the new data, updating the database if there has been a change.

If called with a single argument, it's an obsid to re-query
(any checks are ignored in this case, other than that it is
an invalid obsid)

TODO:

The current scheme for identifying observations to query is
not ideal, since it looks like it includes already-observed
data. This needs to be reviewed (and perhaps concentrate on
observations that have just beem, or are about to be,
observed, rather than in reverse start-time order).

-}

module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)

import Data.Time (getCurrentTime)

import Database.Groundhog (PersistBackend, Cond(Not), Order(Desc)
                          , (&&.)
                          , (==.), (<.) -- (/=.), (||.), (>=.)
                          , delete
                          , isFieldNothing
                          , limitTo
                          , orderBy
                          , select)
import Database.Groundhog.Core (Action, PersistEntity)
import Database.Groundhog.Postgresql (Postgresql, SqlDb, Conn,
                                      count, in_, insert_)

import Formatting (int, sformat)

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (stderr)

import Database (runDb, insertProposal, updateLastModified)
import OCAT (OCAT, queryOCAT
            , isScienceObsE
            , ocatToScience, ocatToNonScience)

import Types (ChandraTime, ScienceObs(..), NonScienceObs(..)
             , ObsIdVal(..), ObsIdStatus(..)
             , Proposal(propNum)
             , Field(NsStatusField, NsStartTimeField
                    , SoStatusField, SoStartTimeField
                    , NsObsIdField, SoObsIdField
                    , IoObsIdField)
             , toChandraTime
             , fromPropNum
             , showCTime
             , toObsIdValStr)

showInt :: Int -> T.Text
showInt = sformat int


-- | What science and non-science observations are there?
--
--   How often should cancelled observations be queried?
--
--   Non-science observations that have not been performed are
--   not queried.
--
--   The responses are ordered by start time, in descending
--   order. A limit is used since a query of OCAT with a large
--   number of obsids failed with an 'invalid request'
--
--   For now, require that a science observation have
--   a start time (which probably just means getting
--   rid of the Unobserved constraint). The idea/hope is that
--   GetSchedule will check these the next time they appear
--   in the STS; they do not need to be updated here.
--
--   Should we remove the "Observed" category from this
--   search, since what could be updated? When does a
--   record go from "Observed" to "Archived"? Note that
--   there are records from 2000 which have an "observed"
--   status - e.g.
--   https://cda.cfa.harvard.edu/chaser/startViewer.do?menuItem=details&obsid=37
--
--   This algorithm needs updating - e.g. may want to occasionally
--   check the observed science observations to see if they
--   have become archived.
--
getUpdateable ::
  (PersistBackend m, SqlDb (Conn m))
  => Int
  -- ^ maximum number of each category (may return less
  --   than this)
  -> ChandraTime
  -> m ([NonScienceObs], [ScienceObs])
getUpdateable nmax tmax = do
  let wantedN = [Unobserved, Scheduled, Observed]
  let wantedS = [Unobserved, Scheduled]
  updateN <- select ((NsStatusField `in_` wantedN &&.
                      NsStartTimeField <. Just tmax)
                     `orderBy` [Desc NsStartTimeField]
                     `limitTo` nmax)
  updateS <- select ((SoStatusField `in_` wantedS &&.
                      Not (isFieldNothing SoStartTimeField))
                     `orderBy` [Desc SoStartTimeField]
                     `limitTo` nmax)
  return (updateN, updateS)


type OCATMap = [(ObsIdVal, Maybe OCAT)]

{-
Oh joy:

cda query for obsid 17376 has it has observed, and not archived,
even though taken in 2014-11-16

-}

-- | For each observation, if the new OCAT-queried version is different,
--   update the database. If there's no response from OCAT
--   then display a screen message but *do not* change the database.
--
--
updateObsIds ::
  (Eq a, PersistEntity a)
  => T.Text
  -- ^ label used in screen messages
  -> (a -> ObsIdVal)
  -- ^ project out the ObsId from the database structure
  -> (ObsIdVal -> OCAT -> IO (Maybe a))
  -- ^ Convert OCAT response to structure
  -> (ObsIdVal -> Action Postgresql ())
  -- ^ forms the condition for identifying the record to delete
  --   in the database.
  -> OCATMap
  -- ^ The response from querying OCAT
  -> [a]
  -- ^ The information in the database
  -> IO Int
  -- ^ The number of records changed in the database
updateObsIds lbl getObsId fromOCAT deleteObs omap osobs =
  let handle old = do
        let obsid = getObsId old
        case lookup obsid omap of
          Just (Just ocat) -> do
            ans <- liftIO (fromOCAT obsid ocat)
            case ans of
              Just new -> if old == new
                          then return 0
                          else deleteObs obsid
                               >> insert_ new
                               >> liftIO (T.putStr (" " <>
                                                    showInt (fromObsId obsid)))
                               >> return 1
              Nothing -> return 0
          _ -> return 0

      go = do
        liftIO (putStr "Processing:")
        ns <- mapM handle osobs
        liftIO (putStrLn "")
        let n = sum ns
        when (n > 0) (liftIO getCurrentTime >>= updateLastModified)
        return n

      ntot = length osobs
      tailMsg = showInt ntot <> " " <> lbl <> " observations"
        
      report n =
        let msg
              | n == 0 = "No updates for any of the " <> tailMsg
              | n < ntot = "Updated " <> showInt n <> " of the " <>
                           tailMsg
              | otherwise = "Updated all " <> tailMsg
                            
        in T.putStrLn msg >> return n
          
  in runDb go >>= report
  

-- Unlike the non-science obs, the to-be-queried obsids are dumped
-- to stdout just so that I can check the logic to create this list
-- is sensible
--
updateScience ::
  OCATMap
  -- ^ The response from querying OCAT
  -> [ScienceObs]
  -- ^ The information in the database
  -> IO Int
  -- ^ The number of records changed in the database
updateScience omap sobs =
  let fromOCAT obsid o = do
        ans <- ocatToScience o
        case ans of
          Left e ->
            T.hPutStrLn stderr
            ("Science ObsId: " <> showInt (fromObsId obsid))
            >> T.hPutStrLn stderr e
            >> return Nothing
          Right (_, x) -> return (Just x)
        
      del obsid = delete (SoObsIdField ==. obsid)
  in showScienceObs sobs
     >> updateObsIds "Science" soObsId fromOCAT del omap sobs


showScienceObs ::
  [ScienceObs]
  -> IO ()
showScienceObs = mapM_ (T.putStrLn . textify) . zip [1..]
  where
    textify (i, ScienceObs {..}) =
      let stime = maybe "<no start>" showCTime soStartTime
          pad = "  "
      in pad <> "[" <> showInt i <> "] ObsId: "
         <> showInt (fromObsId soObsId)
         <> " " <> stime

-- For now assume that if a non-science observation can not be found
-- in the OCAT that it should just be left as is.
--
-- It's unclear how to handle the case of "no response" because
-- it is invalid from "no response" because we haven't yet had
-- a ground-system pass, or the OCAT hasn't been updated with the
-- latest results yet.
--
updateNonScience ::
  OCATMap
  -- ^ The response from querying OCAT
  -> [NonScienceObs]
  -- ^ The information in the database
  -> IO Int
  -- ^ The number of records changed in the database
updateNonScience =
  let fromOCAT obsid o = case ocatToNonScience o of
        Left e ->
            T.hPutStrLn stderr
            ("Engineering ObsId: " <> showInt (fromObsId obsid)) 
            >> T.hPutStrLn stderr e
            >> return Nothing
        Right x -> return (Just x)
                      
      del obsid = delete (NsObsIdField ==. obsid)
  in updateObsIds "Engineering" nsObsId fromOCAT del

-- | Extract any new schedule information and add it to the
--   system.
--
--   If there's any problem downloading or parsing information
--   then the routine will error out.
--
updateSchedule ::
  Int
  -- ^ Maximum number of non-science or science objects
  -> IO ()
updateSchedule nmax = do

  -- How about using a limit of only observations newer
  -- than the database last-modified time - some delta
  -- (e.g. a week) - to reduce pointless queries.
  --
  tnow <- toChandraTime <$> getCurrentTime
  (todoNS, todoS) <- runDb (getUpdateable nmax tnow)

  if null todoNS && null todoS
    then T.putStrLn "No updates needed"
    else do
      let obsidsNS = map nsObsId todoNS
          obsidsS = map soObsId todoS
      ansS <- queryOCAT obsidsS
      case ansS of
        Right omap -> void (updateScience omap todoS)

        Left emsg -> T.hPutStrLn stderr emsg
                     >> exitFailure
  
      ansNS <- queryOCAT obsidsNS
      case ansNS of
        Right omap -> void (updateNonScience omap todoNS)

        Left emsg -> T.hPutStrLn stderr emsg
                     >> exitFailure

  -- This is to help know when I last ran this, so it doesn't matter
  -- if it isn't shown on error
  T.putStrLn ("Query started at: " <> showCTime tnow)


-- | It is an error if this obsid is not in the InvalidObsId table.
updateObsId ::
  ObsIdVal
  -- ^ ObsId to query
  -> IO ()
updateObsId oi = do
  T.putStrLn ("Updating ObsId: " <> showInt (fromObsId oi))
  n <- runDb (count (IoObsIdField ==. oi))
  when (n == 0)
    (do
        let msg = "Error: Obsid is not in the invalid table: " <>
                  showInt (fromObsId oi)
        T.hPutStrLn stderr msg
        exitFailure)

  ns <- runDb (count (SoObsIdField ==. oi))
  unless (ns == 0)
    (do
        let msg = "Error: Science Obsid is already known about :" <>
                  showInt (fromObsId oi)
        T.hPutStrLn stderr msg
        exitFailure)
    
  ne <- runDb (count (NsObsIdField ==. oi))
  unless (ne == 0)
    (do
        let msg = "Error: Engineering Obsid is already known about :" <>
                  showInt (fromObsId oi)
        T.hPutStrLn stderr msg
        exitFailure)

  -- do we assume engineering or science?
  ocat <- queryOCAT [oi]
  case ocat of
    Right omap -> processObsId oi omap
    Left emsg -> T.hPutStrLn stderr emsg
                 >> exitFailure
          

-- What is the difference between returning [] and [(xxx, Nothing)] ?
--
processObsId :: ObsIdVal -> OCATMap -> IO ()
processObsId oi [(oiv, Just ocat)] | oi == oiv = 
  -- check for the type, since this key should always exist
  -- rather than a more permissive "try science, then try
  -- engineering" approach
  case isScienceObsE ocat of
    Left emsg -> T.hPutStrLn stderr emsg >> exitFailure
    Right flag -> if flag
                  then processScience oi ocat
                  else processEngineering oi ocat

  
processObsId _ [(_, Just _)] = do
  -- do not expect this to fire so don't do match
  T.hPutStrLn stderr "UNEXPECTED: ObsIds do not match"
  T.hPutStrLn stderr "Database is NOT updated."
  exitFailure
        
processObsId _ [(_, Nothing)] = do
  T.hPutStrLn stderr "Unable to parse OCAT response"
  T.hPutStrLn stderr "Database is NOT updated."
  exitFailure

processObsId _ [] = do
  T.hPutStrLn stderr "No data retrieved from OCAT query."
  T.hPutStrLn stderr "Database is NOT updated."
  exitFailure

-- This should not happen, so don't bother dealing with it
processObsId _ (_:_) = do
  T.hPutStrLn stderr "Errr, multiple responses to OCAT query."
  T.hPutStrLn stderr "Database is NOT updated."
  exitFailure


processScience :: ObsIdVal -> OCAT -> IO ()
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

  in do
     ans <- ocatToScience ocat
     case ans of
       Right (prop, sobs) -> do
         now <- getCurrentTime
         flag <- runDb (act now sobs prop)
         T.putStrLn "Added science observation"
         when flag (T.putStrLn ("Also added proposal " <>
                                showInt (fromPropNum (propNum prop))))
         
       Left emsg -> do
         T.hPutStrLn stderr "ERROR converting science obsid"
         T.hPutStrLn stderr emsg
         exitFailure
  

-- This doesn't quite match GteSchedule, since in getschedule we have
-- the short-term schedule, so can add in the expected start time
-- from that (if it is missing)
--
processEngineering :: ObsIdVal -> OCAT -> IO ()
processEngineering oi ocat =
  let act now ns = do
        delete (IoObsIdField ==. oi)
        -- do not check if the observation is already known about
        -- because it shouldn't, relying on the DB constraint to
        -- let us know if something is wrong (or has changed in the
        -- database).
        insert_ ns
        updateLastModified now
        
  in case ocatToNonScience ocat of
    Left emsg -> do
      T.hPutStrLn stderr "ERROR converting engineering obsid"
      T.hPutStrLn stderr emsg
      exitFailure

    Right ns -> do
      now <- getCurrentTime
      runDb (act now ns)
      T.putStrLn "Added engineering observation"
  


usage :: IO ()
usage = do
  pName <- T.pack <$> getProgName
  T.hPutStrLn stderr ("Usage: " <> pName <> " [obsid]")
  exitFailure


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> updateSchedule 50 -- could be configureable
    [i] | Just oi <- toObsIdValStr (T.pack i) -> updateObsId oi
    _ -> usage
  
