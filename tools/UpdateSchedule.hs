{-# LANGUAGE FlexibleContexts #-}
{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
{-# Language TupleSections #-}

--
-- Usage:
--    ./updateschedule
--
-- Aim:
--
-- Review the current database to see if there are any ObsIds
-- that may need updating and, if there are, query OCAT for
-- the new data, updating the database if there has been a change.
--

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)

import Data.Monoid ((<>))
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
import Database.Groundhog.Postgresql (Postgresql, SqlDb, Conn, in_, insert_)

import Formatting (int, sformat)

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (stderr)

import Database (runDb, updateLastModified)
import OCAT (OCAT, queryOCAT, ocatToScience, ocatToNonScience)

import Types (ChandraTime(..))
import Types (ScienceObs(..), NonScienceObs(..), ObsIdVal(..))
import Types (Field(..))
import Types (ObsIdStatus(..))

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
getUpdateable ::
  (PersistBackend m, SqlDb (Conn m))
  => Int
  -- ^ maximum number of each category (may return less
  --   than this)
  -> ChandraTime
  -> m ([NonScienceObs], [ScienceObs])
getUpdateable nmax tmax = do
  let wanted = [Unobserved, Scheduled, Observed]
  updateN <- select ((NsStatusField `in_` wanted &&.
                      NsStartTimeField <. Just tmax)
                     `orderBy` [Desc NsStartTimeField]
                     `limitTo` nmax)
  updateS <- select ((SoStatusField `in_` wanted &&.
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
  -> (OCAT -> IO (Maybe a))
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
            ans <- liftIO (fromOCAT ocat)
            case ans of
              Just new -> if old == new
                          then return 0
                          else deleteObs obsid
                               >> insert_ new
                               >> liftIO (putStr (" "
                                                  <> show (fromObsId obsid)))
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
        let msg = if n == 0
                  then "No updates for any of the " <> tailMsg
                  else if n < ntot
                       then "Updated " <> showInt n <> " of the " <>
                            tailMsg
                       else "Updated all " <> tailMsg
        in T.putStrLn msg >> return n
          
  in runDb go >>= report
  

updateScience ::
  OCATMap
  -- ^ The response from querying OCAT
  -> [ScienceObs]
  -- ^ The information in the database
  -> IO Int
  -- ^ The number of records changed in the database
updateScience =
  let fromOCAT o = do
        ans <- ocatToScience o
        case ans of
          Left e -> T.hPutStrLn stderr e >> return Nothing
          Right (_, x) -> return (Just x)
        
      del obsid = delete (SoObsIdField ==. obsid)
  in updateObsIds "Science" soObsId fromOCAT del

  
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
  let fromOCAT o = case ocatToNonScience o of
        Left e -> T.hPutStrLn stderr e >> return Nothing
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
  tnow <- ChandraTime <$> getCurrentTime
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
  

usage :: IO ()
usage = do
  pName <- T.pack <$> getProgName
  T.hPutStrLn stderr ("Usage: " <> pName)
  exitFailure
  
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> updateSchedule 20 -- could be configureable
    _ -> usage
  
