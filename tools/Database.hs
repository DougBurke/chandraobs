{-# LANGUAGE OverloadedStrings #-}

-- | Simple database access shims.
--
--   Many of these are poorly written (e.g. no use of the database
--   or Groundhog API, such as inner joins or primary keys).
--
module Database ( getCurrentObs
                , getObsInfo
                -- , findObsName
                , getSpecialObs
                , getObsId
                , getRecord
                , getSchedule
                , matchSeqNum
                ) where

import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Maybe (catMaybes, listToMaybe)
import Data.Time (UTCTime(..), Day(..), getCurrentTime, addDays)

import Database.Groundhog.Postgresql

import Types
{-
import Types (ObsName(..), ObsIdVal(..), ObsInfo(..), ChandraTime(..), Schedule(..), ScheduleItem(..), ScienceObs(..), NonScienceObs(..))
import Types (Record)
-}

-- | Return the last ScheduleItem to be defined before the given time.
findSI :: (PersistBackend m) => UTCTime -> m (Maybe ScheduleItem)
findSI t = do
  res <- select $ (SiStartField <=. ChandraTime t)
                  `orderBy` [Desc SiStartField]
                  `limitTo` 1
  return $ listToMaybe res

-- | Given a ScheduleItem, return infomation on the observation it
--   refers to.
findItem :: PersistBackend m => ScheduleItem -> m (Maybe Record)
findItem = findObsName . siObsName

findObsName :: PersistBackend m => ObsName -> m (Maybe Record)
findObsName obsname = 
  case obsname of
    SpecialObs s -> (Left `liftM`) `liftM` findNonScience s
    ObsId o -> (Right `liftM`) `liftM` findScience o

findNonScience :: PersistBackend m => String -> m (Maybe NonScienceObs)
findNonScience s = do
  ans <- select $ (NsNameField ==. s) `limitTo` 1
  return $ listToMaybe ans

findScience :: PersistBackend m => ObsIdVal -> m (Maybe ScienceObs)
findScience o = do
  ans <- select $ (SoObsIdField ==. o) `limitTo` 1
  return $ listToMaybe ans

-- | Given a list of schedule items, return the record of the
--   first element, or @Nothing@ if the list is empty.
--
extractRecord:: PersistBackend m => [ScheduleItem] -> m (Maybe Record)
extractRecord[] = return Nothing
extractRecord(si:_) = findItem si

-- | Return the current observation
getCurrentObs :: (MonadIO m, PersistBackend m) => m (Maybe Record)
getCurrentObs = do
  now <- liftIO getCurrentTime
  msi <- findSI now
  case msi of
    Just si -> findItem si
    _ -> return Nothing

-- | Return the first observation to start after the given time.
getNextObs :: (MonadIO m, PersistBackend m) => ChandraTime -> m (Maybe Record)
getNextObs t = do
  res <- select $ (SiStartField >. t)
                  `orderBy` [Asc SiStartField]
                  `limitTo` 1
  extractRecord res

-- | Return the last observation to have started before the given time.
--
--   Perhaps this should check on SiEndTime
getPrevObs :: (MonadIO m, PersistBackend m) => ChandraTime -> m (Maybe Record)
getPrevObs t = do
  res <- select $ (SiStartField <. t)
                  `orderBy` [Desc SiStartField]
                  `limitTo` 1
  extractRecord res
  
-- | Find the current observation and the previous/next ones.
--
--   Allow for the possibility of there being no observation; e.g.
--   because the data base hasn't been updated.
--
--   TODO: should check for some overlap of the observation date +
--         exposure time to the current date, so can suggest that
--         slewing (or if at end that run out of data...)
--
--   It might be quicker to find the current observation and return
--   the following and preceeding rows in one go, but that's an
--   optimisation.
--
getObsInfo :: (MonadIO m, PersistBackend m) => m (Maybe ObsInfo)
getObsInfo = do
  mobs <- getCurrentObs
  case mobs of
    Just obs -> extractObsInfo obs
    Nothing -> return Nothing

-- | Given an observation, extract the previous and next observations.
extractObsInfo :: (MonadIO m, PersistBackend m) => Record -> m (Maybe ObsInfo)
extractObsInfo obs = do
  mprev <- getPrevObs (recordStartTime obs)
  mnext <- getNextObs (recordStartTime obs)
  return $ Just $ ObsInfo obs mprev mnext

findObsInfo :: (MonadIO m, PersistBackend m) => ObsName -> m (Maybe ObsInfo)
findObsInfo oName = do
  res <- select $ (SiObsNameField ==. oName) `limitTo` 1
  mobs <- extractRecord res
  case mobs of
    Just obs -> extractObsInfo obs
    _ -> return Nothing
  
-- | Return the requested "special" observation.
getSpecialObs :: (MonadIO m, PersistBackend m) => String -> m (Maybe ObsInfo)
getSpecialObs = findObsInfo . SpecialObs

-- | Return the requested "science" observation.
getObsId :: (MonadIO m, PersistBackend m) => ObsIdVal -> m (Maybe ObsInfo)
getObsId = findObsInfo . ObsId 

-- | Return the record of the given observation, if it
--   exists.
getRecord :: PersistBackend m => ObsName -> m (Maybe Record)
getRecord = findObsName

-- | TODO: handle the case when the current observation, which has
--   just started, has an exposure time > ndays.
--
getSchedule ::
  (MonadIO m, PersistBackend m)
  => Int    -- ^ Number of days to go back/forward
  -> m Schedule
getSchedule ndays = do
  now <- liftIO getCurrentTime

  -- return all observations which have any part of their
  -- observation within the time span tStart to tEnd.
  --
  let dayNow = utctDay now
      dayEnd = addDays (fromIntegral ndays + 1) dayNow
      dayStart = ModifiedJulianDay $ toModifiedJulianDay dayNow - fromIntegral ndays

      tStart = ChandraTime $ UTCTime dayStart 0
      tEnd   = ChandraTime $ UTCTime dayEnd 0

  res <- select $ ((SiStartField <. tEnd) &&. (SiEndField >=. tStart))
                  `orderBy` [Asc SiStartField]

  -- I used siStart r <= cnow, so keep with that logic for now
  -- (could use siEnd r < cnow)
  let cnow = ChandraTime now
      (aprevs, nexts) = span ((<= cnow) . siStart) res
      (mobs, prevs) = case reverse aprevs of
        [] -> (Nothing, [])
        (current:cs) -> (Just current, reverse cs)

  done <- catMaybes `liftM` mapM findItem prevs
  todo <- catMaybes `liftM` mapM findItem nexts
  mdoing <- case mobs of
              Just obs -> findItem obs
              _ -> return Nothing

  return $ Schedule now ndays done mdoing todo

-- | Find observations with the same sequence number.
--
matchSeqNum :: (MonadIO m, PersistBackend m) => Record -> m [Record]
matchSeqNum (Left _) = return []
matchSeqNum (Right so) = do
  let seqNum = soSequence so
      oid = soObsId so
  -- time sorting probably not needed here as I would expect the
  -- results to be in the correct order anyway
  ans <- select $ ((SoSequenceField ==. seqNum) &&. (SoObsIdField /=. oid))
                  `orderBy` [Asc SoStartTimeField]
  return $ map Right ans

