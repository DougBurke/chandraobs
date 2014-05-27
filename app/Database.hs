{-# LANGUAGE OverloadedStrings #-}

-- | Simple database access shims.

module Database ( Schedule(..)
                , getCurrentObs
                , getObsInfo
                , findObsName
                , getSpecialObs
                , getObsId
                , getRecord
                , getSchedule
                ) where

import Control.Monad (liftM)

import Data.Time (UTCTime(..), Day(..), getCurrentTime, addDays)

import Safe (headMay, lastMay)

import HackData
import PersistentTypes

import Types (ObsName(..))
import Utils (ObsInfo(..))

-- | Return the current observation
getCurrentObs :: IO (Maybe Record)
getCurrentObs = (liftM . liftM) oiCurrentObs getObsInfo

-- | Find the current observation.
--
--   Allow for the possibility of there being no observation; e.g.
--   because the data base hasn't been updated.
--
--   TODO: should check for some overlap of the observation date +
--         exposure time to the current date, so can suggest that
--         slewing (or if at end that run out of data...)
--
getObsInfo :: IO (Maybe ObsInfo)
getObsInfo = do
  now <- getCurrentTime
  -- assume that testSchedule is in ascending time order
  let (prevs, nexts) = span ((<= now) . recordStartTime) testSchedule
  case reverse prevs of
    [] -> return Nothing
    (current:cs) -> return . Just $ ObsInfo current
                                            (headMay cs)
                                            (headMay nexts)

findObsName :: ObsName -> IO (Maybe ObsInfo)
findObsName oName = 
  let notObsName = (/= oName) . recordObsname
      (prevs, nexts) = span notObsName testSchedule
  in case nexts of
    [] -> return Nothing
    (current:cs) -> return . Just $ ObsInfo current
                                            (lastMay prevs)
                                            (headMay cs)

-- | Return the requested "special" observation.
getSpecialObs :: String -> IO (Maybe ObsInfo)
getSpecialObs = findObsName . SpecialObs

-- | Return the requested "science" observation.
getObsId :: Int -> IO (Maybe ObsInfo)
getObsId = findObsName . ObsId

-- | Return the record of the given observation, if it
--   exists.
getRecord :: ObsName -> IO (Maybe Record)
getRecord oName = do
  mobs <- findObsName oName
  case mobs of
    Just (ObsInfo current _ _) -> return $ Just current
    _ -> return Nothing

-- | Store the schedule.
data Schedule = 
   Schedule
   { scTime  :: UTCTime      -- ^ the date when the schedule search was made
   , scDays  :: Int          -- ^ number of days used for the search
   , scDone  :: [Record]     -- ^ those that were done (ascending time order)
   , scDoing :: Maybe Record -- ^ current observation
   , scToDo  :: [Record]     -- ^ those that are to be done (ascending time order)
   }

getSchedule ::
  Int    -- ^ Number of days to go back/forward
  -> IO Schedule
getSchedule ndays = do
  now <- getCurrentTime
  -- assume that testSchedule is in ascending time order
  -- and only consider the start time when comparing to the 
  -- number of days from now
  --
  let dayNow = utctDay now
      dayEnd = addDays (fromIntegral ndays + 1) dayNow
      dayStart = ModifiedJulianDay $ toModifiedJulianDay dayNow - fromIntegral ndays

      tStart = UTCTime dayStart 0
      tEnd   = UTCTime dayEnd 0

      tfilter rs = let t = recordStartTime rs
                   in t >= tStart && t < tEnd
      (prevs, nexts) = span ((<= now) . recordStartTime) $ filter tfilter testSchedule

      (mobs, todos) = case nexts of
         [] -> (Nothing, [])
         (x:xs) -> (Just x, xs)

  return $ Schedule now ndays prevs mobs todos


