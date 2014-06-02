{-# LANGUAGE OverloadedStrings #-}

-- | Simple database access shims.

module Database ( getCurrentObs
                , getObsInfo
                , findObsName
                , getSpecialObs
                , getObsId
                , getRecord
                , getSchedule
                , matchSeqNum
                ) where

import Control.Monad (liftM)

import Data.Time (UTCTime(..), Day(..), getCurrentTime, addDays)

import Safe (headMay, lastMay)

import HackData

import Types (Record(..), ObsName(..), ObsInfo(..), ChandraTime(..), Schedule(..))

-- | Return the current observation
getCurrentObs :: IO (Maybe Record)
getCurrentObs = (liftM . liftM) oiCurrentObs getObsInfo

-- | Given a list of observations, return
--   (past observations in reverse order, 
--    current observation,
--    future observations).
--
splitObs :: 
  UTCTime     -- ^ the current time
  -> [Record] -- ^ observation list, must be time sorted (ascending) and finite
  -> ([Record], Maybe Record, [Record])
splitObs cTime xs =
  let (aprevs, nexts) = span ((<= cTime) . _toUTCTime . recordStartTime) xs
      (mobs, rprevs) = case reverse aprevs of
        [] -> (Nothing, [])
        (current:cs) -> (Just current, cs)
  in (rprevs, mobs, nexts)

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
  let (rprevs, mobs, nexts) = splitObs now testSchedule
  return $ case mobs of
    Just obs -> Just $ ObsInfo obs (headMay rprevs) (headMay nexts)
    _ -> Nothing

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

-- | TODO: handle the case when the current observation, which has
--   just started, has an exposure time > ndays.
--
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

      tStart = ChandraTime $ UTCTime dayStart 0
      tEnd   = ChandraTime $ UTCTime dayEnd 0

      -- TODO: should use dropWhile/takeWhile as know testSchedule
      --       is in ascending time order
      tfilter rs = let t = recordStartTime rs
                   in t >= tStart && t < tEnd

      (rprevs, mobs, nexts) = splitObs now $ filter tfilter testSchedule

  return $ Schedule now ndays (reverse rprevs) mobs nexts

-- | Find observations with the same sequence number.
--
matchSeqNum :: Record -> IO [Record]
matchSeqNum rs = case recordSequence rs of
  Nothing -> return []
  mSeqNum -> let matches r = recordSequence r == mSeqNum && recordObsname r /= recordObsname rs
             in return . filter matches $ testSchedule

