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

import Types (ObsName(..), ObsIdVal(..), ObsInfo(..), ChandraTime(..), Schedule(..), ScheduleItem(..), ScienceObs(..), NonScienceObs(..))
import Types (Record)

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
  -> [ScheduleItem] -- ^ observation list, must be time sorted (ascending) and finite
  -> ([ScheduleItem], Maybe ScheduleItem, [ScheduleItem])
splitObs cTime xs =
  let (aprevs, nexts) = span ((<= cTime) . _toUTCTime . siStart) xs
      (mobs, rprevs) = case reverse aprevs of
        [] -> (Nothing, [])
        (current:cs) -> (Just current, cs)
  in (rprevs, mobs, nexts)

-- TODO: allow for a missing match
findSO :: ObsIdVal -> IO ScienceObs
findSO oi = return $ head $ dropWhile ((/= oi) . soObsId) testScience

-- TODO: allow for a missing match
findNS :: String -> IO NonScienceObs
findNS n = return $ head $ dropWhile ((/= n) . nsName) testNonScience

-- | Extract all the information we know about the observation.
--
-- TODO: allow for a missing match
siToRecord :: ScheduleItem -> IO Record
siToRecord si =
  case siObsName si of
    SpecialObs so -> Left `liftM` findNS so
    ObsId oi -> Right `liftM` findSO oi

-- At present there's no need to have an array version, but that will change.
siToRecords :: [ScheduleItem] -> IO [Record]
siToRecords = mapM siToRecord

temp :: Maybe ScheduleItem -> IO (Maybe Record)
temp (Just si) = Just `liftM` siToRecord si
temp Nothing   = return Nothing

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
  case mobs of
    Just obs -> do
      robs <- siToRecord obs
      mpobs <- temp $ headMay rprevs
      mnobs <- temp $ headMay nexts
      return $ Just $ ObsInfo robs mpobs mnobs
      
    _ -> return Nothing

findObsName :: ObsName -> IO (Maybe ObsInfo)
findObsName oName = 
  let notObsName = (/= oName) . siObsName
      (prevs, nexts) = span notObsName testSchedule
  in case nexts of
    [] -> return Nothing
    (current:cs) -> do
      cobs <- siToRecord current
      mpobs <- temp $ lastMay prevs
      mnobs <- temp $ headMay cs
      return . Just $ ObsInfo cobs mpobs mnobs
      
-- | Return the requested "special" observation.
getSpecialObs :: String -> IO (Maybe ObsInfo)
getSpecialObs = findObsName . SpecialObs

-- | Return the requested "science" observation.
getObsId :: ObsIdVal -> IO (Maybe ObsInfo)
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

      -- TODO: should use dropWhile/takeWhile as we know that testSchedule
      --       is in ascending time order
      tfilter si = let t = siStart si
                   in t >= tStart && t < tEnd

      (rprevs, mobs, nexts) = splitObs now $ filter tfilter testSchedule

  done <- siToRecords $ reverse rprevs
  todo <- siToRecords $ nexts
  mdoing <- temp $ mobs
  
  return $ Schedule now ndays done mdoing todo

-- | Find observations with the same sequence number.
--
{-
matchSeqNum :: ScienceObs -> IO [ScienceObs]
matchSeqNum so =
  let seqNum = soSequence so
      oid = soObsId so
      matches r = soSequence r == seqNum && soObsId r /= oid
  in return . filter matches $ testScience
-}

matchSeqNum :: Record -> IO [Record]
matchSeqNum (Left _) = return []
matchSeqNum (Right so) =
  let seqNum = soSequence so
      oid = soObsId so
      matches r = soSequence r == seqNum && soObsId r /= oid
  in return . map Right . filter matches $ testScience

