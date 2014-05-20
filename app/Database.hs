{-# LANGUAGE OverloadedStrings #-}

-- | Simple database access shims.

module Database ( getCurrentObs
                , getObsInfo
                , findObsName
                , getSpecialObs
                , getObsId
                , getRecord
                ) where

import Control.Monad (liftM)

import Data.Time (getCurrentTime)

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

