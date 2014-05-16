{-# LANGUAGE OverloadedStrings #-}

-- | Simple database access shims.

module Database ( currentRecord
                , getObsInfo
                , findObsName
                , getSpecialObs
                , getObsId
                , getRecord
                ) where

import Safe (headMay, lastMay)

import HackData
import PersistentTypes

import Types (ObsName(..))
import Utils (ObsInfo(..))

-- How to display a record? For now, just pick one
-- in the middle (ie that length testSchedule > 5).
--
-- assume that currentRecord occurs within testShedule
--
currentRecord :: Record
currentRecord = testSchedule !! 2

-- | Find the current observation. At present this is a stub.
--
--   Allow for the possibility of there being no observation; e.g.
--   because the data base hasn't been updated.
getObsInfo :: IO (Maybe ObsInfo)
getObsInfo = 
  let nextObs = headMay $ drop 1 $ dropWhile (/= currentRecord) testSchedule
      prevObs = lastMay $ takeWhile (/= currentRecord) testSchedule
  in return . Just $ ObsInfo currentRecord prevObs nextObs

findObsName :: ObsName -> IO (Maybe ObsInfo)
findObsName oName = 
  let notObsName = (/= oName) . recordObsname
  in case headMay $ dropWhile notObsName testSchedule of
       Just currObs -> 
         let nextObs = headMay $ drop 1 $ dropWhile notObsName testSchedule
             prevObs = lastMay $ takeWhile notObsName testSchedule
         in return . Just $ ObsInfo currObs prevObs nextObs
       _ -> return Nothing

-- | Return the requested "special" observation.
getSpecialObs :: String -> IO (Maybe ObsInfo)
getSpecialObs = findObsName . SpecialObs

-- | Return the requested "science" observation.
getObsId :: Int -> IO (Maybe ObsInfo)
getObsId = findObsName . ObsId

getRecord :: ObsName -> IO (Maybe Record)
getRecord oName = 
  let notObsName = (/= oName) . recordObsname
  in return $ headMay $ dropWhile notObsName testSchedule

