{-# LANGUAGE OverloadedStrings #-}

-- | Set up some types for representing Chandra observations.

module Types ( Record(..)
              , ScheduleItem(..)
              , Sequence(..)  
              , Instrument(..)
              , Grating(..)
              , ObsName(..)
              , ObsInfo(..)
              , ObsStatus(..)
              , ChandraTime(..)
              , getObsStatus
              , showCTime

  ) where

import qualified Text.Blaze.Html5 as H

import Data.Time (UTCTime, formatTime)

import System.Locale (defaultTimeLocale)

-- | The instrument being used.
data Instrument = ACISS | ACISI | HRCI | HRCS deriving (Eq, Show)

instance H.ToMarkup Instrument where
  toMarkup ACISI = "ACIS-I"
  toMarkup ACISS = "ACIS-S"
  toMarkup HRCI  = "HRC-I"
  toMarkup HRCS  = "HRC-S"

instance H.ToValue Instrument where
  toValue ACISI = "ACIS-I"
  toValue ACISS = "ACIS-S"
  toValue HRCI  = "HRC-I"
  toValue HRCS  = "HRC-S"

-- | The grating to be used.
data Grating = LETG | HETG | NONE deriving (Eq, Show)

instance H.ToMarkup Grating where
  toMarkup LETG = "Low Energy Transmission Grating (LETG)"
  toMarkup HETG = "High Energy Transmission Grating (HETG)"
  toMarkup NONE = "No grating"

instance H.ToValue Grating where
  toValue LETG = "Low Energy Transmission Grating (LETG)"
  toValue HETG = "High Energy Transmission Grating (HETG)"
  toValue NONE = "No grating"

data ObsName = SpecialObs String | ObsId Int deriving (Eq, Show)

instance H.ToMarkup ObsName where
  toMarkup (SpecialObs s) = H.toMarkup s
  toMarkup (ObsId i)      = H.toMarkup i

instance H.ToValue ObsName where
  toValue (SpecialObs s) = H.toValue s
  toValue (ObsId i)      = H.toValue i

-- | Represent an entry in the short-term schedule.
--
data Record = Record {
  recordSequence :: Maybe Sequence
  , recordObsname :: ObsName
  , recordContraint :: Maybe Int
  , recordTarget :: String
  , recordStartTime :: UTCTime
  , recordTime :: Double
  , recordInstrument :: Maybe Instrument
  , recordGrating :: Maybe Grating
  , recordRa :: Double
  , recordDec :: Double
  , recordRoll :: Double
  , recordPitch :: Double
  , recordSlew :: Double
  } deriving (Eq, Show)

-- | I just want a simple way of passing around 
--   useful information about an observation.
data ObsInfo = ObsInfo {
  oiCurrentObs :: Record
  , oiPrevObs  :: Maybe Record
  , oiNextObs  :: Maybe Record
  }

-- | A wrapper around `UTCTime` so that we can use our
--   own `ToMarkup` and `ToValue` instances.
--
newtype ChandraTime = ChandraTime { _toUTCTime :: UTCTime }
  deriving (Eq, Ord)

-- | Create a \"nice\" display of the time:
--   \"HH:MM Day, DN Month, Year (UTC)\", where Day and Month
--   are the (full) names and DN is the day number within
--   the month.
--
--   This does not correct for time zones.
showCTime :: ChandraTime -> String
showCTime ct = 
  let utc = _toUTCTime ct
  in formatTime defaultTimeLocale "%R %A, %e %B %Y (UTC)" utc

instance H.ToMarkup ChandraTime where
  toMarkup = H.toMarkup . showCTime

instance H.ToValue ChandraTime where
  toValue = H.toValue . showCTime

data ObsStatus = Done | Doing | Todo deriving Eq

getObsStatus :: 
  (UTCTime, UTCTime) -- observation start and end times
  -> UTCTime        -- current time
  -> ObsStatus
getObsStatus (sTime,eTime) cTime 
  | cTime < sTime    = Todo
  | cTime <= eTime   = Doing
  | otherwise        = Done

-- | Represent a Chandra sequence number.
newtype Sequence = Sequence { _unSequence :: Int } deriving (Eq, Show)

instance H.ToMarkup Sequence where
  toMarkup = H.toMarkup . _unSequence

-- | A scheduled observation (may be in the past, present, or future).
--
data ScheduleItem = ScheduledItem {
    siObsId :: ObsName
    , siSequence :: Maybe Sequence
    } deriving (Eq, Show)

               
