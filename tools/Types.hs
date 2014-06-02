{-# LANGUAGE OverloadedStrings #-}

-- | Set up some types for representing Chandra observations.

module Types ( ScheduleItem(..)
              , Schedule(..)
              , Sequence(..)
              , RA(..), Dec(..)
              , Instrument(..)
              , Grating(..)
              , ObsName(..)
              , ObsIdVal(..)
              , ObsInfo(..)
              , ObsStatus(..)
              , ChandraTime(..)
              , TimeKS(..)
              , ScienceObs(..)
              , ConstrainedObs(..)
              , NonScienceObs(..)
              , getObsStatus
              , toCTime
              , showCTime
              , endCTime

                -- * Temporary types and routines
              , Record
              , recordSequence, recordObsname, recordTarget, recordStartTime, recordTime, recordInstrument, recordGrating, recordRa, recordDec, recordRoll, recordPitch, recordSlew
  ) where

import qualified Text.Blaze.Html5 as H

import Data.Time (UTCTime, addUTCTime, formatTime, readTime)

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

-- | Represent an observation identifier.
--
--   Due to a clash with @ObsName@ we use @ObsIdVal@
--   for now, but it's planned to move to @ObsId@.
newtype ObsIdVal = ObsIdVal { fromObsId :: Int }
  deriving (Eq, Show)

instance H.ToMarkup ObsIdVal where
  toMarkup (ObsIdVal i) = H.toMarkup i

instance H.ToValue ObsIdVal where
  toValue (ObsIdVal i) = H.toValue i

-- | This is likely being deleted, or the @ObsId@ constructor renamed.
data ObsName = SpecialObs String | ObsId ObsIdVal deriving (Eq, Show)

instance H.ToMarkup ObsName where
  toMarkup (SpecialObs s) = H.toMarkup s
  toMarkup (ObsId i)      = H.toMarkup i

instance H.ToValue ObsName where
  toValue (SpecialObs s) = H.toValue s
  toValue (ObsId i)      = H.toValue i

-- | Represent an entry in the short-term schedule.
--
{-
data Record = Record {
  recordSequence :: Maybe Sequence
  , recordObsname :: ObsName
  , recordContraint :: Maybe Int
  , recordTarget :: String
  , recordStartTime :: ChandraTime
  , recordTime :: TimeKS
  , recordInstrument :: Maybe Instrument
  , recordGrating :: Maybe Grating
  , recordRa :: RA
  , recordDec :: Dec
  , recordRoll :: Double
  , recordPitch :: Double
  , recordSlew :: Double
  } deriving (Eq, Show)
-}

type Record = Either NonScienceObs ScienceObs

-- hacks for quickly converting old code

recordSequence :: Record -> Maybe Sequence
recordSequence = either (const Nothing) (Just . soSequence)

recordObsname :: Record -> ObsName
recordObsname = either (SpecialObs . nsName) (ObsId . soObsId)

recordTarget :: Record -> String
recordTarget = either nsTarget soTarget

recordStartTime :: Record -> ChandraTime
recordStartTime = either nsStartTime soStartTime

recordTime :: Record -> TimeKS
recordTime = either nsTime soTime

recordInstrument :: Record -> Maybe Instrument
recordInstrument = either (const Nothing) (Just . soInstrument)

recordGrating :: Record -> Maybe Grating
recordGrating = either (const Nothing) (Just . soGrating)

recordRa :: Record -> RA
recordRa = either nsRa soRa 

recordDec :: Record -> Dec
recordDec = either nsDec soDec 

recordRoll :: Record -> Double
recordRoll = either nsRoll soRoll

recordPitch :: Record -> Double
recordPitch = either nsPitch soPitch

recordSlew :: Record -> Double
recordSlew = either nsSlew soSlew 


-- | I just want a simple way of passing around 
--   useful information about an observation.
data ObsInfo = ObsInfo {
  oiCurrentObs :: Record
  , oiPrevObs  :: Maybe Record
  , oiNextObs  :: Maybe Record
  } deriving (Eq, Show)

-- | A wrapper around `UTCTime` so that we can use our
--   own `ToMarkup` and `ToValue` instances.
--
newtype ChandraTime = ChandraTime { _toUTCTime :: UTCTime }
  deriving (Eq, Ord, Show)

-- | Convert values like "2014:132:03:08:49.668"
-- to a time. This is
--
--  year number : day number : hh : mm : ss.sss
--
-- in UTC (I guess)
--
-- For now assume all inputs are valid. Hopefully the DOY
-- values are all zero-padded to three characters and
-- start at 1.
--
-- This will cause an exception if the input string is
-- incorrectly formatted.
--
toCTime :: String -> ChandraTime
toCTime = ChandraTime . readTime defaultTimeLocale "%Y:%j:%T%Q"

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

endCTime :: ChandraTime -> TimeKS -> ChandraTime
endCTime (ChandraTime start) (TimeKS elen) =
  let delta = fromRational . toRational $ 1000 * elen
  in ChandraTime $ addUTCTime delta start

instance H.ToMarkup ChandraTime where
  toMarkup = H.toMarkup . showCTime

instance H.ToValue ChandraTime where
  toValue = H.toValue . showCTime

data ObsStatus = Done | Doing | Todo deriving Eq

getObsStatus :: 
  (ChandraTime, ChandraTime) -- observation start and end times
  -> UTCTime        -- current time
  -> ObsStatus
getObsStatus (ChandraTime sTime, ChandraTime eTime) cTime 
  | cTime < sTime    = Todo
  | cTime <= eTime   = Doing
  | otherwise        = Done

-- | Represent a Chandra sequence number.
newtype Sequence = Sequence { _unSequence :: Int } deriving (Eq, Show)

instance H.ToMarkup Sequence where
  toMarkup = H.toMarkup . _unSequence

-- | Simple wrappers to avoid mixing up RA and Dec.

newtype RA = RA { _unRA :: Double } deriving (Eq, Show)  

newtype Dec = Dec { _unDec :: Double } deriving (Eq, Show)  

instance H.ToMarkup RA where
  toMarkup = H.toMarkup . _unRA

instance H.ToMarkup Dec where
  toMarkup = H.toMarkup . _unDec

instance H.ToValue RA where
  toValue = H.toValue . _unRA

instance H.ToValue Dec where
  toValue = H.toValue . _unDec

-- | Store the schedule.
data Schedule = 
   Schedule
   { scTime  :: UTCTime      -- ^ the date when the schedule search was made
   , scDays  :: Int          -- ^ number of days used for the search
   , scDone  :: [Record]     -- ^ those that were done (ascending time order)
   , scDoing :: Maybe Record -- ^ current observation
   , scToDo  :: [Record]     -- ^ those that are to be done (ascending time order)
   }

-- | Represent a value in kiloseconds.
newtype TimeKS = TimeKS { _toS :: Double } deriving (Eq, Show)

instance H.ToMarkup TimeKS where
  toMarkup = H.toMarkup . _toS

instance H.ToValue TimeKS where
  toValue = H.toValue . _toS

-- | A scheduled observation (may be in the past, present, or future).
--
--   The information is taken from <http://cxc.cfa.harvard.edu/target_lists/stscheds/>,
--   and contains information we store elsewhere.
data ScheduleItem = ScheduleItem {
    siObsName :: ObsName
    , siStart :: ChandraTime
    , siEnd :: ChandraTime     -- approx end time
    , siDuration :: TimeKS
    } deriving (Eq, Show)

-- | Represent a science observation.
data ScienceObs = ScienceObs {
  soSequence :: Sequence
  , soObsId :: ObsIdVal
  , soTarget :: String
  , soStartTime :: ChandraTime
  , soTime :: TimeKS
  , soInstrument :: Instrument
  , soGrating :: Grating
  , soRa :: RA
  , soDec :: Dec
  , soRoll :: Double
  , soPitch :: Double
  , soSlew :: Double
  , soContraint :: [ConstrainedObs] -- do we ever have multiple constraints?
  } deriving (Eq, Show)

-- | An observation at another facility that overlaps in time with
--   a Chandra observation.
data ConstrainedObs = ConstrainedObs {
  coFacility :: String    -- name of facility
  , coTime :: TimeKS      -- observation length
  } deriving (Eq, Show)

-- | Represent a non-science/cal observation.
data NonScienceObs = NonScienceObs {
  nsName :: String             -- the STS has a string identifier; where does this come from?
  , nsObsId :: ObsIdVal
  , nsTarget :: String
  , nsStartTime :: ChandraTime
  , nsTime :: TimeKS
  , nsRa :: RA
  , nsDec :: Dec
  , nsRoll :: Double
  , nsPitch :: Double
  , nsSlew :: Double
  } deriving (Eq, Show)
