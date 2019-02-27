{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}


-- | Set up some types for representing Chandra observations. The
--   routines here are occasionally only tangentially-related to
--   types, per se.
--
--   I have taken out derived @Show@ instances for many types
--   to catch cases where I was relying on them for serialization
--   when I should not have been.
--
--   Given the way the code has ended up below, perhaps I should be
--   using some form of a lens library.
--
module Types ( ObsIdVal
             , unsafeToObsIdVal
             , toObsIdValStr
             , fromObsId
               
             , ObsIdStatus(..)
             , toObsIdStatus
             , fromObsIdStatus
               
             , InvalidObsId(..)
               
             , Instrument(..)
             , toInstrument
             , fromInstrument
               
             , Grating(..)
             , toGrating
             , fromGrating
               
             , ChipStatus(..)
             , toChipStatus
             , fromChipStatus
               
             , Sequence
             , unsafeToSequence
             , fromSequence
               
             , Telescope
             , toTelescope
               
             , TargetName
             , toTargetName
             , fromTargetName
               
             , TOORequest
             , toTOORequest
             , tooTime
             , tooValue
               
             , TOORequestTime(..)
             , labelToRT
             , rtToLabel
               
             , Cycle
             , unsafeToCycle
             , toCycle
             , fromCycle
             , allCycles

             , JointMission
             , toMission
             , fromMission
             , fromMissionLong
             , includesMission
             , splitToMission
             , getJointObs
             , fromMissionAboutLink
             , fromMissionLongLink

             , RA
             , toRA
             , fromRA
             , showRA
               
             , Dec
             , toDec
             , fromDec
             , showDec

             , ChandraTime
             , toChandraTime
             , toCTime
             , fromChandraTime
             , endCTime
             , showCTime
               
             , TimeKS
             , unsafeToTimeKS
             , fromTimeKS
             , addTimeKS
             , normTimeKS
             , zeroKS
             , isZeroKS
             , showExpTime

             , ObsStatus(..)
             , getObsStatus
               
             , ScienceObs(..)
             , NonScienceObs(..)
             , ObsInfo(..)

             , Schedule(..)
               
             , Record
             , recordObsId
             , recordStartTime
             , recordTime
             , recordTarget
               
             , RestrictedRecord
             , showExpRestricted
             , rrecordObsId
             , rrecordInstrument
             , rrecordGrating
             , rrecordStartTime
             , rrecordEndTime
             , rrecordTime
             , rrecordTarget
             , rrecordRA
             , rrecordDec
               
             , RestrictedSchedule(..)
               
             , RestrictedSO
             , rsoObsId
             , rsoStartTime
             , rsoExposureTime
             , rsoTarget
             , rsoTOO
             , rsoJointWith
             , rsoConstraints
             , rsoConstellation
               
             , RestrictedNS
             , rnsStartTime
             , rnsExposureTime
             , rnsTarget

             , ShortTermTag
             , toShortTermTag
             , fromShortTermTag
               
             , ShortTermSchedule(..)
               
             , ScienceTimeline
             , EngineeringTimeline
               
             , Constraint(..)
             , toConstraint
             , fromConstraint
               
             , ConstraintKind(..)
             , labelToCS
             , csToLabel
             , csToLC

             , ConLong
             , fromConLong
               
             , ConShort
             , toConShort
             , fromConShort
             , getConstellationName
             , getConstellationNameStr
               
             , Proposal(..)
             , ProposalAbstract(..)
             , MissingProposalAbstract(..)
               
             , PropNum
             , toPropNum
             , fromPropNum
               
             , PropCategory
             , PropType(..)
             , toPropType
             , toPropTypeLabel
             , fromPropType

             , SimbadInfo(..)
             , similarName
               
             , SimbadType
             , toSimbadType
             , fromSimbadType
             , noSimbadType
             , simbadTypeToCode
             , simbadTypeToDesc
             , findChildTypes
               
             , SimbadTypeInfo

             , SimbadCode
             , sc1
             , sc2
             , sc3
             , scLevel
             , simbadLabels
               
             , SimbadMatch(..)
             , SimbadNoMatch(..)
               
             , SIMCategory
             , noSimbadLabel

             , SimbadLoc(..)
             , simbadBase
             , toSIMBADLink
               
             , MetaData
             , toMetaData
               -- mdLastModified isn't used anywhere, but is used by
               -- TH/Groundhog, so export it to avoid a "unused top-level
               -- binding warning.
             , mdLastModified

             , ScheduleItem(..)
               
             , handleMigration
               
               -- ^ not using lenses...
             , _2

               -- ^ Groundhog related symbols. PersistEntity and Field
               --   are needed to export the Type Family symbols
               --   used to represent fields/columns, such as
               --   SoObsIdField.
               --
             , ScienceObsConstructor(..)
             , NonScienceObsConstructor(..)
             , InvalidObsIdConstructor(..)
             , MetaDataConstructor(..)
             , MissingProposalAbstractConstructor(..)
             , ProposalAbstractConstructor(..)
             , ProposalConstructor(..)
             , ShortTermScheduleConstructor(..)
             , SimbadInfoConstructor(..)
             , SimbadMatchConstructor(..)
             , SimbadNoMatchConstructor(..)

             -- , PersistEntity(..)
             , Field(..)

             -- utility routines
             , maybeFromText
             
             ) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Arrow (first)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO)

import Data.Aeson (ToJSON(..))
import Data.Aeson.TH (deriveToJSON, defaultOptions, constructorTagModifier, fieldLabelModifier)
import Data.Bits (Bits(..), FiniteBits(..))
import Data.Char (isDigit, isSpace, toLower)
import Data.Function (on)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.String (IsString(..))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime, TimeLocale
                 , addUTCTime, defaultTimeLocale, parseTimeOrError)

-- I am not convinced I'm adding the PersistField values sensibly
import Database.Groundhog.Core
import Database.Groundhog.TH
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)
import Database.Groundhog.Postgresql (runMigration)

import Formatting
import Formatting.Time

import Network.HTTP.Types.URI (renderSimpleQuery)
import Numeric.Natural

import Text.Read (readMaybe)

import Web.Scotty (Parsable(..))

-- make it easy to compile on different systems for now
readTime :: TimeLocale -> String -> String -> UTCTime
readTime = parseTimeOrError True

-- | Convert text into a value, where the value is restricted
--   by a user-specifiable predicate.
--
--   TODO: can this be done without going via String?
maybeFromText :: 
    Read a
    => (a -> b)
    -> (a -> Bool)  -- ^ is value valid for conversion to b?
    -> T.Text
    -> Maybe b
maybeFromText conv p s = do
  a <- readMaybe (T.unpack s)
  if p a then return (conv a) else Nothing

-- | Inclusive range.
inRange :: 
    Ord a 
    => a    -- ^ lower limit 
    -> a    -- ^ upper limit
    -> a    -- ^ value
    -> Bool -- ^ lower limit <= value <= upper limit
inRange lo hi v = (lo <= v) && (v <= hi)

-- | When was the contents of the database last updated.
--
--   Ideally the monad used to query or update the database
--   would identify when this might be necessary from when
--   not.
--
data MetaData = MetaData { mdLastModified :: UTCTime }
     deriving Eq

toMetaData :: UTCTime -> MetaData
toMetaData = MetaData

-- | Some ObsIDs seem to not have an OCAT record, or we can
--   not parse the response; hopefully these are all non-science
--   observations.
--
--   We want a record so we can stop trying to get information on
--   them.
--
data InvalidObsId = InvalidObsId {
  ioObsId :: ObsIdVal     -- ^ The ObsId in question
  , ioChecked :: UTCTime  -- ^ The approximate time this was checked
  , ioMessage :: T.Text   -- ^ reason it's "bad"
  } deriving Eq

{-
This is based on 'Read Color' instance of RWH, page 142, chapter 6
but hacked to allow a list of strings that map to the same token.
-}

tryParse :: [(a, [String])] -> String -> [(a, String)]
tryParse [] _ = []
tryParse ((result, attempts):xs) value =
    -- the assumption here is that there should be only one match; we
    -- could take the first one if there are multiple matches?
    if length res == 1
    then res
    else tryParse xs value
        where
          res = [(result, drop (length attempt) value) |
                 attempt <- attempts, take (length attempt) value == attempt]

-- | The instrument being used.
--
--   The Ord constraint is useful when creating tables but has no
--   real semantic meaning.
data Instrument = ACISI | ACISS | HRCI | HRCS 
  deriving (Eq, Show, Ord)

instance Read Instrument where
  readsPrec _ =
    tryParse [ (ACISS, ["ACIS-S", "ACISS"])
             , (ACISI, ["ACIS-I", "ACISI"])
             , (HRCI, ["HRC-I", "HRCI"])
             , (HRCS, ["HRC-S", "HRCS"])
             ]

fromInstrument :: Instrument -> T.Text
fromInstrument ACISI = "ACIS-I"
fromInstrument ACISS = "ACIS-S"
fromInstrument HRCI  = "HRC-I"
fromInstrument HRCS  = "HRC-S"

toInstrument :: T.Text -> Maybe Instrument
toInstrument "ACIS-I" = Just ACISI
toInstrument "ACIS-S" = Just ACISS
toInstrument "HRC-I"  = Just HRCI
toInstrument "HRC-S"  = Just HRCS

toInstrument "ACISI" = Just ACISI
toInstrument "ACISS" = Just ACISS
toInstrument "HRCI"  = Just HRCI
toInstrument "HRCS"  = Just HRCS

toInstrument _ = Nothing

helpParse :: LT.Text -> (T.Text -> Maybe a) -> LT.Text -> Either LT.Text a
helpParse lbl conv lt =
  let emsg = "Invalid " <> lbl <> ": " <> lt
      t = LT.toStrict lt
  in maybe (Left emsg) Right (conv t)
  

instance Parsable Instrument where
  parseParam = helpParse "instrument name" toInstrument

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
--
--   The Ord constraint is useful when creating tables but has no
--   real semantic meaning.
--
data Grating = LETG | HETG | NONE 
  deriving (Eq, Ord, Show, Read)

fromGrating :: Grating -> T.Text
fromGrating LETG = "LETG"
fromGrating HETG = "HETG"
fromGrating NONE = "NONE"

toGrating :: T.Text -> Maybe Grating
toGrating "LETG" = Just LETG
toGrating "HETG" = Just HETG
toGrating "NONE"  = Just NONE

toGrating _ = Nothing

instance Parsable Grating where
  parseParam = helpParse "grating name" toGrating

instance H.ToMarkup Grating where
  toMarkup LETG = "Low Energy Transmission Grating (LETG)"
  toMarkup HETG = "High Energy Transmission Grating (HETG)"
  toMarkup NONE = "No grating"

instance H.ToValue Grating where
  toValue LETG = "Low Energy Transmission Grating (LETG)"
  toValue HETG = "High Energy Transmission Grating (HETG)"
  toValue NONE = "No grating"

-- | Pairs are recoreded as instrument + "-" + grating
instance Parsable (Instrument, Grating) where
  parseParam tboth =
    case LT.split (=='-') tboth of
      [l, r] -> do
        inst <- parseParam l
        grat <- parseParam r
        return (inst, grat)
      _ -> Left ("Expected instrument-grating: " <> tboth)
    
-- | Represent an observation identifier.
--
--   Due to a clash with @ObsName@ we use @ObsIdVal@
--   for now, but it's planned to move to @ObsId@.
--
--   This should really store a Word32 rather than an Int.
--
newtype ObsIdVal = ObsIdVal { fromObsId :: Int }
  -- deriving (Eq, Ord, Show)
  deriving (Eq, Ord)

-- | No validation!
--
unsafeToObsIdVal :: Int -> ObsIdVal
unsafeToObsIdVal = ObsIdVal


-- I assume that 0 is valid
validObsIdVal :: Int -> Bool
validObsIdVal = inRange 0 65535

-- | Limited validation of the input.
toObsIdValStr :: T.Text -> Maybe ObsIdVal
toObsIdValStr = maybeFromText ObsIdVal validObsIdVal

{-
-- | Limited validation of the input.
toObsIdValInt :: Int -> Maybe ObsIdVal
toObsIdValInt x = if validObsIdVal x
                  then Just (ObsIdVal x)
                  else Nothing
-}

instance Parsable ObsIdVal where
  parseParam = helpParse "ObsId" toObsIdValStr

instance H.ToMarkup ObsIdVal where
  toMarkup = H.toMarkup . fromObsId

instance H.ToValue ObsIdVal where
  toValue = H.toValue . fromObsId

-- | Represent an entry in the schedule; this is
--   a "catch-all" type that is used as I play around with the
--   database.
--
type Record = Either NonScienceObs ScienceObs

-- | This is intended to simplify information flow, rather than
--   be a semantically "meaningful" representation of the data.
--
--   All aboard the tuple train.
--
type RestrictedRecord = Either RestrictedNS RestrictedSO

-- non-science observations
--   obsid - can derive target name from it
--   start time
--   duration
--   location
--
type RestrictedNS = (ObsIdVal, Maybe ChandraTime, TimeKS, RA, Dec)

-- science observations
--   obsid
--   target name   can quey simbad map for the simbad type
--   start time
--   approved duration
--   observed duration
--   instrument and grating
--   joint with
--   turnaround time
--   constraints
--   location
--   constellation
--
type RestrictedSO =
  (ObsIdVal
  , TargetName
  , Maybe ChandraTime
  , TimeKS
  , Maybe TimeKS
  , Instrument
  , Grating
  , Maybe T.Text
  , Maybe TOORequest
  , Constraint
  , Constraint
  , Constraint
  , RA
  , Dec
  , ConShort)
                       

rrecordObsId :: RestrictedRecord -> ObsIdVal
rrecordObsId = either rnsObsId rsoObsId

rrecordTarget :: RestrictedRecord -> TargetName
rrecordTarget = either rnsTarget rsoTarget

rrecordStartTime :: RestrictedRecord -> Maybe ChandraTime
rrecordStartTime = either rnsStartTime rsoStartTime

rrecordEndTime :: RestrictedRecord -> Maybe ChandraTime
rrecordEndTime r =
  let stime = rrecordStartTime r
      duration = rrecordTime r
  in flip endCTime duration <$> stime
     

rrecordTime :: RestrictedRecord -> TimeKS
rrecordTime = either rnsExposureTime rsoExposureTime

rrecordInstrument :: RestrictedRecord -> Maybe Instrument
rrecordInstrument = either (const Nothing) (Just . rsoInstrument)

rrecordGrating :: RestrictedRecord -> Maybe Grating
rrecordGrating = either (const Nothing) (Just . rsoGrating)

rrecordRA :: RestrictedRecord -> RA
rrecordRA = either rnsRA rsoRA

rrecordDec :: RestrictedRecord -> Dec
rrecordDec = either rnsDec rsoDec

{-
rrecordConstellation :: RestrictedRecord -> Maybe ConShort
rrecordConstellation = either (const Nothing) (Just . rsoConstellation)
-}

-- 

rnsObsId :: RestrictedNS -> ObsIdVal
rnsObsId (oi, _, _, _, _) = oi

-- fake the target name from the obsid
-- QUS: is this sensible? We could just send around the
--      target name here as well
rnsTarget :: RestrictedNS -> TargetName
rnsTarget (oi, _, _, _, _) = TN ("CAL-ER (" <>
                                 sformat int (fromObsId oi) <>
                                 ")")

rnsStartTime :: RestrictedNS -> Maybe ChandraTime
rnsStartTime (_, t, _, _, _) = t

rnsExposureTime :: RestrictedNS -> TimeKS
rnsExposureTime (_, _, t, _, _) = t

rnsRA :: RestrictedNS -> RA
rnsRA (_, _, _, ra, _) = ra

rnsDec :: RestrictedNS -> Dec
rnsDec (_, _, _, _, dec) = dec

--

rsoObsId :: RestrictedSO -> ObsIdVal
rsoObsId (oi, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = oi


rsoStartTime :: RestrictedSO -> Maybe ChandraTime
rsoStartTime (_, _, t, _, _, _, _, _, _, _, _, _, _, _, _) = t

rsoTarget :: RestrictedSO -> TargetName
rsoTarget (_, tn, _, _, _, _, _, _, _, _, _, _, _, _, _) = tn

-- | Prefer the observed time if available
rsoExposureTime :: RestrictedSO -> TimeKS
rsoExposureTime (_, _, _, _, Just ta, _, _, _, _, _, _, _, _, _, _) = ta
rsoExposureTime (_, _, _, to, _, _, _, _, _, _, _, _, _, _, _) = to

rsoInstrument :: RestrictedSO -> Instrument
rsoInstrument (_, _, _, _, _, ins, _, _, _, _, _, _, _, _, _) = ins

rsoGrating :: RestrictedSO -> Grating
rsoGrating (_, _, _, _, _, _, grat, _, _, _, _, _, _, _, _) = grat

rsoJointWith :: RestrictedSO -> Maybe T.Text
rsoJointWith (_, _, _, _, _, _, _, jw, _, _, _, _, _, _, _) = jw

rsoTOO :: RestrictedSO -> Maybe TOORequest
rsoTOO (_, _, _, _, _, _, _, _, too, _, _, _, _, _, _) = too

-- The order should be timecritical, monitor, constrained.
--
rsoConstraints :: RestrictedSO -> [Constraint]
rsoConstraints (_, _, _, _, _, _, _, _, _, tcrit, mon, con, _, _, _)
  = [tcrit, mon, con]

rsoRA :: RestrictedSO -> RA
rsoRA (_, _, _, _, _, _, _, _, _, _, _, _, ra, _, _) = ra

rsoDec :: RestrictedSO -> Dec
rsoDec (_, _, _, _, _, _, _, _, _, _, _, _, _, dec, _) = dec

rsoConstellation :: RestrictedSO -> ConShort
rsoConstellation (_, _, _, _, _, _, _, _, _, _, _, _, _, _, con) = con


newtype TargetName = TN { fromTargetName :: T.Text }
                   deriving (Eq, Ord)

-- | There is no attempt to validate the name.
toTargetName :: T.Text -> TargetName
toTargetName = TN

instance IsString TargetName where
  fromString = TN . T.pack

instance Parsable TargetName where
  parseParam = helpParse "TargetName" (Just . TN)

instance ToJSON TargetName where
  toJSON TN {..} = toJSON fromTargetName

instance H.ToMarkup TargetName where
  toMarkup TN {..} = H.toMarkup fromTargetName

instance H.ToValue TargetName where
  toValue TN {..} = H.toValue fromTargetName
  
-- hacks for quickly converting old code; however, the idea of
-- a Record has stuck around for a while, so it may need to stay

{-
recordSequence :: Record -> Maybe Sequence
recordSequence = either (const Nothing) (Just . soSequence)
-}

recordObsId :: Record -> ObsIdVal
recordObsId = either nsObsId soObsId

recordTarget :: Record -> TargetName
recordTarget = either nsTarget soTarget

recordStartTime :: Record -> Maybe ChandraTime
recordStartTime = either nsStartTime soStartTime

{-
recordStartTimeUnsafe :: Record -> ChandraTime
recordStartTimeUnsafe = fromJust . recordStartTime
-}

-- Use the actual time if we have it, otherwise the approved time
recordTime :: Record -> TimeKS
recordTime = either nsTime (\ScienceObs{..} -> fromMaybe soApprovedTime soObservedTime)

{-
recordInstrument :: Record -> Maybe Instrument
recordInstrument = either (const Nothing) (Just . soInstrument)

recordGrating :: Record -> Maybe Grating
recordGrating = either (const Nothing) (Just . soGrating)

recordRa :: Record -> RA
recordRa = either nsRa soRA 

recordDec :: Record -> Dec
recordDec = either nsDec soDec 

recordRoll :: Record -> Double
recordRoll = either nsRoll soRoll
-}

-- | I just want a simple way of passing around 
--   useful information about an observation.
data ObsInfo = ObsInfo {
  oiCurrentObs :: Record
  , oiPrevObs  :: Maybe Record
  , oiNextObs  :: Maybe Record
  }
  -- deriving (Eq, Show)
  deriving Eq

-- | A wrapper around `UTCTime` so that we can use our
--   own `ToMarkup` and `ToValue` instances.
--
newtype ChandraTime = ChandraTime { _toUTCTime :: UTCTime }
  -- deriving (Eq, Ord, Show)
  deriving (Eq, Ord)

toChandraTime :: UTCTime -> ChandraTime
toChandraTime = ChandraTime

fromChandraTime :: ChandraTime -> UTCTime
fromChandraTime = _toUTCTime

readsPrecF :: (Read a) => (a -> b) -> Int -> ReadS b
readsPrecF f i s = 
    let xs = readsPrec i s
    in map (first f) xs

-- Needed for readHelper, used by the PrimitivePersistField instance
instance Read ChandraTime where
  readsPrec = readsPrecF ChandraTime

-- TODO: validate ra as 0 to 360
instance Read RA where
  readsPrec = readsPrecF RA

-- TODO: validate dec as -90 to 90
instance Read Dec where
  readsPrec = readsPrecF Dec

-- TODO: validate time as >= 0
instance Read TimeKS where
  readsPrec = readsPrecF TimeKS

instance Read ObsIdVal where
  readsPrec = readsPrecF ObsIdVal

instance Read Sequence where
  readsPrec = readsPrecF Sequence

instance Read PropNum where
  readsPrec = readsPrecF PropNum

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
showCTime :: ChandraTime -> T.Text
showCTime ct = 
  let utc = _toUTCTime ct

      -- I want the equivalent of
      --   formatTime defaultTimeLocale "%R %A, %e %B %Y (UTC)
      --
      -- %R is same as %H:%M
      -- %A is day of week, long form
      -- %e is day of month, space-padded
      -- %B is month name, long form
      -- %Y is year, no padding (for the use case here it
      --    does not matter about padding)
      --
      tfmt = hm <> " " % dayName <> ", " % dayOfMonthS <>
             " " % monthName <> " " % year

  in sformat (tfmt % " (UTC)") utc

endCTime :: ChandraTime -> TimeKS -> ChandraTime
endCTime (ChandraTime start) (TimeKS elen) =
  let nsec = 1000 * elen
      delta = (fromRational . toRational) nsec
  in ChandraTime (addUTCTime delta start)

instance H.ToMarkup ChandraTime where
  toMarkup = H.toMarkup . showCTime

instance H.ToValue ChandraTime where
  toValue = H.toValue . showCTime

data ObsStatus = Done | Doing | Todo | Unscheduled deriving Eq

getObsStatus :: 
  Maybe (ChandraTime, ChandraTime) -- observation start and end times
  -> UTCTime        -- current time
  -> ObsStatus
getObsStatus (Just (ChandraTime sTime, ChandraTime eTime)) cTime
  | cTime < sTime       = Todo
  | cTime <= eTime      = Doing
  | otherwise           = Done
getObsStatus _ _  = Unscheduled  

-- | Represent a Chandra sequence number.
newtype Sequence = Sequence { _unSequence :: Int } 
   -- deriving (Eq, Ord, Show)
   deriving (Eq, Ord)

unsafeToSequence :: Int -> Sequence
unsafeToSequence = Sequence

fromSequence :: Sequence -> Int
fromSequence = _unSequence

{-
-- | Limited validation of the input.
toSequenceInt :: Int -> Maybe Sequence
toSequenceInt s = if s > 0
                  then Just (Sequence s)
                  else Nothing
-}

-- | Limited validation of the input.
toSequenceStr :: T.Text -> Maybe Sequence
toSequenceStr = maybeFromText Sequence (> 0)

instance Parsable Sequence where
  parseParam = helpParse "Sequence" toSequenceStr

instance H.ToMarkup Sequence where
  toMarkup = H.toMarkup . _unSequence

instance H.ToValue Sequence where
  toValue = H.toValue . _unSequence

-- | Represent a Chandra proposal number.
newtype PropNum = PropNum { _unPropNum :: Int } 
   -- deriving (Eq, Ord, Show)
   deriving (Eq, Ord)

-- | There is no validation.
--
toPropNum :: Int -> PropNum
toPropNum = PropNum

-- | Limited validation of the input (currently only
--   enforces a positive value).
toPropNumStr :: T.Text -> Maybe PropNum
toPropNumStr = maybeFromText PropNum (> 0)

fromPropNum :: PropNum -> Int
fromPropNum = _unPropNum

instance Parsable PropNum where
  parseParam = helpParse "proposal number" toPropNumStr

instance H.ToMarkup PropNum where
  toMarkup = H.toMarkup . _unPropNum

instance H.ToValue PropNum where
  toValue = H.toValue . _unPropNum

-- | Simple wrappers to avoid mixing up RA and Dec.
--
--   Note that equality for RA and Dec is defined
--   with a tolerance, since it looks like there is a difference
--   when serializing to/from the database. A simple
--   absolute tolerance is used.
--
--   There's an Ord instance for Dec but not RA, since it currently
--   isn't needed.
--
newtype RA = RA { _unRA :: Double } 

newtype Dec = Dec { _unDec :: Double } 
  deriving Ord

-- | Currently no validation.
--
toRA :: Double -> RA
toRA = RA

-- | Currently no validation.
--
toDec :: Double -> Dec
toDec = Dec


fromRA :: RA -> Double
fromRA = _unRA

fromDec :: Dec -> Double
fromDec = _unDec


-- Internal routine; assumes that tol is >= 0              
isClose :: Double -> Double -> Double -> Bool
isClose tol a b = let adiff = abs (a - b)
                  in adiff <= tol

atol :: Double
atol = 1e-6

-- | The absolute tolerance is 1e-6, which is about 4 micro arcseconds,
--   unless my math has deserted me.
instance Eq RA where
  (==) = isClose atol `on` _unRA

-- | The absolute tolerance is 1e-6, which is about 4 micro arcseconds,
--   unless my math has deserted me.
instance Eq Dec where
  (==) = isClose atol `on` _unDec

  
splitRA :: RA -> (Int, Int, Double)
splitRA (RA ra) = 
    let rah = ra / 15.0
        (h, r1) = properFraction rah
        ram = r1 * 60
        (m, r2) = properFraction ram
        s = r2 * 60
    in (h, m, s)

int2 :: Format r (Int -> r)
int2 = left 2 '0' %. int

float4 :: Format r (Double -> r)
float4 = left 4 '0' %. fixed 1

-- I do use this in ObsCat.hs for informational purposes, so keep
-- around for now.
--
-- TODO: use Data.Text.Format
showRA :: RA -> T.Text
showRA ra = 
  let (h, m, s) = splitRA ra
  in sformat (int % "h " % int % "m " % fixed 1 % "s") h m s

htmlRA :: RA -> H.Html
htmlRA ra = 
  let (h, m, s) = splitRA ra

      hsym = H.sup "h"
      msym = H.sup "m"
      ssym = H.sup "s"

      hstr = sformat int2 h
      mstr = sformat (" " % int2) m
      sstr = sformat (" " % float4) s

  in H.toMarkup hstr <> hsym <> H.toMarkup mstr
     <> msym <> H.toMarkup sstr <> ssym

-- this is intended for HTML/UTF-8 output, so instead of
-- "d" it uses "\176", aka \u00b0, the degree symbol.
--
-- TODO: use Data.Text.Format
showDec :: Dec -> T.Text
showDec (Dec dec) = 
  let dabs = abs dec
      d, m :: Int
      r1, r2 :: Double
      (d, r1) = properFraction dabs
      dm = r1 * 60
      (m, r2) = properFraction dm
      s = r2 * 60
      c = if dec < 0 then '-' else '+'

  in sformat (char % int2 % "\176 " % int2 % "' " % float4 % "\"") c d m s

instance H.ToMarkup RA where
  -- toMarkup = H.toMarkup . showRA
  toMarkup = H.toMarkup . htmlRA

instance H.ToMarkup Dec where
  toMarkup = H.toMarkup . showDec

-- I could imagine we might want these to be the decimal values
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
   , scSimbad :: M.Map TargetName SimbadInfo
     -- ^ mapping from target to SIMBAD info for the observations in this
     --   schedule (it may be empty)
   }


data RestrictedSchedule = 
   RestrictedSchedule
   { rrTime  :: UTCTime      -- ^ the date when the schedule search was made
   , rrUpdateTime :: Maybe UTCTime
     -- ^ as this time the schedule is outdated, and needs
     --   re-evaluating (to move Doing/ToDo items into Done)
     --
   , rrDays  :: Int          -- ^ number of days used for the search
   , rrDone  :: [RestrictedRecord]     -- ^ those that were done (ascending time order)
   , rrDoing :: Maybe RestrictedRecord -- ^ current observation
   , rrToDo  :: [RestrictedRecord]     -- ^ those that are to be done (ascending time order)
   , rrSimbad :: M.Map TargetName SimbadInfo
     -- ^ mapping from target to SIMBAD info for the observations in this
     --   schedule (it may be empty)
   }


-- | Return just the data that is needed for the timeline.
--
-- Note that ScienceTimeline and RestrictedSO are similar, but
-- not the same.
--
type ScienceTimeline =
  (ObsIdVal
  , PropNum
  , TargetName
  , Maybe ChandraTime      -- Start time
  , Maybe UTCTime    -- public release date
  , TimeKS           -- approved time
  , Maybe TimeKS     -- observed time
  , Instrument
  , Grating
  , Maybe T.Text   -- the data mode
  , Maybe TOORequest
  , ConShort)


type EngineeringTimeline =
  (ObsIdVal
  , TargetName
  , Maybe ChandraTime  -- start time
  , TimeKS  -- length
  )


-- | Represent a value in kiloseconds.
--
--   It is assumed that the time value is >= 0.
--
newtype TimeKS = TimeKS { _toKS :: Double } 
  -- deriving (Eq, Ord, Show)
  deriving (Eq, Ord)

fromTimeKS :: TimeKS -> Double
fromTimeKS = _toKS

unsafeToTimeKS :: Double -> TimeKS
unsafeToTimeKS = TimeKS

zeroKS :: TimeKS
zeroKS = TimeKS 0

-- | The library does not export a smart constructor,
--   so treat anything zero or negative as zero.
--
isZeroKS :: TimeKS -> Bool
isZeroKS (TimeKS a) = a <= 0

addTimeKS :: TimeKS -> TimeKS -> TimeKS
addTimeKS (TimeKS a) (TimeKS b) = TimeKS (a+b)

-- | Divide the time by a value.
normTimeKS :: Integral n => TimeKS -> n -> TimeKS
normTimeKS (TimeKS a) n = TimeKS (a / fromIntegral n)

-- | Convert to a more "friendly" exposure time value.
--
--   As the minimum time appears to be 0.1 ks we do not
--   have to deal with sub minute values, but include
--   just in case. Assume that max is ~ 100ks, which is
--   ~ 28 hours, so need to deal with days. However, this
--   is now being used for aggregating many observations,
--   so need to go to weeks. Ideally would use larger
--   units, but it's a bit murky what is meant there, so
--   let's see how well weeks and days work.
--
--   The current approach converts 24 hours and 50 minutes
--   to "1 day", and does not round up to "1 day and 1 hour".
--   This should probably be addressed. Similarly,
--   instead of rounding to "1 week and 1 day", it returns
--   "1 week" for 7.9 days
--
showExpTime :: TimeKS -> T.Text
showExpTime (TimeKS tks) = 
  let ns = round (tks * 1000) :: Int

      -- This does not include the remainder in the calculation
      -- of the "next" value, which is why we see some
      -- surprising rounding.
      (nm, rs) = divMod ns 60
      (nh, rm) = divMod nm 60
      (nd, rh) = divMod nh 24
      (nw, rd) = divMod nd 7
      (ny, rw) = divMod nw 52
       
      units v1 v2 u1 u2 =
        let us 0 _ = ""
            us 1 u = "1 " <> u
            us x u = sformat (int % " " % stext % "s") x u

            str1 = us v1 u1
            str2 = us v2 u2
            sep = if T.null str1 || T.null str2 then "" else " and "
            
        in str1 <> sep <> str2

  in if ns < 3600
     then units nm rs "minute" "second"
     else if nh < 24
          then units nh rm "hour" "minute"
          else if nd < 7
               then units nd rh "day" "hour"
               else if nw < 52
                    then units nw rd "week" "day"
                    else units ny rw "year" "week"

{-                         
showExp :: Record -> H.Html
showExp = H.toHtml . showExpTime . recordTime
-}

showExpRestricted :: RestrictedRecord -> H.Html
showExpRestricted = H.toHtml . showExpTime . rrecordTime

-- do we want this to be in a nice readable value or in ks?
instance H.ToMarkup TimeKS where
  toMarkup = H.toMarkup . _toKS

instance H.ToValue TimeKS where
  toValue = H.toValue . _toKS

-- | A short-term schedule is identified by a tag - e.g.
--   "DEC2312A".
--
--   There is no validation on the value used as the tag
--   since the format has changed over time; the following
--   have all been used "06AUG16A", "APR2002A", "FEB0402".
--
newtype ShortTermTag = ShortTermTag { fromShortTermTag :: T.Text }
                     deriving (Eq, Ord)

-- | This strips out leading and trailing white space, which
--   appears to be unnescessary but left in, just in case.
--
toShortTermTag :: T.Text -> ShortTermTag
toShortTermTag = ShortTermTag . T.toUpper . T.strip


-- | The short-term schedules for which we have data.
--
data ShortTermSchedule = ShortTermSchedule {
  stsTag :: ShortTermTag
  -- ^ The identifier for the schedule.
  , stsChecked :: UTCTime
  -- ^ The time the schedule was queried
  , stsErrorOnParse :: Maybe T.Text
  -- ^ Was there a problem parsing data.? The intention is to
  --   have a way to say "don't re-try this schedule", although
  --   it is not yet clear if it is really needed.
  }

-- | A scheduled observation (may be in the past, present, or future).
--
--   The information is taken from <http://cxc.cfa.harvard.edu/target_lists/stscheds/>,
--   and contains information we store elsewhere.
--
--   We use this to represent the data we get from the Short-Term
--   schedule pages, but it no longer goes into the database.
--
data ScheduleItem = ScheduleItem {
    siObsId :: ObsIdVal
    , siScienceObs :: Bool
    , siStart :: ChandraTime
    -- , siEnd :: ChandraTime     -- approx end time
    , siDuration :: TimeKS
    , siRA :: RA
    , siDec :: Dec
    , siRoll :: Double
    }
  -- deriving (Eq, Show)
  deriving Eq

-- | Helper function for displaying start times.
--
showStartTime :: Maybe ChandraTime -> T.Text
showStartTime (Just ct) = "at " <> showCTime ct
showStartTime Nothing   = "unscheduled"

-- | Represent a non-science/cal observation.
--
--   There appear to be obsids - e.g. 52323
--   https://cda.cfa.harvard.edu/chaser/startViewer.do?menuItem=details&obsid=52323
--   which claim to be observed but have no start_date field.
--   I am going to assume that these are essentially rejected/discarded
--   but that the OCAT isn't updated.
--
data NonScienceObs = NonScienceObs {
  nsStatus :: ObsIdStatus
  -- , nsName :: T.Text
  -- the STS has a string identifier; where does this come from?
  -- It looks like these names may be removed once the "observation"
  -- is finished.
  , nsObsId :: ObsIdVal
  , nsTarget :: TargetName
  , nsStartTime :: Maybe ChandraTime
  , nsTime :: TimeKS
  , nsRa :: RA
  , nsDec :: Dec
  , nsRoll :: Double
  }
  -- deriving (Eq, Show)
  deriving Eq

-- | This is for debug purposes.
instance Show NonScienceObs where
  show NonScienceObs{..} = 
    "CAL: " <> show (fromObsId nsObsId)
    <> "(" <> T.unpack (fromObsIdStatus nsStatus)
    <> ") for " <> show (_toKS nsTime)
    <> " ks " <> T.unpack (showStartTime nsStartTime)

-- | Is a chip on, off, or optional.
--
--   How many optional chips are allowed?
--
--   I assume ChipD is for dropped? Seen in ObsId 1547
--
data ChipStatus = ChipOn | ChipOff | ChipOpt1 | ChipOpt2 | ChipOpt3 | ChipOpt4 | ChipOpt5 | ChipD
  deriving Eq

toChipStatus :: T.Text -> Maybe ChipStatus
toChipStatus s | s == "Y"  = Just ChipOn
               | s == "N"  = Just ChipOff
               | s == "D"  = Just ChipD
               | s == "O1" = Just ChipOpt1
               | s == "O2" = Just ChipOpt2
               | s == "O3" = Just ChipOpt3
               | s == "O4" = Just ChipOpt4
               | s == "O5" = Just ChipOpt5
               | otherwise = Nothing

fromChipStatus :: ChipStatus -> T.Text
fromChipStatus ChipOn   = "Y"
fromChipStatus ChipOff  = "N"
fromChipStatus ChipD    = "D"
fromChipStatus ChipOpt1 = "O1"
fromChipStatus ChipOpt2 = "O2"
fromChipStatus ChipOpt3 = "O3"
fromChipStatus ChipOpt4 = "O4"
fromChipStatus ChipOpt5 = "O5"

-- | Chandra constraints can be none, required, or preferred
data Constraint = NoConstraint | Preferred | Required
  deriving (Eq, Ord)

toConstraint :: Char -> Maybe Constraint
toConstraint 'N' = Just NoConstraint
toConstraint 'P' = Just Preferred
toConstraint 'Y' = Just Required
toConstraint _   = Nothing

fromConstraint :: Constraint -> Char
fromConstraint NoConstraint = 'N'
fromConstraint Preferred    = 'P'
fromConstraint Required     = 'Y'

-- | Represent those observations with time-critical or monitor
--   constraints. There's no indication of what the constraint
--   type is (i.e. a mapping between this and the `Constraint` type).
--
--   At present, it appears that all monitoring proposals are
--   also time critical. There is a prefered time-critical observation
--   which has no for the constrained field.
--
--   The Ord constraint is for convenience.
data ConstraintKind =
  TimeCritical | Monitor | Constrained
  deriving (Eq, Ord)

{-
-- Report a constraint if it is preferred or required. At present
-- do not distinguish between the two.
getConstraintKinds :: ScienceObs -> [ConstraintKind]
getConstraintKinds ScienceObs {..} =
  let go l f = if f == NoConstraint then [] else [l]
  in mconcat [ go TimeCritical soTimeCritical
             , go Monitor soMonitor
             , go Constrained soConstrained ]
-}

-- | Note that this does not handle "none".
--
--   This is a case-insensitive conversion
labelToCS :: T.Text -> Maybe ConstraintKind
labelToCS =
  let go "timecritical" = Just TimeCritical
      go "monitor" = Just Monitor
      go "constrained" = Just Constrained
      go _ = Nothing
  in go . T.toLower

csToLC :: ConstraintKind -> T.Text
csToLC TimeCritical = "timecritical"
csToLC Monitor = "monitor"
csToLC Constrained = "constrained"

csToLabel :: ConstraintKind -> T.Text
csToLabel TimeCritical = "Time Critical"
csToLabel Monitor = "Monitoring"
csToLabel Constrained = "Constrained"

-- | The status field of an observation. Hopefully this is
--   all the states it could be.
--
data ObsIdStatus =
  Discarded | Canceled | Unobserved | Scheduled | Observed | Archived
   deriving Eq

-- | The conversion is case sensitive.            
toObsIdStatus :: T.Text -> Maybe ObsIdStatus
toObsIdStatus "discarded" = Just Discarded
toObsIdStatus "canceled" = Just Canceled
toObsIdStatus "unobserved" = Just Unobserved
toObsIdStatus "scheduled" = Just Scheduled
toObsIdStatus "observed" = Just Observed
toObsIdStatus "archived" = Just Archived
toObsIdStatus _ = Nothing

fromObsIdStatus :: ObsIdStatus -> T.Text
fromObsIdStatus Discarded = "discarded"
fromObsIdStatus Canceled = "canceled"
fromObsIdStatus Unobserved = "unobserved"
fromObsIdStatus Scheduled = "scheduled"
fromObsIdStatus Observed = "observed"
fromObsIdStatus Archived = "archived"

-- | It would be good to use an enumeration here, but new telescopes
--   could be added, and I do not have a good list of the current
--   choices.
--
--   Turns out the telescope field is not well described as a
--   comma-separated list of names, since there are cases with
--   something like [may be more spaces than in the original record):
--
--   "HST and Ground (VLA, mm, mid-IR, optical, and gamma-ray)"
--   "ESO/Nustar (to be confirmed though)"
--
--   So, the intention is that this now represents the full
--   input string, so really should be called something like
--   OtherTelescopes.
--
newtype Telescope = Telescope { fromTelescope :: T.Text }
                  deriving Eq

-- | There is no validation.
--
toTelescope :: T.Text -> Telescope
toTelescope = Telescope

-- | Represent a science observation, using data from the Chandra observing
--   catalog (OCAT) rather than the short-term schedule page.
--
--   The layout is determined by the desire to store the data in a
--   database using a \"language agnostic\" scheme - i.e. where it
--   would make sense to have a field like
--
--   > soSubArray :: Maybe (Int, Int) -- ^ start row, number of rows
--
--   (which seems to cause problems for Groundhog), I use
--
--   > soSubArrayStart :: Maybe Int
--   > soSubArraySize :: Maybe Int
--
data ScienceObs = ScienceObs {
  soSequence :: Sequence -- TODO: DefaultKey Proposal ?
  , soProposal :: PropNum -- the proposal number
  , soStatus :: ObsIdStatus
  , soObsId :: ObsIdVal
  , soTarget :: TargetName
    
    -- there are times when an observation has no start time; I have
    -- previously used a marked to indicate this (e.g. a value before
    -- Chandra launched), but it complicates things, so let's model
    -- the data properly).
    --
  , soStartTime :: Maybe ChandraTime
  , soApprovedTime :: TimeKS
  , soObservedTime :: Maybe TimeKS
  , soPublicRelease :: Maybe UTCTime -- ^ release date

  , soTimeCritical :: Constraint
  , soMonitor :: Constraint
  , soConstrained :: Constraint

  , soInstrument :: Instrument
  , soGrating :: Grating
  , soDetector :: Maybe T.Text   -- this is only available for archived obs
  , soDataMode :: Maybe T.Text -- use an enumeration; not available for HRC

    -- these are meaningless for HRC observations; in this case ChipStatus
    -- is set to ChipOff
    
  , soACISI0 :: ChipStatus  
  , soACISI1 :: ChipStatus  
  , soACISI2 :: ChipStatus  
  , soACISI3 :: ChipStatus  
  , soACISS0 :: ChipStatus  
  , soACISS1 :: ChipStatus  
  , soACISS2 :: ChipStatus  
  , soACISS3 :: ChipStatus  
  , soACISS4 :: ChipStatus  
  , soACISS5 :: ChipStatus  

  , soJointWith :: Maybe T.Text
  , soJointHST :: Maybe TimeKS
  , soJointNOAO :: Maybe TimeKS
  , soJointNRAO :: Maybe TimeKS
  , soJointRXTE :: Maybe TimeKS
  , soJointSPITZER :: Maybe TimeKS
  , soJointSUZAKU :: Maybe TimeKS
  , soJointXMM :: Maybe TimeKS
  , soJointSWIFT :: Maybe TimeKS
  , soJointNUSTAR :: Maybe TimeKS

  , soMultiTel :: Bool
  , soMultiTelInt :: Double

    -- So, it turns out that Maybe Telescope would be a better
    -- field, but for now keep as [Telescope]. The intention is
    -- that there is either 0 or 1 element. 
    --
  , soMultiTelObs :: [Telescope]
    
  , soTOO :: Maybe TOORequest
  , soRA :: RA
  , soDec :: Dec
  , soConstellation :: ConShort
  , soRoll :: Double
  , soSubArrayStart :: Maybe Int
  , soSubArraySize :: Maybe Int
  } 
  deriving Eq


-- | Bundle up the TOO periods into something a bit more user-friendly.
--   The idea is that the database keeps the original field, but
--   the API provides, where relevant, conversion routines.
--
--   The TOO fields do appear to be open ended, in that I've see
--   "0-4" and "0-5", as well as ">30" and ">=30". So there needs to
--   be some validation to check that we don't come across new values
--   that don't fall into the categorisation below. At least one old
--   TOO field is listed as "FAST" (ObsId 3803), and some have "SLOW".
--
--   Current values include:
--      less than a week   -> immediate / asap
--      1 to 2 weeks       -> quick
--      2 weeks to 1 month -> intermediate
--      more than a month  -> slow
--
--   TODO: do I want a type that carries along both the enumeration
--   and the actual value (essentially as a tag, so that it can
--   be serialized to the database as a string, to avoid schema changes)?
--   Hmmm, might try this, as then have the "nice" TOO handling everywhere.
--
--   I am beginning to think that I should include None in TOORequestTime,
--   which would remove the need for a Maybe for the field, which would
--   then be a schema change.
data TOORequestTime =
  Immediate | Quick | Intermediate | Slow
  deriving (Eq, Ord)

rtToLabel :: TOORequestTime -> T.Text
rtToLabel Immediate = "Immediate"
rtToLabel Quick = "Quick"
rtToLabel Intermediate = "Intermediate"
rtToLabel Slow = "Slow"

-- | Note that this does not handle "none".
--
--   This is a case-insensitive conversion
labelToRT :: T.Text -> Maybe TOORequestTime
labelToRT lbl =
  let go "immediate" = Just Immediate
      go "quick" = Just Quick
      go "intermediate" = Just Intermediate
      go "slow" = Just Slow
      go _ = Nothing
  in go (T.toLower lbl)

data TOORequest = TR { trType :: TOORequestTime
                     , trValue :: T.Text }

tooTime :: TOORequest -> TOORequestTime
tooTime = trType

tooValue :: TOORequest -> T.Text
tooValue = trValue

{-
-- | This creates a request with an empty value field,
--   which is not ideal and should only be used to create
--   a request used in a comparison, rather than one that
--   will make it to the database.
--
--   I may change to create a time range value that is
--   appropriate for the type.
--
rtToRequest :: TOORequestTime -> TOORequest
rtToRequest rt = TR rt ""
-}

-- | A request is considered equal if its \"type\" is equal,
--   not its actual value. This is potentially unsafe.
--   
instance Eq TOORequest where
  (==) = (==) `on` trType

-- | Needed as the data is stored as Maybe TOORequest (requirement of
--   groundhog 0.7).
--
instance NeverNull TOORequest

-- | Assume that the actual value is not important here, just the type.
--   This could be changed to use an object
instance ToJSON TOORequest where
  toJSON TR {..} = toJSON (rtToLabel trType)

-- | Convert a TOO label.
--
--   The label is assumed to be of the form
--       a-b   so use b as the limit
--       >b
--       >=b
--       FAST  - which means what? Going to assume Immediate.
--       SLOW  - which means what? Going to assume Slow.
--
--   A check is made to ensure both a and b are integers,
--   mainly because it was easier, but also to ensure
--   we don't have a case like "-34"
--
--   TODO: should this deal with the "n/a" case?
--
toTOORequest :: T.Text -> Maybe TOORequest
toTOORequest "FAST" = Just (TR Immediate "FAST")
toTOORequest "SLOW" = Just (TR Immediate "SLOW")
toTOORequest too =
  let toNat :: T.Text -> Maybe Natural
      toNat = readMaybe . T.unpack

      toksHyphen = map toNat (T.splitOn "-" too)

      ladder highVal
        | highVal < 7 = Immediate
        | highVal <= 15 = Quick
        | highVal <= 30 = Intermediate
        | otherwise = Slow

      -- NOTE: assume that >x or >=x is only valid for x=30
      mlbl =
        if too `elem` [">30", ">=30"]
        then Just Slow
        else case toksHyphen of
          [Just _, Just highVal] -> Just (ladder highVal)
          _ -> Nothing

  in case mlbl of
    Just lbl -> Just (TR lbl too)
    _ -> Nothing

{-
-- | Convert a TOO label. This uses a heuristic, rather than converting
--   the input string to a numeric value and deriving the value from
--   those.
--
toTOORequest :: T.Text -> Maybe TOORequest
toTOORequest too = ($ too) `fmap` lookup too tooMap

tooMap :: [(T.Text, T.Text -> TOORequest)]
tooMap =
  [ ("0-4", TR Immediate)
  , ("0-5", TR Immediate)
  , ("4-12", TR Quick)
  , ("4-15", TR Quick)
  , ("5-15", TR Quick)
  , ("15-30", TR Intermediate)
  , ("12-30", TR Intermediate)
  , (">=30", TR Slow)
  , (">30", TR Slow)
  ]
-}

-- | What are the possible joint missions? This enumeration is for
--   end-user code and is not to be used in the database, as there's
--   no guarantee it won't change and I can not be bothered with
--   updating the schema.
--
--   The Ord instance is added so that the type can be used in
--   various contexts - in particular the key to a map - but
--   it has no semantic meaning.
--
data JointMission =
  HST | NOAO | NRAO | RXTE | Spitzer | Suzaku | XMM | Swift | NuSTAR
  deriving (Eq, Ord)

-- Valid mappings for the mission names
-- TODO: could use missionMap with logic like "check for
-- all upper case" instead of this
vmissionMap :: [(T.Text, JointMission)]
vmissionMap =
  [ ("HST", HST)
  , ("NOAO", NOAO)
  , ("NRAO", NRAO)
  , ("RXTE", RXTE)
  , ("Spitzer", Spitzer)
  , ("SPITZER", Spitzer)
  , ("Suzaku", Suzaku)
  , ("SUZAKU", Suzaku)
  , ("XMM", XMM)
  , ("Swift", Swift)
  , ("SWIFT", Swift)
  , ("NuSTAR", NuSTAR)
  , ("NUSTAR", NuSTAR)
  ]

toMission :: T.Text -> Maybe JointMission
toMission m = lookup m vmissionMap

instance Parsable JointMission where
  parseParam = helpParse "mission name" toMission

-- Hmmm, lose the ability for the compiler to catch a missing
-- value like this.
missionMap :: [(JointMission, (T.Text, T.Text, H.AttributeValue))]
missionMap =
  [ (HST, ("HST", "Hubble Space Telescope (HST)"
          , "https://www.nasa.gov/mission_pages/hubble/main/index.html"))
  , (NOAO, ("NOAO", "National Optical Astronomy Observatory (NOAO)"
           , "http://www.noao.edu/"))
  , (NRAO, ("NRAO", "National Radio Astronomy Observatory (NRAO)"
           , "http://www.nrao.edu/"))
  , (RXTE, ("RXTE", "Rossi X-ray Timeing Explorer (RXTE)"
           , "https://heasarc.gsfc.nasa.gov/docs/xte/rxte.html"))
  , (Spitzer, ("Spitzer", "Spitzer Space Telescope"
              , "http://www.nasa.gov/mission_pages/spitzer/main/index.html"))
  , (Suzaku, ("Suzaku", "Suzaku X-ray satellite"
             , "https://www.nasa.gov/suzaku/"))
  , (XMM, ("XMM", "X-ray Multi Mirror Mission (XMM-Newton)"
          , "http://sci.esa.int/xmm-newton/"))
  , (Swift, ("Swift", "Swift Gamma-Ray Burst Mission"
            , "http://swift.gsfc.nasa.gov/"))
  , (NuSTAR, ("NuSTAR", "Nuclear Spectroscopic Telescope Array (NuSTAR)"
             , "http://www.nustar.caltech.edu/"))
  ]

fromMission :: JointMission -> T.Text
fromMission m = case lookup m missionMap of
  Just x -> _1 x
  Nothing -> error "Internal error: missing mission fromMission"

fromMissionLong :: JointMission -> T.Text
fromMissionLong m = case lookup m missionMap of
  Just x -> _2 x
  Nothing -> error "Internal error: missing mission fromMissionLong"

-- | Link to the search page.
--
--   This should really be in Utils.
--
fromMissionLongLink :: JointMission -> H.Html
fromMissionLongLink m = case lookup m missionMap of
  Just x -> let url = H.toValue ("/search/joint/" <> _1 x)
            in (H.a H.! A.href url) (H.toHtml (_2 x))
  Nothing -> error "Internal error: missing mission fromMissionLongLink"

-- | Link to a page about the mission.
fromMissionAboutLink :: JointMission -> H.Html
fromMissionAboutLink m = case lookup m missionMap of
  Just x -> (H.a H.! A.href (_3 x)) (H.toHtml (_2 x))
  Nothing -> error "Internal error: missing mission fromMissionAboutLink"

-- | Does the list of Joint-with observatories include
--   the mission.
--
includesMission :: JointMission -> T.Text -> Bool
includesMission m = case lookup m missionMap of
  Just x -> T.isInfixOf (_1 x)
  Nothing -> error "Internal error: missing mission includesMission"

-- | Convert the joint-with field into a list of missions.
--
--   The assumption is that a CXO-??? value only has a single mission,
--   i.e. there's no values like "CXO-HST,NRAO".
--
splitToMission ::
  T.Text
  -> [JointMission]
splitToMission term =
  let cterm = if "CXO-" `T.isPrefixOf` term then T.drop 4 term else term
      toks = T.splitOn "-" cterm
  in mapMaybe toMission toks


{-
-- The "field" variant is at the end of the module since it needs
-- the TH code to have been evaluated.
--
missionSelector :: JointMission -> ScienceObs -> Maybe TimeKS
missionSelector HST = soJointHST
missionSelector NOAO = soJointNOAO
missionSelector NRAO = soJointNRAO
missionSelector RXTE = soJointRXTE
missionSelector Spitzer = soJointSPITZER
missionSelector Suzaku = soJointSUZAKU
missionSelector XMM = soJointXMM
missionSelector Swift = soJointSWIFT
missionSelector NuSTAR = soJointNUSTAR
-}

{-
-- | What observations \"overlap\" this one, and are
--   publically available.
--   The overlap is determined by the find_chandra_obsid script
--   from CIAO.
--
data OverlapObs = 
  OverlapObs {
    ovObsId :: ObsIdVal       -- ^ field being checked
    , ovOverlapId :: ObsIdVal -- ^ overlaps ovObsId and is publically available
    , ovDistance :: Double    -- ^ distance from ovObsId in arcminutes
    , ovCheck :: UTCTime      -- ^ when was the check made; this is probably not useful
  } deriving Eq
-}

-- | Short form for constellation names (e.g. UMa for Ursa Major).
--
--   See <http://www.astro.wisc.edu/~dolan/constellations/constellation_list.html>.
newtype ConShort = ConShort { fromConShort :: T.Text }
  deriving (Eq, Ord)

-- | Warning: there is no validation done on the input.
instance IsString ConShort where
  fromString = ConShort . T.pack

toConShort :: T.Text -> Maybe ConShort
toConShort k =
  let out = ConShort k
  in const out `fmap` lookup out constellationMap

instance Parsable ConShort where
  parseParam = helpParse "Constellation name" toConShort

-- | Long form for constellation names (e.g. Ursa Major),
--   although there's no validation that the string is valid.
--
--   See <http://www.astro.wisc.edu/~dolan/constellations/constellation_list.html>.
newtype ConLong = ConLong { fromConLong :: T.Text }
  deriving Eq

-- | There is no validation done on the input.
instance IsString ConLong where
  fromString = ConLong . T.pack

-- | This can fail if either the input is invalid or the mapping table
--   is missing something.
getConstellationName :: ConShort -> Maybe ConLong
getConstellationName = flip lookup constellationMap

-- | Similar to `getConstellationName`, except that unknown constellaion
--   abbreviations are left unchanged (so it's not really unsafe in the
--   normal usage of Haskell).
--
getConstellationNameUnsafe :: ConShort -> ConLong
getConstellationNameUnsafe con = fromMaybe (ConLong (fromConShort con)) (getConstellationName con)

-- | Similar to `getConstellationName`, except that unknown constellaion
--   abbreviations are left unchanged (so it's not really unsafe in the
--   normal usage of Haskell).
--
getConstellationNameStr :: ConShort -> T.Text
getConstellationNameStr = fromConLong . getConstellationNameUnsafe

-- map from short to long form
-- See http://www.astro.wisc.edu/~dolan/constellations/constellation_list.html
--     http://www.astro.wisc.edu/~dolan/constellations/constellations.html
constellationMap :: [(ConShort, ConLong)]
constellationMap = 
 [ ("And", "Andromeda")
 , ("Ant", "Antlia")
 , ("Aps", "Apus")
 , ("Aqr", "Aquarius")
 , ("Aql", "Aquila")
 , ("Ara", "Ara")
 , ("Ari", "Aries")
 , ("Aur", "Auriga")
 , ("Boo", "Boötes")
 , ("Cae", "Caelum")
 , ("Cam", "Camelopardalis")
 , ("Cnc", "Cancer")
 , ("CVn", "Canes Venatici")
 , ("CMa", "Canis Major")
 , ("CMi", "Canis Minor")
 , ("Cap", "Capricornus")
 , ("Car", "Carina")
 , ("Cas", "Cassiopeia")
 , ("Cen", "Centaurus")
 , ("Cep", "Cepheus")
 , ("Cet", "Cetus")
 , ("Cha", "Chamaeleon")
 , ("Cir", "Circinus")
 , ("Col", "Columba")
 , ("Com", "Coma Berenices")
 , ("CrA", "Corona Austrina")
 , ("CrB", "Corona Borealis")
 , ("Crv", "Corvus")
 , ("Crt", "Crater")
 , ("Cru", "Crux")
 , ("Cyg", "Cygnus")
 , ("Del", "Delphinus")
 , ("Dor", "Dorado")
 , ("Dra", "Draco")
 , ("Equ", "Equuleus")
 , ("Eri", "Eridanus")
 , ("For", "Fornax")
 , ("Gem", "Gemini")
 , ("Gru", "Grus")
 , ("Her", "Hercules")
 , ("Hor", "Horologium")
 , ("Hya", "Hydra")
 , ("Hyi", "Hydrus")
 , ("Ind", "Indus")
 , ("Lac", "Lacerta")
 , ("Leo", "Leo")
 , ("LMi", "Leo Minor")
 , ("Lep", "Lepus")
 , ("Lib", "Libra")
 , ("Lup", "Lupus")
 , ("Lyn", "Lynx")
 , ("Lyr", "Lyra")
 , ("Men", "Mensa")
 , ("Mic", "Microscopium")
 , ("Mon", "Monoceros")
 , ("Mus", "Musca")
 , ("Nor", "Norma")
 , ("Oct", "Octans")
 , ("Oph", "Ophiuchus")
 , ("Ori", "Orion")
 , ("Pav", "Pavo")
 , ("Peg", "Pegasus")
 , ("Per", "Perseus")
 , ("Phe", "Phoenix")
 , ("Pic", "Pictor")
 , ("Psc", "Pisces")
 , ("PsA", "Piscis Austrinus")
 , ("Pup", "Puppis")
 , ("Pyx", "Pyxis")
 , ("Ret", "Reticulum")
 , ("Sge", "Sagitta")
 , ("Sgr", "Sagittarius")
 , ("Sco", "Scorpius")
 , ("Scl", "Sculptor")
 , ("Sct", "Scutum")
 , ("Ser", "Serpens")
 , ("Sex", "Sextans")
 , ("Tau", "Taurus")
 , ("Tel", "Telescopium")
 , ("Tri", "Triangulum")
 , ("TrA", "Triangulum Australe")
 , ("Tuc", "Tucana")
 , ("UMa", "Ursa Major")
 , ("UMi", "Ursa Minor")
 , ("Vel", "Vela")
 , ("Vir", "Virgo")
 , ("Vol", "Volans")
 , ("Vul", "Vulpecula")
 ]

  

-- | Return the list of other observations that are joint - but not
--   necessarily contemperaneous - with this one.
--   
--   It turns out that you can have the soJointWIth field set but
--   no corresponding soJoint field. See ObsId 15662.
--
--   Should this return JointMission rather than a IsString?
getJointObs :: (IsString s) => ScienceObs -> [(s, TimeKS)]
getJointObs ScienceObs{..} = 
  let toJ (l,r) = (l,) `fmap` r
  in mapMaybe toJ 
          [ ("HST", soJointHST)
          , ("NOAO", soJointNOAO)
          , ("NRAO", soJointNRAO)
          , ("RXTE", soJointRXTE)
          , ("Spitzer", soJointSPITZER)
          , ("Suzaku", soJointSUZAKU)
          , ("XMM", soJointXMM)
          , ("Swift", soJointSWIFT)
          , ("NuSTAR", soJointNUSTAR)
          ]

-- Apparently this is needed if I want a field with Maybe (Int, Int),
-- but I am seeing some problems, along the lines of
--
-- dbType Maybe#Tuple2##Int#Int: expected DbTypePrimitive, got DbEmbedded (EmbeddedDef False [("val0",DbTypePrimitive DbInt64 False Nothing Nothing),("val1",DbTypePrimitive DbInt64 False Nothing Nothing)]) Nothing
--
-- so taking out for now
-- instance NeverNull (Int, Int)

-- | This is for debug purposes.
instance Show ScienceObs where
  show ScienceObs{..} = 
    "Science: " <> show (fromObsId soObsId)
    <> " " <> T.unpack (fromTargetName soTarget)
    <> " with "
    <> show soInstrument
    <> "+"
    <> show soGrating
    <> " for " <> show (_toKS soApprovedTime)
    <> " ks " <> T.unpack (showStartTime soStartTime)
    <> " " <> T.unpack (fromObsIdStatus soStatus)

{-
-- | Has the observation been archived? If so, we assume that the observational
--   parameters we care about are not going to change. This may turn out to be
--   a bad idea.
--
isArchived :: ScienceObs -> Bool
isArchived ScienceObs{..} = soStatus == Archived
-}

-- | This could be an enumeration, but there's always the possibility
--   that the set of values could change, so leave as is for now.
--
type PropCategory = T.Text

-- | Store information on a proposal, obtained from the OCAT.
--
--   This does *not* contain the abstract, as this requires a
--   different query (e.g. to NASA/ADS or a different OCAT page).
--   For now the abstract is stored in a separate table.
--
data Proposal = Proposal {
  propNum :: PropNum
  -- , propSeqNum :: Sequence
  , propName :: T.Text
  , propPI :: T.Text
  , propCategory :: PropCategory
  , propType :: T.Text
    -- TODO: change to use the PropType enumeration, but
    -- that has implications on the database, especially
    -- as the set of types has increased with time
  , propCycle :: T.Text
    -- Should this be an enumeration? It is "open ended",
    -- in that there's no fixed end value.
  }
  -- deriving (Eq, Show)
  deriving Eq

-- | Proposals are ordered by proposal number.
instance Ord Proposal where
  compare = compare `on` propNum

-- | This is for debug purposes.
instance Show Proposal where
  show Proposal{..} = 
    "Proposal: " <> show (_unPropNum propNum)
    <> " " <> T.unpack propName
    <> " PI " <> T.unpack propPI

-- | The abstract for a proposal. Ideally it would be included in
--   the Proposal record, but for historical reasons it isn't.
--
--   Should there be something like
--      paKey :: DefaultKey Proposal
--   here? I haven't decided how things are going to work, so leave
--   this for now.
--
--   Note that I have explicitly chosen not to include the PI in
--   this record as:
--      a) there could be privacy issues
--      b) while it is interesting to see how science is connected,
--         the PI is a single person, not the whole team working on
--         data
--
--   However, the PI *is* stored in the Proposal record.
--
data ProposalAbstract = ProposalAbstract {
  paNum :: PropNum
  , paTitle :: T.Text
    -- ^ This is left over from initial attempts using ADS, where the
    --   case was different. Left in in case it is still useful.
  , paAbstract :: T.Text
  } deriving Eq

-- | Proposal abstracts are ordered by the proposal number.
instance Ord ProposalAbstract where
  compare = compare `on` paNum

instance Show ProposalAbstract where
  show ProposalAbstract{..} =
    "Proposal Abstract: " <> show (_unPropNum paNum)
    <> " " <> T.unpack (T.take 40 paAbstract) <> "..."

-- | Record abstracts which have been attempted but for which
--   something went wrong. This is (hopefully) going to all be
--   "Missing abstract" (i.e. the OCAT does not have the information)
--   but there could be network issues too.
--
data MissingProposalAbstract = MissingProposalAbstract {
  mpNum :: PropNum
  -- ^ proposal number
  , mpReason :: T.Text
    -- ^ reason for failure
  , mpChecked :: UTCTime
    -- ^ approximate time the query was made
  }



-- | Enumeration for the different proposal categories.
--   This is not currently used in the database - e.g. for
--   the Proposal type - but should be. Although, as it is not
--   a fixed list, i.e. the cotents have changed over time,
--   leaving it stored in the database as a free-form
--   value has its benefits.
--
--   The Ord instance is for convenience.
data PropType =
  CAL | CCT | DDT | GO | GTO | TOO
  deriving (Eq, Ord)

-- | For now the conversion is case sensitive, and
--   only supports the short-form.
toPropType :: T.Text -> Maybe PropType
toPropType "CAL" = Just CAL
toPropType "CCT" = Just CCT
toPropType "DDT" = Just DDT
toPropType "GO" = Just GO
toPropType "GTO" = Just GTO
toPropType "TOO" = Just TOO
toPropType _ = Nothing

fromPropType :: PropType -> T.Text
fromPropType CAL = "CAL"
fromPropType CCT = "CCT"
fromPropType DDT = "DDT"
fromPropType GO = "GO"
fromPropType GTO = "GTO"
fromPropType TOO = "TOO"

instance Parsable PropType where
  parseParam = helpParse "Proposal type" toPropType

toPropTypeLabel :: PropType -> T.Text
toPropTypeLabel CAL = "Calibration Observation"
toPropTypeLabel CCT = "Catalog Cool Target"
toPropTypeLabel DDT = "Director's Discretionary Time"
toPropTypeLabel GO = "General Observer"
toPropTypeLabel GTO = "Guaranteed-Time Observation"
toPropTypeLabel TOO = "Target of Opportunity"


{-
-- | An observation at another facility that overlaps in time with
--   a Chandra observation.
data ConstrainedObs = ConstrainedObs {
  coFacility :: String    -- name of facility
  , coTime :: TimeKS      -- observation length
  }
  -- deriving (Eq, Show)
  deriving Eq
-}

-- It would have been nice to encode the hierarchy here, so that
-- a search could be made for all elements that match a particular
-- level in the hierarchy.

-- | This is the short form of the SIMBAD type. I have enforced the
--   restriction that this is at most three characters, but of
--   course there's at least one four-letter match ("Pec?").
--   For now I am going to ignore this since it appears that
--   "Pec" is still unique, so internally things should
--   be okay (unless there's parsing issues as well).
--
newtype SimbadType = SimbadType { fromSimbadType :: T.Text }
  deriving Eq

-- TODO: need a converter to a URL fragment - e.g. '?' needs protecting!
--       actually, need to check this, since I have seen it work

-- | This constructor ensures that the type is three letters
--   or less.
toSimbadType :: T.Text -> Maybe SimbadType
toSimbadType s | T.length s > 0 && T.length s < 4 = Just (SimbadType s)
               | otherwise = Nothing

noSimbadType :: SimbadType
noSimbadType = SimbadType "000"

instance Parsable SimbadType where
  parseParam = helpParse "Simbad type" toSimbadType

-- | TODO: Should this be an enumeration?
--
--   This is the "long form" of a SIMBAD type.
type SIMCategory = T.Text

-- | Identifies those sources for which we have no SIMBAD information.
noSimbadLabel :: SIMCategory
noSimbadLabel = "Unidentified"

-- | Information on an object identifier, retrieved from SIMBAD.
--
--   I originally tried to combine the SIMBAD information with
--   the observation information, but I have finally decided to
--   have three structures
--
--     - `SimbadInfo` which stores the information from SIMBAD
--
--     - `SimbadMatch` which matches a `ScienceObs` to a `SimbadInfo`
--
--     - `SimbadNoMatch` which indicates that we have not found a
--       match when searching SIMBAD.
--
data SimbadInfo = SimbadInfo {
  smiName :: TargetName
    -- ^ the primary identifier for the object
  , smiType3 :: SimbadType
    -- ^ short form identifier for siType
  , smiType :: SIMCategory
    -- ^ the primary type of the object (long form)
  }
                deriving Eq

-- | Indicates that there is a SIMBAD match for the
--   target name.
--
data SimbadMatch = SimbadMatch {
    smmTarget :: TargetName   -- ^ target name
    , smmSearchTerm :: T.Text   -- ^ value used for the simbad search
    , smmInfo :: DefaultKey SimbadInfo
    , smmLastChecked :: UTCTime
  }
--  deriving Eq
deriving instance Eq SimbadMatch


-- | Indicates that there is no SIMBAD match for the
--   target name. This could be folded into `SimbadMatch`
--   if we can have a `Maybe (DefaultKey SimbadInfo)`
--   as a type?
--
data SimbadNoMatch = SimbadNoMatch {
    smnTarget :: TargetName   -- ^ target name
    , smnSearchTerm :: T.Text   -- ^ value used for the simbad search
    , smnLastChecked :: UTCTime
  }
  deriving Eq


{-
-- | What does SIMBAD know about this object?
--
--   At present `SimbadSearch` is a union of `SimbadNoMatch` and
--   `SimbadMatch`.
--
type SimbadSearch = Either SimbadNoMatch SimbadMatch
-}

-- | Do we consider the two names to be the same?
--
--   Strip out all spaces; convert to lower case.
--
--   We do not use a simple edit distance comparison here
--   since we do not want to equate 3C292 and 3C232.
--
similarName :: SimbadInfo -> TargetName -> Bool
similarName SimbadInfo{..} target =
  let conv = T.filter (not . isSpace) . T.toLower . fromTargetName
  in ((==) `on` conv) target smiName

-- | The short and long forms of the type information from SIMBAD.
type SimbadTypeInfo = (SimbadType, SIMCategory)

{-
-- | The Simbad name is used for ordering.
--
--   Why do we need an Ord constraint?
instance Ord SimbadInfo where
  compare = compare `on` smiName
-}

-- | Hard code the hierarchy from http://cds.u-strasbg.fr/cgi-bin/Otype?X
--
--   Unfortunately I did not include the numeric identifiers above
--   and I'm too lazy to re-build the database with a new schema.
--

{-
00.00.00.0: Unknown            ?       Object of unknown nature
00.02.00.0: Transient          ev      transient event
01.00.00.0:  Radio             Rad     Radio-source
01.02.00.0:    Radio(m)        mR      metric Radio-source
01.04.00.0:    Radio(cm)       cm      centimetric Radio-source
01.06.00.0:    Radio(mm)       mm      millimetric Radio-source
01.08.00.0:    Radio(sub-mm)   smm     sub-millimetric source
01.11.00.0:    HI              HI      HI (21cm) source
01.12.00.0:    radioBurst      rB      radio Burst
01.14.00.0:    Maser           Mas     Maser
02.00.00.0:  IR                IR      Infra-Red source
02.02.00.0:    IR>30um         FIR     Far-IR source  (λ >= 30 µm)
02.04.00.0:    IR<10um         NIR     Near-IR source (λ < 10 µm)
03.00.00.0:  Red               red     Very red source
03.03.00.0:    RedExtreme      ERO     Extremely Red Object
04.00.00.0:  Blue              blu     Blue object
05.00.00.0:  UV                UV      UV-emission source
06.00.00.0:  X                 X       X-ray source
06.02.00.0:    ULX?            UX?     Ultra-luminous X-ray candidate
06.10.00.0:    ULX             ULX     Ultra-luminous X-ray source
07.00.00.0:  gamma             gam     gamma-ray source
07.03.00.0:    gammaBurst      gB      gamma-ray Burst
08.00.00.0:  Inexistent        err     Not an object (error, artefact, ...)
09.00.00.0:  Gravitation       grv     Gravitational Source
09.03.00.0:    LensingEv       Lev     (Micro)Lensing Event
09.06.00.0:    Candidate_LensSystem  LS?     Possible gravitational lens System
09.07.00.0:    Candidate_Lens  Le?     Possible gravitational lens
09.08.00.0:    Possible_lensImage LI?     Possible gravitationally lensed image
09.09.00.0:    GravLens        gLe     Gravitational Lens
09.11.00.0:    GravLensSystem  gLS     Gravitational Lens System (lens+images)
10.00.00.0:  Candidates	..?	Candidate objects
10.01.00.0:    Possible_G      G?      Possible Galaxy
10.02.00.0:    Possible_SClG   SC?     Possible Supercluster of Galaxies
10.03.00.0:    Possible_ClG	C?G	Possible Cluster of Galaxies
10.04.00.0:    Possible_GrG	Gr?	Possible Group of Galaxies
10.06.00.0:    Possible_As*     As?	
10.11.00.0:    Candidate_**	**?	Physical Binary Candidate
10.11.01.0:      Candidate_EB*	EB?	Eclipsing Binary Candidate
10.11.10.0:	 Candidate_Symb* Sy?	Symbiotic Star Candidate
10.11.11.0:      Candidate_CV*	CV?	Cataclysmic Binary Candidate
10.11.11.6:      Candidate_Nova	No?	Nova Candidate
10.11.12.0:      Candidate_XB*	XB?	X-ray binary Candidate
10.11.12.2:      Candidate_LMXB LX?	Low-Mass X-ray binary Candidate
10.11.12.3:      Candidate_HMXB HX?	High-Mass X-ray binary Candidate
10.12.00.0:    Candidate_Pec*	Pec?	Possible Peculiar Star
10.12.01.0:      Candidate_YSO Y*?  	Young Stellar Object Candidate
10.12.02.0:      Candidate_pMS* pr? 	Pre-main sequence Star Candidate
10.12.02.3:        Candidate_TTau* TT?	T Tau star Candidate
10.12.03.0:      Candidate_C*	C*? 	Possible Carbon Star
10.12.04.0:      Candidate_S*  S*? 	Possible S Star
10.12.05.0:      Candidate_OH	OH?     Possible Star with envelope of OH/IR type
10.12.06.0:      Candidate_CH  CH? 	Possible Star with envelope of CH type
10.12.07.0:      Candidate_WR* WR?     Possible Wolf-Rayet Star
10.12.08.0:      Candidate_Be* Be?     Possible Be Star
10.12.09.0:      Candidate_Ae* Ae?     Possible Herbig Ae/Be Star
10.12.11.0:      Candidate_HB*	HB?    Possible Horizontal Branch Star
10.12.11.2:        Candidate_RRLyr RR? Possible Star of RR Lyr type
10.12.11.3:        Candidate_Cepheid Ce? Possible Cepheid
10.12.12.0:      Candidate_RGB* RB?    Possible Red Giant Branch star
10.12.13.0:      Candidate_SG*  sg?  	Possible Supergiant star
10.12.13.3:      Candidate_RSG* s?r    Possible Red supergiant star
10.12.13.4:      Candidate_YSG* s?y    Possible Yellow supergiant star
10.12.13.5:      Candidate_BSG* s?b    Possible Blue supergiant star
10.12.14.0:      Candidate_AGB* AB?  	Asymptotic Giant Branch Star candidate
10.12.14.1:      Candidate_LP*  LP?     Long Period Variable candidate
10.12.14.2:      Candidate_Mi*  Mi?     Mira candidate
10.12.14.3:      Candiate_sr*   sv?     Semi-regular variable candidate
10.12.15.0:      Candidate_post-AGB* pA? Post-AGB Star Candidate
10.12.16.0:      Candidate_BSS BS?     Candidate blue Straggler Star
10.12.18.0:      Candidate_WD* WD?     White Dwarf Candidate
10.12.20.0:      Candidate_NS  N*?     Neutron Star Candidate
10.12.22.0:      Candidate_BH  BH?     Black Hole Candidate
10.12.23.0:      Candidate_SN* SN?     SuperNova Candidate
10.12.24.0:      Candidate_low-mass* LM? Low-mass star candidate
10.12.26.0:      Candidate_brownD* BD?	Brown Dwarf Candidate
 12.00.00.0:  multiple_object   mul     Composite object
 12.01.00.0:    Region          reg     Region defined in the sky
 12.01.05.0:      Void          vid     Underdense region of the Universe
 12.02.00.0:    SuperClG        SCG     Supercluster of Galaxies
 12.03.00.0:    ClG             ClG     Cluster of Galaxies
 12.04.00.0:    GroupG          GrG     Group of Galaxies
 12.04.05.0:      Compact_Gr_G  CGG     Compact Group of Galaxies
 12.05.00.0:    PairG           PaG     Pair of Galaxies
 12.05.05.0:      IG            IG      Interacting Galaxies
 12.09.00.0:    Cl*?    	C?*	Possible (open) star cluster
 12.10.00.0:    GlCl?           Gl?     Possible Globular Cluster
 12.11.00.0:    Cl*             Cl*     Cluster of Stars
 12.11.01.0:      GlCl          GlC     Globular Cluster
 12.11.02.0:      OpCl          OpC     Open (galactic) Cluster
 12.12.00.0:    Assoc*          As*     Association of Stars
 12.12.01.0:      Stream*       St*     Stellar Stream
 12.12.02.0:      MouvGroup     MGr     Moving Group
 12.13.00.0:    **              **      Double or multiple star
 12.13.01.0:      EB*           EB*     Eclipsing binary
 12.13.01.1:        EB*Algol    Al*     Eclipsing binary of Algol type (detached)
 12.13.01.2:        EB*betLyr   bL*     Eclipsing binary of beta Lyr type (semi-detached)
 12.13.01.3:        EB*WUMa     WU*     Eclipsing binary of W UMa type (contact binary)
 12.13.01.8:        EB*Planet   EP*     Star showing eclipses by its planet
 12.13.02.0:      SB*           SB*     Spectroscopic binary
 12.13.05.0:	  EllipVar	El*	Ellipsoidal variable Star
 12.13.10.0:	  Symbiotic*	Sy*     Symbiotic Star
 12.13.11.0:      CataclyV*     CV*     Cataclysmic Variable Star
 12.13.11.2:        DQHer       DQ*     CV DQ Her type (intermediate polar)
 12.13.11.3:        AMHer       AM*     CV of AM Her type (polar)
 12.13.11.5:        Nova-like   NL*     Nova-like Star
 12.13.11.6:        Nova        No*     Nova
 12.13.11.7:        DwarfNova   DN*     Dwarf Nova
 12.13.12.0:      XB            XB*     X-ray Binary
 12.13.12.2:        LMXB        LXB     Low Mass X-ray Binary
 12.13.12.3:        HMXB        HXB     High Mass X-ray Binary
 13.00.00.0:  ISM               ISM     Interstellar matter
 13.01.00.0:    PartofCloud     PoC     Part of Cloud
 13.02.00.0:    PN?             PN?     Possible Planetary Nebula
 13.03.00.0:    ComGlob         CGb     Cometary Globule
 13.04.00.0:    Bubble          bub     Bubble
 13.06.00.0:    EmObj           EmO     Emission Object
 13.08.00.0:    Cloud           Cld     Cloud
 13.08.03.0:      GalNeb        GNe     Galactic Nebula
 13.08.04.0:      BrNeb         BNe     Bright Nebula
 13.08.06.0:      DkNeb         DNe     Dark Cloud (nebula)
 13.08.07.0:      RfNeb         RNe     Reflection Nebula
 13.08.12.0:      MolCld        MoC     Molecular Cloud
 13.08.12.3:        Globule     glb     Globule (low-mass dark cloud)
 13.08.12.6:        denseCore   cor     Dense core
 13.08.12.8:        SFregion    SFR     Star forming region
 13.08.13.0:      HVCld         HVC     High-velocity Cloud
 13.09.00.0:    HII             HII     HII (ionized) region
 13.10.00.0:    PN              PN      Planetary Nebula
 13.11.00.0:    HIshell         sh      HI shell
 13.12.00.0:    SNR?            SR?     SuperNova Remnant Candidate
 13.13.00.0:    SNR             SNR     SuperNova Remnant
 13.14.00.0:    Circumstellar	cir     CircumStellar matter
 13.14.01.0:	outflow?        of?	Outflow candidate
 13.14.15.0:	Outflow         out	Outflow
 13.14.16.0:	HH		HH	Herbig-Haro Object
 14.00.00.0:  Star              *       Star
 14.01.00.0:    *inCl           *iC     Star in Cluster
 14.02.00.0:    *inNeb          *iN     Star in Nebula
 14.03.00.0:    *inAssoc        *iA     Star in Association
 14.04.00.0:    *in**           *i*     Star in double system
 14.05.00.0:    V*?             V*?     Star suspected of Variability
 14.06.00.0:    Pec*            Pe*     Peculiar Star
 14.06.01.0:      HB*           HB*     Horizontal Branch Star
 14.06.02.0:      YSO           Y*O     Young Stellar Object
 14.06.02.4:        Ae*		Ae*     Herbig Ae/Be star
 14.06.05.0:      Em*           Em*     Emission-line Star
 14.06.05.3:        Be*         Be*     Be Star
 14.06.06.0:      BlueStraggler BS*     Blue Straggler Star
 14.06.10.0:      RGB*          RG*     Red Giant Branch star
 14.06.12.0:      AGB*          AB*     Asymptotic Giant Branch Star (He-burning)
 14.06.12.3:        C*          C*      Carbon Star
 14.06.12.6:        S*          S*      S Star
 14.06.13.0:      SG*           sg*     Evolved supergiant star
 14.06.13.3:      RedSG*        s*r     Red supergiant star
 14.06.13.4:      YellowSG*     s*y     Yellow supergiant star
 14.06.13.5:      BlueSG*       s*b     Blue supergiant star
 14.06.15.0:      post-AGB*     pA*     Post-AGB Star (proto-PN)
 14.06.16.0:      WD*           WD*     White Dwarf
 14.06.16.1:        pulsWD*     ZZ*     Pulsating White Dwarf
 14.06.17.0:      low-mass*     LM*     Low-mass star (M<1solMass)
 14.06.18.0:      brownD*       BD*     Brown Dwarf (M<0.08solMass)
 14.06.19.0:      Neutron*      N*      Confirmed Neutron Star
 14.06.23.0:      OH/IR         OH*     OH/IR star
 14.06.24.0:      CH            CH*     Star with envelope of CH type
 14.06.25.0:      pMS*          pr*     Pre-main sequence Star
 14.06.25.3:        TTau*       TT*     T Tau-type Star
 14.06.30.0:      WR*           WR*     Wolf-Rayet Star
 14.07.00.0:    PM*             PM*     High proper-motion Star
 14.08.00.0:    HV*             HV*     High-velocity Star
 14.09.00.0:    V*              V*      Variable Star
 14.09.01.0:      Irregular_V*  Ir*     Variable Star of irregular type
 14.09.01.1:        Orion_V*    Or*     Variable Star of Orion Type
 14.09.01.2:        Rapid_Irreg_V* RI*  Variable Star with rapid variations
 14.09.03.0:      Eruptive*     Er*     Eruptive variable Star
 14.09.03.1:        Flare*      Fl*     Flare Star
 14.09.03.2:        FUOr        FU*     Variable Star of FU Ori type
 14.09.03.4:        Erupt*RCrB  RC*     Variable Star of R CrB type
 14.09.03.5:        RCrB_Candidate RC?  Variable Star of R CrB type candiate
 14.09.04.0:      RotV*         Ro*     Rotationally variable Star
 14.09.04.1:        RotV*alf2CVn a2*    Variable Star of alpha2 CVn type
 14.09.04.3:        Pulsar      Psr     Pulsar
 14.09.04.4:        BYDra       BY*     Variable of BY Dra type
 14.09.04.5:        RSCVn       RS*     Variable of RS CVn type
 14.09.05.0:      PulsV*        Pu*     Pulsating variable Star
 14.09.05.2:        RRLyr       RR*     Variable Star of RR Lyr type
 14.09.05.3:        Cepheid     Ce*     Cepheid variable Star
 14.09.05.5:        PulsV*delSct dS*    Variable Star of delta Sct type
 14.09.05.6:        PulsV*RVTau RV*     Variable Star of RV Tau type
 14.09.05.7:        PulsV*WVir  WV*     Variable Star of W Vir type
 14.09.05.8:        PulsV*bCep  bC*     Variable Star of beta Cep type
 14.09.05.9:        deltaCep    cC*     Classical Cepheid (delta Cep type)
 14.09.05.10:       gammaDor    gD*     Variable Star of gamma Dor type
 14.09.05.11:       pulsV*SX    SX*     Variable Star of SX Phe type (subdwarf)
 14.09.06.0:      LPV*          LP*     Long-period variable star
 14.09.06.1:        Mira        Mi*     Variable Star of Mira Cet type
 14.09.06.4:        semi-regV*  sr*     Semi-regular pulsating Star
 14.09.08.0:      SN            SN*     SuperNova
 14.14.00.0:    Sub-stellar     su*     Sub-stellar object
 14.14.02.0:      Planet?       Pl?     Extra-solar Planet Candidate
 14.14.10.0:      Planet        Pl      Extra-solar Confirmed Planet
 15.00.00.0:  Galaxy            G       Galaxy
 15.01.00.0:    PartofG         PoG     Part of a Galaxy
 15.02.00.0:    GinCl           GiC     Galaxy in Cluster of Galaxies
 15.02.02.0:      BClG          BiC     Brightest galaxy in a Cluster (BCG)
 15.03.00.0:    GinGroup        GiG     Galaxy in Group of Galaxies
 15.04.00.0:    GinPair         GiP     Galaxy in Pair of Galaxies
 15.05.00.0:    High_z_G        HzG     Galaxy with high redshift
 15.06.00.0:    AbsLineSystem   ALS     Absorption Line system
 15.06.01.0:      Ly-alpha_ALS  LyA     Ly alpha Absorption Line system
 15.06.02.0:      DLy-alpha_ALS DLA     Damped Ly-alpha Absorption Line system
 15.06.03.0:      metal_ALS     mAL     metallic Absorption Line system
 15.06.05.0:      Ly-limit_ALS  LLS	Lyman limit system
 15.06.08.0:      Broad_ALS	BAL	Broad Absorption Line system
 15.07.00.0:    RadioG          rG      Radio Galaxy
 15.08.00.0:    HII_G           H2G     HII Galaxy
 15.09.00.0:    LSB_G           LSB     Low Surface Brightness Galaxy
 15.10.00.0:    AGN_Candidate   AG?     Possible Active Galaxy Nucleus
 15.10.07.0:      QSO_Candidate Q?      Possible Quasar
 15.10.11.0:      Blazar_Candidate Bz?  Possible Blazar
 15.10.17.0:      BLLac_Candidate BL?   Possible BL Lac
 15.11.00.0:    EmG             EmG     Emission-line galaxy
 15.12.00.0:    StarburstG      SBG     Starburst Galaxy
 15.13.00.0:    BlueCompG       bCG     Blue compact Galaxy
 15.14.00.0:    LensedImage     LeI     Gravitationally Lensed Image
 15.14.01.0:      LensedG       LeG     Gravitationally Lensed Image of a Galaxy
 15.14.07.0:      LensedQ       LeQ     Gravitationally Lensed Image of a Quasar
 15.15.00.0:    AGN             AGN     Active Galaxy Nucleus
 15.15.01.0:      LINER         LIN     LINER-type Active Galaxy Nucleus
 15.15.02.0:      Seyfert       SyG     Seyfert Galaxy
 15.15.02.1:        Seyfert_1   Sy1     Seyfert 1 Galaxy
 15.15.02.2:        Seyfert_2   Sy2     Seyfert 2 Galaxy
 15.15.03.0:      Blazar        Bla     Blazar
 15.15.03.1:        BLLac       BLL     BL Lac - type object
 15.15.03.2:        OVV         OVV     Optically Violently Variable object
 15.15.04.0:      QSO           QSO     Quasar

-}

data SimbadCode =
  SimbadCode { _sc1 :: Int, _sc2 :: Int, _sc3 :: Int, _sc4 :: Int,
               _scLevel :: Int }
  deriving (Eq, Ord)

sc1 :: SimbadCode -> Int
sc1 = _sc1

sc2 :: SimbadCode -> Int
sc2 = _sc2

sc3 :: SimbadCode -> Int
sc3 = _sc3

scLevel :: SimbadCode -> Int
scLevel = _scLevel

-- Should the Ord instance be derived manually, so that _scLevel
-- can be removed from the check, or does it not matter?

instance Show SimbadCode where
  show SimbadCode {..} = T.unpack txt
    where
      txt = sformat (int2 % "." % int2 % "." % int2 % "." % int)
            _sc1 _sc2 _sc3 _sc4

-- | The list is assumed to be in ascending SimbadCode order,
--   but this is not checked.
--
simbadLabels :: [(SimbadCode, SimbadType, T.Text)]
simbadLabels =
  let rowConv ((i1, i2, i3, i4), _, l2, l3) = do
        sc <- either (const Nothing) Just (toSC4 i1 i2 i3 i4)
        -- st <- maybe (Left ("Invalid SIMBAD type: " <> l2)) Right (toSimbadType l2)
        st <- toSimbadType l2
        return (sc, st, l3)

      check xs = if length xs == length stbl
                 then xs
                 else error "*internal error* converting SIMBAD table"

      stbl :: [((Int, Int, Int, Int), T.Text, T.Text, T.Text)]
      stbl = 
        [
          ((00, 00, 00, 0), "Unknown", "?", "Object of unknown nature")
        , ((00, 02, 00, 0), "Transient", "ev", "transient event")
        , ((01, 00, 00, 0), "Radio", "Rad", "Radio-source")
        , ((01, 02, 00, 0), "Radio(m)", "mR", "metric Radio-source")
        , ((01, 04, 00, 0), "Radio(cm)", "cm", "centimetric Radio-source")
        , ((01, 06, 00, 0), "Radio(mm)", "mm", "millimetric Radio-source")
        , ((01, 08, 00, 0), "Radio(sub-mm)", "smm", "sub-millimetric source")
        , ((01, 11, 00, 0), "HI", "HI", "HI (21cm) source")
        , ((01, 12, 00, 0), "radioBurst", "rB", "radio Burst")
        , ((01, 14, 00, 0), "Maser", "Mas", "Maser")
        , ((02, 00, 00, 0), "IR", "IR", "Infra-Red source")
          -- looks like unicode does not get mapped to JSON well for the following labels
        , ((02, 02, 00, 0), "IR>30um", "FIR", "Far-IR source (λ >= 30 µm)")
        , ((02, 04, 00, 0), "IR<10um", "NIR", "Near-IR source (λ < 10 µm)")
        , ((03, 00, 00, 0), "Red", "red", "Very red source")
        , ((03, 03, 00, 0), "RedExtreme", "ERO", "Extremely Red Object")
        , ((04, 00, 00, 0), "Blue", "blu", "Blue object")
        , ((05, 00, 00, 0), "UV", "UV", "UV-emission source")
        , ((06, 00, 00, 0), "X", "X", "X-ray source")
        , ((06, 02, 00, 0), "ULX?", "UX?", "Ultra-luminous X-ray candidate")
        , ((06, 10, 00, 0), "ULX", "ULX", "Ultra-luminous X-ray source")
        , ((07, 00, 00, 0), "gamma", "gam", "gamma-ray source")
        , ((07, 03, 00, 0), "gammaBurst", "gB", "gamma-ray Burst")
        , ((08, 00, 00, 0), "Inexistent", "err", "Not an object (error, artefact, ...)")
        , ((09, 00, 00, 0), "Gravitation", "grv", "Gravitational Source")
        , ((09, 03, 00, 0), "LensingEv", "Lev", "(Micro)Lensing Event")
        , ((09, 06, 00, 0), "Candidate_LensSystem", "LS?", "Possible gravitational lens System")
        , ((09, 07, 00, 0), "Candidate_Lens", "Le?", "Possible gravitational lens")
        , ((09, 08, 00, 0), "Possible_lensImage", "LI?", "Possible gravitationally lensed image")
        , ((09, 09, 00, 0), "GravLens", "gLe", "Gravitational Lens")
        , ((09, 11, 00, 0), "GravLensSystem", "gLS", "Gravitational Lens System (lens+images)")
        , ((10, 00, 00, 0), "Candidates", "..?", "Candidate objects")
        , ((10, 01, 00, 0), "Possible_G", "G?", "Possible Galaxy")
        , ((10, 02, 00, 0), "Possible_SClG", "SC?", "Possible Supercluster of Galaxies")
        , ((10, 03, 00, 0), "Possible_ClG", "C?G", "Possible Cluster of Galaxies")
        , ((10, 04, 00, 0), "Possible_GrG", "Gr?", "Possible Group of Galaxies")
        , ((10, 06, 00, 0), "Possible_As*", "As?", "")  -- there appears to be no lable for this
        , ((10, 11, 00, 0), "Candidate_**", "**?", "Physical Binary Candidate")
        , ((10, 11, 01, 0), "Candidate_EB*", "EB?", "Eclipsing Binary Candidate")
        , ((10, 11, 10, 0), "Candidate_Symb*", "Sy?", "Symbiotic Star Candidate")
        , ((10, 11, 11, 0), "Candidate_CV*", "CV?", "Cataclysmic Binary Candidate")
        , ((10, 11, 11, 6), "Candidate_Nova", "No?", "Nova Candidate")
        , ((10, 11, 12, 0), "Candidate_XB*", "XB?", "X-ray binary Candidate")
        , ((10, 11, 12, 2), "Candidate_LMXB", "LX?", "Low-Mass X-ray binary Candidate")
        , ((10, 11, 12, 3), "Candidate_HMXB", "HX?", "High-Mass X-ray binary Candidate")
          -- SimbadType must be 3 characters max, so replace Pec? with Pec
          -- TODO: this is not ideal!
          -- , ((10, 12, 00, 0), "Candidate_Pec*", "Pec?", "Possible Peculiar Star")
        , ((10, 12, 00, 0), "Candidate_Pec*", "Pec", "Possible Peculiar Star")
        , ((10, 12, 01, 0), "Candidate_YSO", "Y*?", "Young Stellar Object Candidate")
        , ((10, 12, 02, 0), "Candidate_pMS*", "pr?", "Pre-main sequence Star Candidate")
        , ((10, 12, 02, 3), "Candidate_TTau*", "TT?", "T Tau star Candidate")
        , ((10, 12, 03, 0), "Candidate_C*", "C*?", "Possible Carbon Star")
        , ((10, 12, 04, 0), "Candidate_S*", "S*?", "Possible S Star")
        , ((10, 12, 05, 0), "Candidate_OH", "OH?", "Possible Star with envelope of OH/IR type")
        , ((10, 12, 06, 0), "Candidate_CH", "CH?", "Possible Star with envelope of CH type")
        , ((10, 12, 07, 0), "Candidate_WR*", "WR?", "Possible Wolf-Rayet Star")
        , ((10, 12, 08, 0), "Candidate_Be*", "Be?", "Possible Be Star")
        , ((10, 12, 09, 0), "Candidate_Ae*", "Ae?", "Possible Herbig Ae/Be Star")
        , ((10, 12, 11, 0), "Candidate_HB*", "HB?", "Possible Horizontal Branch Star")
        , ((10, 12, 11, 2), "Candidate_RRLyr", "RR?", "Possible Star of RR Lyr type")
        , ((10, 12, 11, 3), "Candidate_Cepheid", "Ce?", "Possible Cepheid")
        , ((10, 12, 12, 0), "Candidate_RGB*", "RB?", "Possible Red Giant Branch star")
        , ((10, 12, 13, 0), "Candidate_SG*", "sg?", "Possible Supergiant star")
        , ((10, 12, 13, 3), "Candidate_RSG*", "s?r", "Possible Red supergiant star")
        , ((10, 12, 13, 4), "Candidate_YSG*", "s?y", "Possible Yellow supergiant star")
        , ((10, 12, 13, 5), "Candidate_BSG*", "s?b", "Possible Blue supergiant star")
        , ((10, 12, 14, 0), "Candidate_AGB*", "AB?", "Asymptotic Giant Branch Star candidate")
        , ((10, 12, 14, 1), "Candidate_LP*", "LP?", "Long Period Variable candidate")
        , ((10, 12, 14, 2), "Candidate_Mi*", "Mi?", "Mira candidate")
        , ((10, 12, 14, 3), "Candiate_sr*", "sv?", "Semi-regular variable candidate")
        , ((10, 12, 15, 0), "Candidate_post-AGB*", "pA?", "Post-AGB Star Candidate")
        , ((10, 12, 16, 0), "Candidate_BSS", "BS?", "Candidate blue Straggler Star")
        , ((10, 12, 18, 0), "Candidate_WD*", "WD?", "White Dwarf Candidate")
        , ((10, 12, 20, 0), "Candidate_NS", "N*?", "Neutron Star Candidate")
        , ((10, 12, 22, 0), "Candidate_BH", "BH?", "Black Hole Candidate")
        , ((10, 12, 23, 0), "Candidate_SN*", "SN?", "SuperNova Candidate")
        , ((10, 12, 24, 0), "Candidate_low-mass*", "LM?", "Low-mass star candidate")
        , ((10, 12, 26, 0), "Candidate_brownD*", "BD?", "Brown Dwarf Candidate")
        , ((12, 00, 00, 0), "multiple_object", "mul", "Composite object")
        , ((12, 01, 00, 0), "Region", "reg", "Region defined in the sky")
        , ((12, 01, 05, 0), "Void", "vid", "Underdense region of the Universe")
        , ((12, 02, 00, 0), "SuperClG", "SCG", "Supercluster of Galaxies")
        , ((12, 03, 00, 0), "ClG", "ClG", "Cluster of Galaxies")
        , ((12, 04, 00, 0), "GroupG", "GrG", "Group of Galaxies")
        , ((12, 04, 05, 0), "Compact_Gr_G", "CGG", "Compact Group of Galaxies")
        , ((12, 05, 00, 0), "PairG", "PaG", "Pair of Galaxies")
        , ((12, 05, 05, 0), "IG", "IG", "Interacting Galaxies")
        , ((12, 09, 00, 0), "Cl*?", "C?*", "Possible (open) star cluster")
        , ((12, 10, 00, 0), "GlCl?", "Gl?", "Possible Globular Cluster")
        , ((12, 11, 00, 0), "Cl*", "Cl*", "Cluster of Stars")
        , ((12, 11, 01, 0), "GlCl", "GlC", "Globular Cluster")
        , ((12, 11, 02, 0), "OpCl", "OpC", "Open (galactic) Cluster")
        , ((12, 12, 00, 0), "Assoc*", "As*", "Association of Stars")
        , ((12, 12, 01, 0), "Stream*", "St*", "Stellar Stream")
        , ((12, 12, 02, 0), "MouvGroup", "MGr", "Moving Group")
        , ((12, 13, 00, 0), "**", "**", "Double or multiple star")
        , ((12, 13, 01, 0), "EB*", "EB*", "Eclipsing binary")
        , ((12, 13, 01, 1), "EB*Algol", "Al*", "Eclipsing binary of Algol type (detached)")
        , ((12, 13, 01, 2), "EB*betLyr", "bL*", "Eclipsing binary of beta Lyr type (semi-detached)")
        , ((12, 13, 01, 3), "EB*WUMa", "WU*", "Eclipsing binary of W UMa type (contact binary)")
        , ((12, 13, 01, 8), "EB*Planet", "EP*", "Star showing eclipses by its planet")
        , ((12, 13, 02, 0), "SB*", "SB*", "Spectroscopic binary")
        , ((12, 13, 05, 0), "EllipVar", "El*", "Ellipsoidal variable Star")
        , ((12, 13, 10, 0), "Symbiotic*", "Sy*", "Symbiotic Star")
        , ((12, 13, 11, 0), "CataclyV*", "CV*", "Cataclysmic Variable Star")
        , ((12, 13, 11, 2), "DQHer", "DQ*", "CV DQ Her type (intermediate polar)")
        , ((12, 13, 11, 3), "AMHer", "AM*", "CV of AM Her type (polar)")
        , ((12, 13, 11, 5), "Nova-like", "NL*", "Nova-like Star")
        , ((12, 13, 11, 6), "Nova", "No*", "Nova")
        , ((12, 13, 11, 7), "DwarfNova", "DN*", "Dwarf Nova")
        , ((12, 13, 12, 0), "XB", "XB*", "X-ray Binary")
        , ((12, 13, 12, 2), "LMXB", "LXB", "Low Mass X-ray Binary")
        , ((12, 13, 12, 3), "HMXB", "HXB", "High Mass X-ray Binary")
        , ((13, 00, 00, 0), "ISM", "ISM", "Interstellar matter")
        , ((13, 01, 00, 0), "PartofCloud", "PoC", "Part of Cloud")
        , ((13, 02, 00, 0), "PN?", "PN?", "Possible Planetary Nebula")
        , ((13, 03, 00, 0), "ComGlob", "CGb", "Cometary Globule")
        , ((13, 04, 00, 0), "Bubble", "bub", "Bubble")
        , ((13, 06, 00, 0), "EmObj", "EmO", "Emission Object")
        , ((13, 08, 00, 0), "Cloud", "Cld", "Cloud")
        , ((13, 08, 03, 0), "GalNeb", "GNe", "Galactic Nebula")
        , ((13, 08, 04, 0), "BrNeb", "BNe", "Bright Nebula")
        , ((13, 08, 06, 0), "DkNeb", "DNe", "Dark Cloud (nebula)")
        , ((13, 08, 07, 0), "RfNeb", "RNe", "Reflection Nebula")
        , ((13, 08, 12, 0), "MolCld", "MoC", "Molecular Cloud")
        , ((13, 08, 12, 3), "Globule", "glb", "Globule (low-mass dark cloud)")
        , ((13, 08, 12, 6), "denseCore", "cor", "Dense core")
        , ((13, 08, 12, 8), "SFregion", "SFR", "Star forming region")
        , ((13, 08, 13, 0), "HVCld", "HVC", "High-velocity Cloud")
        , ((13, 09, 00, 0), "HII", "HII", "HII (ionized) region")
        , ((13, 10, 00, 0), "PN", "PN", "Planetary Nebula")
        , ((13, 11, 00, 0), "HIshell", "sh", "HI shell")
        , ((13, 12, 00, 0), "SNR?", "SR?", "SuperNova Remnant Candidate")
        , ((13, 13, 00, 0), "SNR", "SNR", "SuperNova Remnant")
        , ((13, 14, 00, 0), "Circumstellar", "cir", "CircumStellar matter")
        , ((13, 14, 01, 0), "outflow?", "of?", "Outflow candidate")
        , ((13, 14, 15, 0), "Outflow", "out", "Outflow")
        , ((13, 14, 16, 0), "HH", "HH", "Herbig-Haro Object")
        , ((14, 00, 00, 0), "Star", "*", "Star")
        , ((14, 01, 00, 0), "*inCl", "*iC", "Star in Cluster")
        , ((14, 02, 00, 0), "*inNeb", "*iN", "Star in Nebula")
        , ((14, 03, 00, 0), "*inAssoc", "*iA", "Star in Association")
        , ((14, 04, 00, 0), "*in**", "*i*", "Star in double system")
        , ((14, 05, 00, 0), "V*?", "V*?", "Star suspected of Variability")
        , ((14, 06, 00, 0), "Pec*", "Pe*", "Peculiar Star")
        , ((14, 06, 01, 0), "HB*", "HB*", "Horizontal Branch Star")
        , ((14, 06, 02, 0), "YSO", "Y*O", "Young Stellar Object")
        , ((14, 06, 02, 4), "Ae*", "Ae*", "Herbig Ae/Be star")
        , ((14, 06, 05, 0), "Em*", "Em*", "Emission-line Star")
        , ((14, 06, 05, 3), "Be*", "Be*", "Be Star")
        , ((14, 06, 06, 0), "BlueStraggler", "BS*", "Blue Straggler Star")
        , ((14, 06, 10, 0), "RGB*", "RG*", "Red Giant Branch star")
        , ((14, 06, 12, 0), "AGB*", "AB*", "Asymptotic Giant Branch Star (He-burning)")
        , ((14, 06, 12, 3), "C*", "C*", "Carbon Star")
        , ((14, 06, 12, 6), "S*", "S*", "S Star")
        , ((14, 06, 13, 0), "SG*", "sg*", "Evolved supergiant star")
        , ((14, 06, 13, 3), "RedSG*", "s*r", "Red supergiant star")
        , ((14, 06, 13, 4), "YellowSG*", "s*y", "Yellow supergiant star")
        , ((14, 06, 13, 5), "BlueSG*", "s*b", "Blue supergiant star")
        , ((14, 06, 15, 0), "post-AGB*", "pA*", "Post-AGB Star (proto-PN)")
        , ((14, 06, 16, 0), "WD*", "WD*", "White Dwarf")
        , ((14, 06, 16, 1), "pulsWD*", "ZZ*", "Pulsating White Dwarf")
        , ((14, 06, 17, 0), "low-mass*", "LM*", "Low-mass star (M<1solMass)")
        , ((14, 06, 18, 0), "brownD*", "BD*", "Brown Dwarf (M<0.08solMass)")
        , ((14, 06, 19, 0), "Neutron*", "N*", "Confirmed Neutron Star")
        , ((14, 06, 23, 0), "OH/IR", "OH*", "OH/IR star")
        , ((14, 06, 24, 0), "CH", "CH*", "Star with envelope of CH type")
        , ((14, 06, 25, 0), "pMS*", "pr*", "Pre-main sequence Star")
        , ((14, 06, 25, 3), "TTau*", "TT*", "T Tau-type Star")
        , ((14, 06, 30, 0), "WR*", "WR*", "Wolf-Rayet Star")
        , ((14, 07, 00, 0), "PM*", "PM*", "High proper-motion Star")
        , ((14, 08, 00, 0), "HV*", "HV*", "High-velocity Star")
        , ((14, 09, 00, 0), "V*", "V*", "Variable Star")
        , ((14, 09, 01, 0), "Irregular_V*", "Ir*", "Variable Star of irregular type")
        , ((14, 09, 01, 1), "Orion_V*", "Or*", "Variable Star of Orion Type")
        , ((14, 09, 01, 2), "Rapid_Irreg_V*", "RI*", "Variable Star with rapid variations")
        , ((14, 09, 03, 0), "Eruptive*", "Er*", "Eruptive variable Star")
        , ((14, 09, 03, 1), "Flare*", "Fl*", "Flare Star")
        , ((14, 09, 03, 2), "FUOr", "FU*", "Variable Star of FU Ori type")
        , ((14, 09, 03, 4), "Erupt*RCrB", "RC*", "Variable Star of R CrB type")
        , ((14, 09, 03, 5), "RCrB_Candidate", "RC?", "Variable Star of R CrB type candiate")
        , ((14, 09, 04, 0), "RotV*", "Ro*", "Rotationally variable Star")
        , ((14, 09, 04, 1), "RotV*alf2CVn", "a2*", "Variable Star of alpha2 CVn type")
        , ((14, 09, 04, 3), "Pulsar", "Psr", "Pulsar")
        , ((14, 09, 04, 4), "BYDra", "BY*", "Variable of BY Dra type")
        , ((14, 09, 04, 5), "RSCVn", "RS*", "Variable of RS CVn type")
        , ((14, 09, 05, 0), "PulsV*", "Pu*", "Pulsating variable Star")
        , ((14, 09, 05, 2), "RRLyr", "RR*", "Variable Star of RR Lyr type")
        , ((14, 09, 05, 3), "Cepheid", "Ce*", "Cepheid variable Star")
        , ((14, 09, 05, 5), "PulsV*delSct", "dS*", "Variable Star of delta Sct type")
        , ((14, 09, 05, 6), "PulsV*RVTau", "RV*", "Variable Star of RV Tau type")
        , ((14, 09, 05, 7), "PulsV*WVir", "WV*", "Variable Star of W Vir type")
        , ((14, 09, 05, 8), "PulsV*bCep", "bC*", "Variable Star of beta Cep type")
        , ((14, 09, 05, 9), "deltaCep", "cC*", "Classical Cepheid (delta Cep type)")
        , ((14, 09, 05, 10), "gammaDor", "gD*", "Variable Star of gamma Dor type")
        , ((14, 09, 05, 11), "pulsV*SX", "SX*", "Variable Star of SX Phe type (subdwarf)")
        , ((14, 09, 06, 0), "LPV*", "LP*", "Long-period variable star")
        , ((14, 09, 06, 1), "Mira", "Mi*", "Variable Star of Mira Cet type")
        , ((14, 09, 06, 4), "semi-regV*", "sr*", "Semi-regular pulsating Star")
        , ((14, 09, 08, 0), "SN", "SN*", "SuperNova")
        , ((14, 14, 00, 0), "Sub-stellar", "su*", "Sub-stellar object")
        , ((14, 14, 02, 0), "Planet?", "Pl?", "Extra-solar Planet Candidate")
        , ((14, 14, 10, 0), "Planet", "Pl", "Extra-solar Confirmed Planet")
        , ((15, 00, 00, 0), "Galaxy", "G", "Galaxy")
        , ((15, 01, 00, 0), "PartofG", "PoG", "Part of a Galaxy")
        , ((15, 02, 00, 0), "GinCl", "GiC", "Galaxy in Cluster of Galaxies")
        , ((15, 02, 02, 0), "BClG", "BiC", "Brightest galaxy in a Cluster (BCG)")
        , ((15, 03, 00, 0), "GinGroup", "GiG", "Galaxy in Group of Galaxies")
        , ((15, 04, 00, 0), "GinPair", "GiP", "Galaxy in Pair of Galaxies")
        , ((15, 05, 00, 0), "High_z_G", "HzG", "Galaxy with high redshift")
        , ((15, 06, 00, 0), "AbsLineSystem", "ALS", "Absorption Line system")
        , ((15, 06, 01, 0), "Ly-alpha_ALS", "LyA", "Ly alpha Absorption Line system")
        , ((15, 06, 02, 0), "DLy-alpha_ALS", "DLA", "Damped Ly-alpha Absorption Line system")
        , ((15, 06, 03, 0), "metal_ALS", "mAL", "metallic Absorption Line system")
        , ((15, 06, 05, 0), "Ly-limit_ALS", "LLS", "Lyman limit system")
        , ((15, 06, 08, 0), "Broad_ALS", "BAL", "Broad Absorption Line system")
        , ((15, 07, 00, 0), "RadioG", "rG", "Radio Galaxy")
        , ((15, 08, 00, 0), "HII_G", "H2G", "HII Galaxy")
        , ((15, 09, 00, 0), "LSB_G", "LSB", "Low Surface Brightness Galaxy")
        , ((15, 10, 00, 0), "AGN_Candidate", "AG?", "Possible Active Galaxy Nucleus")
        , ((15, 10, 07, 0), "QSO_Candidate", "Q?", "Possible Quasar")
        , ((15, 10, 11, 0), "Blazar_Candidate", "Bz?", "Possible Blazar")
        , ((15, 10, 17, 0), "BLLac_Candidate", "BL?", "Possible BL Lac")
        , ((15, 11, 00, 0), "EmG", "EmG", "Emission-line galaxy")
        , ((15, 12, 00, 0), "StarburstG", "SBG", "Starburst Galaxy")
        , ((15, 13, 00, 0), "BlueCompG", "bCG", "Blue compact Galaxy")
        , ((15, 14, 00, 0), "LensedImage", "LeI", "Gravitationally Lensed Image")
        , ((15, 14, 01, 0), "LensedG", "LeG", "Gravitationally Lensed Image of a Galaxy")
        , ((15, 14, 07, 0), "LensedQ", "LeQ", "Gravitationally Lensed Image of a Quasar")
        , ((15, 15, 00, 0), "AGN", "AGN", "Active Galaxy Nucleus")
        , ((15, 15, 01, 0), "LINER", "LIN", "LINER-type Active Galaxy Nucleus")
        , ((15, 15, 02, 0), "Seyfert", "SyG", "Seyfert Galaxy")
        , ((15, 15, 02, 1), "Seyfert_1", "Sy1", "Seyfert 1 Galaxy")
        , ((15, 15, 02, 2), "Seyfert_2", "Sy2", "Seyfert 2 Galaxy")
        , ((15, 15, 03, 0), "Blazar", "Bla", "Blazar")
        , ((15, 15, 03, 1), "BLLac", "BLL", "BL Lac - type object")
        , ((15, 15, 03, 2), "OVV", "OVV", "Optically Violently Variable object")
        , ((15, 15, 04, 0), "QSO", "QSO", "Quasar")
        ]

  in check (mapMaybe rowConv stbl)


-- | Return the matching code for the type.
--
--   As there's no compile-time check that there's a match between
--   the two, return a Maybe.
--
simbadTypeToCode :: SimbadType -> Maybe SimbadCode
simbadTypeToCode stype =
  case dropWhile ((/= stype) . _2) simbadLabels of
    ((sc, _, _):_) -> Just sc
    [] -> Nothing

{-
-- | The reverse of simbadTypeToCode.
--
simbadCodeToType :: SimbadCode -> Maybe SimbadType
simbadCodeToType sc =
  case dropWhile ((/= sc) . _1) simbadLabels of
    ((_, stype, _):_) -> Just stype
    [] -> Nothing

-- | Note that this returns Text rather than String,
--   which is different to simbadTypeToDesc.
--
simbadCodeToDesc :: SimbadCode -> Maybe T.Text
simbadCodeToDesc sc =
  case dropWhile ((/= sc) . _1) simbadLabels of
    ((_, _, lbl):_) -> Just lbl
    [] -> Nothing
-}
  
-- | Return the long description for the type.
--
--   As there's no compile-time check that there's a match between
--   the two, return a Maybe.
--
simbadTypeToDesc :: SimbadType -> Maybe T.Text
simbadTypeToDesc stype =
  case dropWhile ((/= stype) . _2) simbadLabels of
    ((_, _, txt):_) -> Just txt
    [] -> Nothing


{-
-- | Do we have a child type (i.e. an element on the branch of
--   the parent).
--
isChildType ::
  SimbadCode -- ^ parent
  -> SimbadCode  -- ^ potential child
  -> Bool
isChildType parent child =
  let toA SimbadCode{..} = [_sc1, _sc2, _sc3, _sc4]
      p = take plvl (toA parent)
      c = take clvl (toA child)

      plvl = _scLevel parent
      clvl = _scLevel child

  in (_scLevel parent < _scLevel child) && (p == c)

-- | Return the parent code.
getSimbadParent ::
  SimbadCode
  -> Maybe SimbadCode
  -- ^ Returns Nothing if the input is at the top level.
getSimbadParent SimbadCode{..} =
  let plvl = _scLevel - 1
      pcodes = take plvl [_sc1, _sc2, _sc3, _sc4]
      [pc1, pc2, pc3, pc4] = pcodes ++ replicate (4-plvl) 0
      parent = SimbadCode pc1 pc2 pc3 pc4 plvl
      
  in if plvl < 1
     then Nothing
     else case filter ((==parent) . _1) simbadLabels of
       [_] -> Just parent
       _ -> Nothing

-}

-- | validate input arguments

toSC4 :: Int -> Int -> Int -> Int -> Either T.Text SimbadCode
toSC4 s1 s2 s3 s4 = do
  i1 <- ivalidate s1 0 15 "1"
  i2 <- ivalidate s2 0 15 "2"
  i3 <- ivalidate s3 0 30 "3"
  i4 <- ivalidate s4 0 11 "4"
  let lvl = 4 - length (takeWhile (==0) [i4, i3, i2])
  return (SimbadCode i1 i2 i3 i4 lvl)
  
ivalidate :: Int -> Int -> Int -> T.Text -> Either T.Text Int
ivalidate v minv maxv lbl =
  let emsg = sformat ("Component " % stext % " range "
                      % int % " to " % int % " sent " % int)
             lbl minv maxv v
  in if v >= minv && v <= maxv
     then Right v
     else Left emsg

-- | Given a Simbad type, identify all the children
--   of that type. The parent is not included in the
--   return list.
--
--   This is not guaranteed to be efficient!
--
findChildTypes ::
  SimbadType
  -> [(SimbadCode, SimbadType, T.Text)]  -- ^ empty for unknown types or leaf nodes.
findChildTypes parent =
  let futures = dropWhile ((/= parent) . _2) simbadLabels
      getLevel = _scLevel . _1
  in case futures of
    (x:xs) -> takeWhile ((> getLevel x) . getLevel) xs
    [] -> []


_1 :: (a, b, c) -> a
_1 (f1, _, _) = f1

_2 :: (a, b, c) -> b
_2 (_, f2, _) = f2

_3 :: (a, b, c) -> c
_3 (_, _, f3) = f3


-- | Which SIMBAD should be queried (this is in case one is down).
--
data SimbadLoc = SimbadCDS | SimbadCfA deriving Eq

simbadBase :: IsString s => SimbadLoc -> s
simbadBase SimbadCDS = "http://simbad.u-strasbg.fr/simbad/"
simbadBase SimbadCfA = "http://simbad.harvard.edu/simbad/"

-- | Return a link to the SIMBAD site (Strasbourg) for this object.
--
-- TODO: need to protect the link
toSIMBADLink :: SimbadLoc -> TargetName -> T.Text
toSIMBADLink sloc name =
  let qry = [ ("Ident", encodeUtf8 (fromTargetName name))
            -- , ("NbIdent", "1")
            -- , ("Radius", "2")
            -- , ("Radius.unit", "arcmin")
            -- , ("submit", "submit id")
            ]

      qryB = renderSimpleQuery True qry

  in decodeUtf8 (simbadBase sloc <> "sim-id" <> qryB)


-- | Represent Chandra observing cycles.
--
-- This type is not used in the database, but is for passing information
-- around the server.
--
-- For now we just have it as a label around the text content, and
-- rely on the fact that the cycles are given as 2-digit integers,
-- so 00 < 01 < 10 < 19 < 20 < all
--
newtype Cycle = Cycle { fromCycle :: T.Text }
           deriving (Eq, Ord)

-- | Labelled as unsafe since there is no processing of the text.
unsafeToCycle :: T.Text -> Cycle
unsafeToCycle = Cycle

{-
-- | The natural values are sorted in increasing order and "all" is
--   last.
instance Ord Cycle where
  (CycleNat n1) `compare` (CycleNat n2) = compare n1 n2
  CycleAll `compare` CycleAll = EQ
  CycleAll `compare` (CycleNat _) = GT
  (CycleNat _) `compare` CycleAll = LT
-}

allCycles :: Cycle
allCycles = Cycle "all"

-- | For now support all cycles up to 99 as can not be bothered with
--   an upper limit.
--
toCycle :: T.Text -> Maybe Cycle
toCycle "all" = Just (Cycle "all")
toCycle x = case T.uncons x of
  Just (c1, x1) -> case T.uncons x1 of
    Just (c2, x2) | T.null x2 && isDigit c1 && isDigit c2 -> Just (Cycle x)
    _ -> Nothing
  _ -> Nothing


instance Parsable Cycle where
  parseParam = helpParse "cycle" toCycle

               
-- * Groundhog instances
--
-- based on the Database.Groundhog.Instances code
--

readHelper :: Read a => PersistValue -> String -> a
readHelper s errMessage = case s of
  PersistString str -> readHelper' str
  PersistText str -> readHelper' (T.unpack str)
  PersistByteString str -> readHelper' (B8.unpack str)
  _ -> error ("readHelper: " <> errMessage)
  where
    readHelper' str = case reads str of
      (a, _):_ -> a
      _        -> error ("readHelper: " <> errMessage)

-- | Helper function for converting string/text values.
--
--   Groundhog version 0.8 seems to have introduced the PersistText
--   constructor which is now being used rather than PersistString.
--
--   This could include PersistByteString but so far it doesn't seem
--   worth it (and PersistString support could probably be dropped).
--
textValueHelper :: String -> (T.Text -> Maybe b) -> PersistValue -> b
textValueHelper lbl f p =
  let (sval, mv) = case p of
        PersistString s -> (s, f (T.pack s))
        PersistText t -> (T.unpack t, f t)
        _ -> (show p, Nothing)
        
      errVal = error ("Unexpected " <> lbl <> ": " <> sval)
  in fromMaybe errVal mv
  
instance NeverNull ChandraTime
instance NeverNull RA
instance NeverNull Dec
instance NeverNull TimeKS
instance NeverNull PropNum
instance NeverNull Sequence
instance NeverNull ObsIdVal
instance NeverNull Instrument
instance NeverNull Grating
instance NeverNull ChipStatus
instance NeverNull SimbadType
instance NeverNull ObsIdStatus

-- times

_pType :: DbType -> a -> b -> DbType
_pType a _ _ = a

instance PersistField ChandraTime where
  persistName _ = "ChandraTime"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType = _pType (DbTypePrimitive DbDayTime False Nothing Nothing)

instance PrimitivePersistField ChandraTime where
  toPrimitivePersistValue = PersistUTCTime . _toUTCTime
  fromPrimitivePersistValue (PersistUTCTime a) = ChandraTime a
  -- fromPrimitivePersistValue (PersistZonedTime (ZT a)) = zonedTimeToUTC a
  fromPrimitivePersistValue x = readHelper x ("Expected ChandraTime (UTCTime), received: " ++ show x)

-- double values

doubleType :: DbType
doubleType = DbTypePrimitive DbReal False Nothing Nothing

instance PersistField RA where
  persistName _ = "RA"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType = _pType doubleType

instance PersistField Dec where
  persistName _ = "Dec"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType = _pType doubleType

instance PersistField TimeKS where
  persistName _ = "TimeKS"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType = _pType doubleType

instance PrimitivePersistField RA where
  toPrimitivePersistValue = PersistDouble . _unRA
  fromPrimitivePersistValue (PersistDouble a) = RA a
  fromPrimitivePersistValue (PersistInt64 a) = RA (fromIntegral a)
  fromPrimitivePersistValue x = readHelper x ("Expected RA (double), received: " ++ show x)

instance PrimitivePersistField Dec where
  toPrimitivePersistValue = PersistDouble . _unDec
  fromPrimitivePersistValue (PersistDouble a) = Dec a
  fromPrimitivePersistValue (PersistInt64 a) = Dec (fromIntegral a)
  fromPrimitivePersistValue x = readHelper x ("Expected Dec (double), received: " ++ show x)

instance PrimitivePersistField TimeKS where
  toPrimitivePersistValue = PersistDouble . _toKS
  fromPrimitivePersistValue (PersistDouble a) = TimeKS a
  fromPrimitivePersistValue (PersistInt64 a) = TimeKS (fromIntegral a)
  fromPrimitivePersistValue x = readHelper x ("Expected TimeKS (double), received: " ++ show x)

-- integer values

-- since this is exported, as I'm too lazy to set up an export list,
-- use an underscore to indicate it's "special"
--
_iType :: FiniteBits b => a -> b -> DbType
_iType _ a = DbTypePrimitive (if finiteBitSize a == 32 then DbInt32 else DbInt64) False Nothing Nothing

instance PersistField ObsIdVal where
  persistName _ = "ObsIdVal"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType = _iType

instance PersistField PropNum where
  persistName _ = "PropNum"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType = _iType

instance PersistField Sequence where
  persistName _ = "Sequence"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType = _iType

instance PrimitivePersistField PropNum where
  toPrimitivePersistValue = PersistInt64 . fromIntegral . _unPropNum
  fromPrimitivePersistValue (PersistInt64 a) = PropNum (fromIntegral a)
  fromPrimitivePersistValue (PersistDouble a) = PropNum (truncate a)
  fromPrimitivePersistValue x = readHelper x ("Expected PropNum (Integer), received: " ++ show x)

instance PrimitivePersistField Sequence where
  toPrimitivePersistValue = PersistInt64 . fromIntegral . _unSequence
  fromPrimitivePersistValue (PersistInt64 a) = Sequence (fromIntegral a)
  fromPrimitivePersistValue (PersistDouble a) = Sequence (truncate a)
  fromPrimitivePersistValue x = readHelper x ("Expected Sequence (Integer), received: " ++ show x)

instance PrimitivePersistField ObsIdVal where
  toPrimitivePersistValue = PersistInt64 . fromIntegral . fromObsId
  fromPrimitivePersistValue (PersistInt64 a) = ObsIdVal (fromIntegral a)
  fromPrimitivePersistValue (PersistDouble a) = ObsIdVal (truncate a)
  fromPrimitivePersistValue x = readHelper x ("Expected ObsIdVal (Integer), received: " ++ show x)

-- enumeration/string-like types

stringType :: DbType
stringType = DbTypePrimitive DbString False Nothing Nothing

instance PersistField Instrument where
  persistName _ = "Instrument"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType = _pType stringType

instance PersistField Grating where
  persistName _ = "Grating"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType = _pType stringType

-- can we force a size on the string?
instance PersistField ChipStatus where
  persistName _ = "ChipStatus"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType = _pType stringType

-- can we force a size on the string?
instance PersistField SimbadType where
  persistName _ = "SimbadType"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType = _pType stringType

instance PersistField Constraint where
  persistName _ = "Constraint"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType = _pType stringType

instance PersistField ConShort where
  persistName _ = "ConShort"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType = _pType stringType

instance PersistField TOORequest where
  persistName _ = "TOORequest"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType = _pType stringType

instance PersistField ShortTermTag where
  persistName _ = "ShortTermTag"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType = _pType stringType

instance PrimitivePersistField ShortTermTag where
  toPrimitivePersistValue = PersistText . fromShortTermTag
  fromPrimitivePersistValue = textValueHelper "ShortTermTag"
                              (Just . ShortTermTag)

instance PersistField ObsIdStatus where
  persistName _ = "ObsIdStatus"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType = _pType stringType

instance PrimitivePersistField ObsIdStatus where
  toPrimitivePersistValue = PersistText . fromObsIdStatus
  fromPrimitivePersistValue = textValueHelper "ObsIdStatus" toObsIdStatus
  

instance PrimitivePersistField Instrument where
  toPrimitivePersistValue = PersistText . fromInstrument
  fromPrimitivePersistValue = textValueHelper "Instrument" toInstrument
  
instance PrimitivePersistField Grating where
  toPrimitivePersistValue = PersistText . fromGrating
  fromPrimitivePersistValue = textValueHelper "Grating" toGrating

instance PrimitivePersistField ChipStatus where
  toPrimitivePersistValue = PersistText . fromChipStatus
  fromPrimitivePersistValue = textValueHelper "chips status" toChipStatus
  
instance PrimitivePersistField SimbadType where
  toPrimitivePersistValue = PersistText . fromSimbadType
  fromPrimitivePersistValue = textValueHelper "Simbad Type" toSimbadType
  
instance PrimitivePersistField Constraint where
  toPrimitivePersistValue = PersistText . T.singleton . fromConstraint
  fromPrimitivePersistValue =
    textValueHelper "constraint" (T.uncons >=> (toConstraint . fst))

instance PrimitivePersistField ConShort where
  toPrimitivePersistValue = PersistText . fromConShort
  fromPrimitivePersistValue = textValueHelper "ConShort" toConShort
  
-- | Note that the actual value is stored, rather than the enumeration.
--   This could lead to problems if the mapping rules change and are not
--   checked against the values stored in the database.
--
instance PrimitivePersistField TOORequest where
  toPrimitivePersistValue = PersistText . trValue
  fromPrimitivePersistValue = textValueHelper "TOO" toTOORequest
  
instance PersistField TargetName where
  persistName _ = "TargetName"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType = _pType stringType

instance PrimitivePersistField TargetName where
  toPrimitivePersistValue = PersistText . fromTargetName
  fromPrimitivePersistValue = textValueHelper "TargetName" (Just . TN)
  
instance PersistField Telescope where
  persistName _ = "Telescope"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType = _pType stringType

instance PrimitivePersistField Telescope where
  toPrimitivePersistValue = PersistText . fromTelescope
  fromPrimitivePersistValue = textValueHelper "Telescope" (Just . Telescope)
  
-- needed for persistent integer types

instance Bits PropNum where
  (PropNum a) .&. (PropNum b) = PropNum (a .&. b)
  (PropNum a) .|. (PropNum b) = PropNum (a .|. b)
  (PropNum a) `xor` (PropNum b) = PropNum (a `xor` b)
  complement = PropNum . complement . _unPropNum
  shift a i = PropNum $ shift (_unPropNum a) i
  rotate a i = PropNum $ rotate (_unPropNum a) i
  bitSize = fromMaybe (error "invalid bitsize") . bitSizeMaybe
  bitSizeMaybe = bitSizeMaybe . _unPropNum
  isSigned = isSigned . _unPropNum
  testBit a = testBit (_unPropNum a)
  bit = PropNum . bit
  popCount = popCount . _unPropNum

instance Bits Sequence where
  (Sequence a) .&. (Sequence b) = Sequence (a .&. b)
  (Sequence a) .|. (Sequence b) = Sequence (a .|. b)
  (Sequence a) `xor` (Sequence b) = Sequence (a `xor` b)
  complement = Sequence . complement . _unSequence
  shift a i = Sequence $ shift (_unSequence a) i
  rotate a i = Sequence $ rotate (_unSequence a) i
  bitSize = fromMaybe (error "invalid bitsize") . bitSizeMaybe
  bitSizeMaybe = bitSizeMaybe . _unSequence
  isSigned = isSigned . _unSequence
  testBit a = testBit (_unSequence a)
  bit = Sequence . bit
  popCount = popCount . _unSequence

instance Bits ObsIdVal where
  (ObsIdVal a) .&. (ObsIdVal b) = ObsIdVal (a .&. b)
  (ObsIdVal a) .|. (ObsIdVal b) = ObsIdVal (a .|. b)
  (ObsIdVal a) `xor` (ObsIdVal b) = ObsIdVal (a `xor` b)
  complement = ObsIdVal . complement . fromObsId
  shift a i = ObsIdVal $ shift (fromObsId a) i
  rotate a i = ObsIdVal $ rotate (fromObsId a) i
  bitSize = fromMaybe (error "invalid bitsize") . bitSizeMaybe
  bitSizeMaybe = bitSizeMaybe . fromObsId
  isSigned = isSigned . fromObsId
  testBit a = testBit (fromObsId a)
  bit = ObsIdVal . bit
  popCount = popCount . fromObsId

instance FiniteBits PropNum where
  finiteBitSize = finiteBitSize . _unPropNum

instance FiniteBits Sequence where
  finiteBitSize = finiteBitSize . _unSequence

instance FiniteBits ObsIdVal where
  finiteBitSize = finiteBitSize . fromObsId


handleMigration ::
  (PersistBackend m, MonadIO m) => m ()
handleMigration =
  runMigration $ do
    migrate (undefined :: ScienceObs)
    migrate (undefined :: NonScienceObs)
    -- migrate (undefined :: OverlapObs)  do we use this yet?
    migrate (undefined :: Proposal)
    migrate (undefined :: ProposalAbstract)
    migrate (undefined :: MissingProposalAbstract)
    migrate (undefined :: SimbadInfo)
    migrate (undefined :: SimbadMatch)
    migrate (undefined :: SimbadNoMatch)
    migrate (undefined :: MetaData)
    migrate (undefined :: InvalidObsId)
    migrate (undefined :: ShortTermSchedule)


-- We do not take advantage of the database here (eg unique fields,
-- or relations between entities).
--
-- also, could save some code above by taking advantage of the
-- "primitive" option in the GroundHog TH
--
-- does the name field on the uniques entry need to be unique?
--
-- How about restricting the lengths of some text fields?
--
mkPersist defaultCodegenConfig [groundhog|
- entity: ScienceObs
  constructors:
    - name: ScienceObs
      uniques:
        - name: ScienceObsIdConstraint
          fields: [soObsId]
- entity: NonScienceObs
  constructors:
    - name: NonScienceObs
      uniques:
        - name: NonScienceObsIdConstraint
          fields: [nsObsId]
- entity: Proposal
  constructors:
    - name: Proposal
      uniques:
        - name: PropConstraint
          fields: [propNum]
- entity: ProposalAbstract
  constructors:
    - name: ProposalAbstract
      uniques:
        - name: PropAbstractConstraint
          fields: [paNum]
- entity: MissingProposalAbstract
  constructors:
    - name: MissingProposalAbstract
      uniques:
        - name: MissingAbstractConstraint
          fields: [mpNum]
- entity: SimbadInfo
  constructors:
    - name: SimbadInfo
      uniques:
        - name: SimbadInfoConstraint
          fields: [smiName]
- entity: SimbadMatch
  constructors:
    - name: SimbadMatch
      uniques:
        - name: SimbadMatchConstraint
          fields: [smmTarget]
- entity: SimbadNoMatch
  constructors:
    - name: SimbadNoMatch
      uniques:
        - name: SimbadNoMatchConstraint
          fields: [smnTarget]
- entity: MetaData
  constructors:
    - name: MetaData
      uniques:
        - name: MetaDataLastModifiedConstraint
          fields: [mdLastModified]
- entity: InvalidObsId
  constructors:
    - name: InvalidObsId
      uniques:
        - name: InvalidObsIdConstraint
          fields: [ioObsId]
- entity: ShortTermSchedule
  constructors:
    - name: ShortTermSchedule
      uniques:
        - name: ShortTermScheduleTagConstraint
          fields: [stsTag]

|]


{-
Turns out I can not use these for Database::fetchJointMission
so comment out for now.

missionSelectorField ::
  JointMission
  -> Field ScienceObs ScienceObsConstructor (Maybe TimeKS)
missionSelectorField HST = SoJointHSTField
missionSelectorField NOAO = SoJointNOAOField
missionSelectorField NRAO = SoJointNRAOField
missionSelectorField RXTE = SoJointRXTEField
missionSelectorField Spitzer = SoJointSPITZERField
missionSelectorField Suzaku = SoJointSUZAKUField
missionSelectorField XMM = SoJointXMMField
missionSelectorField Swift = SoJointSWIFTField
missionSelectorField NuSTAR = SoJointNUSTARField
-}


-- Use Template Haskell to derive the necessary To/FromJSON
-- instances (seeing as we use TH already for GroundHog)
--
-- note: Record is a type alias for Either, so do not need to
-- derive any instances
--
-- it would have been nice to have a consistent naming scheme to reduce
-- the number of different 'drop x' statements below
--
-- for now only bother with the ToJSON instances as do not need
-- FromJSON.
--

$(deriveToJSON defaultOptions{fieldLabelModifier = drop 2, constructorTagModifier = map toLower} ''NonScienceObs)
$(deriveToJSON defaultOptions{fieldLabelModifier = drop 2, constructorTagModifier = map toLower} ''ScienceObs)

-- $(deriveToJSON defaultOptions{fieldLabelModifier = drop 2, constructorTagModifier = map toLower} ''Record)

$(deriveToJSON defaultOptions{fieldLabelModifier = drop 2, constructorTagModifier = map toLower} ''ObsInfo)
$(deriveToJSON defaultOptions{fieldLabelModifier = drop 3, constructorTagModifier = map toLower} ''ChandraTime)
$(deriveToJSON defaultOptions{fieldLabelModifier = drop 2, constructorTagModifier = map toLower} ''ChipStatus)
$(deriveToJSON defaultOptions{fieldLabelModifier = drop 3, constructorTagModifier = map toLower} ''RA)
$(deriveToJSON defaultOptions{fieldLabelModifier = drop 3, constructorTagModifier = map toLower} ''Dec)
$(deriveToJSON defaultOptions{fieldLabelModifier = drop 4, constructorTagModifier = map toLower} ''ConShort)
$(deriveToJSON defaultOptions{fieldLabelModifier = drop 2, constructorTagModifier = map toLower} ''Constraint)
$(deriveToJSON defaultOptions{fieldLabelModifier = drop 4, constructorTagModifier = map toLower} ''ObsIdVal)
$(deriveToJSON defaultOptions{fieldLabelModifier = drop 2, constructorTagModifier = map toLower} ''Instrument)
$(deriveToJSON defaultOptions{fieldLabelModifier = drop 2, constructorTagModifier = map toLower} ''Grating)
$(deriveToJSON defaultOptions{fieldLabelModifier = drop 3, constructorTagModifier = map toLower} ''TimeKS)
$(deriveToJSON defaultOptions{fieldLabelModifier = drop 3, constructorTagModifier = map toLower} ''PropNum)
$(deriveToJSON defaultOptions{fieldLabelModifier = drop 3, constructorTagModifier = map toLower} ''Sequence)
$(deriveToJSON defaultOptions{fieldLabelModifier = drop 3, constructorTagModifier = map toLower} ''SimbadInfo)
$(deriveToJSON defaultOptions{fieldLabelModifier = drop 4, constructorTagModifier = map toLower} ''SimbadType)
$(deriveToJSON defaultOptions{fieldLabelModifier = drop 4, constructorTagModifier = map toLower} ''Proposal)
-- $(deriveToJSON defaultOptions{fieldLabelModifier = drop 2, constructorTagModifier = map toLower} ''ProposalAbstract)

-- TODO: is the drop correct?
$(deriveToJSON defaultOptions{fieldLabelModifier = drop 2, constructorTagModifier = map toLower} ''ObsIdStatus)
$(deriveToJSON defaultOptions{fieldLabelModifier = drop 2, constructorTagModifier = map toLower} ''Telescope)
