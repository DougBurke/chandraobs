{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Types (MetaData(..)
             , fromMetaData, toMetaData
             , ScienceObs(..), ScienceObsRaw(..)
             , NonScienceObs(..), InvalidObsId(..)
             --
             , Record
             , recordObsId
             , recordStartTime
             , recordEndTime
             , recordTime
             , recordTarget
             , ObsInfo(..)
             --
             , TelescopeId
             , Sequence
             , unsafeToSequence
             , fromSequence
             , ObsIdStatus(..)
             , fromObsIdStatus, toObsIdStatus
             , ObsIdVal(..)
             , fromObsIdVal
             , toObsIdValStr
             , unsafeToObsIdVal
             , Proposal(..)
             , ProposalAbstract(..)
             , MissingProposalAbstract(..)
             , PropNum
             , fromPropNum, toPropNum
             , TOORequest
             , fromTOORequest, toTOORequest
             , tooTime, rtToLabel
             , Telescope
             , fromTelescope, toTelescope
             , ChandraTime
             , toChandraTime
             , fromChandraTime
             , showCTime
             , TargetName
             , fromTargetName, toTargetName
             , Constraint(NoConstraint)
             , fromConstraint, toConstraint
             , Instrument(..)
             , fromInstrument, toInstrument
             , Grating(..)
             , fromGrating, toGrating
             , RA
             , fromRA, toRA
             , showRA
             , Dec
             , fromDec, toDec
             , showDec
             , ConShort
             , fromConShort, toConShort
             , TimeKS
             , fromTimeKS, {- toTimeKS, -} unsafeToTimeKS
             , isZeroKS
             , showExpTime
             , ChipStatus(..)
             , fromChipStatus, toChipStatus
             , SimbadMatch(..), SimbadNoMatch(..), SimbadInfo(..)
             , SimbadInfoKey
             , toSI, siExpr
             , toSM, smExpr
             , toSNM, snmExpr
             , SimbadType, fromSimbadType, toSimbadType
             , SIMCategory(..), fromSIMCategory
             
             , SimbadLoc(..)
             , simbadBase
             -- , toSIMBADLink
             
             , TelescopeValues(..)
             --
             , metaDataSchema
             , scienceObsRawSchema
             , nonScienceObsSchema, invalidObsIdSchema
             , proposalSchema, proposalAbstractSchema
             , missingProposalAbstractSchema
             , simbadMatchSchema, simbadNoMatchSchema, simbadInfoSchema
             , telescopeValuesSchema

             --
             , _2
             
             )
  where

import qualified Data.Text as T

import Data.Function (on)
import Data.Int (Int16, Int32, Int64)
import Data.Maybe (fromMaybe)
import Data.String (IsString(..))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (UTCTime, addUTCTime)

import Formatting (Format, (%), (%.), sformat)
import Formatting.Formatters (char, left, fixed, int, stext)
import Formatting.Time (hm, dayName, dayOfMonthS, monthName, year)

import GHC.Generics (Generic)

import Network.HTTP.Types.URI (renderSimpleQuery)
import Numeric.Natural (Natural)

import Rel8 (Column
            , DBEq, DBOrd, DBType(..)
            , Expr
            , Name
            , Rel8able
            , Result
            , TableSchema(..)
            , lit
            , nextval
            , parseTypeInformation
            , unsafeCoerceExpr
            )

import Text.Read (readMaybe)


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
  if p a then pure (conv a) else Nothing

-- | Inclusive range.
inRange :: 
    Ord a 
    => a    -- ^ lower limit 
    -> a    -- ^ upper limit
    -> a    -- ^ value
    -> Bool -- ^ lower limit <= value <= upper limit
inRange lo hi v = (lo <= v) && (v <= hi)


-- | A wrapper around `UTCTime` so that we can use our
--   own `ToMarkup` and `ToValue` instances.
--
newtype ChandraTime = ChandraTime { fromChandraTime :: UTCTime }
  deriving newtype (DBEq, DBType, DBOrd)
  deriving (Eq, Ord)

toChandraTime :: UTCTime -> ChandraTime
toChandraTime = ChandraTime

-- | Create a \"nice\" display of the time:
--   \"HH:MM Day, DN Month, Year (UTC)\", where Day and Month
--   are the (full) names and DN is the day number within
--   the month.
--
--   This does not correct for time zones.
showCTime :: ChandraTime -> T.Text
showCTime ct = 
  let utc = fromChandraTime ct

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


-- | Helper function for displaying start times.
--
showStartTime :: Maybe ChandraTime -> T.Text
showStartTime (Just ct) = "at " <> showCTime ct
showStartTime Nothing   = "unscheduled"


endCTime :: ChandraTime -> TimeKS -> ChandraTime
endCTime (ChandraTime start) (TimeKS elen) =
  let nsec = 1000 * elen
      delta = (fromRational . toRational) nsec
  in ChandraTime (addUTCTime delta start)


-- | Represent a value in kiloseconds.
--
--   It is assumed that the time value is >= 0.
--
newtype TimeKS = TimeKS { fromTimeKS :: Double } 
  deriving newtype (DBEq, DBType, DBOrd)
  deriving (Eq, Ord)

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


newtype RA = RA { fromRA :: Double } 
  deriving newtype (DBType, DBEq)

-- | Currently no validation.
--
toRA :: Double -> RA
toRA = RA

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

newtype Dec = Dec { fromDec :: Double } 
  deriving newtype (DBType, DBEq)

-- | Currently no validation.
--
toDec :: Double -> Dec
toDec = Dec

-- this is intended for HTML/UTF-8 output, so instead of
-- "d" it uses "\176", aka \u00b0, the degree symbol.
--
-- TODO: use Data.Text.Format
showDec ::
  Bool
  -- ^ If True then use UTF-8 for degrees, otherwise 'd'
  -> Dec
  -> T.Text
showDec flag (Dec dec) = 
  let dabs = abs dec
      d, m :: Int
      r1, r2 :: Double
      (d, r1) = properFraction dabs
      dm = r1 * 60
      (m, r2) = properFraction dm
      s = r2 * 60
      c = if dec < 0 then '-' else '+'

      sep = if flag then '\176' else 'd'

  in sformat (char % int2 % char % " " % int2 % "' " % float4 % "\"") c d sep m s

-- Internal routine; assumes that tol is >= 0              
isClose :: Double -> Double -> Double -> Bool
isClose tol a b = let adiff = abs (a - b)
                  in adiff <= tol

atol :: Double
atol = 1e-6

-- | The absolute tolerance is 1e-6, which is about 4 micro arcseconds,
--   unless my math has deserted me.
instance Eq RA where
  (==) = isClose atol `on` fromRA

-- | The absolute tolerance is 1e-6, which is about 4 micro arcseconds,
--   unless my math has deserted me.
instance Eq Dec where
  (==) = isClose atol `on` fromDec


-- | The Telescope abstraction is not greaty because the data does not make it obvious
--   what the constituents are.
--
newtype Telescope = Telescope { fromTelescope :: T.Text }
  deriving newtype (DBEq, DBType, Eq)

toTelescope :: T.Text -> Telescope
toTelescope = Telescope


-- | Represent a Chandra proposal number.
newtype PropNum = PropNum { fromPropNum64 :: Int64 }  -- should this be Int32?
  deriving newtype (DBEq, DBType, DBOrd)
  deriving (Eq, Ord)

-- | There is no validation.
--
toPropNum :: Int -> PropNum
toPropNum = PropNum . fromIntegral

fromPropNum :: PropNum -> Int
fromPropNum = fromIntegral . fromPropNum64


-- | When was the contents of the database last updated.
--
--   Ideally the monad used to query or update the database
--   would identify when this might be necessary from when
--   not.
--
newtype MetaData f = MetaData
  { mdLastModified :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

metaDataSchema :: TableSchema (MetaData Name)
metaDataSchema = TableSchema
  { name = "MetaData"
  , columns = MetaData
    { mdLastModified = "mdLastModified"
    }
  }

toMetaData :: UTCTime -> MetaData Result
toMetaData = MetaData

fromMetaData :: MetaData Result -> UTCTime
fromMetaData = mdLastModified


-- | Represent the row-mapping for the sorMultiTelObs field.
--
newtype TelescopeId = TelescopeId { fromTelescopeId :: Int64 }
  deriving newtype (DBEq, DBOrd, DBType)
  deriving (Eq, Ord)


-- Since ScienceObs doesn't directly encode the stored data
-- then we
--
-- a) do not create a schema
-- b) do not paramtrize access with a functor [NOT YET IMPLEMENTED]
--

data ScienceObs f =
  ScienceObs
  { soSequence :: Column f Sequence
  , soProposal :: Column f PropNum
  , soStatus :: Column f ObsIdStatus
  , soObsId :: Column f ObsIdVal
  , soTarget :: Column f TargetName
  --
  , soStartTime :: Column f (Maybe ChandraTime)
  , soApprovedTime :: Column f TimeKS
  , soObservedTime :: Column f (Maybe TimeKS)
  , soPublicRelease :: Column f (Maybe UTCTime)
  --
  , soTimeCritical :: Column f Constraint
  , soMonitor :: Column f Constraint
  , soConstrained :: Column f Constraint
  --
  , soInstrument :: Column f Instrument
  , soGrating :: Column f Grating
  , soDetector :: Column f (Maybe T.Text)
  , soDataMode :: Column f (Maybe T.Text)
  --
  , soACISI0 :: Column f ChipStatus
  , soACISI1 :: Column f ChipStatus
  , soACISI2 :: Column f ChipStatus
  , soACISI3 :: Column f ChipStatus
  , soACISS0 :: Column f ChipStatus
  , soACISS1 :: Column f ChipStatus
  , soACISS2 :: Column f ChipStatus
  , soACISS3 :: Column f ChipStatus
  , soACISS4 :: Column f ChipStatus
  , soACISS5 :: Column f ChipStatus
  --
  , soJointWith :: Column f (Maybe T.Text)
  , soJointHST :: Column f (Maybe TimeKS)
  , soJointNOAO :: Column f (Maybe TimeKS)
  , soJointNRAO :: Column f (Maybe TimeKS)
  , soJointRXTE :: Column f (Maybe TimeKS)
  , soJointSPITZER :: Column f (Maybe TimeKS)
  , soJointSUZAKU :: Column f (Maybe TimeKS)
  , soJointXMM :: Column f (Maybe TimeKS)
  , soJointSWIFT :: Column f (Maybe TimeKS)
  , soJointNUSTAR :: Column f (Maybe TimeKS)
  --
  , soMultiTel :: Column f Bool
  , soMultiTelInt :: Column f Double
  , soMultiTelObs :: Column f [Telescope]
  --
  , soTOO :: Column f (Maybe TOORequest)
  , soRA :: Column f RA
  , soDec :: Column f Dec
  , soConstellation :: Column f ConShort
  , soRoll :: Column f Double
  --
  , soSubArrayStart :: Column f (Maybe Int32)
  , soSubArraySize :: Column f (Maybe Int32)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- | This is for debug purposes.
instance f ~ Result => Show (ScienceObs f) where
  show ScienceObs{..} = 
    "Science: " <> show (fromObsIdVal soObsId)
    <> " " <> T.unpack (fromTargetName soTarget)
    <> " with "
    <> show soInstrument
    <> "+"
    <> show soGrating
    <> " for " <> show (fromTimeKS soApprovedTime)
    <> " ks " <> T.unpack (showStartTime soStartTime)
    <> " " <> T.unpack (fromObsIdStatus soStatus)


-- Access the data as stored in the database. This is intended to be an internal
-- representation.
--
data ScienceObsRaw f =
  ScienceObsRaw
  { sorSequence :: Column f Sequence
  , sorProposal :: Column f PropNum
  , sorStatus :: Column f ObsIdStatus
  , sorObsId :: Column f ObsIdVal
  , sorTarget :: Column f TargetName
  --
  , sorStartTime :: Column f (Maybe ChandraTime)
  , sorApprovedTime :: Column f TimeKS
  , sorObservedTime :: Column f (Maybe TimeKS)
  , sorPublicRelease :: Column f (Maybe UTCTime)
  --
  , sorTimeCritical :: Column f Constraint
  , sorMonitor :: Column f Constraint
  , sorConstrained :: Column f Constraint
  --
  , sorInstrument :: Column f Instrument
  , sorGrating :: Column f Grating
  , sorDetector :: Column f (Maybe T.Text)
  , sorDataMode :: Column f (Maybe T.Text)
  --
  , sorACISI0 :: Column f ChipStatus
  , sorACISI1 :: Column f ChipStatus
  , sorACISI2 :: Column f ChipStatus
  , sorACISI3 :: Column f ChipStatus
  , sorACISS0 :: Column f ChipStatus
  , sorACISS1 :: Column f ChipStatus
  , sorACISS2 :: Column f ChipStatus
  , sorACISS3 :: Column f ChipStatus
  , sorACISS4 :: Column f ChipStatus
  , sorACISS5 :: Column f ChipStatus
  --
  , sorJointWith :: Column f (Maybe T.Text)
  , sorJointHST :: Column f (Maybe TimeKS)
  , sorJointNOAO :: Column f (Maybe TimeKS)
  , sorJointNRAO :: Column f (Maybe TimeKS)
  , sorJointRXTE :: Column f (Maybe TimeKS)
  , sorJointSPITZER :: Column f (Maybe TimeKS)
  , sorJointSUZAKU :: Column f (Maybe TimeKS)
  , sorJointXMM :: Column f (Maybe TimeKS)
  , sorJointSWIFT :: Column f (Maybe TimeKS)
  , sorJointNUSTAR :: Column f (Maybe TimeKS)
  --
  , sorMultiTel :: Column f Bool
  , sorMultiTelInt :: Column f Double
  , sorMultiTelObs :: Column f TelescopeId
  --
  , sorTOO :: Column f (Maybe TOORequest)
  , sorRA :: Column f RA
  , sorDec :: Column f Dec
  , sorConstellation :: Column f ConShort
  , sorRoll :: Column f Double
  --
  , sorSubArrayStart :: Column f (Maybe Int32)
  , sorSubArraySize :: Column f (Maybe Int32)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)


scienceObsRawSchema :: TableSchema (ScienceObsRaw Name)
scienceObsRawSchema = TableSchema
  { name = "ScienceObs"
  , columns = ScienceObsRaw
    { sorSequence = "soSequence"
    , sorProposal = "soProposal"
    , sorStatus = "soStatus"
    , sorObsId = "soObsId"
    , sorTarget = "soTarget"
    , sorStartTime = "soStartTime"  
    , sorApprovedTime = "soApprovedTime"
    , sorObservedTime = "soObservedTime"
    , sorPublicRelease = "soPublicRelease"
    , sorTimeCritical = "soTimeCritical"
    , sorMonitor = "soMonitor"
    , sorConstrained = "soConstrained"
    , sorInstrument = "soInstrument"
    , sorGrating = "soGrating"
    , sorDetector = "soDetector"
    , sorDataMode = "soDataMode"
    , sorACISI0 = "soACISI0"
    , sorACISI1 = "soACISI1"
    , sorACISI2 = "soACISI2"
    , sorACISI3 = "soACISI3"
    , sorACISS0 = "soACISS0"
    , sorACISS1 = "soACISS1"
    , sorACISS2 = "soACISS2"
    , sorACISS3 = "soACISS3"
    , sorACISS4 = "soACISS4"
    , sorACISS5 = "soACISS5"
    , sorJointWith = "soJointWith"
    , sorJointHST = "soJointHST"
    , sorJointNOAO = "soJointNOAO"
    , sorJointNRAO = "soJointNRAO"
    , sorJointRXTE = "soJointRXTE"
    , sorJointSPITZER = "soJointSPITZER"
    , sorJointSUZAKU = "soJointSUZAKU"
    , sorJointXMM = "soJointXMM"
    , sorJointSWIFT = "soJointSWIFT"
    , sorJointNUSTAR = "soJointNUSTAR"
    , sorMultiTel = "soMultiTel"
    , sorMultiTelInt = "soMultiTelInt"
    , sorMultiTelObs = "soMultiTelObs"
    , sorTOO = "soTOO"
    , sorRA = "soRA"
    , sorDec = "soDec"
    , sorConstellation = "soConstellation"
    , sorRoll = "soRoll"
    , sorSubArrayStart = "soSubArrayStart"
    , sorSubArraySize = "soSubArraySize"
    }
  }

data NonScienceObs f = NonScienceObs
  { nsStatus :: Column f ObsIdStatus
  , nsObsId :: Column f ObsIdVal
  , nsTarget :: Column f TargetName
  , nsStartTime :: Column f (Maybe ChandraTime)
  , nsTime :: Column f TimeKS
  , nsRa :: Column f RA
  , nsDec :: Column f Dec
  , nsRoll :: Column f Double
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- | This is for debug purposes.
instance f ~ Result => Show (NonScienceObs f) where
  show NonScienceObs{..} = 
    "CAL: " <> show (fromObsIdVal nsObsId)
    <> "(" <> T.unpack (fromObsIdStatus nsStatus)
    <> ") for " <> show (fromTimeKS nsTime)
    <> " ks " <> T.unpack (showStartTime nsStartTime)


nonScienceObsSchema :: TableSchema (NonScienceObs Name)
nonScienceObsSchema = TableSchema
  { name = "NonScienceObs"
  , columns = NonScienceObs
    { nsStatus = "nsStatus"
    , nsObsId = "nsObsId"
    , nsTarget = "nsTarget"
    , nsStartTime = "nsStartTime"
    , nsTime = "nsTime"
    , nsRa = "nsRa"
    , nsDec = "nsDec"
    , nsRoll = "nsRoll"
    }
  }


data InvalidObsId f = InvalidObsId
  { ioObsId :: Column f ObsIdVal     -- ^ The ObsId in question
  , ioChecked :: Column f UTCTime  -- ^ The approximate time this was checked
  , ioMessage :: Column f T.Text   -- ^ reason it's "bad"
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

invalidObsIdSchema :: TableSchema (InvalidObsId Name)
invalidObsIdSchema = TableSchema
  { name = "InvalidObsId"
  , columns = InvalidObsId
    { ioObsId = "ioObsId"
    , ioChecked = "ioChecked"
    , ioMessage = "ioMessage"
    }
  }

  
data ProposalAbstract f = ProposalAbstract
  { paNum :: Column f PropNum
  , paTitle :: Column f T.Text
  , paAbstract :: Column f T.Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

proposalAbstractSchema :: TableSchema (ProposalAbstract Name)
proposalAbstractSchema = TableSchema
  { name = "ProposalAbstract"
  , columns = ProposalAbstract
    { paNum = "paNum"
    , paTitle = "paTitle"
    , paAbstract = "paAbstract"
    }
  }

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
data Proposal f = Proposal
  { propNum :: Column f PropNum
  , propName :: Column f T.Text
  , propPI :: Column f T.Text
  , propCategory :: Column f PropCategory
  , propType :: Column f T.Text
  -- TODO: change to use the PropType enumeration, but
  -- that has implications on the database, especially
  -- as the set of types has increased with time
  , propCycle :: Column f T.Text
  -- Should this be an enumeration? It is "open ended",
  -- in that there's no fixed end value.
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)


{-
-- | Proposals are ordered by proposal number.
instance Ord (Proposal f) where
  compare = compare `on` propNum
-}


proposalSchema :: TableSchema (Proposal Name)
proposalSchema = TableSchema
  { name = "Proposal"
  , columns = Proposal
    { propNum = "propNum"
    , propName = "propName"
    , propPI = "propPI"
    , propCategory = "propCategory"
    , propType = "propType"
    , propCycle = "propCycle"
    }
  }


-- | Record abstracts which have been attempted but for which
--   something went wrong. This is (hopefully) going to all be
--   "Missing abstract" (i.e. the OCAT does not have the information)
--   but there could be network issues too.
--
data MissingProposalAbstract f =
  MissingProposalAbstract
  { mpNum :: Column f PropNum
  -- ^ proposal number
  , mpReason :: Column f T.Text
  -- ^ reason for failure
  , mpChecked :: Column f UTCTime
  -- ^ approximate time the query was made
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

missingProposalAbstractSchema :: TableSchema (MissingProposalAbstract Name)
missingProposalAbstractSchema = TableSchema
  { name = "MissingProposalAbstract"
  , columns = MissingProposalAbstract
    { mpNum = "mpNum"
    , mpReason = "mpReason"
    , mpChecked = "mpChecked"
    }
  }


-- | This was created by groundhog to suppport storing [Telescope].
--
data TelescopeValues f = TelescopeValues
  { tvId :: Column f TelescopeId
  , tvOrd :: Column f Int16   -- do we need this?
  , tvValue :: Column f Telescope
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

telescopeValuesSchema :: TableSchema (TelescopeValues Name)
telescopeValuesSchema = TableSchema
  { name = "List##Telescope#values"
  , columns = TelescopeValues
    { tvId = "id"
    , tvOrd = "ord"
    , tvValue = "value"
    }
  }

-- | Represent an entry in the schedule; this is
--   a "catch-all" type that is used as I play around with the
--   database.
--
type Record = Either (NonScienceObs Result) (ScienceObs Result)

-- hacks for quickly converting old code; however, the idea of
-- a Record has stuck around for a while, so it may need to stay

recordObsId :: Record -> ObsIdVal
recordObsId = either nsObsId soObsId

recordTarget :: Record-> TargetName
recordTarget = either nsTarget soTarget

recordStartTime :: Record -> Maybe ChandraTime
recordStartTime = either nsStartTime soStartTime

recordEndTime :: Record -> Maybe ChandraTime
recordEndTime r =
  let stime = recordStartTime r
      duration = recordTime r
  in flip endCTime duration <$> stime

-- Use the actual time if we have it, otherwise the approved time
recordTime :: Record -> TimeKS
recordTime = either nsTime (\ScienceObs{..} -> fromMaybe soApprovedTime soObservedTime)


-- | I just want a simple way of passing around 
--   useful information about an observation.
data ObsInfo = ObsInfo {
  oiCurrentObs :: Record
  , oiPrevObs  :: Maybe Record
  , oiNextObs  :: Maybe Record
  }


-- | Represent a Chandra sequence number.
newtype Sequence = Sequence { fromSequence64 :: Int64 } 
   deriving newtype (DBEq, DBOrd, DBType)
   deriving (Eq, Ord)

-- | No validation!
unsafeToSequence :: Int -> Sequence
unsafeToSequence = Sequence . fromIntegral

fromSequence :: Sequence -> Int
fromSequence = fromIntegral . fromSequence64


newtype ObsIdVal = ObsIdVal { fromObsId64 :: Int64 }
  deriving newtype (DBEq, DBOrd, DBType)
  deriving (Eq, Ord)

fromObsIdVal :: ObsIdVal -> Int
fromObsIdVal = fromIntegral . fromObsId64

-- | No validation!
--
unsafeToObsIdVal :: Int -> ObsIdVal
unsafeToObsIdVal = ObsIdVal . fromIntegral

-- I assume that 0 is valid
validObsIdVal :: Int64 -> Bool
validObsIdVal = inRange 0 65535

-- | Limited validation of the input.
toObsIdValStr :: T.Text -> Maybe ObsIdVal
toObsIdValStr = maybeFromText ObsIdVal validObsIdVal


data ObsIdStatus = 
  Discarded | Canceled | Unobserved | Scheduled | Observed | Archived
  deriving (DBEq, DBOrd, Eq)

instance DBType ObsIdStatus where
  typeInformation = parseTypeInformation fromOIS toOIS typeInformation
    where
      fromOIS t = case toObsIdStatus t of
        Just v -> Right v
        _ -> Left ("Unknown ObsIdStatus: '" <> T.unpack t <> "'")

      toOIS = fromObsIdStatus

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


newtype TargetName = TargetName { fromTargetName :: T.Text }
  deriving newtype (DBEq, DBType, DBOrd)
  deriving (Eq, Ord)

toTargetName :: T.Text -> TargetName
toTargetName = TargetName

-- | The instrument being used.
--
--   The Ord constraint is useful when creating tables but has no
--   real semantic meaning.
data Instrument = ACISI | ACISS | HRCI | HRCS 
  deriving (DBEq, DBOrd, Eq, Ord, Show)

instance DBType Instrument where
  typeInformation = parseTypeInformation fromI toI typeInformation
    where
      fromI t = case toInstrument t of
        Just v -> Right v
        _ -> Left ("Unknown Instrument: '" <> T.unpack t <> "'")

      toI = fromInstrument

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


-- | The grating to be used.
--
--   The Ord constraint is useful when creating tables but has no
--   real semantic meaning.
--
data Grating = LETG | HETG | NONE 
  deriving (DBEq, Eq, Show)

instance DBType Grating where
  typeInformation = parseTypeInformation fromG toG typeInformation
    where
      fromG t = case toGrating t of
        Just v -> Right v
        _ -> Left ("Unknown Grating: '" <> T.unpack t <> "'")

      toG = fromGrating

fromGrating :: Grating -> T.Text
fromGrating LETG = "LETG"
fromGrating HETG = "HETG"
fromGrating NONE = "NONE"

toGrating :: T.Text -> Maybe Grating
toGrating "LETG" = Just LETG
toGrating "HETG" = Just HETG
toGrating "NONE"  = Just NONE

toGrating _ = Nothing


-- | Chandra constraints can be none, required, or preferred
data Constraint = NoConstraint | Preferred | Required
  deriving (DBEq, DBOrd, Eq, Ord)

instance DBType Constraint where
  typeInformation = parseTypeInformation fromC toC typeInformation
    where
      fromC t = case toConstraint t of
        Just v -> Right v
        _ -> Left ("Unknown Constraint: '" <> [t] <> "'")

      toC = fromConstraint

toConstraint :: Char -> Maybe Constraint
toConstraint 'N' = Just NoConstraint
toConstraint 'P' = Just Preferred
toConstraint 'Y' = Just Required
toConstraint _   = Nothing

fromConstraint :: Constraint -> Char
fromConstraint NoConstraint = 'N'
fromConstraint Preferred    = 'P'
fromConstraint Required     = 'Y'

-- | Is a chip on, off, or optional.
--
--   How many optional chips are allowed?
--
--   I assume ChipD is for dropped? Seen in ObsId 1547
--
data ChipStatus = ChipOn | ChipOff | ChipOpt1 | ChipOpt2 | ChipOpt3 | ChipOpt4 | ChipOpt5 | ChipD
  deriving (DBEq, Eq)

instance DBType ChipStatus where
  typeInformation = parseTypeInformation fromCS toCS typeInformation
    where
      fromCS t = case toChipStatus t of
        Just v -> Right v
        _ -> Left ("Unknown Chip Status: '" <> T.unpack t <> "'")

      toCS = fromChipStatus

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


-- | Long form for constellation names (e.g. Ursa Major),
--   although there's no validation that the string is valid.
--
--   See <http://www.astro.wisc.edu/~dolan/constellations/constellation_list.html>.
newtype ConLong = ConLong { fromConLong :: T.Text }
  deriving (Eq)

{-
-- | There is no validation done on the input.
instance IsString ConLong where
  fromString = ConLong . T.pack
-}

-- | Short form for constellation names (e.g. UMa for Ursa Major).
--
--   See <http://www.astro.wisc.edu/~dolan/constellations/constellation_list.html>.
newtype ConShort = ConShort { fromConShort :: T.Text }
  deriving newtype (DBEq, DBOrd)
  deriving (Eq, Ord)

{-
-- | Warning: there is no validation done on the input.
instance IsString ConShort where
  fromString = ConShort . T.pack
-}

toConShort :: T.Text -> Maybe ConShort
toConShort k =
  let out = ConShort k
  in const out `fmap` lookup out constellationMap

instance DBType ConShort where
  typeInformation = parseTypeInformation fromCS toCS typeInformation
    where
      fromCS t = case toConShort t of
        Just v -> Right v
        _ -> Left ("Unknown Constellation (short): '" <> T.unpack t <> "'")

      toCS = fromConShort


-- map from short to long form
-- See http://www.astro.wisc.edu/~dolan/constellations/constellation_list.html
--     http://www.astro.wisc.edu/~dolan/constellations/constellations.html
constellationMap :: [(ConShort, ConLong)]
constellationMap = 
 [ (ConShort "And", ConLong "Andromeda")
 , (ConShort "Ant", ConLong "Antlia")
 , (ConShort "Aps", ConLong "Apus")
 , (ConShort "Aqr", ConLong "Aquarius")
 , (ConShort "Aql", ConLong "Aquila")
 , (ConShort "Ara", ConLong "Ara")
 , (ConShort "Ari", ConLong "Aries")
 , (ConShort "Aur", ConLong "Auriga")
 , (ConShort "Boo", ConLong "Boötes")
 , (ConShort "Cae", ConLong "Caelum")
 , (ConShort "Cam", ConLong "Camelopardalis")
 , (ConShort "Cnc", ConLong "Cancer")
 , (ConShort "CVn", ConLong "Canes Venatici")
 , (ConShort "CMa", ConLong "Canis Major")
 , (ConShort "CMi", ConLong "Canis Minor")
 , (ConShort "Cap", ConLong "Capricornus")
 , (ConShort "Car", ConLong "Carina")
 , (ConShort "Cas", ConLong "Cassiopeia")
 , (ConShort "Cen", ConLong "Centaurus")
 , (ConShort "Cep", ConLong "Cepheus")
 , (ConShort "Cet", ConLong "Cetus")
 , (ConShort "Cha", ConLong "Chamaeleon")
 , (ConShort "Cir", ConLong "Circinus")
 , (ConShort "Col", ConLong "Columba")
 , (ConShort "Com", ConLong "Coma Berenices")
 , (ConShort "CrA", ConLong "Corona Austrina")
 , (ConShort "CrB", ConLong "Corona Borealis")
 , (ConShort "Crv", ConLong "Corvus")
 , (ConShort "Crt", ConLong "Crater")
 , (ConShort "Cru", ConLong "Crux")
 , (ConShort "Cyg", ConLong "Cygnus")
 , (ConShort "Del", ConLong "Delphinus")
 , (ConShort "Dor", ConLong "Dorado")
 , (ConShort "Dra", ConLong "Draco")
 , (ConShort "Equ", ConLong "Equuleus")
 , (ConShort "Eri", ConLong "Eridanus")
 , (ConShort "For", ConLong "Fornax")
 , (ConShort "Gem", ConLong "Gemini")
 , (ConShort "Gru", ConLong "Grus")
 , (ConShort "Her", ConLong "Hercules")
 , (ConShort "Hor", ConLong "Horologium")
 , (ConShort "Hya", ConLong "Hydra")
 , (ConShort "Hyi", ConLong "Hydrus")
 , (ConShort "Ind", ConLong "Indus")
 , (ConShort "Lac", ConLong "Lacerta")
 , (ConShort "Leo", ConLong "Leo")
 , (ConShort "LMi", ConLong "Leo Minor")
 , (ConShort "Lep", ConLong "Lepus")
 , (ConShort "Lib", ConLong "Libra")
 , (ConShort "Lup", ConLong "Lupus")
 , (ConShort "Lyn", ConLong "Lynx")
 , (ConShort "Lyr", ConLong "Lyra")
 , (ConShort "Men", ConLong "Mensa")
 , (ConShort "Mic", ConLong "Microscopium")
 , (ConShort "Mon", ConLong "Monoceros")
 , (ConShort "Mus", ConLong "Musca")
 , (ConShort "Nor", ConLong "Norma")
 , (ConShort "Oct", ConLong "Octans")
 , (ConShort "Oph", ConLong "Ophiuchus")
 , (ConShort "Ori", ConLong "Orion")
 , (ConShort "Pav", ConLong "Pavo")
 , (ConShort "Peg", ConLong "Pegasus")
 , (ConShort "Per", ConLong "Perseus")
 , (ConShort "Phe", ConLong "Phoenix")
 , (ConShort "Pic", ConLong "Pictor")
 , (ConShort "Psc", ConLong "Pisces")
 , (ConShort "PsA", ConLong "Piscis Austrinus")
 , (ConShort "Pup", ConLong "Puppis")
 , (ConShort "Pyx", ConLong "Pyxis")
 , (ConShort "Ret", ConLong "Reticulum")
 , (ConShort "Sge", ConLong "Sagitta")
 , (ConShort "Sgr", ConLong "Sagittarius")
 , (ConShort "Sco", ConLong "Scorpius")
 , (ConShort "Scl", ConLong "Sculptor")
 , (ConShort "Sct", ConLong "Scutum")
 , (ConShort "Ser", ConLong "Serpens")
 , (ConShort "Sex", ConLong "Sextans")
 , (ConShort "Tau", ConLong "Taurus")
 , (ConShort "Tel", ConLong "Telescopium")
 , (ConShort "Tri", ConLong "Triangulum")
 , (ConShort "TrA", ConLong "Triangulum Australe")
 , (ConShort "Tuc", ConLong "Tucana")
 , (ConShort "UMa", ConLong "Ursa Major")
 , (ConShort "UMi", ConLong "Ursa Minor")
 , (ConShort "Vel", ConLong "Vela")
 , (ConShort "Vir", ConLong "Virgo")
 , (ConShort "Vol", ConLong "Volans")
 , (ConShort "Vul", ConLong "Vulpecula")
 ]


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
  deriving (DBEq, Eq)

tooTime :: TOORequest -> TOORequestTime
tooTime = trType

fromTOORequest :: TOORequest -> T.Text
fromTOORequest = trValue

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
      --       although ObsId 24755 has >=40
      mlbl =
        if too `elem` [">30", ">=30", ">40", ">=40"]
        then Just Slow
        else case toksHyphen of
          [Just _, Just highVal] -> Just (ladder highVal)
          _ -> Nothing

  in case mlbl of
    Just lbl -> Just (TR lbl too)
    _ -> Nothing


instance DBType TOORequest where
  typeInformation = parseTypeInformation fromTR toTR typeInformation
    where
      fromTR t = case toTOORequest t of
        Just v -> Right v
        _ -> Left ("Unknown TOO request: '" <> T.unpack t <> "'")

      toTR = fromTOORequest

newtype SimbadMatchKey = SimbadMatchKey { fromKey :: Int64 }
  deriving newtype (DBEq, DBType, Eq)
  
newtype SimbadNoMatchKey = SimbadNoMatchKey { fromKey :: Int64 }
  deriving newtype (DBEq, DBType, Eq)
  
newtype SimbadInfoKey = SimbadInfoKey { fromKey :: Int64 }
  deriving newtype (DBEq, DBType, Eq)

-- | Indicates that there is a SIMBAD match for the
--   target name.
--
data SimbadMatch f = SimbadMatch {
    smmId :: Column f SimbadMatchKey
    , smmTarget :: Column f TargetName   -- ^ target name
    , smmSearchTerm :: Column f T.Text   -- ^ value used for the simbad search
    , smmInfo :: Column f SimbadInfoKey
    , smmLastChecked :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

simbadMatchSchema :: TableSchema (SimbadMatch Name)
simbadMatchSchema = TableSchema
  { name = "SimbadMatch"
  , columns = SimbadMatch
    { smmId = "id"
    , smmTarget = "smmTarget"
    , smmSearchTerm = "smmSearchTerm"
    , smmInfo = "smmInfo"
    , smmLastChecked = "smmLastChecked"
    }
  }

-- | This fakes the id field, which must be replaced by smExpr before use.
--
toSM ::
  TargetName
  -> T.Text
  -> SimbadInfoKey
  -> UTCTime
  -> SimbadMatch Result
toSM tgt sterm info checked =
  SimbadMatch { smmId = undefined
              , smmTarget = tgt
              , smmSearchTerm = sterm
              , smmInfo = info
              , smmLastChecked = checked
              }

-- | Convert a SimbadMatch structure to an expression, creating a
--   *new* smmId field.
--
smExpr :: SimbadMatch Result -> SimbadMatch Expr
smExpr orig =
  SimbadMatch { smmId = unsafeCoerceExpr (nextval "SimbadMatch_id_seq")
              , smmTarget = lit (smmTarget orig)
              , smmSearchTerm = lit (smmSearchTerm orig)
              , smmInfo = lit (smmInfo orig)
              , smmLastChecked = lit (smmLastChecked orig)
              }

-- | Indicates that there is no SIMBAD match for the
--   target name.
--
data SimbadNoMatch f = SimbadNoMatch {
    smnId :: Column f SimbadNoMatchKey
    , smnTarget :: Column f TargetName   -- ^ target name
    , smnSearchTerm :: Column f T.Text   -- ^ value used for the simbad search
    , smnLastChecked :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

simbadNoMatchSchema :: TableSchema (SimbadNoMatch Name)
simbadNoMatchSchema = TableSchema
  { name = "SimbadNoMatch"
  , columns = SimbadNoMatch
    { smnId = "id"
    , smnTarget = "smnTarget"
    , smnSearchTerm = "smnSearchTerm"
    , smnLastChecked = "smnLastChecked"
    }
  }

-- | This fakes the id field, which must be replaced by snmExpr before use.
--
toSNM ::
  TargetName
  -> T.Text
  -> UTCTime
  -> SimbadNoMatch Result
toSNM tgt sterm checked =
  SimbadNoMatch { smnId = undefined
                , smnTarget = tgt
                , smnSearchTerm = sterm
                , smnLastChecked = checked
                }

-- | Convert a SimbadNoMatch structure to an expression, creating a
--   *new* smnId field.
--
snmExpr :: SimbadNoMatch Result -> SimbadNoMatch Expr
snmExpr orig =
  SimbadNoMatch { smnId = unsafeCoerceExpr (nextval "SimbadNoMatch_id_seq")
                , smnTarget = lit (smnTarget orig)
                , smnSearchTerm = lit (smnSearchTerm orig)
                , smnLastChecked = lit (smnLastChecked orig)
                }

{-
-- | What does SIMBAD know about this object?
--
--   At present `SimbadSearch` is a union of `SimbadNoMatch` and
--   `SimbadMatch`.
--
type SimbadSearch = Either SimbadNoMatch SimbadMatch
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
  deriving newtype (DBEq, DBType, Eq)


-- TODO: need a converter to a URL fragment - e.g. '?' needs protecting!
--       actually, need to check this, since I have seen it work

-- | This constructor ensures that the type is three letters
--   or less.
toSimbadType :: T.Text -> Maybe SimbadType
toSimbadType s | T.length s > 0 && T.length s < 4 = Just (SimbadType s)
               | otherwise = Nothing

noSimbadType :: SimbadType
noSimbadType = SimbadType "000"


-- | TODO: Should this be an enumeration?
--
--   This is the "long form" of a SIMBAD type.
newtype SIMCategory = SIMCategory { fromSIMCategory :: T.Text }
  deriving newtype (DBEq, DBType, Eq)

-- | Identifies those sources for which we have no SIMBAD information.
noSimbadLabel :: SIMCategory
noSimbadLabel = SIMCategory "Unidentified"


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
data SimbadInfo f = SimbadInfo {
  smiId :: Column f SimbadInfoKey
  , smiName :: Column f TargetName
    -- ^ the primary identifier for the object
  , smiType3 :: Column f SimbadType
    -- ^ short form identifier for siType
  , smiType :: Column f SIMCategory
    -- ^ the primary type of the object (long form)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

simbadInfoSchema :: TableSchema (SimbadInfo Name)
simbadInfoSchema = TableSchema
  { name = "SimbadInfo"
  , columns = SimbadInfo
    { smiId = "id"
    , smiName = "smiName"
    , smiType3 = "smiType3"
    , smiType = "smiType"
    }
  }
    
-- | This fakes the id field, which must be replaced by siExpr before use.
--
toSI ::
  TargetName
  -> SimbadType
  -> SIMCategory
  -> SimbadInfo Result
toSI sName sType3 sType =
  SimbadInfo { smiId = undefined
             , smiName = sName
             , smiType3 = sType3
             , smiType = sType
             }

-- | Convert a SimbadInfo structure to an expression, creating a
--   *new* smiId field.
--
siExpr :: SimbadInfo Result -> SimbadInfo Expr
siExpr orig =
  SimbadInfo { smiId = unsafeCoerceExpr (nextval "SimbadInfo_iq_seq")
             , smiName = lit (smiName orig)
             , smiType3 = lit (smiType3 orig)
             , smiType = lit (smiType orig)
             }


{-X
-- | Do we consider the two names to be the same?
--
--   Strip out all spaces; convert to lower case.
--
--   We do not use a simple edit distance comparison here
--   since we do not want to equate 3C292 and 3C232.
--
similarName :: SimbadInfo f -> TargetName -> Bool
similarName SimbadInfo{..} target =
  let conv = T.filter (not . isSpace) . T.toLower . fromTargetName
  in ((==) `on` conv) target smiName

X-}

{-X
-- | The short and long forms of the type information from SIMBAD.
type SimbadTypeInfo = (SimbadType, SIMCategory)
X-}

{-
-- | The Simbad name is used for ordering.
--
--   Why do we need an Ord constraint?
instance Ord SimbadInfo where
  compare = compare `on` smiName
-}


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


-- Not using lenses for now.
--
{-
_1 :: (a, b, c) -> a
_1 (f1, _, _) = f1
-}

_2 :: (a, b, c) -> b
_2 (_, f2, _) = f2

{-
_3 :: (a, b, c) -> c
_3 (_, _, f3) = f3
-}
