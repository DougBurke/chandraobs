{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}


-- | Set up some types for representing Chandra observations. The
--   routines here are occasionally only tangentially-related to
--   types, per se.
--
--   I have taken out derived @Show@ instances for many types
--   to catch cases where I was relying on them for serialization
--   when I should not have been.
--
--   Gien the way the code has ended up below, perhaps I should be
--   using some form of a lens library.
--
module Types where

-- Since Template Haskell is being used to create a number of
-- symbols, I have decided to export everything from this module
-- rather than try to track and document it.

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Lazy as LT

import qualified Text.Blaze.Html5 as H

import Control.Arrow (first)
import Control.Monad.Logger (NoLoggingT)

#if MIN_VERSION_base(4, 7, 0)
import Data.Bits (Bits(..), FiniteBits(..))
#else
import Data.Bits (Bits(..))
#endif

import Data.Function (on)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.String (IsString(..))
import Data.Time (UTCTime, addUTCTime, formatTime, readTime)

-- I am not convinced I'm adding the PersistField values sensibly
import Database.Groundhog.Core
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)
import Database.Groundhog.TH
import Database.Groundhog.Postgresql

import Network.HTTP.Types.URI (renderSimpleQuery)

import System.Locale (defaultTimeLocale)

import Text.Printf

import Web.Scotty (Parsable(..))

-- | Isn't this in base now?
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- | The instrument being used.
data Instrument = ACISS | ACISI | HRCI | HRCS 
  deriving (Eq, Show, Read)

fromInstrument :: Instrument -> String
fromInstrument ACISI = "ACIS-I"
fromInstrument ACISS = "ACIS-S"
fromInstrument HRCI  = "HRC-I"
fromInstrument HRCS  = "HRC-S"

toInstrument :: String -> Maybe Instrument
toInstrument "ACIS-I" = Just ACISI
toInstrument "ACIS-S" = Just ACISS
toInstrument "HRC-I"  = Just HRCI
toInstrument "HRC-S"  = Just HRCS

toInstrument "ACISI" = Just ACISI
toInstrument "ACISS" = Just ACISS
toInstrument "HRCI"  = Just HRCI
toInstrument "HRCS"  = Just HRCS

toInstrument _ = Nothing

instance Parsable Instrument where
  parseParam t = 
    let tstr = LT.unpack t
        emsg = "Invalid instrument name: " <> t
    in maybe (Left emsg) Right (toInstrument tstr)

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
data Grating = LETG | HETG | NONE 
  deriving (Eq, Show, Read)

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
  -- deriving (Eq, Ord, Show)
  deriving (Eq, Ord)

instance H.ToMarkup ObsIdVal where
  toMarkup = H.toMarkup . fromObsId

instance H.ToValue ObsIdVal where
  toValue = H.toValue . fromObsId

-- | Represent an entry in the schedule; this is
--   a "catch-all" type that is used as I play around with the
--   database.
--
type Record = Either NonScienceObs ScienceObs

-- hacks for quickly converting old code

recordSequence :: Record -> Maybe Sequence
recordSequence = either (const Nothing) (Just . soSequence)

recordObsId :: Record -> ObsIdVal
recordObsId = either nsObsId soObsId

recordTarget :: Record -> String
recordTarget = either nsTarget soTarget

recordStartTime :: Record -> ChandraTime
recordStartTime = either nsStartTime soStartTime

-- Use the actual time if we have it, otherwise the approved time
recordTime :: Record -> TimeKS
recordTime = either nsTime (\ScienceObs{..} -> fromMaybe soApprovedTime soObservedTime)

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

-- Needed for readHelper, used by the PrimitivePersistField instance
instance Read ChandraTime where
  readsPrec i = \s -> let xs = readsPrec i s
                      in map (first ChandraTime) xs

-- TODO: validate ra as 0 to 360
instance Read RA where
  readsPrec i = \s -> let xs = readsPrec i s
                      in map (first RA) xs

-- TODO: validate dec as -90 to 90
instance Read Dec where
  readsPrec i = \s -> let xs = readsPrec i s
                      in map (first Dec) xs

-- TODO: validate time as >= 0
instance Read TimeKS where
  readsPrec i = \s -> let xs = readsPrec i s
                      in map (first TimeKS) xs

instance Read ObsIdVal where
  readsPrec i = \s -> let xs = readsPrec i s
                      in map (first ObsIdVal) xs

instance Read Sequence where
  readsPrec i = \s -> let xs = readsPrec i s
                      in map (first Sequence) xs

instance Read PropNum where
  readsPrec i = \s -> let xs = readsPrec i s
                      in map (first PropNum) xs

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
newtype Sequence = Sequence { _unSequence :: Int } 
   -- deriving (Eq, Ord, Show)
   deriving (Eq, Ord)

instance H.ToMarkup Sequence where
  toMarkup = H.toMarkup . _unSequence

instance H.ToValue Sequence where
  toValue = H.toValue . _unSequence

-- | Represent a Chandra proposal number.
newtype PropNum = PropNum { _unPropNum :: Int } 
   -- deriving (Eq, Ord, Show)
   deriving (Eq, Ord)

-- | Limited validation of the input (currently only
--   enforces a positive value).
toPropNumStr :: String -> Maybe PropNum
toPropNumStr s = do
  p <- maybeRead s
  if p > 0 then return (PropNum p) else Nothing

instance Parsable PropNum where
  parseParam t = 
    let tstr = LT.unpack t
        emsg = "Invalid proposal number: " <> t
    in maybe (Left emsg) Right (toPropNumStr tstr)

instance H.ToMarkup PropNum where
  toMarkup = H.toMarkup . _unPropNum

instance H.ToValue PropNum where
  toValue = H.toValue . _unPropNum

-- | Simple wrappers to avoid mixing up RA and Dec.

newtype RA = RA { _unRA :: Double } 
  -- deriving (Eq, Show)  
  deriving Eq

newtype Dec = Dec { _unDec :: Double } 
  -- deriving (Eq, Show)  
  deriving Eq

showRA :: RA -> String
showRA (RA ra) = 
  let rah = ra / 15.0
      h, m :: Int
      r1, r2 :: Double
      (h, r1) = properFraction rah
      ram = r1 * 60
      (m, r2) = properFraction ram
      s = r2 * 60
  in printf "%dh %dm %.1fs" h m s

showDec :: Dec -> String
showDec (Dec dec) = 
  let dabs = abs dec
      d, m :: Int
      r1, r2 :: Double
      (d, r1) = properFraction dabs
      dm = r1 * 60
      (m, r2) = properFraction dm
      s = r2 * 60
      c = if dec < 0 then '-' else '+'
  in printf "%c%dd %d' %.1f\"" c d m s

instance H.ToMarkup RA where
  toMarkup = H.toMarkup . showRA

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
   }

-- | Represent a value in kiloseconds.
newtype TimeKS = TimeKS { _toS :: Double } 
  -- deriving (Eq, Ord, Show)
  deriving (Eq, Ord)

-- | Convert a more "friendly" exposure time value.
--
--   As the minimum time appears to be 0.1 ks we do not
--   have to deal with sub minute values, but include
--   just in case. Assume that max is ~ 100ks, which is
--   ~ 28 hours, so need to deal with days.
--
--   The rounding may be a bit surprising, since
--   1 day + 1 minute will get reported as
--   "1 day 1 hour".
--
showExpTime :: TimeKS -> String
showExpTime (TimeKS tks) = 
  let s = tks * 1000
      m = s / 60
      h = m / 60

  in if s < 3600
     then showUnits s 60 "minute" "second"
     else if h < 24
          then showUnits m 60 "hour" "minute"
          else showUnits h 24 "day" "hour"

showExp :: Record -> H.Html
showExp = H.toHtml . showExpTime . recordTime

-- | Make a nice readable value; ie
--   "x unit1 y unit2"
--
--   This returns "" if the value is 0. At the moment this
--   is a feature, but it may change.
showUnits :: 
  Double      -- value in units of unit2
  -> Int      -- scale value
  -> String   -- unit1: singular unit (scale * unit2)
  -> String   -- unit2: unit 
  -> String
showUnits v s u1 u2 = 
  let v1 = ceiling v :: Int -- round up
      (a, b) = v1 `divMod` s

      units 0 _ = ""
      units 1 u = "1 " ++ u
      units x u = show x ++ " " ++ u ++ "s"

      astr = units a u1
      bstr = units b u2

      sep = if null astr || null bstr then "" else " and "

  in astr ++ sep ++ bstr

-- do we want this to be in a nice readable value or in ks?
instance H.ToMarkup TimeKS where
  toMarkup = H.toMarkup . _toS

instance H.ToValue TimeKS where
  toValue = H.toValue . _toS

-- | A scheduled observation (may be in the past, present, or future).
--
--   The information is taken from <http://cxc.cfa.harvard.edu/target_lists/stscheds/>,
--   and contains information we store elsewhere.
data ScheduleItem = ScheduleItem {
    siObsId :: ObsIdVal
    , siScienceObs :: Bool
    , siStart :: ChandraTime
    , siEnd :: ChandraTime     -- approx end time
    , siDuration :: TimeKS
    }
  -- deriving (Eq, Show)
  deriving Eq

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
  }
  -- deriving (Eq, Show)
  deriving Eq

-- | This is for debug purposes.
instance Show NonScienceObs where
  show NonScienceObs{..} = 
    concat [ "CAL: ", show (fromObsId nsObsId)
           , " for ", show (_toS nsTime)
           , " ks at ", showCTime nsStartTime
           ]

-- | Is a chip on, off, or optional.
--
--   How many optional chips are allowed?
data ChipStatus = ChipOn | ChipOff | ChipOpt1 | ChipOpt2 | ChipOpt3 | ChipOpt4 | ChipOpt5
  deriving Eq

toChipStatus :: String -> Maybe ChipStatus
toChipStatus s | s == "Y"  = Just ChipOn
               | s == "N"  = Just ChipOff
               | s == "O1" = Just ChipOpt1
               | s == "O2" = Just ChipOpt2
               | s == "O3" = Just ChipOpt3
               | s == "O4" = Just ChipOpt4
               | s == "O5" = Just ChipOpt5
               | otherwise = Nothing

fromChipStatus :: ChipStatus -> String
fromChipStatus ChipOn = "Y"
fromChipStatus ChipOff = "N"
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
  , soStatus :: String    -- use an enumeration
  , soObsId :: ObsIdVal
  , soTarget :: String
  , soStartTime :: ChandraTime
  , soApprovedTime :: TimeKS
  , soObservedTime :: Maybe TimeKS

  , soTimeCritical :: Constraint
  , soMonitor :: Constraint
  , soConstrained :: Constraint

  , soInstrument :: Instrument
  , soGrating :: Grating
  , soDetector :: Maybe String   -- this is only available for archived obs
  , soDataMode :: Maybe String -- use an enumeration; not available for HRC

  -- these are meaningless for HRC observations; in this case ChipStatus is set to ?
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

  -- , soJointWith :: [(String, TimeKS)] -- could use an enumeration
  -- UGH: hard coding the joint missions
  , soJointWith :: Maybe String
  , soJointHST :: Maybe TimeKS
  , soJointNOAO :: Maybe TimeKS
  , soJointNRAO :: Maybe TimeKS
  , soJointRXTE :: Maybe TimeKS
  , soJointSPITZER :: Maybe TimeKS
  , soJointSUZAKU :: Maybe TimeKS
  , soJointXMM :: Maybe TimeKS
  , soJointSWIFT :: Maybe TimeKS
  , soJointNUSTAR :: Maybe TimeKS

  , soTOO :: Maybe String -- contains values like "0-4" "4-15"; presumably #days for turn around
  , soRA :: RA
  , soDec :: Dec
  , soConstellation :: ConShort -- name of the constellation the observation is in  
  , soRoll :: Double
  , soSubArrayStart :: Maybe Int
  , soSubArraySize :: Maybe Int
  } 
  -- deriving (Eq, Show)
  deriving Eq
    -- deriving instance Show ScienceObs

-- | Short form for constellation names (e.g. UMa for Ursa Major).
--
--   See <http://www.astro.wisc.edu/~dolan/constellations/constellation_list.html>.
newtype ConShort = ConShort { fromConShort :: String }
  deriving Eq

-- | There is no validation done on the input.
instance IsString ConShort where
  fromString = ConShort

toConShort :: String -> Maybe ConShort
toConShort k =
  let out = ConShort k
  in const out `fmap` lookup out constellationMap

instance Parsable ConShort where
  parseParam t = 
    let tstr = LT.unpack t
        emsg = "Invalid Constellation name: " <> t
    in maybe (Left emsg) Right (toConShort tstr)

-- | Long form for constellation names (e.g. Ursa Major),
--   although there's no validation that the string is valid.
--
--   See <http://www.astro.wisc.edu/~dolan/constellations/constellation_list.html>.
newtype ConLong = ConLong { fromConLong :: String }
  deriving Eq

-- | There is no validation done on the input.
instance IsString ConLong where
  fromString = ConLong

-- | This can fail if either the input is invalid or the mapping table
--   is missing something.
getConstellationName :: ConShort -> Maybe ConLong
getConstellationName = flip lookup constellationMap

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

  

-- | Return the list of other observations that are contemporaneous with
--   this one.
--   
--   It turns out that you can have the soJointWIth field set but
--   no corresponding soJoint field. See ObsId 15662.
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
    concat [ "Science: ", show (fromObsId soObsId)
           , " ", soTarget
           , " with "
           , show soInstrument, "+", show soGrating
           , " approved for ", show (_toS soApprovedTime)
           , " ks at ", showCTime soStartTime
           ]

-- | Has the observation been archived? If so, we assume that the observational
--   parameters we care about are not going to change. This may turn out to be
--   a bad idea.
--
isArchived :: ScienceObs -> Bool
isArchived ScienceObs{..} = soStatus == "archived"

-- | Store information on a proposal, obtained from the OCAT.
data Proposal = Proposal {
  propNum :: PropNum
  -- , propSeqNum :: Sequence
  , propName :: String
  , propPI :: String
  , propCategory :: String
  , propType :: String -- could use an enumeration
  , propCycle :: String -- ditto, this is the proposal cycle, not the observing cycle
  }
  -- deriving (Eq, Show)
  deriving Eq

-- | Proposals are ordered by proposal number.
instance Ord Proposal where
  compare = compare `on` propNum

-- | This is for debug purposes.
instance Show Proposal where
  show Proposal{..} = 
    concat [ "Proposal: ", show (_unPropNum propNum)
           , " ", propName
           , " PI ", propPI
           ]

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

-- | This is the short form of the SIMBAD type.
--
newtype SimbadType = SimbadType { fromSimbadType :: String }
  deriving Eq

-- | This constructor ensures that the type is three letters
--   or less.
toSimbadType :: String -> Maybe SimbadType
toSimbadType s@(_:[]) = Just $ SimbadType s
toSimbadType s@(_:_:[]) = Just $ SimbadType s
toSimbadType s@(_:_:_:[]) = Just $ SimbadType s
toSimbadType _ = Nothing

instance Parsable SimbadType where
  parseParam t = 
    let tstr = LT.unpack t
        emsg = "Invalid Simbad type: " <> t
    in maybe (Left emsg) Right (toSimbadType tstr)

-- | Information on an object identifier, retrieved from SIMBAD.
--   At present only a very-limited amount of information is returned
--   and the structure isn't very Haskell-like.
--
--   TODO:
--
--    - it is ridiculous that siName/siType3/siType are stored
--      here, since they are really a separate record.
--
data SimbadInfo = SimbadInfo {
   siTarget :: String      -- ^ target name (presumed unique)
   , siSimilar :: Bool     -- ^ @True@ if @siName@ and @siTarget@ are similar enough
                           -- that they are considered the same (e.g. spaces and case
                           -- differences)
   , siName :: Maybe String      -- ^ the primary identifier for the object
   , siType3 :: Maybe SimbadType      -- ^ short form identifier for siType
   , siType :: Maybe String      -- ^ the primary type of the object (long form)
   , siRA :: Maybe RA
   , siDec :: Maybe Dec
   , siLastChecked :: UTCTime
  }
  deriving Eq

-- | The short and long forms of the type information from SIMBAD.
type SimbadTypeInfo = (SimbadType, String)

-- | The name of the target string is used for ordering, since it is
--   assumed that the name is unique.
instance Ord SimbadInfo where
  compare = compare `on` siTarget

-- | Return a link to the SIMBAD site (Strasbourg) for this object.
--
-- TODO: need to protect the link
toSIMBADLink :: String -> String
toSIMBADLink name = 
  let qry = [ ("Ident", B8.pack name)
            -- , ("NbIdent", "1")
            -- , ("Radius", "2")
            -- , ("Radius.unit", "arcmin")
            -- , ("submit", "submit id")
            ]

      qryB = renderSimpleQuery True qry

  in B8.unpack $ "http://simbad.harvard.edu/simbad/sim-id" <> qryB
      
-- * Groundhog instances
--
-- based on the Database.Groundhog.Instances code
--

readHelper :: Read a => PersistValue -> String -> a
readHelper s errMessage = case s of
  PersistString str -> readHelper' str
  PersistByteString str -> readHelper' (B8.unpack str)
  _ -> error $ "readHelper: " ++ errMessage
  where
    readHelper' str = case reads str of
      (a, _):_ -> a
      _        -> error $ "readHelper: " ++ errMessage

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

-- times

instance PersistField ChandraTime where
  persistName _ = "ChandraTime"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbTypePrimitive DbDayTime False Nothing Nothing  

instance PrimitivePersistField ChandraTime where
  toPrimitivePersistValue _ = PersistUTCTime . _toUTCTime
  fromPrimitivePersistValue _ (PersistUTCTime a) = ChandraTime a
  -- fromPrimitivePersistValue _ (PersistZonedTime (ZT a)) = zonedTimeToUTC a
  fromPrimitivePersistValue _ x = readHelper x ("Expected ChandraTime (UTCTime), received: " ++ show x)

-- double values

instance PersistField RA where
  persistName _ = "RA"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbTypePrimitive DbReal False Nothing Nothing

instance PersistField Dec where
  persistName _ = "Dec"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbTypePrimitive DbReal False Nothing Nothing

instance PersistField TimeKS where
  persistName _ = "TimeKS"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbTypePrimitive DbReal False Nothing Nothing

instance PrimitivePersistField RA where
  toPrimitivePersistValue _ = PersistDouble . _unRA
  fromPrimitivePersistValue _ (PersistDouble a) = RA a
  fromPrimitivePersistValue _ (PersistInt64 a) = RA $ fromIntegral a
  fromPrimitivePersistValue _ x = readHelper x ("Expected RA (double), received: " ++ show x)

instance PrimitivePersistField Dec where
  toPrimitivePersistValue _ = PersistDouble . _unDec
  fromPrimitivePersistValue _ (PersistDouble a) = Dec a
  fromPrimitivePersistValue _ (PersistInt64 a) = Dec $ fromIntegral a
  fromPrimitivePersistValue _ x = readHelper x ("Expected Dec (double), received: " ++ show x)

instance PrimitivePersistField TimeKS where
  toPrimitivePersistValue _ = PersistDouble . _toS
  fromPrimitivePersistValue _ (PersistDouble a) = TimeKS a
  fromPrimitivePersistValue _ (PersistInt64 a) = TimeKS $ fromIntegral a
  fromPrimitivePersistValue _ x = readHelper x ("Expected TimeKS (double), received: " ++ show x)

-- integer values

instance PersistField ObsIdVal where
  persistName _ = "ObsIdVal"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType a = DbTypePrimitive (if finiteBitSize a == 32 then DbInt32 else DbInt64) False Nothing Nothing where
#if !MIN_VERSION_base(4, 7, 0)
    finiteBitSize = bitSize
#endif

instance PersistField PropNum where
  persistName _ = "PropNum"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType a = DbTypePrimitive (if finiteBitSize a == 32 then DbInt32 else DbInt64) False Nothing Nothing where
#if !MIN_VERSION_base(4, 7, 0)
    finiteBitSize = bitSize
#endif

instance PersistField Sequence where
  persistName _ = "Sequence"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType a = DbTypePrimitive (if finiteBitSize a == 32 then DbInt32 else DbInt64) False Nothing Nothing where
#if !MIN_VERSION_base(4, 7, 0)
    finiteBitSize = bitSize
#endif

instance PrimitivePersistField PropNum where
  toPrimitivePersistValue _ = PersistInt64 . fromIntegral . _unPropNum
  fromPrimitivePersistValue _ (PersistInt64 a) = PropNum $ fromIntegral a
  fromPrimitivePersistValue _ (PersistDouble a) = PropNum $ truncate a
  fromPrimitivePersistValue _ x = readHelper x ("Expected PropNum (Integer), received: " ++ show x)

instance PrimitivePersistField Sequence where
  toPrimitivePersistValue _ = PersistInt64 . fromIntegral . _unSequence
  fromPrimitivePersistValue _ (PersistInt64 a) = Sequence $ fromIntegral a
  fromPrimitivePersistValue _ (PersistDouble a) = Sequence $ truncate a
  fromPrimitivePersistValue _ x = readHelper x ("Expected Sequence (Integer), received: " ++ show x)

instance PrimitivePersistField ObsIdVal where
  toPrimitivePersistValue _ = PersistInt64 . fromIntegral . fromObsId
  fromPrimitivePersistValue _ (PersistInt64 a) = ObsIdVal $ fromIntegral a
  fromPrimitivePersistValue _ (PersistDouble a) = ObsIdVal $ truncate a
  fromPrimitivePersistValue _ x = readHelper x ("Expected ObsIdVal (Integer), received: " ++ show x)

-- enumeration/string-like types

instance PersistField Instrument where
  persistName _ = "Instrument"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbTypePrimitive DbString False Nothing Nothing

instance PersistField Grating where
  persistName _ = "Grating"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbTypePrimitive DbString False Nothing Nothing

-- can we force a size on the string?
instance PersistField ChipStatus where
  persistName _ = "ChipStatus"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbTypePrimitive DbString False Nothing Nothing

-- can we force a size on the string?
instance PersistField SimbadType where
  persistName _ = "SimbadType"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbTypePrimitive DbString False Nothing Nothing

instance PersistField Constraint where
  persistName _ = "Constraint"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbTypePrimitive DbString False Nothing Nothing

instance PersistField ConShort where
  persistName _ = "ConShort"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ = DbTypePrimitive DbString False Nothing Nothing

instance PrimitivePersistField Instrument where
  {- The Groundhog tutorial [1] had the following, but this fails to
     compile with
         Could not deduce (PrimitivePersistField String)
           arising from a use of ‘toPrimitivePersistValue’

     so trying something a bit different

     [1] https://www.fpcomplete.com/user/lykahb/groundhog

  toPrimitivePersistValue p a = toPrimitivePersistValue p $ show a
  fromPrimitivePersistValue p x = read $ fromPrimitivePersistValue p x
  -}
  toPrimitivePersistValue _ = PersistString . show
  fromPrimitivePersistValue _ (PersistString s) = read s
  -- fromPrimitivePersistValue _ (PersistByteString bs) = read $ B8.unpack bs
  fromPrimitivePersistValue _ x = readHelper x ("Expected Instrument (String), received: " ++ show x)

instance PrimitivePersistField Grating where
  {-
  toPrimitivePersistValue p a = toPrimitivePersistValue p $ show a
  fromPrimitivePersistValue p x = read $ fromPrimitivePersistValue p x
  -}
  toPrimitivePersistValue _ = PersistString . show
  fromPrimitivePersistValue _ (PersistString s) = read s
  -- fromPrimitivePersistValue _ (PersistByteString bs) = read $ B8.unpack bs
  fromPrimitivePersistValue _ x = readHelper x ("Expected Instrument (String), received: " ++ show x)

instance PrimitivePersistField ChipStatus where
  {-
  toPrimitivePersistValue p a = toPrimitivePersistValue p $ show a
  fromPrimitivePersistValue p x = read $ fromPrimitivePersistValue p x
  -}
  toPrimitivePersistValue _ = PersistString . fromChipStatus
  fromPrimitivePersistValue _ (PersistString s) = fromMaybe (error ("Unexpected chip status: " ++ s)) $ toChipStatus s
  -- fromPrimitivePersistValue _ (PersistByteString bs) = read $ B8.unpack bs
  fromPrimitivePersistValue _ x = error $ "Expected ChipStatus (String), received: " ++ show x

instance PrimitivePersistField SimbadType where
  {-
  toPrimitivePersistValue p a = toPrimitivePersistValue p $ show a
  fromPrimitivePersistValue p x = read $ fromPrimitivePersistValue p x
  -}
  toPrimitivePersistValue _ = PersistString . fromSimbadType
  fromPrimitivePersistValue _ (PersistString s) = fromMaybe (error ("Unexpected Simbad Type: " ++ s)) $ toSimbadType s
  -- fromPrimitivePersistValue _ (PersistByteString bs) = read $ B8.unpack bs
  fromPrimitivePersistValue _ x = error $ "Expected SimbadType (String), received: " ++ show x

instance PrimitivePersistField Constraint where
  {-
  toPrimitivePersistValue p a = toPrimitivePersistValue p $ show a
  fromPrimitivePersistValue p x = read $ fromPrimitivePersistValue p x
  -}
  toPrimitivePersistValue _ a = PersistString (fromConstraint a : [])

  fromPrimitivePersistValue _ (PersistString s) = case s of
    [c] -> fromMaybe (error ("Unexpected constraint value: " ++ s)) $ toConstraint c
    _ -> error ("Unexpected constraint value: " ++ s)

  -- fromPrimitivePersistValue _ (PersistByteString bs) = read $ B8.unpack bs
  fromPrimitivePersistValue _ x = error $ "Expected Constraint (1 character String), received: " ++ show x

instance PrimitivePersistField ConShort where
  {-
  toPrimitivePersistValue p a = toPrimitivePersistValue p $ show a
  fromPrimitivePersistValue p x = read $ fromPrimitivePersistValue p x
  -}
  toPrimitivePersistValue _ = PersistString . fromConShort
  fromPrimitivePersistValue _ (PersistString s) = fromMaybe (error ("Unexpected Constellation type: " ++ s)) $ toConShort s
  -- fromPrimitivePersistValue _ (PersistByteString bs) = read $ B8.unpack bs
  fromPrimitivePersistValue _ x = error $ "Expected ConShort (String), received: " ++ show x

-- needed for persistent integer types

instance Bits PropNum where
  (PropNum a) .&. (PropNum b) = PropNum (a .&. b)
  (PropNum a) .|. (PropNum b) = PropNum (a .|. b)
  (PropNum a) `xor` (PropNum b) = PropNum (a `xor` b)
  complement = PropNum . complement . _unPropNum
  shift a i = PropNum $ shift (_unPropNum a) i
  rotate a i = PropNum $ rotate (_unPropNum a) i
#if MIN_VERSION_base(4, 7, 0)
  bitSize = fromMaybe (error "invalid bitsize") . bitSizeMaybe
  bitSizeMaybe = bitSizeMaybe . _unPropNum
#else
  bitSize = bitSize . _unPropNum
#endif
  isSigned = isSigned . _unPropNum
  testBit a i = testBit (_unPropNum a) i
  bit = PropNum . bit
  popCount = popCount . _unPropNum

instance Bits Sequence where
  (Sequence a) .&. (Sequence b) = Sequence (a .&. b)
  (Sequence a) .|. (Sequence b) = Sequence (a .|. b)
  (Sequence a) `xor` (Sequence b) = Sequence (a `xor` b)
  complement = Sequence . complement . _unSequence
  shift a i = Sequence $ shift (_unSequence a) i
  rotate a i = Sequence $ rotate (_unSequence a) i
#if MIN_VERSION_base(4, 7, 0)
  bitSize = fromMaybe (error "invalid bitsize") . bitSizeMaybe
  bitSizeMaybe = bitSizeMaybe . _unSequence
#else
  bitSize = bitSize . _unSequence
#endif
  isSigned = isSigned . _unSequence
  testBit a i = testBit (_unSequence a) i
  bit = Sequence . bit
  popCount = popCount . _unSequence

instance Bits ObsIdVal where
  (ObsIdVal a) .&. (ObsIdVal b) = ObsIdVal (a .&. b)
  (ObsIdVal a) .|. (ObsIdVal b) = ObsIdVal (a .|. b)
  (ObsIdVal a) `xor` (ObsIdVal b) = ObsIdVal (a `xor` b)
  complement = ObsIdVal . complement . fromObsId
  shift a i = ObsIdVal $ shift (fromObsId a) i
  rotate a i = ObsIdVal $ rotate (fromObsId a) i
#if MIN_VERSION_base(4, 7, 0)
  bitSize = fromMaybe (error "invalid bitsize") . bitSizeMaybe
  bitSizeMaybe = bitSizeMaybe . fromObsId
#else
  bitSize = bitSize . fromObsId
#endif
  isSigned = isSigned . fromObsId
  testBit a i = testBit (fromObsId a) i
  bit = ObsIdVal . bit
  popCount = popCount . fromObsId

#if MIN_VERSION_base(4, 7, 0)
instance FiniteBits PropNum where
  finiteBitSize = finiteBitSize . _unPropNum

instance FiniteBits Sequence where
  finiteBitSize = finiteBitSize . _unSequence

instance FiniteBits ObsIdVal where
  finiteBitSize = finiteBitSize . fromObsId
#endif

-- We do not take advantage of the database here (eg unique fields,
-- or relations between entities).
--
-- also, could save some code above by taking advantage of the
-- "primitive" option in the GroundHog TH
--
-- does the name field on the uniques entry need to be unique?
--
mkPersist defaultCodegenConfig [groundhog|
- entity: ScheduleItem
  constructors:
    - name: ScheduleItem
      uniques:
        - name: ScheduleitemObsIdConstraint
          fields: [siObsId]
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
- entity: SimbadInfo
  constructors:
    - name: SimbadInfo
      uniques:
        - name: SimbadInfoConstraint
          fields: [siTarget]
|]

handleMigration :: DbPersist Postgresql (NoLoggingT IO) ()
handleMigration =
  runMigration defaultMigrationLogger $ do
    migrate (undefined :: ScheduleItem)
    migrate (undefined :: ScienceObs)
    migrate (undefined :: NonScienceObs)
    migrate (undefined :: Proposal)
    migrate (undefined :: SimbadInfo)

