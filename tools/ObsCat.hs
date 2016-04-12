{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-

TODO:

*Database> jws
[Just "CXO-XMM",Just "NRAO+NOAO",Just "HST+XMM",Just "NRAO",Just "NuSTAR+NRAO",Just "Suzaku",Just "HST+NRAO",Just "Swift",Just "XMM+NRAO",Just "CXO-HST",Just "NOAO",Just "HST",Just "XMM",Just "CXO-Spitzer",Just "NuSTAR"]

For at least one CXO-HST case, the jointWith HST field is Nothing,
which seems wrong: ObsId 15642 (which has no jointWith fields)

I guess the CXO- prefix is messing things up. Nope, it's that -
as shown below - there is no information on the amount of
HST time. Which means that the soJointXXXX fields are not sufficient
to identify a joint mission.

I wonder what the following mean:

("MULTITEL","Y")
("MULTITEL_INT","0.0")
("MULTITEL_OBS","HST")


*Main> ocat <- makeObsCatQuery True (ObsIdVal 15642)
##### Map 1 START #####
("ACIS","Y")
("ALT_TRIG","0")
("APP_EXP","20.00")
("BEP_PACK","VF")
("CATEGORY","STARS AND WD")
("CHARGE_CYCLE","14")
("CONSTR","Y")
("CONSTR_RMK","Y")
("COUNT_RATE","2.6685633559375335")
("DATAMODE","VFAINT")
("DROPPED_CHIP_CNT","0")
("D_CYC","N")
("Dec","+26 42 19.90")
("EFFICIENT","Y")
("EPOCH","54864.58321")
("EST_CNT_RATE","0.0023")
("EVENT_COUNT","50079")
("EVFIL","N")
("EVFIL_LO","0.0")
("EVFIL_RA","0.0")
("EXP_TIME","18.77")
("F_TIME","0.0")
("GRAT","NONE")
("HRC","N")
("HST","0.0")
("I0","N")
("I1","N")
("I2","N")
("I3","N")
("INSTR","ACIS-S")
("JOINT","CXO-HST")
("MODE","TE")
("MONITOR","N")
("MULTITEL","Y")
("MULTITEL_INT","0.0")
("MULTITEL_OBS","HST")
("NOAO","0.0")
("NRAO","0.0")
("NUSTAR","0.0")
("OBJ","NONE")
("OBJ_TYPE","NO")
("OBSERVER","Wheatley")
("OBSID","15642")
("OBS_CYCLE","14")
("OC_COL","1")
("OC_ROW","1")
("OC_SUM","N")
("PEND","0.99")
("PERIOD","2.643898")
("PE_MARG","0.03")
("PHOTO","Y")
("PI_NAME","Ehrenreich")
("PROP_CYCLE","14")
("PROP_TITLE","Properties and dynamics of the upper atmosphere of the hot-Neptune GJ 436b")
("PR_NUM","14200978")
("PR_TIME","0.0")
("PSTART","0.9")
("PS_MARG","0.03")
("PUBLIC_AVAIL","2015-06-25 08:15:17")
("RA","11 42 11.40")
("RASTER","N")
("READOUT_DETECTOR","ACIS-7")
("ROLL","N")
("ROW_CNT","0")
("RXTE","0.0")
("S0","N")
("S1","N")
("S2","N")
("S3","Y")
("S4","N")
("S5","N")
("SEC_CNT","0")
("SEC_TIME","0.0")
("SEG_MAX_NUM","0")
("SEQ_NUM","200912")
("SIMODE","TE_00914")
("SOE_ROLL","251.2841")
("SPECTRA_MAX_COUNT","0.0")
("SPITZER","0.0")
("SPWIN","N")
("START_DATE","2014-06-23 10:42:32")
("STATUS","archived")
("STRT_ROW","0")
("SUBARY","NONE")
("SUZAKU","0.0")
("SWIFT","0.0")
("TARGET_NAME","GJ 436")
("TIME_CRIT","Y")
("TOO_START","0.0")
("TOO_STOP","0.0")
("TYPE","GO")
("UNINT","N")
("VMAG","10.6")
("WINDOW","N")
("XMM","0.0")
("Y_AMP","0.0")
("Y_FREQ","0.0")
("Y_OFF","0.15")
("Y_PHASE","0.0")
("Z_AMP","0.0")
("Z_FREQ","0.0")
("Z_OFF","-0.25")
("Z_PHASE","0.0")
##### Map 1 END #####



-}

-- | Access data from the Chandra observational catalog and add it
--   to the database.
--
--   Usage:
--       obscat [debug]
--       obscat [obsid]
--
--     where the obsid argument is for debugging
--
-- TODO:
--    extract more info
--
--    how to update existing info
--
--    add in the data as we get it, rather than do all the
--    queries then all the insertions. This makes it a bit
--    more "resilient" to occasional failures (i.e. do not
--    re-query for information if we can help it).
--
--    The handling of non-science observations is not ideal,
--    since they can end up sticking around in the ScheduleItem
--    table, leading to unnecessary OCAT queries (e.g. for
--    discarded items).
--
--    Non-Science observations are complicated since they only
--    appear in the ObsCat after they have run, so they
--    are initially set with values from the ScheduleItem
--    table, and then replaced after they have been run.
--    The nsName field is used to determine whether they have
--    been set from the ObsCat (if the value is nsInObsCatName
--    then they have been updated). I am also using the logic
--    that if they are in the obscat but with no START_DATE
--    field then they are "discarded", but this is a large
--    number, and so I am not sure if it is correct or sensible.
--

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Control.Applicative ((<$>))
#endif

import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT)

import Data.Char (ord)
import Data.Either (isLeft, rights)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid ((<>))
import Data.Word (Word8)
import Data.Text.Encoding (decodeUtf8')

import Database.Groundhog.Postgresql

import Network (withSocketsDo)
import Network.HTTP.Conduit

import System.Environment (getArgs, getEnv, getProgName)
import System.Exit (ExitCode(ExitSuccess), exitFailure)
import System.IO (hFlush, stderr)
import System.IO.Temp (withSystemTempFile)
-- import System.Process (readProcessWithExitCode, system)
import System.Process (system)

#if defined(MIN_VERSION_time) && MIN_VERSION_time(1,5,0)
import Data.Time (TimeLocale, defaultTimeLocale, readSTime)
#else
import Data.Time (readsTime)
import System.Locale (defaultTimeLocale)
#endif

import Data.Time (UTCTime, getCurrentTime)

import Database (insertScienceObs
                , replaceNonScienceObs
                , replaceScienceObs
                , insertProposal
                , cleanupDiscarded
                , cleanDataBase
                , putIO
                , runDb
                , discarded
                , notArchived
                , notDiscarded
                , nsInObsCatName
                , notFromObsCat
                )
import Types

-- | Assume that TYPE=ER in the OCAT means that this is a
--   non-science observation, otherwise it is a science one.
--
nonScienceType :: T.Text
nonScienceType = "ER"

isScienceObsE :: OCAT -> Either T.Text Bool
isScienceObsE m =
  toTextE m "TYPE" >>= \ans -> return (ans /= nonScienceType)

#if defined(MIN_VERSION_time) && MIN_VERSION_time(1,5,0)
-- make it easy to compile on different systems for now
readsTime :: TimeLocale -> String -> ReadS UTCTime
readsTime = readSTime True
#endif

-- | What is the URL needed to query the ObsCat?
getObsCatQuery :: ObsIdVal -> String
getObsCatQuery oid = 
  let oi = show (fromObsId oid)
  in "http://cda.harvard.edu/srservices/ocatDetails.do?obsid="
     <> oi <> "&format=text"

-- | Return the constellation name for this position. The code exits if
--   there is a problem running prop_precess.
--
--   For now we only deal with a single position at a time,
--   and it requires that CIAO/prop_precess is available.
--
getConstellation :: RA -> Dec -> IO ConShort
getConstellation ra dec = do

  -- this raises an exception if ASCDS_INSTALL is not set
  path <- getEnv "ASCDS_INSTALL"
  
  withSystemTempFile "in.coords" $ \inName inHdl ->
    withSystemTempFile "out.constellation" $ \outName outHdl -> do
      let long = _unRA ra
          lat = _unDec dec
          coords = T.pack (show long) <> " " <> T.pack (show lat)
          cstr = "prop_precess from j/deg to con p0: "
                 <> inName <> ": " <> outName

          full = ". " <> path <> "/bin/ciao.bash > /dev/null; "
                 <> cstr <> " > /dev/null"
      
      T.hPutStrLn inHdl coords
      hFlush inHdl
      
      rval <- system full
      when (rval /= ExitSuccess) $ do
        let estr = "ERROR: unable to run prop_precess on "
                   <> coords <> "\n  " <> T.pack cstr <> "\n"
        T.hPutStrLn stderr estr
        exitFailure
    
      cName <- T.hGetLine outHdl
      case toConShort cName of
        Just con -> return con
        Nothing -> do
          let merr = "Unexpected constellation short form: '" <> cName <> "'"
          T.hPutStrLn stderr merr
          exitFailure

{-

-- | What publically-available observations overlap this one?
--
--   The overlap is found using the find_chandra_obsid script,
--   using a max radius of 10 arcminutes.
getOverlaps :: ObsIdVal -> RA -> Dec -> IO [OverlapObs]
getOverlaps oid ra dec = do

  -- this raises an exception if ASCDS_INSTALL is not set
  -- path <- getEnv "ASCDS_INSTALL"

  let long = show (_unRA ra)
      lat = show (_unDec dec)
      args = [long, lat, "radius=10"]

  -- do we need to set up the CIAO environment as we did above? probably
  (rval, sout, serr) <- readProcessWithExitCode "find_chandra_obsid" args ""
  when (rval /= ExitSuccess) $ do
    hPutStrLn stderr ("ERROR: unable to run find_chandra_obsid on " ++ show args ++ "\n  " ++ serr ++ "\n")
    exitFailure
    
  now <- getCurrentTime
  let toO xs = 
        -- this does not handle invalid output
        let (idvalstr:rdiststr:_) = words xs 
            idval = ObsIdVal (read idvalstr)
            rdist = read rdiststr
        in OverlapObs oid idval rdist now

  return (map toO (drop 1 (lines sout)))

-}

w8 :: Char -> Word8
w8 = fromIntegral . ord

type OCAT = M.Map L.ByteString L.ByteString

-- Could rewrite the Maybe forms as simplifications of the
-- Either forms, but leave that for a later revamp.

lookupE :: L.ByteString -> OCAT -> Either T.Text L.ByteString
lookupE k m = case M.lookup k m of
  Just ans -> return ans
  Nothing -> Left ("Missing key: " <> T.pack (show k))

maybeReadLBS :: Read a => L.ByteString -> Maybe a
maybeReadLBS = maybeRead . L8.unpack

lbsToText :: L.ByteString -> Either T.Text T.Text
lbsToText lbs =
  either (Left . T.pack . show) Right (decodeUtf8' (L.toStrict lbs))
  

-- Would be nice to indicate the conversion type in the error message
--
-- TODO: improve the hybrid Sting/Text behavior here
eitherReadLBS :: Read a => L.ByteString -> Either T.Text a
eitherReadLBS lbs =
  let val = L8.unpack lbs
      emsg = case lbsToText lbs of
        Right txt -> "Unable to read value from: " <> txt
        Left eval -> "Unable to decode bytestring to Text before maybeRead: "
                     <> eval
  in maybe (Left emsg) Right (maybeRead val)

toWrapper :: Read a => (a -> b) -> L.ByteString -> OCAT -> Maybe b
toWrapper f k m = M.lookup k m >>= fmap f . maybeReadLBS

toWrapperE :: Read a => (a -> b) -> L.ByteString -> OCAT -> Either T.Text b
toWrapperE f k m = do
  kval <- lookupE k m
  case eitherReadLBS kval of
    Right val -> Right (f val)
    Left emsg -> Left ("Unable to read key: " <> T.pack (show k)
                       <> " -> " <> emsg)

toSequenceE :: OCAT -> Either T.Text Sequence
toSequenceE = toWrapperE Sequence "SEQ_NUM"

toPropNumE :: OCAT -> Either T.Text PropNum
toPropNumE = toWrapperE PropNum "PR_NUM"

toObsIdE :: OCAT -> Either T.Text ObsIdVal
toObsIdE = toWrapperE ObsIdVal "OBSID"

toTimeKS :: OCAT -> L.ByteString -> Maybe TimeKS
toTimeKS m k = toWrapper TimeKS k m

toTimeKSE :: OCAT -> L.ByteString -> Either T.Text TimeKS
toTimeKSE m k = toWrapperE TimeKS k m

-- A value of 0.0 is converted to @Nothing@.
toJointTime :: OCAT -> L.ByteString -> Maybe TimeKS
toJointTime m k = toTimeKS m k >>= \t -> if t <= TimeKS 0.0 then Nothing else Just t

toText :: OCAT -> L.ByteString -> Maybe T.Text
toText m lbl = either (const Nothing) Just (toTextE m lbl)

toTextE :: OCAT -> L.ByteString -> Either T.Text T.Text
toTextE m lbl =
  let nokey = "Missing key: " <> T.pack (show lbl)
  in case M.lookup lbl m of
    Just bs -> lbsToText bs
    Nothing -> Left nokey

toRead :: Read a => OCAT -> L.ByteString -> Maybe a
toRead m lbl = toWrapper id lbl m

toReadE :: Read a => OCAT -> L.ByteString -> Either T.Text a
toReadE m lbl = toWrapperE id lbl m

toCT :: OCAT -> L.ByteString -> Maybe ChandraTime
toCT m lbl = ChandraTime <$> toUTC m lbl

{-
toCTE :: OCAT -> L.ByteString -> Either T.Text ChandraTime
toCTE m lbl = ChandraTime <$> toUTCE m lbl
-}

lbsToUTC :: L.ByteString -> Maybe UTCTime
lbsToUTC =
  let c = fmap fst . listToMaybe . readsTime defaultTimeLocale "%F %T"
  in c . L8.unpack
  
lbsToUTCE :: L.ByteString -> Either T.Text UTCTime
lbsToUTCE lbs =
  let c = fmap fst . listToMaybe . readsTime defaultTimeLocale "%F %T"
      val = L8.unpack lbs
      emsg = case lbsToText lbs of
        Right txt -> "Unable to convert to UTCTime: " <> txt
        Left cerr -> "Conversion to UTCTime, sent: " <> cerr
        
  in maybe (Left emsg) Right (c val)
  
toUTC :: OCAT -> L.ByteString -> Maybe UTCTime
toUTC m lbl = M.lookup lbl m >>= lbsToUTC

{-
toUTCE :: OCAT -> L.ByteString -> Either T.Text UTCTime
toUTCE m lbl = lookupE lbl m >>= lbsToUTCE
-}

-- TODO: catch parse errors
toRAE :: OCAT -> Either T.Text RA
toRAE mm = do
  let tR lbs = 
        let (h:m:s:_) = map (read . L8.unpack) (L.split (w8 ' ') lbs)
        in RA (15.0 * (h + (m + s/60.0) / 60.0))

  raVal <- lookupE "RA" mm
  return (tR raVal)

-- TODO: catch parse errors
toDecE :: OCAT -> Either T.Text Dec
toDecE mm = do
  let tD lbs = 
        let (d:m:s:_) = map (read . L8.unpack) (L.split (w8 ' ') rest)
            (sval, rest) = case L.uncons lbs of
                     Just (c, cs) | c == w8 '-' -> (-1, cs)
                                  | c == w8 '+' -> (1, cs)
                                  | otherwise   -> (1, lbs) -- should not happen but just in case
                     _ -> (0, "0 0 0") -- if it's empty we have a problem
        in Dec (sval * (abs d + (m + s/60.0) / 60.0))

  decVal <- lookupE "Dec" mm
  return (tD decVal)

conv :: T.Text -> (T.Text -> Maybe a) -> T.Text -> Either T.Text a
conv lbl f a = maybe (Left ("Unknown " <> lbl <> ": " <> a)) Right (f a)

toInstE :: OCAT -> Either T.Text Instrument
toInstE m = do
  val <- toTextE m "INSTR"
  conv "instrument" toInstrument val

-- TODO: check these are the serializations used by OCAT
toGratingE :: OCAT -> Either T.Text Grating
toGratingE m = do
  grat <- toTextE m "GRAT"
  case toGrating grat of
    Just g -> Right g
    Nothing -> Left ("Unknown grating: " <> grat)

toCSE :: OCAT -> L.ByteString -> Either T.Text ChipStatus
toCSE m k = do
  cs <- toTextE m k
  conv "ChipStatus" toChipStatus cs

toCE :: OCAT -> L.ByteString -> Either T.Text Constraint
toCE m k = do
  val <- lookupE k m
  let ktxt = either id id (lbsToText k)
      vtxt = either id id (lbsToText val)
  case L8.uncons val of
    Just (c, rest) -> if L8.null rest
                      then maybe
                           (Left ("unknown constraint: " <> T.singleton c))
                           Right (toConstraint c)
                      else Left ("Constraint value " <> ktxt <> "=" <> vtxt)
                           
    _ -> Left ("empty string (?) key=" <> ktxt <> " val=" <> vtxt)

toProposalE :: OCAT -> Either T.Text Proposal
toProposalE m = do
  pNum <- toPropNumE m
  piName <- toTextE m "PI_NAME"
  cat <- toTextE m "CATEGORY"
  pType <- toTextE m "TYPE"
  pCycle <- toTextE m "PROP_CYCLE"
  
  -- have seen one proposal (cycle 00, calibration obs) without a title
  let defAns = if pType == "CAL" then "Calibration Observation" else "Unknown"
      pName = fromMaybe defAns (toText m "PROP_TITLE")
      
  return Proposal {
        propNum = pNum
        , propName = pName
        , propPI = piName
        , propCategory = cat
        , propType = pType
        , propCycle = pCycle
     }

toPos :: OCAT -> Either T.Text (RA, Dec, Double)
toPos m = do
  ra <- toRAE m
  dec <- toDecE m
  roll <- toReadE m "SOE_ROLL"
  return (ra, dec, roll)


-- | Note, this is an *INCOMPLETE* ScienceObs since the
--   @soConstellation@ field is filled with an error statement;
--   this is because we need IO to fill in this field so it's
--   left to a later pass.  
--
--   Observations can have no START_DATE field, but the current
--   schema requires there to be one.
--
--   I am moving towards "hacking in" support for ObsId values with
--   no start_data field, in that they will be given a start_date
--   far in the future, with the idea that these are observations
--   that may be done (as long as they are not discarded). Ideally
--   the start-date field would be a maybe but it currently is not.
--   The reason for storing these is that it is possible for an obsid
--   to go from scheduled to having no start date, which means we
--   need to either remove the obsid or update it.
--
toSOE :: OCAT -> Either T.Text ScienceObs
toSOE m = do

  isScience <- isScienceObsE m
  unless isScience (Left "not a science observation")

  seqNum <- toSequenceE m
  pNum <- toPropNumE m

  status <- toTextE m "STATUS"
  obsid <- toObsIdE m
  target <- toTextE m "TARGET_NAME"

  -- Turns out this may not exist. If it does not, then
  -- replace with a time far in the future (IFF status is
  -- not discarded). Note there's an explicit check to
  -- not include cases where the start date exists but can
  -- not be parsed, since that indicates a situation that
  -- needs to be resolved differently).
  --
  -- sTime <- toCTE m "START_DATE"
  sTime <- if status == discarded
           then Right discardedTime  -- is this possible?
           else case M.lookup "START_DATE" m of
             Just sd -> ChandraTime <$> lbsToUTCE sd
             Nothing -> Right futureTime 
  
  appExp <- toTimeKSE m "APP_EXP"
  let obsExp = toTimeKS m "EXP_TIME"

  let relDate = toUTC m "PUBLIC_AVAIL"

  timeCrit <- toCE m "TIME_CRIT"
  monitor <- toCE m "MONITOR"
  constrained <- toCE m "CONSTR"

  inst <- toInstE m
  grat <- toGratingE m
  let det = toText m "READOUT_DETECTOR"
      datamode = toText m "DATAMODE"

  haveACIS <- toTextE m "ACIS"
  -- haveHRC <- toStringE m "HRC"

  -- are these included in HRC obs? No
  let convCS = if haveACIS == "Y"
               then toCSE m
               else const (Right ChipOff)

  acisi0 <- convCS "I0"
  acisi1 <- convCS "I1"
  acisi2 <- convCS "I2"
  acisi3 <- convCS "I3"
  aciss0 <- convCS "S0"
  aciss1 <- convCS "S1"
  aciss2 <- convCS "S2"
  aciss3 <- convCS "S3"
  aciss4 <- convCS "S4"
  aciss5 <- convCS "S5"

  let jnames = do
        bs <- M.lookup "JOINT" m
        ns <- either (const Nothing) Just (lbsToText bs)
        if ns == "None" then Nothing else Just ns

      hst = toJointTime m "HST"
      noao = toJointTime m "NOAO"
      nrao = toJointTime m "NRAO"
      rxte = toJointTime m "RXTE"
      spitzer = toJointTime m "SPITZER"
      suzaku = toJointTime m "SUZAKU"
      xmm = toJointTime m "XMM"
      swift = toJointTime m "SWIFT"
      nustar = toJointTime m "NUSTAR"

      too = toText m "TOO_TYPE"

  (ra, dec, roll) <- toPos m
  
  let subStart = toRead m "STRT_ROW" >>= \s -> if s > 0 then Just s else Nothing
      subSize = toRead m "ROW_CNT" >>= \s -> if s > 0 then Just s else Nothing

  return ScienceObs {
    soSequence = seqNum
    , soProposal = pNum
    , soStatus = status
    , soObsId = obsid
    , soTarget = target
    , soStartTime = sTime
    , soApprovedTime = appExp
    , soObservedTime = obsExp
    , soPublicRelease = relDate

    , soTimeCritical = timeCrit
    , soMonitor = monitor
    , soConstrained = constrained

    , soInstrument = inst
    , soGrating = grat
    , soDetector = det
    , soDataMode = datamode

    , soACISI0 = acisi0
    , soACISI1 = acisi1
    , soACISI2 = acisi2
    , soACISI3 = acisi3
    , soACISS0 = aciss0
    , soACISS1 = aciss1
    , soACISS2 = aciss2
    , soACISS3 = aciss3
    , soACISS4 = aciss4
    , soACISS5 = aciss5

    , soJointWith = jnames
    , soJointHST = hst
    , soJointNOAO = noao
    , soJointNRAO = nrao
    , soJointRXTE = rxte
    , soJointSPITZER = spitzer
    , soJointSUZAKU = suzaku
    , soJointXMM = xmm
    , soJointSWIFT = swift
    , soJointNUSTAR = nustar

    , soTOO = too
    , soRA = ra
    , soDec = dec
    , soConstellation =
        -- this is ugly
        error ("The constellation field for ObsId "
               <> show (fromObsId obsid)
               <> " has not been filled in!")
    , soRoll = roll
    , soSubArrayStart = subStart
    , soSubArraySize = subSize
     }

-- | The name field is used to indicate a "discarded" observation,
--   which here is taken to be a record with no START_DATE field.
--   In this case the startTime field is set to discardedTime.
--
--   Perhaps the name field should be set to the status field?
--   This could complicate things, since I would need to know
--   the full enumeration for this field.
--
--   This would be easier if I bothered to migrate the database so that
--   new fields could be added to NonScienceObs.
--
toNSE :: OCAT -> Either T.Text NonScienceObs
toNSE m = do

  isScience <- isScienceObsE m
  when isScience (Left "a science observation")
  
  -- status <- toString m "STATUS"
  obsid <- toObsIdE m
  
  -- turns out this may be a maybe (presumably for cancelled obs)
  let msTime = toCT m "START_DATE"
  
  appExp <- toTimeKSE m "APP_EXP"
  -- let obsExp = toTimeKS m "EXP_TIME" not sure what this is

  (ra, dec, roll) <- toPos m

  -- TODO: creating the name should be a utility function
  let target = T.pack ("CAL-ER (" <> show (fromObsId obsid) <> ")")
      rTime = appExp

      (name, sTime) = case msTime of
        Just v -> (nsInObsCatName, v)
        _ -> (discarded, discardedTime)
                                                       
  return NonScienceObs {
    nsName = name
    , nsObsId = obsid
    , nsTarget = target
    , nsStartTime = sTime
    , nsTime = rTime
    , nsRa = ra
    , nsDec = dec
    , nsRoll = roll
    }


{-

Possibly interesting fields:

  HRC - bool? 

  CONSTR MONITOR TIME_CRIT  - bool ?

  SUBARY

SEQ_NUM	STATUS	OBSID	PR_NUM	TARGET_NAME	GRID_NAME	INSTR	GRAT	TYPE	OBS_CYCLE	PROP_CYCLE	CHARGE_CYCLE	START_DATE	PUBLIC_AVAIL	READOUT_DETECTOR	DATAMODE	JOINT	HST	NOAO	NRAO	RXTE	SPITZER	SUZAKU	XMM	SWIFT	NUSTAR	CATEGORY	SEG_MAX_NUM	PROP_TITLE	PI_NAMEOBSERVER	APP_EXP	EXP_TIME	RA	Dec	SOE_ROLL	TIME_CRIT	Y_OFF	Z_OFF	X_SIM	Z_SIM	RASTER	OBJ_TYPE	OBJ	NUDGE	PHOTO	VMAG	EST_CNT_RATE	FORDER_CNT_RATE	COUNT_RATE	EVENT_COUNT	DITHER	Y_AMP	Y_FREQ	Y_PHASE	Z_AMP	Z_FREQ	Z_PHASE	ROLL	WINDOW	UNINT	MONITOR	PRE_ID	MON_MINMON_MAX	GROUP_ID	CONSTR	EPOCH	PERIOD	PSTART	PS_MARG	PEND	PE_MARG	TOO_TYPE	TOO_START	TOO_STOP	SIMODE	HRC	SPECT_MODE	BLANK_ENU_HI	V_HI	U_LO	V_LO	TIMING	Z_BLK	ACIS	MODE	BEP_PACK	DROPPED_CHIP_CNT	I0	I1	I2	I3	S0	S1	S2	S3	S4	S5	SPECTRA_MAX_COUNT	MULTIPLE_SPECTRAL_LINES	SUBARY	STRT_ROW	ROW_CNT	D_CYC	SEC_CNT	PR_TIME	SEC_TIME	F_TIME	OC_SUM	OC_ROW	OC_COL	EVFIL	EVFIL_LO	EVFIL_RA	EFFICIENT	SPWIN
901116	scheduled	16196	15900142	30 Doradus		ACIS-I	NONE	GO	15	15	15	2014-05-30 00:22:47			VFAINT	None	0.0	0.0	0.0	0.0	0.0	0.0	0.0	0.0	0.0	EXTRAGALACTIC DIFFUSE EMISSION AND SURVEYS	0	The Tarantula -- Revealed by X-rays (T-ReX): A Definitive Chandra Investigation of 30 Doradus	Townsley	Townsley	68.00		05 38 42.40	-69 06 02.90	202.00286	N	-0.2	-0.25			N	NO	NONE		N		0.05					0.0	0.0	0.0	0.0	0.0	0.0	NN	N	N					N	0.0	0.0	0.0	0.0	0.0	0.0		0.0	0.0	TE_004DE	N		YTE	VF	0	Y	Y	Y	Y	N	N	O1	O2	N	N	20000.0	Y	NONE	0	0	N	0	0.0	0.0	0.0	N	1	1	Y	0.1	12.0	Y	N

-}

-- | Add in the constellation field; this may fail
--   which will exit the code.
addConstellation :: ScienceObs -> IO ScienceObs
addConstellation so = do
  constellation <- getConstellation (soRA so) (soDec so)
  return $ so { soConstellation = constellation }


-- | Query the archive for information on this obsid.
--
--   Assume that there is only one match.
makeObsCatQuery ::
  Bool    -- ^ True for debug output (a dump of the fields)
  -> ObsIdVal
  -> IO (Maybe OCAT)
makeObsCatQuery flag oid = do
  rsp <- simpleHttp (getObsCatQuery oid)
  -- TODO: why not just convert to Text straight away?
  let isHash x = case L.uncons x of 
                   Just (y, _) -> y == w8 '#'
                   _ -> False

      ls = dropWhile isHash (L.split (w8 '\n') rsp)
      tokenize = L.split (w8 '\t')
      hdr = tokenize (head ls)
      -- could use positional information, but use a map instead
      dl = filter (not . L.null) (drop 2 ls)
      dropNulls = filter (not . L.null . snd)
      out = map (M.fromList . dropNulls . zip hdr . tokenize) dl

  when flag $ forM_ (zip [(1::Int)..] out) $ \(i,m) -> do
    let itxt = T.pack (show i)
    T.putStrLn ("##### Map " <> itxt <> " START #####")
    forM_ (M.toList m) print     
    T.putStrLn ("##### Map " <> itxt <> " END #####")

  case out of
    [x] -> return (Just x)
    [] -> return Nothing
    (x:_) -> do
      T.putStrLn ("WARNING: multiple responses for ObsId "
                  <> T.pack (show (fromObsId oid))
                  <> " - using first match")
      return (Just x)

-- | Query the OCat about this science observation.
--
--   The first match reported is taken; screen messages are displayed
--   if multiple matches are found or the data can not be
--   processed.
--
queryScience :: 
  Bool         -- set @True@ for debug output 
  -> ObsIdVal 
  -> IO (Either T.Text (Proposal, ScienceObs))
queryScience flag oid = do
  mout <- makeObsCatQuery flag oid
  let mans = case mout of
        Just out -> do
          prop <- toProposalE out
          so <- toSOE out
          return (prop, so)
        _ -> Left "No output from obscat call"

  case mans of
    Right (p, so) -> do
      -- need to add the constellation
      so2 <- addConstellation so
      return (Right (p, so2))

    Left emsg -> do
      -- for now skip those with issues
      T.putStrLn ("SKIP: unable to parse science ObsId "
                  <> T.pack (show (fromObsId oid))
                  <> ": " <> emsg)
      return (Left emsg)


-- | Query the OCat about this non-science observation.
--
--   This function errors out most ungracefully if multiple matches
--   are found or if one/both of the output values can not be
--   created.
--
queryNonScience :: 
  Bool         -- set @True@ for debug output 
  -> ObsIdVal 
  -> IO (Either T.Text NonScienceObs)
queryNonScience flag oid = do
  mout <- makeObsCatQuery flag oid
  let mans = case mout of
        Just out -> toNSE out
        _ -> Left "No output from obscat call"
  
      otxt = T.pack (show (fromObsId oid))

  when (isLeft mans) 
    (T.putStrLn ("SKIP: unable to parse non-science ObsId " <> otxt))
  return mans


-- | Identify:
--     - all science observations in the ScheduleItem table
--     - all non-science observations that have not been read
--       from the obscat and that are not discarded
--       (TODO: add a restriction to date
--       before now, but the number this would cut out is
--       likely small, so do not bother with)
--       [the idea is that there should be no non-science observations
--        in the ScheduleItem table any more]
--     - science observations which are not archived
--         if observed, only if soPublicRelease < now
--         (if no field, then re-check)
--       For now, remove discarded obsids (the assumption being that they
--       are not going to get recycled).
--
-- Should this be in the PersistBackend monad rather than making
-- it a transaction? Probably not, because want the updates to
-- be incremental so that they can be redone.
--
findMissingObsIds :: IO ([ObsIdVal], [ObsIdVal], [ObsIdVal])
findMissingObsIds = do
  now <- getCurrentTime
  runDb $ do
    -- the SiScienceObsField check is technically not needed,
    -- as I believe there should only be science obs in the
    -- table now, but leave in as this constraint may change,
    -- and I could be wrong.
    swants <- project SiObsIdField (SiScienceObsField ==. True)
    nswants <- project NsObsIdField notFromObsCat
    
    -- Could do this in the database, but split up the queries.
    -- The idea is to reduce the number of obscat queries for observations
    -- that are unlikely to have changed; that is, observed but not
    -- yet in the public domain.
    --
    unarchived <- project SoObsIdField (notArchived &&. notDiscarded)
    notpublic <- project SoObsIdField ((SoStatusField ==. ("observed"::T.Text))
                                       &&. (SoPublicReleaseField >=. Just now))
  
    let tosearch = S.fromList unarchived `S.difference` S.fromList notpublic
    return (swants, nswants, S.toList tosearch)

slen :: [a] -> T.Text
slen = T.pack . show . length

-- | Add the "missing" results to the database and replace the
--   "unarchived" results (they may or may not have changed but
--   easiest for me is just to update the database).
--
addResults ::
  [(Proposal, ScienceObs)]
  -> [(Proposal, ScienceObs)]
  -> [NonScienceObs]  -- ^ no longer "missing" but "not from obscat"
  -> IO ()
addResults [] [] [] = T.putStrLn "# No data needs to be added to the database."
addResults missing unarchived nsmissing = do
  T.putStrLn ("# Adding " <> slen missing <> " missing results")
  T.putStrLn ("# Adding " <> slen unarchived <> " unarchived results")
  T.putStrLn ("# Adding " <> slen nsmissing <> " missing non-science")
  
  let props = S.toList (S.fromList (map fst missing ++ map fst unarchived))

      lprint :: Show a => a -> DbPersist Postgresql (NoLoggingT IO) ()
      lprint = liftIO . print

      disp ::
        Show a
        => (a -> DbPersist Postgresql (NoLoggingT IO) Bool)
        -> a
        -> DbPersist Postgresql (NoLoggingT IO) ()
      disp act val = do
        flag <- act val
        when flag (lprint val)
  
  runDb $ do
      putIO "## missing"
      forM_ missing (\(_,so) -> disp insertScienceObs so)
      putIO "## unarchived"
      forM_ unarchived (\(_,so) -> replaceScienceObs so >> lprint so)
      putIO "## proposals"
      forM_ props (disp insertProposal)

      putIO "## non-science"
      forM_ nsmissing replaceNonScienceObs

      -- ensure that any discarded observations are removed from the
      -- schedule, and any observations for which we now have an obscat
      -- value are removed (this latter should be handled by some
      -- of the above calls but leave as is for now).
      cleanupDiscarded
      cleanDataBase


{-
-- | add in the overlap observations; that is, the table of
--   overlaps and information about the overlap obs if it is
--   not known about.
--
--   We do not bother finding overlaps of these overlaps.
--
addOverlaps :: Bool -> [OverlapObs] -> IO ()
addOverlaps f os = do
  let oids = S.toList (S.fromList (map ovOverlapId os))
  putStrLn ("# Processing " ++ slen os ++ " overlaps with " ++
            slen oids ++ " different ObsIds")

  -- avoid FlexibleInstances by being explicit with types; could make
  -- the monad more general (I guess) but leave as is for now
  let countOid :: ObsIdVal -> DbPersist Postgresql (NoLoggingT IO) Int 
      countOid oid = count (SoObsIdField ==. oid)
  cs <- runDb (forM oids countOid)
  let unknowns = map fst (filter ((==0) . snd) (zip oids cs))
  putStrLn $ "# Of these, there are " ++ slen unknowns ++ " 'new' ObsIds"

  res1 <- mapM (queryScience f) unknowns
  res <- check res1 unknowns

  let (props1, sobs) = unzip res
      props = S.toList (S.fromList props1)

  putStrLn ("# Adding " ++ slen sobs ++ " observations")
  putStrLn ("# Adding " ++ slen props ++ " proposals")
  runDb $ do
    -- could get away with insert for sobs if we can assume that
    -- no other process is updating the database
    forM_ sobs insertScienceObs
    forM_ props insertProposal

-}

-- | Report if any obsids are missing from the OCAT results.
--
--   This does NOT include the overlap obs (i.e. they are ignored here)
check ::
  [Either b a]  -- ^ result
  -> [ObsIdVal]      -- ^ the obsid goes with the result
  -> IO [a]          -- ^ results that are not missing
check ms os = do
  let missing = map snd (filter (isLeft . fst) (zip ms os))
  unless (null missing) $ do
    T.putStrLn ("### There are "
                <> slen missing
                <> " ObsIds with no OCAT data:")
    forM_ missing (print . fromObsId)
  return (rights ms)

-- | The flag is @True@ to get debug output from the query calls.
--
--   Loop through every item in the ScheduleItem table and add it
--   to the relevant table in the database.
--
updateDB :: Bool -> IO ()
updateDB f = withSocketsDo $ do
  T.putStrLn "# Querying the database"
  (missing, nsmissing, unarchived) <- findMissingObsIds
  T.putStrLn ("# Found " <> slen missing <> " missing and " <>
              slen unarchived <> " science ObsIds needing to be queried")
  T.putStrLn ("# Found " <> slen nsmissing <> " missing non-science ObsIds")

  unless (null missing) (T.putStrLn "# Processing missing science")
  res1 <- mapM (queryScience f) missing
  mres <- check res1 missing

  unless (null unarchived) (T.putStrLn "# Processing 'missing' science obs")
  res2 <- mapM (queryScience f) unarchived
  ures <- check res2 unarchived

  unless (null nsmissing) (T.putStrLn "# Processing missing non-science")
  res3 <- mapM (queryNonScience f) nsmissing
  nsres <- check res3 nsmissing

  addResults mres ures nsres

  {-
  -- add in the overlaps; commented out for now
  let sos = map snd mres ++ map snd ures
  putStrLn $ "## Processing " ++ slen sos ++ " observations for overlaps"
  overlaps <- forM sos $ \ScienceObs{..} -> getOverlaps soObsId soRA soDec
  addOverlaps f $ concat overlaps
  -}

  (missing1, nsmissing1, unarchived1) <- findMissingObsIds
  when (missing /= missing1)
    (T.putStrLn ("# Number missing: " <> slen missing <>
                 " -> " <> slen missing1))
  when (unarchived /= unarchived1)
    (T.putStrLn ("# Number Sci: " <> slen unarchived <>
                 " -> " <> slen unarchived1))
  when (nsmissing /= nsmissing1)
    (T.putStrLn ("# Number Non-sci: " <> slen nsmissing <>
                 " -> " <> slen nsmissing1))
  
viewObsId :: Int -> IO ()
viewObsId oid = withSocketsDo $ do
  let otxt = T.pack (show oid)
  ans <- queryScience True (ObsIdVal oid)
  case ans of
    Left emsg -> T.putStrLn ("Nothing found for ObsId "
                             <> otxt <> ": " <> emsg)
    Right (prop, so) -> do
      T.putStrLn ("## ObsId " <> otxt)
      print prop
      print so
      dump so

dump :: ScienceObs -> IO ()
dump ScienceObs{..} = do
  T.putStrLn "------ dump"
  print (_unSequence soSequence)
  T.putStrLn soStatus
  print (fromObsId soObsId)
  T.putStrLn soTarget
  -- discardedTime is only used for non-science obs
  T.putStrLn (if soStartTime == futureTime
              then "** Observation has no scheduled observation date"
              else showCTime soStartTime)
  T.putStrLn (showExpTime soApprovedTime)
  print (fmap showExpTime soObservedTime)
  T.putStrLn ("Public availability: " <> T.pack (show soPublicRelease))

  let fC lbl c = lbl <> ": " <> T.singleton (fromConstraint c)
  T.putStrLn (fC "time critical" soTimeCritical)
  T.putStrLn (fC "      monitor" soMonitor)
  T.putStrLn (fC "  constrained" soConstrained)

  print soInstrument
  print soGrating
  print soDetector
  print soDataMode

  let achip lbl val =
        T.putStrLn ("ACIS" <> lbl <> " " <> fromChipStatus val)
  achip "I0" soACISI0
  achip "I1" soACISI1
  achip "I2" soACISI2
  achip "I3" soACISI3
  achip "S0" soACISS0
  achip "S1" soACISS1
  achip "S2" soACISS2
  achip "S3" soACISS3
  achip "S4" soACISS4
  achip "S5" soACISS5

  print soJointWith
  let joint lbl val =
        let tval = T.pack (show (fmap _toKS val))
        in T.putStrLn ("Joint " <> lbl <> " " <> tval)
  joint "HST" soJointHST
  joint "NOAO" soJointNOAO
  joint "NRAO" soJointNRAO
  joint "RXTE" soJointRXTE
  joint "SPITZER" soJointSPITZER
  joint "SUZAKU" soJointSUZAKU
  joint "XMM" soJointXMM
  joint "SWIFT" soJointSWIFT
  joint "NUSTAR" soJointNUSTAR

  print soTOO
  T.putStrLn (showRA soRA)
  T.putStrLn (showDec soDec)
  T.putStrLn ("which is in constellation: " <> fromConShort soConstellation)
  print soRoll
  print soSubArrayStart
  print soSubArraySize

usage :: IO ()
usage = do
  pName <- T.pack <$> getProgName
  T.hPutStrLn stderr ("Usage: " <> pName)
  T.hPutStrLn stderr ("       " <> pName <> " debug")
  T.hPutStrLn stderr ("       " <> pName <> " <obsid>")
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> updateDB False
    [x] | x == "debug" -> updateDB True
        | otherwise -> case (fmap fst . listToMaybe) (reads x) of
             Just oid -> viewObsId oid
             _ -> usage

    _ -> usage

