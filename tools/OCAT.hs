{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OCAT (OCAT
            , queryOCAT
            , ocatToScience
            , ocatToNonScience

            , noDataInOCAT
            , isScienceObsE
              
            , dumpScienceObs
            , slen
            ) where

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


import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad (forM_, unless, when)

import Data.List (intercalate)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8')

import Network.HTTP.Conduit
import Network.HTTP.Types.Header (Header)

import System.Environment (getEnv)
import System.Exit (ExitCode(ExitSuccess), exitFailure)
import System.IO (hFlush, stderr)
import System.IO.Temp (withSystemTempFile)
-- import System.Process (readProcessWithExitCode, system)
import System.Process (system)

import Data.Time (TimeLocale, UTCTime, defaultTimeLocale, readSTime)

import Text.Read (readMaybe)

import Types

userAgent :: Header
userAgent = ("User-Agent", "chandraobs dburke@cfa.harvard.edu")

-- | Assume that TYPE=ER in the OCAT means that this is a
--   non-science observation, otherwise it is a science one.
--
nonScienceType :: T.Text
nonScienceType = "ER"

isScienceObsE :: OCAT -> Either T.Text Bool
isScienceObsE m =
  toTextE m "TYPE" >>= \ans -> return (ans /= nonScienceType)

-- make it easy to compile on different systems for now
readsTime :: TimeLocale -> String -> ReadS UTCTime
readsTime = readSTime True

-- | What is the URL needed to query the ObsCat?
getObsCatQuery :: [ObsIdVal] -> String
getObsCatQuery oids = 
  let ois = map (show . fromObsId) oids
      oitxt = intercalate "," ois
  in "http://cda.harvard.edu/srservices/ocatDetails.do?obsid="
     <> oitxt <> "&format=text"

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

type OCAT = M.Map T.Text T.Text

-- Could rewrite the Maybe forms as simplifications of the
-- Either forms, but leave that for a later revamp.

lookupE :: T.Text -> OCAT -> Either T.Text T.Text
lookupE k m = case M.lookup k m of
  Just ans -> return ans
  Nothing -> Left ("Missing key: " <> k)

readMaybeText :: Read a => T.Text -> Maybe a
readMaybeText = readMaybe . T.unpack


-- Would be nice to indicate the conversion type in the error message
--
-- TODO: improve the hybrid Sting/Text behavior here
--
eitherReadText :: Read a => T.Text -> Either T.Text a
eitherReadText txt =
  let emsg = "Unable to read value from: " <> txt
  in maybe (Left emsg) Right (readMaybeText txt)

toWrapper :: Read a => (a -> b) -> T.Text -> OCAT -> Maybe b
toWrapper f k m = M.lookup k m >>= fmap f . readMaybeText

toWrapperE :: Read a => (a -> b) -> T.Text -> OCAT -> Either T.Text b
toWrapperE f k m = do
  kval <- lookupE k m
  case eitherReadText kval of
    Right val -> Right (f val)
    Left emsg -> Left ("Unable to read key: " <> k
                       <> " -> " <> emsg)

toSequenceE :: OCAT -> Either T.Text Sequence
toSequenceE = toWrapperE Sequence "SEQ_NUM"

toPropNumE :: OCAT -> Either T.Text PropNum
toPropNumE = toWrapperE PropNum "PR_NUM"

toObsIdE :: OCAT -> Either T.Text ObsIdVal
toObsIdE = toWrapperE ObsIdVal "OBSID"

toTimeKS :: OCAT -> T.Text -> Maybe TimeKS
toTimeKS m k = toWrapper TimeKS k m

toTimeKSE :: OCAT -> T.Text -> Either T.Text TimeKS
toTimeKSE m k = toWrapperE TimeKS k m

-- A value of 0.0 is converted to @Nothing@.
toJointTime :: OCAT -> T.Text -> Maybe TimeKS
toJointTime m k = toTimeKS m k >>= \t -> if t <= TimeKS 0.0 then Nothing else Just t

toText :: OCAT -> T.Text -> Maybe T.Text
toText m lbl = either (const Nothing) Just (toTextE m lbl)

toTextE :: OCAT -> T.Text -> Either T.Text T.Text
toTextE m lbl =
  let nokey = "Missing key: " <> lbl
  in case M.lookup lbl m of
    Just bs -> Right bs
    Nothing -> Left nokey

toRead :: Read a => OCAT -> T.Text -> Maybe a
toRead m lbl = toWrapper id lbl m

toReadE :: Read a => OCAT -> T.Text -> Either T.Text a
toReadE m lbl = toWrapperE id lbl m

toCT :: OCAT -> T.Text -> Maybe ChandraTime
toCT m lbl = ChandraTime <$> toUTC m lbl

{-
toCTE :: OCAT -> L.ByteString -> Either T.Text ChandraTime
toCTE m lbl = ChandraTime <$> toUTCE m lbl
-}

textToUTC :: T.Text -> Maybe UTCTime
textToUTC =
  let c = fmap fst . listToMaybe . readsTime defaultTimeLocale "%F %T"
  in c . T.unpack
  
textToUTCE :: T.Text -> Either T.Text UTCTime
textToUTCE txt =
  let c = fmap fst . listToMaybe . readsTime defaultTimeLocale "%F %T"
      val = T.unpack txt
      emsg = "Unable to convert to UTCTime: " <> txt
  in maybe (Left emsg) Right (c val)
  
toUTC :: OCAT -> T.Text -> Maybe UTCTime
toUTC m lbl = M.lookup lbl m >>= textToUTC

{-
toUTCE :: OCAT -> L.ByteString -> Either T.Text UTCTime
toUTCE m lbl = lookupE lbl m >>= lbsToUTCE
-}

-- TODO: catch parse errors
toRAE :: OCAT -> Either T.Text RA
toRAE mm = do
  let tR txt = 
        let (h:m:s:_) = map (read . T.unpack) (T.splitOn " " txt)
        in RA (15.0 * (h + (m + s/60.0) / 60.0))

  raVal <- lookupE "RA" mm
  return (tR raVal)

-- TODO: catch parse errors
toDecE :: OCAT -> Either T.Text Dec
toDecE mm = do
  let tD lbs = 
        let (d:m:s:_) = map (read . T.unpack) (T.splitOn " " rest)
            (sval, rest) = case T.uncons lbs of
                     Just (c, cs) | c == '-'  -> (-1, cs)
                                  | c == '+'  -> (1, cs)
                                  | otherwise -> (1, lbs) -- should not happen but just in case
                     _ -> (0, "0 0 0") -- if it's empty we have a problem
        in Dec (sval * (abs d + (m + s/60.0) / 60.0))

  decVal <- lookupE "Dec" mm
  return (tD decVal)

conv :: T.Text -> (T.Text -> Maybe a) -> T.Text -> Either T.Text a
conv lbl f a = maybe (Left ("Unknown " <> lbl <> ": " <> a)) Right (f a)

toStatusE :: OCAT -> Either T.Text ObsIdStatus
toStatusE m = do
  val <- toTextE m "STATUS"
  conv "status" toObsIdStatus val

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

toCSE :: OCAT -> T.Text -> Either T.Text ChipStatus
toCSE m k = do
  cs <- toTextE m k
  conv "ChipStatus" toChipStatus cs

toCE :: OCAT -> T.Text -> Either T.Text Constraint
toCE m k = do
  val <- lookupE k m
  case T.uncons val of
    Just (c, rest) -> if T.null rest
                      then maybe
                           (Left ("unknown constraint: " <> T.singleton c))
                           Right (toConstraint c)
                      else Left ("Constraint value " <> k <> "=" <> val)
                           
    _ -> Left ("empty string (?) key=" <> k <> " val=" <> val)

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


toSOE :: OCAT -> ConShort -> Either T.Text ScienceObs
toSOE m con = do

  isScience <- isScienceObsE m
  unless isScience (Left "not a science observation")

  seqNum <- toSequenceE m
  pNum <- toPropNumE m

  status <- toStatusE m
  obsid <- toObsIdE m
  target <- toTextE m "TARGET_NAME"

  sTime <- if status == Discarded
           then Right Nothing
           else case M.lookup "START_DATE" m of
             Just sd -> (Just . ChandraTime) <$> textToUTCE sd
             Nothing -> Right Nothing
  
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
        ns <- M.lookup "JOINT" m
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

  -- MULTITEL processing; assume that MULTITEL_INT is always present
  -- but MULTITEL_OBS may not be
  multiTelVal <- toTextE m "MULTITEL"
  multiTelInt <- toReadE m "MULTITEL_INT"
  
  let multiTel = multiTelVal == "Y"
      multiTelObs = case toText m "MULTITEL_OBS" of
        Nothing -> []
        Just xs -> map Telescope (T.splitOn ", " xs)
      
  -- NOTE: error out here in case of an invalid conversion
  too <- case toText m "TOO_TYPE" of
    Just tooReq -> case toTOORequest tooReq of
      Nothing -> Left ("UNEXPECTED TOO_TYPE: " <> tooReq)
      ans -> Right ans
    Nothing -> Right Nothing

  -- Note that RA and Dec have probably already been extracted
  -- (to calculate the constellation)
  --
  (ra, dec, roll) <- toPos m
  
  let subStart = toRead m "STRT_ROW" >>= \s -> if s > 0 then Just s else Nothing
      subSize = toRead m "ROW_CNT" >>= \s -> if s > 0 then Just s else Nothing

  return ScienceObs {
    soSequence = seqNum
    , soProposal = pNum
    , soStatus = status
    , soObsId = obsid
    , soTarget = TN target
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

    , soMultiTel = multiTel
    , soMultiTelInt = multiTelInt
    , soMultiTelObs = multiTelObs
                      
    , soTOO = too
    , soRA = ra
    , soDec = dec
    , soConstellation = con

    , soRoll = roll
    , soSubArrayStart = subStart
    , soSubArraySize = subSize
     }

-- | Name/target fields seem to change once the observation has
--   happened.
--
--   Should this send in the ScheduleItem?
--
ocatToNonScience :: OCAT -> Either T.Text NonScienceObs
ocatToNonScience m = do

  isScience <- isScienceObsE m
  when isScience (Left "a science observation")
  
  status <- toStatusE m
  obsid <- toObsIdE m
  
  appExp <- toTimeKSE m "APP_EXP"
  -- let obsExp = toTimeKS m "EXP_TIME" not sure what this is

  (ra, dec, roll) <- toPos m

  -- TODO: creating the name should be a utility function
  let target = T.pack ("CAL-ER (" <> show (fromObsId obsid) <> ")")
      rTime = appExp

      -- in previous versions (when there was no status field)
      -- the nsName field was used to encode the status.
      --
                                                       
  return NonScienceObs {
    nsStatus = status
    -- , nsName = name
    , nsObsId = obsid
    , nsTarget = TN target
    , nsStartTime = toCT m "START_DATE"
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

-- | Query the archive for information on these obsids.
--
--   TODO:
--    - could save the response to disk for later inspection
--      (e.g. to see if there are fields we need/want or to understand
--      the domain).
--
makeObsCatQuery ::
  Bool    -- ^ True for debug output (a dump of the fields)
  -> [ObsIdVal]
  -> IO (Either T.Text [OCAT])
  -- ^ The query either failed (Left error-message)
  --   or returned something. Note that it is not guaranteed
  --   that all values are included in the response (i.e. it
  --   is possible for the call to succeed but the return
  --   does not contain information on one, some, or all
  --   of the input obsids.
  --
makeObsCatQuery flag oids = do

  req <- parseRequest (getObsCatQuery oids)
  let hdrs = userAgent : requestHeaders req
      req' = req { requestHeaders = hdrs }

  mgr <- newManager tlsManagerSettings
  rsp <- httpLbs req' mgr

  let rsplbs = responseBody rsp
  
      lbsToText :: L.ByteString -> Either T.Text T.Text
      lbsToText lbs =
        either (Left . T.pack . show) Right (decodeUtf8' (L.toStrict lbs))
  
  case lbsToText rsplbs of
    Right ans -> Right <$> processResponse flag ans
        
    Left emsg -> do
      T.hPutStrLn stderr ("Unable to query OCAT: " <> emsg)
      return (Left emsg)


processResponse ::
  Bool
  -> T.Text
  -> IO [OCAT]
  -- It is assumed that there is either a response for an ObsId
  -- or not. Errors are indicated by the input ObsIdVal not
  -- being in the output list.
  --
processResponse flag rsp = do
  let isHash x = case T.uncons x of 
                   Just (y, _) -> y == '#'
                   _ -> False

      ls = dropWhile isHash (T.splitOn "\n" rsp)
      tokenize = T.splitOn "\t"
      hdr = tokenize (head ls)

      notNull = not . T.null
      
      -- could use positional information, but use a map instead
      dl = filter notNull (drop 2 ls)
      dropNulls = filter (notNull . snd)
      out = map (M.fromList . dropNulls . zip hdr . tokenize) dl

  when flag $ forM_ (zip [(1::Int)..] out) $ \(i,m) -> do
    let itxt = T.pack (show i)
    T.putStrLn ("##### Map " <> itxt <> " START #####")
    forM_ (M.toList m) print     
    T.putStrLn ("##### Map " <> itxt <> " END #####")

  return out


-- | What does OCAT know about these observations?
--
queryOCAT ::
  [ObsIdVal]
  -> IO (Either T.Text [(ObsIdVal, Maybe OCAT)])
queryOCAT oids = do
  ansE <- makeObsCatQuery False oids

  -- assume that every valid record has an obsid
  let getKV o = case toObsIdE o of
        Left _ -> Nothing
        Right oid -> Just (oid, o)

  case ansE of
    Right ans -> let omap = M.fromList (mapMaybe getKV ans)
                     find oid = (oid, M.lookup oid omap)
                     out = map find oids
                 in return (Right out)

    Left emsg -> return (Left emsg)
  

-- | The ObsId is not recognized by OCAT.
noDataInOCAT :: T.Text
noDataInOCAT = "No OCAT information for this ObsId"


ocatToScience :: 
  OCAT
  -> IO (Either T.Text (Proposal, ScienceObs))
ocatToScience ocat = 
  let vals = do
        prop <- toProposalE ocat
        ra <- toRAE ocat
        dec <- toDecE ocat
        return (prop, ra, dec)
        
  in case vals of
      Left emsg -> return (Left emsg)
      Right (prop, ra, dec) -> do
        con <- getConstellation ra dec
        case toSOE ocat con of
          Left emsg -> return (Left emsg)
          Right so -> return (Right (prop, so))


slen :: [a] -> T.Text
slen = T.pack . show . length

{-

I have never gotten around to working on the overlap code!

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

dumpScienceObs :: ScienceObs -> IO ()
dumpScienceObs ScienceObs{..} = do
  T.putStrLn "------ dump"
  print (_unSequence soSequence)
  T.putStrLn (fromObsIdStatus soStatus)
  print (fromObsId soObsId)
  T.putStrLn (fromTargetName soTarget)
  T.putStrLn (case soStartTime of
               Nothing -> "** Observation has no scheduled observation date"
               Just t -> showCTime t)
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

  T.putStrLn (maybe "No TOO constraint" trValue soTOO)
  T.putStrLn (showRA soRA)
  T.putStrLn (showDec soDec)
  T.putStrLn ("which is in constellation: " <> fromConShort soConstellation)
  print soRoll
  print soSubArrayStart
  print soSubArraySize

