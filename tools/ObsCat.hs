{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Access data from the Chandra observational catalog and add it
--   to the database.
--
--   Usage:
--       obscat [obsid]
--
--     where the obsid argument is for debugging
--

{-
TODO: extract more info
TODO: how to update existing info
-}

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Arrow ((&&&))
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)

import Data.Char (ord)
import Data.Maybe (catMaybes, isNothing, listToMaybe)
import Data.Time (readsTime)
import Data.Word (Word8)

import Database.Groundhog.Postgresql

import Network (withSocketsDo)
import Network.HTTP.Conduit

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Locale (defaultTimeLocale)

import Types

queryURL :: ObsIdVal -> String
queryURL oid = 
  let oi = show $ fromObsId oid
  in "http://cda.harvard.edu/srservices/ocatDetails.do?obsid=" ++ oi ++ "&format=text"

w8 :: Char -> Word8
w8 = fromIntegral . ord

type OCAT = M.Map L.ByteString L.ByteString

maybeRead :: Read a => L.ByteString -> Maybe a
maybeRead = fmap fst . listToMaybe . reads . L8.unpack

toWrapper :: Read a => (a -> b) -> L.ByteString -> OCAT -> Maybe b
toWrapper f k m = M.lookup k m >>= fmap f . maybeRead

toSequence :: OCAT -> Maybe Sequence
toSequence = toWrapper Sequence "SEQ_NUM"

toPropNum :: OCAT -> Maybe PropNum
toPropNum = toWrapper PropNum "PR_NUM"

toObsId :: OCAT -> Maybe ObsIdVal
toObsId = toWrapper ObsIdVal "OBSID"

toTimeKS :: OCAT -> L.ByteString -> Maybe TimeKS
toTimeKS m k = toWrapper TimeKS k m

-- A value of 0.0 is converted to @Nothing@.
toJointTime :: OCAT -> L.ByteString -> Maybe TimeKS
toJointTime m k = toTimeKS m k >>= \t -> if t <= TimeKS 0.0 then Nothing else Just t

toString :: OCAT -> L.ByteString -> Maybe String
toString m lbl = fmap L8.unpack $ M.lookup lbl m

toRead :: Read a => OCAT -> L.ByteString -> Maybe a
toRead m lbl = toWrapper id lbl m

toCT :: OCAT -> L.ByteString -> Maybe ChandraTime
toCT m lbl =
  let toC = fmap (ChandraTime . fst) . listToMaybe . readsTime defaultTimeLocale "%F %T"
  in M.lookup lbl m >>= toC . L8.unpack

toRA :: OCAT -> Maybe RA
toRA = 
  let tR lbs = 
        let (h:m:s:_) = map (read . L8.unpack) $ L.split (w8 ' ') lbs
        in RA $ 15.0 * (h + (m + s/60.0) / 60.0) 
  in fmap tR . M.lookup "RA"

toDec :: OCAT -> Maybe Dec
toDec = 
  let tD lbs = 
        let (d:m:s:_) = map (read . L8.unpack) $ L.split (w8 ' ') rest
            (sval, rest) = case L.uncons lbs of
                     Just (c, cs) | c == w8 '-' -> ((-1), cs)
                                  | c == w8 '+' -> (1, cs)
                                  | otherwise   -> (1, lbs) -- should not happen but just in case
                     _ -> (0, "0 0 0") -- if it's empty we have a problem
        in Dec $ sval * (abs d + (m + s/60.0) / 60.0) 
  in fmap tD . M.lookup "Dec"

toInstrument :: OCAT -> Maybe Instrument
toInstrument m = 
  let toI "ACIS-I" = Just ACISI
      toI "ACIS-S" = Just ACISS
      toI "HRC-I"  = Just HRCI
      toI "HRC-S"  = Just HRCS
      toI _ = Nothing
  in M.lookup "INSTR" m >>= toI . L8.unpack

-- TODO: check these are the serializations used by OCAT
toGrating :: OCAT -> Maybe Grating
toGrating m = 
  let toG "NONE" = Just NONE
      toG "HETG" = Just HETG
      toG "LETG" = Just LETG
      toG _ = Nothing
  in M.lookup "GRAT" m >>= toG . L8.unpack

toCS :: OCAT -> L.ByteString -> Maybe ChipStatus
toCS m k = M.lookup k m >>= toChipStatus . L8.unpack

toProposal :: OCAT -> Maybe Proposal
toProposal m = do
  pNum <- toPropNum m
  pName <- toString m "PROP_TITLE"
  piName <- toString m "PI_NAME"
  cat <- toString m "CATEGORY"
  pType <- toString m "TYPE"
  pCycle <- toString m "PROP_CYCLE"
  return Proposal {
        propNum = pNum
        , propName = pName
        , propPI = piName
        , propCategory = cat
        , propType = pType
        , propCycle = pCycle
     }

toSO :: OCAT -> Maybe ScienceObs
toSO m = do
  seqNum <- toSequence m
  pNum <- toPropNum m
  status <- toString m "STATUS"
  obsid <- toObsId m
  target <- toString m "TARGET_NAME"
  sTime <- toCT m "START_DATE"
  appExp <- toTimeKS m "APP_EXP"
  let obsExp = toTimeKS m "EXP_TIME"
  inst <- toInstrument m
  grat <- toGrating m
  let det = fmap L8.unpack $ M.lookup "READOUT_DETECTOR" m
  datamode <- toString m "DATAMODE"

  -- are these included in HRC obs?
  acisi0 <- toCS m "I0"
  acisi1 <- toCS m "I1"
  acisi2 <- toCS m "I2"
  acisi3 <- toCS m "I3"
  aciss0 <- toCS m "S0"
  aciss1 <- toCS m "S1"
  aciss2 <- toCS m "S2"
  aciss3 <- toCS m "S3"
  aciss4 <- toCS m "S4"
  aciss5 <- toCS m "S5"

  let jnames = M.lookup "JOINT" m >>= \ns -> if ns == "None" then Nothing else Just (L8.unpack ns)

      hst = toJointTime m "HST"
      noao = toJointTime m "NOAO"
      nrao = toJointTime m "NRAO"
      rxte = toJointTime m "RXTE"
      spitzer = toJointTime m "SPITZER"
      suzaku = toJointTime m "SUZAKU"
      xmm = toJointTime m "XMM"
      swift = toJointTime m "SWIFT"
      nustar = toJointTime m "NUSTAR"

  let too = fmap L8.unpack $ M.lookup "TOO_TYPE" m
  ra <- toRA m
  dec <- toDec m
  roll <- toRead m "SOE_ROLL"
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

    -- , soJointWith = []  -- TODO  [(String, TimeKS)] -- could use an enumeration
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
    , soRoll = roll
    , soACISChIPS = Nothing -- :: Maybe String -- 10 character string with Y/N/<integer> for optional values
    , soSubArrayStart = subStart
    , soSubArraySize = subSize
     }

{-

SEQ_NUM	STATUS	OBSID	PR_NUM	TARGET_NAME	GRID_NAME	INSTR	GRAT	TYPE	OBS_CYCLE	PROP_CYCLE	CHARGE_CYCLE	START_DATE	PUBLIC_AVAIL	READOUT_DETECTOR	DATAMODE	JOINT	HST	NOAO	NRAO	RXTE	SPITZER	SUZAKU	XMM	SWIFT	NUSTAR	CATEGORY	SEG_MAX_NUM	PROP_TITLE	PI_NAMEOBSERVER	APP_EXP	EXP_TIME	RA	Dec	SOE_ROLL	TIME_CRIT	Y_OFF	Z_OFF	X_SIM	Z_SIM	RASTER	OBJ_TYPE	OBJ	NUDGE	PHOTO	VMAG	EST_CNT_RATE	FORDER_CNT_RATE	COUNT_RATE	EVENT_COUNT	DITHER	Y_AMP	Y_FREQ	Y_PHASE	Z_AMP	Z_FREQ	Z_PHASE	ROLL	WINDOW	UNINT	MONITOR	PRE_ID	MON_MINMON_MAX	GROUP_ID	CONSTR	EPOCH	PERIOD	PSTART	PS_MARG	PEND	PE_MARG	TOO_TYPE	TOO_START	TOO_STOP	SIMODE	HRC	SPECT_MODE	BLANK_ENU_HI	V_HI	U_LO	V_LO	TIMING	Z_BLK	ACIS	MODE	BEP_PACK	DROPPED_CHIP_CNT	I0	I1	I2	I3	S0	S1	S2	S3	S4	S5	SPECTRA_MAX_COUNT	MULTIPLE_SPECTRAL_LINES	SUBARY	STRT_ROW	ROW_CNT	D_CYC	SEC_CNT	PR_TIME	SEC_TIME	F_TIME	OC_SUM	OC_ROW	OC_COL	EVFIL	EVFIL_LO	EVFIL_RA	EFFICIENT	SPWIN
901116	scheduled	16196	15900142	30 Doradus		ACIS-I	NONE	GO	15	15	15	2014-05-30 00:22:47			VFAINT	None	0.0	0.0	0.0	0.0	0.0	0.0	0.0	0.0	0.0	EXTRAGALACTIC DIFFUSE EMISSION AND SURVEYS	0	The Tarantula -- Revealed by X-rays (T-ReX): A Definitive Chandra Investigation of 30 Doradus	Townsley	Townsley	68.00		05 38 42.40	-69 06 02.90	202.00286	N	-0.2	-0.25			N	NO	NONE		N		0.05					0.0	0.0	0.0	0.0	0.0	0.0	NN	N	N					N	0.0	0.0	0.0	0.0	0.0	0.0		0.0	0.0	TE_004DE	N		YTE	VF	0	Y	Y	Y	Y	N	N	O1	O2	N	N	20000.0	Y	NONE	0	0	N	0	0.0	0.0	0.0	N	1	1	Y	0.1	12.0	Y	N

-}

-- | Query the OCat about this observation.
--
--   This function errors out most ungracefully if multiple matches
--   are found or if one/both of the output values can not be
--   created.
--
queryObsId :: 
  Bool         -- set @True@ for debug output 
  -> ObsIdVal 
  -> IO (Maybe (Proposal, ScienceObs))
queryObsId flag oid = do
  let qry = queryURL oid
  rsp <- simpleHttp qry
  let isHash x = case L.uncons x of 
                   Just (y, _) -> y == w8 '#'
                   _ -> False

      ls = dropWhile isHash $ L.split (w8 '\n') rsp
      tokenize = L.split (w8 '\t')
      hdr = tokenize $ head ls
      -- could use positional information, but use a map instead
      dl = filter (not . L.null) $ drop 2 ls
      out = map (M.fromList . dropNulls . zip hdr . tokenize) dl

      dropNulls = filter (not . L.null . snd)

      ans = map (toProposal &&& toSO) out

  when flag $ forM_ (zip [(1::Int)..] out) $ \(i,m) -> do
    putStrLn $ "##### Map " ++ show i ++ " START #####"
    forM_ (M.toList m) print     
    putStrLn $ "##### Map " ++ show i ++ " END #####"

  case ans of
    [] -> return Nothing
    [(Just p, Just so)] -> return $ Just (p, so)
    _ -> error $ "ObsId " ++ show (fromObsId oid) ++ " - Expected 0 or 1 matches, found " ++ show ans

-- | Query the database to find any scheduled observations
--   which do not have any correspinding OCAT info, and
--   those OCAT observations which are not archived
--   (i.e. may need to be changed)
--
findMissingObsIds :: IO ([ObsIdVal], [ObsIdVal])
findMissingObsIds = do
  (want, have, unarchived) <- withPostgresqlConn "user=postgres password=postgres dbname=chandraobs host=127.0.0.1" $ 
    runDbConn $ do
      handleMigration
      allObsIds <- project SiObsIdField $ (SiScienceObsField ==. True)
      haveObsIds <- project SoObsIdField $ CondEmpty      
      unArchivedObsIds <- project SoObsIdField $ (SoStatusField /=. ("archived" :: String))
      return (allObsIds, haveObsIds, unArchivedObsIds)

  let swant = S.fromList want
      shave = S.fromList have
  return $ (S.toList (swant `S.difference` shave), unarchived)

slen :: [a] -> String
slen = show . length

-- | Add the "missing" results to the database and replace the
--   "unarchived" results (they may or may not have changed but
--   easiest for me is just to update the database).
--
addResults :: [(Proposal, ScienceObs)] -> [(Proposal, ScienceObs)] -> IO ()
addResults [] [] = putStrLn "# No data needs to be added to the database."
addResults missing unarchived = do
  putStrLn $ "# Adding " ++ slen missing ++ " missing results"
  putStrLn $ "# Adding " ++ slen unarchived ++ " unarchived results"
  let props = S.toList $ S.fromList $ map fst missing ++ map fst unarchived
  withPostgresqlConn "user=postgres password=postgres dbname=chandraobs host=127.0.0.1" $ 
    runDbConn $ do
      -- the following will fail if the data already exists (e.g. proposals and unarchived results)
      forM_ missing $ \(_,so) -> liftIO (print so) >> insert so
      forM_ unarchived $ \(_,so) -> liftIO (print so) >> insert so
      forM_ props $ \p -> liftIO (print p) >> insert p

-- | Report if any obsids are missing from the OCAT results.
--
check :: [Maybe (Proposal, ScienceObs)] -> [ObsIdVal] -> IO [(Proposal, ScienceObs)]
check ms os = do
  let missing = map snd $ filter (isNothing . fst) $ zip ms os
  when (not (null missing)) $ do
    putStrLn $ "### There are " ++ slen missing ++ " ObsIds with no OCAT data:"
    forM_ missing $ print . fromObsId
  return $ catMaybes ms

updateDB :: IO ()
updateDB = withSocketsDo $ do
  putStrLn "# Querying the database"
  (missing, unarchived) <- findMissingObsIds
  putStrLn $ "# Found " ++ slen missing ++ " missing and " ++ slen unarchived ++ " unarchived ObsIds"

  when (not (null missing)) $ putStrLn "# Processing missing"
  res1 <- mapM (queryObsId False) missing
  mres <- check res1 missing

  when (not (null missing)) $ putStrLn "# Processing unarchived"
  res2 <- mapM (queryObsId False) unarchived
  ures <- check res2 unarchived

  addResults mres ures

viewObsId :: Int -> IO ()
viewObsId oid = withSocketsDo $ do
  ans <- queryObsId True (ObsIdVal oid)
  case ans of
    Nothing -> putStrLn $ "Nothing found for ObsId " ++ show oid ++ "!"
    Just (prop, so) -> do
      putStrLn $ "## ObsId " ++ show oid
      print prop
      print so
      dump so

dump :: ScienceObs -> IO ()
dump ScienceObs{..} = do
  putStrLn "------ dump"
  print (_unSequence soSequence)
  putStrLn soStatus
  print (fromObsId soObsId)
  putStrLn soTarget
  putStrLn $ showCTime soStartTime
  putStrLn $ showExpTime soApprovedTime
  putStrLn $ show (fmap showExpTime soObservedTime)
  print soInstrument
  print soGrating
  print soDetector
  putStrLn soDataMode

  let achip lbl val = putStrLn $ "ACIS" ++ lbl ++ " " ++ fromChipStatus val
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
  let joint lbl val = putStrLn $ "Joint " ++ lbl ++ " " ++ show (fmap _toS val)
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
  putStrLn $ showRA soRA
  putStrLn $ showDec soDec
  print soRoll
  print soACISChIPS
  print soSubArrayStart
  print soSubArraySize

usage :: IO ()
usage = do
  pName <- getProgName
  hPutStrLn stderr $ "Usage: " ++ pName ++ " [obsid]"
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> updateDB
    [x] -> case fmap fst . listToMaybe $ reads x of
             Just oid -> viewObsId oid
             _ -> usage
    _ -> usage

