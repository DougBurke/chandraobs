{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Access data from the Chandra observational catalog and add it
--   to the database.
--
--   Usage:
--       obscat [debug]
--       obscat [obsid]
--       obscat simbad <name>
--
--     where the obsid and simbad/<name> arguments are for debugging
--
-- TODO:
--    extract more info
--    how to update existing info
--

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT)

import Data.Char (ord)
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe)
import Data.Monoid ((<>), mconcat)
import Data.Time (UTCTime, getCurrentTime, readsTime)
import Data.Word (Word8)

import Database.Groundhog.Postgresql

import Network (withSocketsDo)
import Network.HTTP.Conduit

import System.Environment (getArgs, getEnv, getProgName)
import System.Exit (ExitCode(ExitSuccess), exitFailure)
import System.IO (hFlush, hGetLine, hPutStrLn, stderr)
import System.IO.Temp (withSystemTempFile)
import System.Locale (defaultTimeLocale)
import System.Process (readProcessWithExitCode, system)

import Database (insertScienceObs
                , insertProposal
                , insertSimbadInfo
                , insertSimbadSearch)
import Types

-- | What is the URL needed to query the ObsCat?
queryObsCat :: ObsIdVal -> String
queryObsCat oid = 
  let oi = show $ fromObsId oid
  in "http://cda.harvard.edu/srservices/ocatDetails.do?obsid=" ++ oi ++ "&format=text"

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
          coords = show long ++ " " ++ show lat
          cstr = "prop_precess from j/deg to con p0: " ++ inName ++ ": " ++ outName

          full = ". " ++ path ++ "/bin/ciao.bash > /dev/null; " ++ cstr ++ " > /dev/null"
      
      hPutStrLn inHdl coords
      hFlush inHdl
      
      rval <- system full
      when (rval /= ExitSuccess) $ do
        hPutStrLn stderr $ "ERROR: unable to run prop_precess on " ++ coords ++ "\n  " ++ cstr ++ "\n"
        exitFailure
    
      cName <- hGetLine outHdl
      return $ fromMaybe (error ("Unexpected constellation short form: '" ++ cName ++ "'")) $ toConShort cName
      
-- | What publically-available observations overlap this one?
--
--   The overlap is found using the find_chandra_obsid script,
--   using a max radius of 10 arcminutes.
getOverlaps :: ObsIdVal -> RA -> Dec -> IO [OverlapObs]
getOverlaps oid ra dec = do

  -- this raises an exception if ASCDS_INSTALL is not set
  -- path <- getEnv "ASCDS_INSTALL"

  let long = show $ _unRA ra
      lat = show $ _unDec dec
      args = [long,lat, "radius=10"]

  -- do we need to set up the CIAO environment as we did above? probably
  (rval, sout, serr) <- readProcessWithExitCode "find_chandra_obsid" args ""
  when (rval /= ExitSuccess) $ do
    hPutStrLn stderr $ "ERROR: unable to run find_chandra_obsid on " ++ show args ++ "\n  " ++ serr ++ "\n"
    exitFailure
    
  now <- getCurrentTime
  let toO xs = 
        -- this does not handle invalid output
        let (idvalstr:rdiststr:_) = words xs 
            idval = ObsIdVal $ read idvalstr
            rdist = read rdiststr
        in OverlapObs oid idval rdist now

  return $ map toO $ drop 1 $ lines sout  

-- | Try and clean up SIMBAD identifiers:
--
--   NAME xxx -> xxx (seen with NAME Chandra Deep Field South)
--
cleanupName :: String -> String
cleanupName s =
  if "NAME " `isPrefixOf` s
  then drop 5 s
  else s

-- use ADS mirror first.
--
-- Note that I choose to use a text-based format for returning the data,
-- rather than a VoTable, since it's easier to create a parser for the
-- former.
--
-- TODO:
--    look for opportunities like "blah blah offset[...]" where "blah blah"
--    is a match; of course, how do we then encode/match this to
--    ScienceObservation? How about "M31 BHXNe"
--

-- TODO: XXX how to clean up the name ??

querySIMBAD :: 
  Bool       -- ^ @True@ for debug output
  -> ObsIdVal 
  -> String  -- ^ object name
  -> IO (SimbadSearch, Maybe SimbadInfo)
querySIMBAD f obsid objname = do
  putStrLn $ "Querying SIMBAD for " ++ objname
  let -- we POST the script to SIMBAD
      script = mconcat [
                 "format object \"%MAIN_ID\t%OTYPE(3)\t%OTYPE(V)\t%COO(d;A D)\\n\"\n"
                 , "query id ", BS8.pack searchTerm, "\n" ]

      -- TODO: support roll over to Strasbourg site if the ADS mirror
      --       is not accessible
      -- uriBase = "http://simbad.u-strasbg.fr/"
      uriBase = "http://simbad.harvard.edu/"
      uri = uriBase <> "simbad/sim-script"

      -- TODO: "clean" the tatget name of known "issues"
      --       that make Simbad matches fail
      searchTerm = objname

  when f $ putStrLn ">> Script:" >> print script

  cTime <- getCurrentTime
  req <- parseUrl uri
  let hdrs = ("User-Agent", "chandraobs-obscat") : requestHeaders req
      req' = urlEncodedBody [("script", script)] $ req { requestHeaders = hdrs }
  rsp <- withManager $ httpLbs req'
  let body = L8.unpack $ responseBody rsp
      -- TODO: need to handle data that does not match expectations
      --       eg if there's an error field, display it and return nothing ...
      dropUntilNext c = drop 1 . dropWhile (not . c)
      ls = dropWhile null $ dropUntilNext ("::data::" `isPrefixOf`) $ lines body

  when f $ putStrLn ">> Response:" >> putStrLn body >> putStrLn ">> object info:" >> print ls
  let rval = listToMaybe ls >>= parseObject 
  -- TODO: should have displayed the error string so this can be ignored
  when (isNothing rval) $ putStrLn " -- no match found" >> putStrLn " -- response:" >> putStrLn body

  let searchInfo = SimbadSearch {
                     smsObsId = obsid
                     , smsSearchTerm = searchTerm
                     , smsLastChecked = cTime
                     }

      results = case rval of
        Just (a,b,c) -> Just SimbadInfo {
                                  smObsId = obsid
                                  , smTarget = objname
                                  , smName = a
                                  , smType3 = b
                                  , smType = c
                                }
        Nothing      -> Nothing

  return (searchInfo, results)

-- | Assume we have a line from SIMBAD using the script interface using the
--   format given in querySIMBAD.
parseObject :: String -> Maybe (String, SimbadType, String)
parseObject txt = 
  let toks = splitOn "\t" txt
      toT s = fromMaybe (error ("Simbad Type > 3 characters! <" ++ s ++ ">"))
                     $ toSimbadType s
  in case toks of
    (name:otype3:otype:_:[]) -> Just (cleanupName name, toT otype3, otype)
    _ -> Nothing

w8 :: Char -> Word8
w8 = fromIntegral . ord

type OCAT = M.Map L.ByteString L.ByteString

maybeReadLBS :: Read a => L.ByteString -> Maybe a
maybeReadLBS = maybeRead . L8.unpack

toWrapper :: Read a => (a -> b) -> L.ByteString -> OCAT -> Maybe b
toWrapper f k m = M.lookup k m >>= fmap f . maybeReadLBS

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
toString m lbl = L8.unpack <$> M.lookup lbl m

toRead :: Read a => OCAT -> L.ByteString -> Maybe a
toRead m lbl = toWrapper id lbl m

toCT :: OCAT -> L.ByteString -> Maybe ChandraTime
toCT m lbl = ChandraTime `fmap` toUTC m lbl

toUTC :: OCAT -> L.ByteString -> Maybe UTCTime
toUTC m lbl =
  let c = fmap fst . listToMaybe . readsTime defaultTimeLocale "%F %T"
  in M.lookup lbl m >>= c . L8.unpack

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
                     Just (c, cs) | c == w8 '-' -> (-1, cs)
                                  | c == w8 '+' -> (1, cs)
                                  | otherwise   -> (1, lbs) -- should not happen but just in case
                     _ -> (0, "0 0 0") -- if it's empty we have a problem
        in Dec $ sval * (abs d + (m + s/60.0) / 60.0) 
  in fmap tD . M.lookup "Dec"

toInst :: OCAT -> Maybe Instrument
toInst m = M.lookup "INSTR" m >>= toInstrument . L8.unpack

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

toC :: OCAT -> L.ByteString -> Maybe Constraint
toC m k = do
  val <- M.lookup k m
  case L8.uncons val of
    Just (c, rest) -> if L8.null rest
                      then toConstraint c
                      else error $ "Constraint value " ++ L8.unpack k ++ "=" ++ L8.unpack val
    _ -> Nothing

toProposal :: OCAT -> Maybe Proposal
toProposal m = do
  pNum <- toPropNum m
  piName <- toString m "PI_NAME"
  cat <- toString m "CATEGORY"
  pType <- toString m "TYPE"
  pCycle <- toString m "PROP_CYCLE"
  -- have seen one proposal (cycle 00, calibration obs) without a title
  -- pName <- toString m "PROP_TITLE"
  let pName = fromMaybe (if pType == "CAL" then "Calibration Observation" else "Unknown") $ toString m "PROP_TITLE"
  return Proposal {
        propNum = pNum
        , propName = pName
        , propPI = piName
        , propCategory = cat
        , propType = pType
        , propCycle = pCycle
     }

-- | Note, this is an *INCOMPLETE* ScienceObs since the
--   @soConstellation@ field is filled with an error statement;
--   this is because we need IO to fill in this field so it's
--   left to a later pass.  
--
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

  let relDate = toUTC m "PUBLIC_AVAIL"

  timeCrit <- toC m "TIME_CRIT"
  monitor <- toC m "MONITOR"
  constrained <- toC m "CONSTR"

  inst <- toInst m
  grat <- toGrating m
  let det = L8.unpack <$> M.lookup "READOUT_DETECTOR" m
  let datamode = toString m "DATAMODE"

  haveACIS <- toString m "ACIS"
  -- haveHRC <- toString m "HRC"

  -- are these included in HRC obs? No
  let convCS = if haveACIS == "Y"
               then toCS m
               else const (Just ChipOff)

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

  let too = L8.unpack <$> M.lookup "TOO_TYPE" m
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
    , soConstellation = error $ "The constellation field for ObsId " ++ show (fromObsId obsid) ++ " has not been filled in!"              
    , soRoll = roll
    , soSubArrayStart = subStart
    , soSubArraySize = subSize
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
  let qry = queryObsCat oid
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
    [(Just p, Just so)] -> do
      so2 <- addConstellation so
      return $ Just (p, so2)
    _ -> error $ "ObsId " ++ show (fromObsId oid) ++ " - Expected 0 or 1 matches, found " ++ show ans

-- | Run a database action.
doDB :: DbPersist Postgresql (NoLoggingT IO) a -> IO a
doDB = 
  withPostgresqlConn "user=postgres password=postgres dbname=chandraobs host=127.0.0.1" .
  runDbConn 

-- | Query the database to find any scheduled observations
--   which do not have any correspinding OCAT info, and
--   those OCAT observations which are not archived
--   (i.e. may need to be changed)
--
findMissingObsIds :: IO ([ObsIdVal], [ObsIdVal])
findMissingObsIds = do
  (want, have, unarchived) <- doDB $ do
      handleMigration
      allObsIds <- project SiObsIdField (SiScienceObsField ==. True)
      haveObsIds <- project SoObsIdField CondEmpty      
      unArchivedObsIds <- project SoObsIdField (SoStatusField /=. ("archived" :: String))
      return (allObsIds, haveObsIds, unArchivedObsIds)

  let swant = S.fromList want
      shave = S.fromList have
  return (S.toList (swant `S.difference` shave), unarchived)

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
  doDB $ do
      forM_ missing $ \(_,so) -> liftIO (print so) >> insertScienceObs so
      forM_ unarchived $ \(_,so) -> liftIO (print so) >> insertScienceObs so
      forM_ props $ \p -> liftIO (print p) >> insertProposal p

-- | Query SIMBAD for any targets we do not know about.
--
--   For now we do not take advantage of the "age" field
--   of the simbad search structure; i.e. we do not
--   re-search old results, or even check to see if the
--   logic for searches has changed (i.e. compare the field name,
--   after \"cleaning\" to that used in the previous search).
--
--   Also, the comparison is per obsid rather than search name,
--   so multiple observations of the same field will end up
--   repeating the same simbad query.
-- 
identifyObjects :: [ScienceObs] -> IO ()
identifyObjects sos = do
  putStrLn $ "## Checking SIMBAD for " ++ slen sos ++ " targets"
  dobsids <- doDB $ project SmsObsIdField CondEmpty
  let dset = S.fromList dobsids
      todos = filter (flip S.member dset . fst) $ map (soObsId &&& soTarget) sos
  putStrLn $ "### After checking database, querying " ++ slen todos ++ " targets"
  rsps <- mapM (uncurry (querySIMBAD False)) todos
  let (searches, minfos) = unzip rsps
      ans = catMaybes minfos
  putStrLn $ "## Found " ++ slen ans ++ " results from " ++ slen searches ++ "searches"
  doDB $ forM_ ans insertSimbadInfo 
  doDB $ forM_ searches insertSimbadSearch

-- | add in the overlap observations; that is, the table of
--   overlaps and information about the overlap obs if it is
--   not known about.
--
--   We do not bother finding overlaps of these overlaps.
--
addOverlaps :: Bool -> [OverlapObs] -> IO ()
addOverlaps f os = do
  let oids = S.toList $ S.fromList $ map ovOverlapId os
  putStrLn $ "# Processing " ++ slen os ++ " overlaps with " ++ slen oids ++ " different ObsIds"

  let countOid oid = count (SoObsIdField ==. oid)
  cs <- doDB $ forM oids countOid 
  let unknowns = map fst $ filter ((==0) . snd) $ zip oids cs
  putStrLn $ "# Of these, there are " ++ slen unknowns ++ " 'new' ObsIds"

  res1 <- mapM (queryObsId f) unknowns
  res <- check res1 unknowns

  let (props1, sobs) = unzip res
      props = S.toList $ S.fromList props1

  putStrLn $ "# Adding " ++ slen sobs ++ " observations"
  putStrLn $ "# Adding " ++ slen props ++ " proposals" 
  doDB $ do
    -- could get away with insert for sobs if we can assume that
    -- no other process is updating the database
    forM_ sobs insertScienceObs
    forM_ props insertProposal

-- | Report if any obsids are missing from the OCAT results.
--
--   This does NOT include the overlap obs (i.e. they are ignored here)
check :: [Maybe a] -> [ObsIdVal] -> IO [a]
check ms os = do
  let missing = map snd $ filter (isNothing . fst) $ zip ms os
  unless (null missing) $ do
    putStrLn $ "### There are " ++ slen missing ++ " ObsIds with no OCAT data:"
    forM_ missing $ print . fromObsId
  return $ catMaybes ms

-- | The flag is @True@ to get debug output from the @queryObsId@ calls.
updateDB :: Bool -> IO ()
updateDB f = withSocketsDo $ do
  putStrLn "# Querying the database"
  (missing, unarchived) <- findMissingObsIds
  putStrLn $ "# Found " ++ slen missing ++ " missing and " ++ slen unarchived ++ " unarchived ObsIds"

  unless (null missing) $ putStrLn "# Processing missing"
  res1 <- mapM (queryObsId f) missing
  mres <- check res1 missing

  unless (null missing) $ putStrLn "# Processing unarchived"
  res2 <- mapM (queryObsId f) unarchived
  ures <- check res2 unarchived

  addResults mres ures

  -- Look for any new objects (only worth doing this in the
  -- new set of objects since I am assuming the SIMBAD 
  -- update rate for the types of object name we will find as
  -- Chandra targets is low. That is not to say we should not
  -- periodically look for updated or new information on
  -- existing targets.
  --
  identifyObjects $ map snd mres

  -- add in the overlaps
  let sos = map snd mres ++ map snd ures
  putStrLn $ "## Processing " ++ slen sos ++ " observations for overlaps"
  overlaps <- forM sos $ \ScienceObs{..} -> getOverlaps soObsId soRA soDec
  addOverlaps f $ concat overlaps

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
  print $ fmap showExpTime soObservedTime
  putStrLn $ "Public availability: " ++ show soPublicRelease

  let fC lbl c = lbl ++ ": " ++ [fromConstraint c]
  putStrLn $ fC "time critical" soTimeCritical
  putStrLn $ fC "      monitor" soMonitor
  putStrLn $ fC "  constrained" soConstrained

  print soInstrument
  print soGrating
  print soDetector
  print soDataMode

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
  putStrLn $ "which is in " ++ fromConShort soConstellation
  print soRoll
  print soSubArrayStart
  print soSubArraySize

printSimbadInfo :: SimbadInfo -> IO ()
printSimbadInfo SimbadInfo{..} = do
  putStrLn "## SIMBAD response"
  putStrLn $ " target: " ++ smTarget
  putStrLn $ "   name: " ++ smName
  putStrLn $ "   type: " ++ fromSimbadType smType3
  putStrLn $ "   type: " ++ smType

usage :: IO ()
usage = do
  pName <- getProgName
  hPutStrLn stderr $ "Usage: " ++ pName
  hPutStrLn stderr $ "       " ++ pName ++ " debug"
  hPutStrLn stderr $ "       " ++ pName ++ " <obsid>"
  hPutStrLn stderr $ "       " ++ pName ++ " simbad <name>"
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> updateDB False
    [x] | x == "debug" -> updateDB True
        | otherwise -> case fmap fst . listToMaybe $ reads x of
             Just oid -> viewObsId oid
             _ -> usage
    ("simbad":name:[]) -> 
      querySIMBAD True (ObsIdVal 1) name 
      >>= maybe (putStrLn ("No match found for: " ++ name))
                printSimbadInfo . snd

    _ -> usage

