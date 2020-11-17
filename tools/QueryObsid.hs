{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

-- Report on an observation, either from the database or from OCAT.
--
-- Usage:
--    ./queryobsid obsid
--        --dump
--        --ocat
--
-- Query the database or OCAT for the observation. The --dump and
-- --ocat flags can not be used together.
--

module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad (forM_, unless, when)

import Database.Groundhog.Postgresql ( (==.) )
       
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (stderr)

import Database (getSimbadInfo, maybeSelect, runDb)
import OCAT (OCAT, dumpScienceObs, isScienceObsE, queryOCAT, ocatToScience)
import Types ( ObsIdVal
             , ScienceObs(..)
             , Proposal(..)
             , SimbadInfo(..)
             , TargetName
             , Constraint(NoConstraint)
             , Instrument(ACISI, ACISS)
             , ChipStatus(ChipOff)
             , fromObsId, toObsIdValStr
             , fromTargetName
             , fromObsIdStatus
             , fromPropNum
             , tooValue, tooTime, rtToLabel
             , showRA
             , showDec
             , showCTime
             , showExpTime
             , fromInstrument
             , fromGrating
             , fromConstraint
             , fromTelescope
             , fromChipStatus
             , fromSimbadType
             , Field(SoObsIdField, PropNumField))


usage :: IO ()
usage = do
  pName <- T.pack <$> getProgName
  T.hPutStrLn stderr ("Usage: " <> pName <> " obsid")
  T.hPutStrLn stderr "    --dump"
  T.hPutStrLn stderr "    --ocat"
  exitFailure


main :: IO ()
main = do
  args <- getArgs
  let dump = "--dump" `elem` args
      ocat = "--ocat" `elem` args
      nargs = filter (\x -> x `notElem` ["--dump", "--ocat"]) args

  case nargs of
    [ostr] | Just oi <- toObsIdValStr (T.pack ostr) -> setupQuery oi dump ocat
    _ -> usage


setupQuery :: ObsIdVal -> Bool -> Bool -> IO ()
setupQuery obsid _ True = do
  T.putStrLn ("# Querying OCAT for " <> T.pack (show (fromObsId obsid)))
  res <- queryOCAT [obsid]
  case res of
    Right [(_, Just ocat)] -> reportOCAT ocat
    Right _ -> T.hPutStrLn stderr "UNABLE TO QUERY OCAT DATA!"
               >> exitFailure
    Left emsg -> T.hPutStrLn stderr ("FAILED: " <> emsg)
                 >> exitFailure

setupQuery obsid dump _ = do
  T.putStrLn ("# Querying the database for " <> T.pack (show (fromObsId obsid)))
  res <- runDb (maybeSelect (SoObsIdField ==. obsid))
  case res of
    Nothing -> T.putStrLn "ERROR: no observation was found."
               >> exitFailure
    Just obs -> if dump
                then dumpScienceObs obs
                else queryObsId obs


reportOCAT :: OCAT -> IO ()
reportOCAT ocat =
  case isScienceObsE ocat of
    Left emsg -> T.hPutStrLn stderr emsg
                 >> exitFailure
    Right flag -> if flag
                  then processScience ocat
                  else T.putStrLn "An engineering observation"


-- We do not look for SIMBAD info here.
--
processScience :: OCAT -> IO ()
processScience ocat = do
  mobs <- ocatToScience ocat
  case mobs of
    Right (prop, obs) -> reportScience obs (Just prop) Nothing
    Left emsg -> T.hPutStrLn stderr emsg
                 >> exitFailure
                 

queryObsId :: ScienceObs -> IO ()
queryObsId obs = do
  mprop <- runDb (maybeSelect (PropNumField ==. soProposal obs))
  msim <- runDb (getSimbadInfo (soTarget obs))
  reportScience obs mprop msim


reportSimbad :: TargetName -> SimbadInfo -> IO ()
reportSimbad target SimbadInfo{..} = do
  when (smiName /= target) $
    T.putStrLn (" -> '" <> fromTargetName smiName <> "'")
  T.putStrLn ("    " <> fromSimbadType smiType3)
  T.putStrLn ("    " <> smiType)


reportProposal :: Proposal -> IO ()
reportProposal Proposal{..} = do
  T.putStrLn (" -> " <> propName)
  T.putStrLn ("    " <> propPI)
  T.putStrLn ("    " <> propCategory)
  T.putStrLn ("    " <> propType)
  T.putStrLn ("    " <> propCycle)


reportScience ::
  ScienceObs
  -> Maybe Proposal
  -> Maybe SimbadInfo
  -> IO ()
reportScience ScienceObs{..} mprop msim = do
  T.putStrLn ("ObsId:         " <> T.pack (show (fromObsId soObsId)))
  T.putStrLn ("Target:        '" <> fromTargetName soTarget <> "'")
  T.putStrLn ("RA:            " <> showRA soRA)
  T.putStrLn ("Dec:           " <> showDec False soDec)
  case msim of
    Just sim -> reportSimbad soTarget sim
    Nothing -> pure ()
    
  T.putStrLn ("Status:        " <> fromObsIdStatus soStatus)
  T.putStrLn ("Proposal:      " <> T.pack (show (fromPropNum soProposal)))
  case mprop of
    Just prop -> reportProposal prop
    Nothing -> pure ()

  case soTOO of
    Just too -> T.putStrLn ("TOO:           " <> tooValue too <>
                            " - " <> rtToLabel (tooTime too))
    Nothing -> pure ()
                            
  case soStartTime of
    Just st -> T.putStrLn ("Obs date:      " <> showCTime st)
    Nothing -> pure ()
    
  case soObservedTime of
    Just ot -> T.putStrLn ("Actual time:   " <> showExpTime ot)
    Nothing -> pure ()
    
  T.putStrLn ("Approved:      " <> showExpTime soApprovedTime)

  T.putStrLn ("Instrument:    " <> fromInstrument soInstrument)
  T.putStrLn ("Grating:       " <> fromGrating soGrating)

  case soJointWith of
    Just miss -> T.putStrLn ("Joint with:  " <> miss)
    Nothing -> pure ()

  when (soTimeCritical /= NoConstraint) $
    T.putStrLn ("Time critical: " <> T.singleton (fromConstraint soTimeCritical))
  when (soMonitor /= NoConstraint) $
    T.putStrLn ("Monitor:       " <> T.singleton (fromConstraint soMonitor))
  when (soConstrained /= NoConstraint) $
    T.putStrLn ("Constrained:   " <> T.singleton (fromConstraint soConstrained))

  when soMultiTel $
    T.putStrLn ("MultiTel:      " <> T.pack (show soMultiTelInt))

  forM_ soMultiTelObs $ \tel -> T.putStrLn ("  Other:" <> fromTelescope tel)
                
  case soDetector of
    Just det -> T.putStrLn ("Detector:      " <> det)
    Nothing -> pure ()
  case soDataMode of
    Just det -> T.putStrLn ("Data Mode:     " <> det)
    Nothing -> pure ()

  when (soInstrument `elem` [ACISI, ACISS]) $ do
    let put lbl status = unless (status == ChipOff) $
          T.putStrLn ("  ACIS-" <> lbl <> ": " <> fromChipStatus status)

    put "I0" soACISI0
    put "I1" soACISI1
    put "I2" soACISI2
    put "I3" soACISI3
    put "S0" soACISS0
    put "S1" soACISS1
    put "S2" soACISS2
    put "S3" soACISS3
    put "S4" soACISS4
    put "S5" soACISS5
