{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

--
-- Usage:
--    ./gettimeline
--
-- Aim:
--
-- Write, to stdout, the current "timeline" - a restricted set
-- of columns about each observation (observed or scheduled), in
-- time order (earliest first).
--

module Main (main) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T 
import qualified Data.Text.IO as T
import qualified Formatting as F

import Control.Monad (forM_, unless)

import Data.List (sort)
import Data.Maybe (fromMaybe)
-- import Data.Monoid ((<>))
import Data.Time ( UTCTime(..)
                 , timeOfDayToDayFraction
                 , timeToTimeOfDay
                 , toModifiedJulianDay)

import Database.Groundhog (PersistBackend, Conn, project)
import Database.Groundhog.Postgresql (SqlDb)

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (stderr)

import Database (getTimeline, runDb, isValidScienceObs)

import Sorted (fromSL)
import Types ( Field(SoObsIdField, SoRAField, SoDecField)
             , ScienceTimeline
             , ObsIdVal, RA, Dec
             , SimbadInfo(smiType3)
             , TargetName
             , PropNum, PropCategory
             , Proposal(propNum, propCategory)
             , fromChandraTime
             , fromGrating
             , fromInstrument
             , fromObsId
             , fromSimbadType
             , fromTargetName
             , fromTimeKS
             , fromRA, fromDec
             )


-- This could be more generic, but trying to identify where
-- specific conversions are needed for Text.
--
-- I think we could get away with just 'Int -> T.Text'.
--
showInt :: (Integral a, F.Buildable a) => a -> T.Text
showInt = F.sformat F.int

showFloat :: (Real a, F.Buildable a) => a -> T.Text
showFloat = F.sformat F.float

-- could convert to a string representation
showUTCTime :: UTCTime -> T.Text
showUTCTime ut =
  let x :: Double
      x = fromIntegral (toModifiedJulianDay (utctDay ut))
        + fromRational (timeOfDayToDayFraction (timeToTimeOfDay (utctDayTime ut)))
  in showFloat x
  

-- Write out, as tab-separated values
report :: [T.Text] -> IO ()
report = T.putStrLn . T.intercalate "\t"


-- the timeline doesn't include ra,dec - which we need...
--
getPos ::
  (PersistBackend m, SqlDb (Conn m))
  => m [(ObsIdVal, RA, Dec)]
getPos = project (SoObsIdField, SoRAField, SoDecField) isValidScienceObs


dumpScience ::
  M.Map TargetName SimbadInfo
  -> M.Map PropNum Proposal
  -> M.Map PropCategory Int
  -> M.Map ObsIdVal (RA, Dec)
  -> ScienceTimeline
  -> IO ()
dumpScience simbadMap proposalMap catMap posMap so =
  let (obsidVal, propnum, targetName, mStartTime, _,
       approvedTime, mObservedTime, instrument, grating, _, _, _) = so

      obsLen = fromTimeKS (fromMaybe approvedTime mObservedTime)

      si = maybe "n/a" (fromSimbadType . smiType3) (M.lookup targetName simbadMap)
      cat = case M.lookup propnum proposalMap of
        Just p -> maybe "-1" showInt (M.lookup (propCategory p) catMap)
        Nothing -> "-1"
      
      -- the start time is "guaranteed" to exist with this query,
      -- so I don't know why I have it as a Maybe (perhaps it is
      -- used in multiple contexts?)
      --
      startTime = maybe "0.0" (showUTCTime . fromChandraTime) mStartTime

      -- assume this exists
      pos = maybe (-1.0, -100.0) gp (M.lookup obsidVal posMap)
      gp (ra, dec) = (fromRA ra, fromDec dec)
      
      cols = [ showInt (fromObsId obsidVal)
             , showFloat (fst pos)
             , showFloat (snd pos)
             , startTime
             , showFloat obsLen
             , fromInstrument instrument
             , fromGrating grating
             , cat
             , si
             , fromTargetName targetName
             ]
      
  in report cols


dumpTimeline :: IO ()
dumpTimeline = do
  dbData <- runDb (do
                      posInfo <- getPos
                      (sciences, _, simbadMap, proposals) <- getTimeline
                      pure (posInfo, sciences, simbadMap, proposals)
                  )

  report ["obsid", "ra", "dec", "start", "obslen_ks", "instrument", "grating", "category", "simbad", "target"]
  
  let (posInfo, sciences, simbadMap, proposals) = dbData
  
      proposalMap = M.fromList (map (\p -> (propNum p, p)) proposals)
      posMap = M.fromList (map (\(k, r, d) -> (k, (r, d))) posInfo)

      -- convert proposal category into an enumeration
      --   this saves space
      --   and hides the "," problem in 'SN, SNR ...'
      --   (although now switching to tab separation)
      --
      -- we don't report the enumeration type, which makes interpreting
      -- the results a bit annoying
      --
      catNames = sort (S.toList (S.fromList (map propCategory proposals)))
      catMap = M.fromList (zip catNames [1..])
      
  forM_ (fromSL sciences) (dumpScience simbadMap proposalMap catMap posMap)


usage :: IO ()
usage = do
  pName <- T.pack <$> getProgName
  T.hPutStrLn stderr ("Usage: " <> pName)
  exitFailure
  
main :: IO ()
main = do
  args <- getArgs
  unless (null args) usage

  dumpTimeline
