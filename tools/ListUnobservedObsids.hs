{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

--
-- Usage:
--    ./listunobservedobsids
--
-- Aim:
--
-- List those obsids which are in the database but are not marked as
-- observed (which can just mean that OCAT hasn't been updated yet).
--

module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad (forM_)

import Database.Groundhog (Order(Asc), select)
import Database.Groundhog.Postgresql (orderBy, notIn_)

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (stderr)

import Database (runDb)

import Types (ScienceObs(..)
             , ObsIdStatus(Observed, Archived)
             , Field(SoStatusField, SoObsIdField)
             , fromObsId
             , fromTargetName
             , showRA
             , showDec
             , fromObsIdStatus
             , showCTime
             , showExpTime
             )     


displayScience :: (Int, ScienceObs) -> IO ()
displayScience (ctr, ScienceObs{..}) = do
  let conv = T.pack . show
  T.putStrLn ("# " <> conv ctr)
  T.putStrLn ("ObsId:         " <> conv(fromObsId soObsId))
  T.putStrLn ("Target:        '" <> fromTargetName soTarget <> "'")
  T.putStrLn ("RA:            " <> showRA soRA)
  T.putStrLn ("Dec:           " <> showDec False soDec)
  T.putStrLn ("Status:        " <> fromObsIdStatus soStatus)

  case soStartTime of
    Just st -> T.putStrLn ("Obs date:      " <> showCTime st)
    Nothing -> T.putStrLn ("Obs date:      <UNKNOWN>")

  -- We do not expect this to be set, so flag it as strange
  case soObservedTime of
    Just ot -> T.putStrLn ("**Actual time: " <> showExpTime ot)
    Nothing -> pure ()
    
  T.putStrLn ("Approved:      " <> showExpTime soApprovedTime)

  T.putStrLn "------------"


             
listUnobservedScience :: IO ()
listUnobservedScience =
  let getScience = select ((SoStatusField `notIn_` [Observed, Archived])
                           `orderBy` [Asc SoObsIdField])

  in do
    xs <- runDb getScience
    putStrLn ("# Found " <> show (length xs) <> " science entries")
    forM_ (zip [1..] xs) displayScience


-- For now we do not bother with the non-science observations.
--
listUnobserved :: IO ()
listUnobserved = listUnobservedScience
  

usage :: IO ()
usage = do
  pName <- T.pack <$> getProgName
  T.hPutStrLn stderr ("Usage: " <> pName)
  exitFailure
  
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> listUnobserved
    _ -> usage
