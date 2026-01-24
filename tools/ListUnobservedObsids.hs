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

import qualified Rel8

import Control.Monad (forM_)

import Data.Functor.Contravariant ((>$<))

import Hasql.Connection (release)

import Rel8 ((&&.), (/=.))
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (stderr)

import Database (getConnection
                , runDb)

import Types (ScienceObsRaw(..)
             , ObsIdStatus(Observed, Archived)
             , fromObsId64
             , fromTargetName
             , showRA
             , showDec
             , fromObsIdStatus
             , showCTime
             , showExpTime
             , scienceObsRawSchema
             )     


-- As we do not include the "multi-tel" / telescope information we can
-- stick with the Raw version.
--
displayScience :: (Int, ScienceObsRaw Rel8.Result) -> IO ()
displayScience (ctr, ScienceObsRaw{..}) = do
  T.putStrLn ("# " <> (T.pack . show) ctr)
  T.putStrLn ("ObsId:         " <> (T.pack . show . fromObsId64) sorObsId)
  T.putStrLn ("Target:        '" <> fromTargetName sorTarget <> "'")
  T.putStrLn ("RA:            " <> showRA sorRA)
  T.putStrLn ("Dec:           " <> showDec False sorDec)
  T.putStrLn ("Status:        " <> fromObsIdStatus sorStatus)

  case sorStartTime of
    Just st -> T.putStrLn ("Obs date:      " <> showCTime st)
    Nothing -> T.putStrLn "Obs date:      <UNKNOWN>"

  -- We do not expect this to be set, so flag it as strange
  case sorObservedTime of
    Just ot -> T.putStrLn ("**Actual time: " <> showExpTime ot)
    Nothing -> pure ()
    
  T.putStrLn ("Approved:      " <> showExpTime sorApprovedTime)

  T.putStrLn "------------"


             
listUnobservedScience :: IO ()
listUnobservedScience =
  let getScience =
        Rel8.orderBy (sorObsId >$< Rel8.asc)
        $ Rel8.each scienceObsRawSchema
        >>= Rel8.filter (\r ->
                           let val = sorStatus r
                           in val /=. Rel8.lit Observed
                              &&. val /=. Rel8.lit Archived)
        
  in do
    conn <- getConnection
    xs <- runDb conn (Rel8.select getScience)
    release conn
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
