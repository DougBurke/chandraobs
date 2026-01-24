{-# LANGUAGE OverloadedStrings #-}

-- | Report the currently running observation.
--
-- Usage:
--    ./getcurrent
--
-- List the current obsid
--

module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Time (getCurrentTime)

import Hasql.Connection (release)

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (stderr)

import Database (getConnection
                , getObsInfo
                , reportSize)
import Types (ObsInfo(..))


usage :: IO ()
usage = do
  pName <- T.pack <$> getProgName
  T.hPutStrLn stderr ("Usage: " <> pName)
  exitFailure


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> queryDB
    _ -> usage


queryDB :: IO ()
queryDB = do
  now <- getCurrentTime
  T.putStrLn ("The current time is: " <> T.pack (show now))
  conn <- getConnection
  _ <- reportSize conn
  res <- getObsInfo conn
  case res of
    Nothing -> T.putStrLn "ERROR: no observation was found."
    Just oi -> reportOI oi

  release conn
  

reportOI :: ObsInfo -> IO ()
reportOI oi = do
  let p m a = T.putStrLn m >> print a 
      noData = T.putStrLn "NO DATA FOUND"
      rep = either (p "# Non-science observation") (p "# Science observation")
      printObs = maybe noData rep
        
  T.putStrLn "\n### Current observation:"
  rep (oiCurrentObs oi)

  T.putStrLn "\n### Previous observation:"
  printObs (oiPrevObs oi)

  T.putStrLn "\n### Next observation:"
  printObs (oiNextObs oi)
