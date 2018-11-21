{-# LANGUAGE FlexibleContexts #-}
{-# Language OverloadedStrings #-}

--
-- Usage:
--    ./listmissingobsids start end
--
-- Aim:
--
-- List those obsids between start and end which are not in the database.
-- There is no check on the status of these obsids, just whether we know
-- anything about them.
--

module Main (main) where

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad (forM_)

import Data.List (sort)
import Data.Monoid ((<>))

import Database.Groundhog (PersistBackend
                          , (&&.)
                          , (>=.)
                          , (<=.)
                          , project)
-- import Database.Groundhog.Postgresql (SqlDb, Conn, in_, insert_)

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (stderr)

import Database (runDb)

import Types (ObsIdVal(..)
             , Field(NsObsIdField, SoObsIdField, IoObsIdField)
             , unsafeToObsIdVal
             , toObsIdValStr)


-- | Return obsids within the inclusive range that we know nothing
--   about.
--
findMissing ::
  PersistBackend m
  => ObsIdVal  -- ^ Start obsid
  -> ObsIdVal  -- ^ End obsid, must be >= start
  -> m [ObsIdVal]
  -- ^ ObsIds >= start and <= end which are not in the database
findMissing start end = do
  let s1 = fromObsId start
      e1 = fromObsId end

      requested = Set.fromList [s1 .. e1]
      
  sobs <- project SoObsIdField (SoObsIdField >=. start &&. SoObsIdField <=. end)
  nsobs <- project NsObsIdField (NsObsIdField >=. start &&. NsObsIdField <=. end)
  iobs <- project IoObsIdField (IoObsIdField >=. start &&. IoObsIdField <=. end)

  let make = Set.fromList . map fromObsId
      found = Set.unions (map make [sobs, nsobs, iobs])

      -- This is a bit of a large hammer for a simple algorithm
      missing = requested `Set.difference` found

      out = map unsafeToObsIdVal (Set.toList missing)
        
  return (sort out)
  
process ::
  ObsIdVal     -- ^ Start obsid
  -> ObsIdVal  -- ^ end obsid, assumed to be >= start
  -> IO ()
process start end | start > end =
                T.hPutStrLn stderr "Error: start > end" >> exitFailure
              | otherwise = do
  missing <- runDb (findMissing start end)
  putStrLn ("# Found " <> show (length missing) <> " missing")
  forM_ missing (print . fromObsId)
                  

usage :: IO ()
usage = do
  pName <- T.pack <$> getProgName
  T.hPutStrLn stderr ("Usage: " <> pName <> " start end")
  exitFailure
  
main :: IO ()
main = do
  args <- getArgs

  let act sstr estr = do
        let conv = toObsIdValStr . T.pack
        s <- conv sstr
        e <- conv estr
        return (s, e)

  case args of
    [startStr, endStr] -> case act startStr endStr of
      Just (s, e) -> process s e
      _ -> T.hPutStrLn stderr "Unable to convert input to ObsId."
           >> exitFailure
           
    _ -> usage
