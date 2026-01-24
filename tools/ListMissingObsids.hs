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

import qualified Rel8

import Control.Monad (forM_)

import Data.List (sort)

import Hasql.Connection (release)

import Rel8 ((&&.), (<=.), (>=.))

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (stderr)

import Database (getConnection
                , runDb)

import Types (ObsIdVal(..)
             , sorObsId
             , nsObsId
             , ioObsId
             , unsafeToObsIdVal
             , toObsIdValStr
             , scienceObsRawSchema
             , nonScienceObsSchema
             , invalidObsIdSchema
             )


-- | Return obsids within the inclusive range that we know nothing
--   about.
--
findMissing ::
  ObsIdVal  -- ^ Start obsid
  -> ObsIdVal  -- ^ End obsid, must be >= start
  -> IO [ObsIdVal]
  -- ^ ObsIds >= start and <= end which are not in the database
findMissing start end = do
  let s1 = fromObsId64 start
      e1 = fromObsId64 end

      requested = Set.fromList [s1 .. e1]

      get schema proj =
        Rel8.each schema
        >>= pure . proj
        >>= Rel8.filter (\r -> r >=. Rel8.lit start &&. r <=. Rel8.lit end)

  conn <- getConnection
  sobs <- runDb conn $ Rel8.select (get scienceObsRawSchema sorObsId)
  nsobs <- runDb conn $ Rel8.select (get nonScienceObsSchema nsObsId)
  iobs <- runDb conn $ Rel8.select (get invalidObsIdSchema ioObsId)
  release conn

  let make = Set.fromList . map fromObsId64
      found = Set.unions (map make [sobs, nsobs, iobs])

      -- This is a bit of a large hammer for a simple algorithm
      missing = requested `Set.difference` found

      out = map unsafeToObsIdVal (Set.toList missing)

  pure (sort out)
  
process ::
  ObsIdVal     -- ^ Start obsid
  -> ObsIdVal  -- ^ end obsid, assumed to be >= start
  -> IO ()
process start end | start > end =
                T.hPutStrLn stderr "Error: start > end" >> exitFailure
              | otherwise = do
                  missing <- findMissing start end
                  putStrLn ("# Found " <> show (length missing) <> " missing")
                  forM_ missing (print . fromObsId64)
                  

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
        pure (s, e)

  case args of
    [startStr, endStr] -> case act startStr endStr of
      Just (s, e) -> process s e
      _ -> T.hPutStrLn stderr "Unable to convert input to ObsId."
           >> exitFailure
           
    _ -> usage
