{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}

-- Can we understand how the soMultiTelObs field has been encoded?
--
-- a) soMultiTel can be False but soMultiTelInt > 0
--
-- b) how is soMultiTelObs handled?
--

module Main (main) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad (forM_, unless, when)

-- import Data.Functor.Contravariant ((>$<))

import Hasql.Connection (Connection, release)

import Rel8 ( (==.)
            , Expr
            , Query
            -- , asc
            , absent
            , each
            , someExpr
            -- , orderBy
            , select
            , where_
            )

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (stderr)

import Database ( getConnection
                , runDb
                )
import Types ( ObsIdVal
             , fromObsIdVal
             , Telescope
             , fromTelescope
             , TelescopeId
             , ScienceObsRaw(..)
             , TelescopeValues(..)
             , scienceObsRawSchema
             , telescopeValuesSchema
             )


usage :: IO ()
usage = do
  pName <- T.pack <$> getProgName
  T.hPutStrLn stderr ("Usage: " <> pName)
  exitFailure


main :: IO ()
main = do
  args <- getArgs
  unless (null args) $ usage
  conn <- getConnection
  withTel <- findTelescopes conn
  withOutTel <- findNoTelescopes conn
  release conn

  T.putStrLn "# Telescopes"
  forM_ (withTel) $ \(obsid, tels) -> do

    let otxt = T.pack (show (fromObsIdVal obsid))
        showNum = T.pack . show
        n = NE.length tels

    T.putStrLn ("ObsId: " <> otxt)

    when (n /= 1) $
      T.putStrLn ("  !! " <> showNum n <> " telescopes")

    forM_ (NE.toList tels) $ \tel ->
      T.putStrLn ("  " <> fromTelescope tel)

  T.putStrLn "\n# No Telescopes"
  forM_ (withOutTel) $ \obsid ->
    let otxt = T.pack (show (fromObsIdVal obsid))
    in T.putStrLn ("ObsId: " <> otxt)
  

telescopes:: Query (Expr ObsIdVal, Expr TelescopeId)
telescopes = do
  sobs <- each scienceObsRawSchema
  -- sobs2 <- orderBy (sorObsId >$< asc) (pure sobs)
  -- pure (sorObsId sobs2, sorMultiTelObs sobs2)
  pure (sorObsId sobs, sorMultiTelObs sobs)


findTelescope :: Expr TelescopeId -> Query (Expr Telescope)
findTelescope rowNum = do
  tele <- each telescopeValuesSchema
  where_ (tvId tele ==. rowNum)
  pure (tvValue tele)


queryTelescopes :: Query (Expr ObsIdVal, Expr (NE.NonEmpty Telescope))
queryTelescopes = do
  (sobs, idVal) <- telescopes
  teles <- someExpr (findTelescope idVal)
  pure (sobs, teles)


-- Is this the best way to do it?
queryNoTelescopes :: Query (Expr ObsIdVal)
queryNoTelescopes = do
  (sobs, idVal) <- telescopes
  absent (findTelescope idVal)
  pure sobs


-- | Find all the telescopes.
--
findTelescopes :: Connection -> IO [(ObsIdVal, NE.NonEmpty Telescope)]
findTelescopes conn = runDb conn (select queryTelescopes)


-- | Find the observations without telescopes.
--
findNoTelescopes :: Connection -> IO [ObsIdVal]
findNoTelescopes conn = runDb conn (select queryNoTelescopes)
