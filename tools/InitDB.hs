{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

--
-- Initialize the database using the manually-curated information
-- stored in HackData. At present this is to the local Postgres
-- instance.
--

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import Database.Groundhog.Postgresql

import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Database (reportSize)
import HackData
import Types

main :: IO ()
main = withPostgresqlConn "user=postgres password=postgres dbname=chandraobs host=127.0.0.1" $ 
  runDbConn $ do
    handleMigration

    reportSize

    n1 <- countAll (undefined :: ScheduleItem)
    n2 <- countAll (undefined :: ScienceObs)
    n3 <- countAll (undefined :: ScienceObsFull)
    n4 <- countAll (undefined :: NonScienceObs)
    n5 <- countAll (undefined :: Proposal)

    let cts = [n1, n2, n3, n4, n5]
    when (any (/=0) cts) $ do
      liftIO $ hPutStrLn stderr $ "ERROR: there is existing data!"
      liftIO $ exitFailure

    liftIO $ putStrLn "Inserting schedule"
    mapM_ insert testSchedule

    liftIO $ putStrLn "Inserting science obs"
    mapM_ insert testScience

    liftIO $ putStrLn "Inserting non-science obs"
    mapM_ insert testNonScience

    reportSize
