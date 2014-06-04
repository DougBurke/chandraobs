{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

--
-- GroundHog test
--

import Control.Monad.IO.Class (liftIO)

import Database.Groundhog.Postgresql

import Types
import HackData

-- XXX postgres connecton string

main :: IO ()
main = withPostgresqlConn "user=postgres password=postgres dbname=chandraobs host=127.0.0.1" $ runDbConn $ do
  runMigration defaultMigrationLogger $ do
    migrate (undefined :: ScheduleItem)
    migrate (undefined :: ScienceObs)
    migrate (undefined :: NonScienceObs)

  liftIO $ putStrLn $ "Inserting schedule"
  mapM_ insert testSchedule

  liftIO $ putStrLn $ "Inserting science obs"
  mapM_ insert testScience

  liftIO $ putStrLn $ "Inserting non-science obs"
  mapM_ insert testNonScience

