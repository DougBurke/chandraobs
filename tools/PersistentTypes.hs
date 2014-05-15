{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Set up the types used to represent the database schema.

module PersistentTypes where

import Database.Persist.TH

import Types

-- | The `dss`, `pspc`, and `rass` fields may not be needed since
--   they can be generated knowing the sequence and obsname fields.
--   I have left in for now in case this is not true in all cases -
--   e.g. those fields for which there is no PSPC match; does the
--   GIF still exist and it is just a blank file?
--
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Record
  sequence Int Maybe
  obsname ObsName
  contraint Int Maybe
  target String
  startTime String
  time Double
  instrument Instrument Maybe
  grating Grating Maybe
  ra Double
  dec Double
  roll Double
  pitch Double
  slew Double
  dss String Maybe
  pspc String Maybe
  rass  String Maybe
  deriving Eq
  deriving Show

|]

