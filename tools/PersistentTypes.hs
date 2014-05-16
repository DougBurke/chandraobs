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
  deriving Eq
  deriving Show

|]

