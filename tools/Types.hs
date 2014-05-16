{-# LANGUAGE OverloadedStrings #-}

-- | Set up some types for representing Chandra observations.

module Types (Instrument(..),
              Grating(..),
              ObsName(..)
  ) where

import qualified Text.Blaze.Html5 as H

import qualified Data.Text as T

import Data.Monoid ((<>))
import Database.Persist.Class
import Database.Persist.Sql

-- | The instrument being used.
data Instrument = ACISS | ACISI | HRCI | HRCS deriving (Eq, Show)

-- | The grating to be used.
data Grating = LETG | HETG | NONE deriving (Eq, Show)

-- | This can not be called ObsId since then the TH stuff
-- used by persistent thinks it's an Id field and so
-- falls over as bits and pieces are missing for that to
-- work. The easy solution is to rename the type to ObsName.
--
data ObsName = SpecialObs String | ObsId Int deriving (Eq, Show)

instance H.ToMarkup ObsName where
  toMarkup (SpecialObs s) = H.toMarkup s
  toMarkup (ObsId i)      = H.toMarkup i

instance H.ToValue ObsName where
  toValue (SpecialObs s) = H.toValue s
  toValue (ObsId i)      = H.toValue i

instance PersistField Instrument where
  toPersistValue = PersistText . T.pack . show
  fromPersistValue (PersistText s) = case T.unpack s of
    "ACISS" -> Right ACISS
    "ACISI" -> Right ACISI
    "HRCS"  -> Right HRCS
    "HRCI"  -> Right HRCI
    _       -> Left $ "Unexpected value converting to Instrument: " <> s
  fromPersistValue _ = Left "Unable to convert data type to Instrument"

instance PersistField Grating where
  toPersistValue = PersistText . T.pack . show
  fromPersistValue (PersistText s) = case T.unpack s of
    "HETG" -> Right HETG
    "LETG" -> Right LETG
    "NONE" -> Right NONE
    _       -> Left $ "Unexpected value converting to Grating: " <> s
  fromPersistValue _ = Left "Unable to convert data type to Grating"

instance PersistField ObsName where
  toPersistValue (ObsId i) = toPersistValue (T.empty, i)
  toPersistValue (SpecialObs o) = toPersistValue (T.pack o, 0::Int)

  fromPersistValue v = 
    case fromPersistValue v of
      Right (so, oi)
        | oi == 0   -> if null so 
                       then Left "ObsId is stored as ('',0)"
                       else Right (SpecialObs so)
        | null so   -> Right (ObsId oi)
        | otherwise -> Left "ObsId has both components set"

      Left e -> Left e

instance PersistFieldSql Instrument where
  sqlType _ = SqlString

instance PersistFieldSql Grating where
  sqlType _ = SqlString

-- This instance relies on the fact that (String,Int) is
-- stored as a SqlString, which I found by looking through
-- the persistent code. Ideally this knowledge wouldn't
-- be hard-coded.
--
instance PersistFieldSql ObsName where
  sqlType _ = SqlString

