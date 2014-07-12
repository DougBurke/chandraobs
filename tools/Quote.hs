{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

--
-- quasi quoter for the Chandra Short-Term schedule syntax
--

module Quote (stsParse, stsParseFile, dummyParse) where

import qualified Language.Haskell.TH as TH

import Data.Time

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax hiding (Dec)

import Types
import Parser (parseSTS)

stsParse :: QuasiQuoter
stsParse = QuasiQuoter stsExpr (error "quotePat STS") (error "quoteType STS") (error "QuoteDec STS")

stsParseFile :: QuasiQuoter
stsParseFile = quoteFile stsParse

instance Lift NonScienceObs where
  lift NonScienceObs{..} = [| NonScienceObs nsName nsObsId nsTarget nsStartTime nsTime nsRa nsDec nsRoll |]

instance Lift ScheduleItem where
  lift ScheduleItem{..} = [| ScheduleItem siObsId siScienceObs siStart siEnd siDuration |]

instance Lift ObsIdVal where
  lift ObsIdVal{..} = [| ObsIdVal fromObsId |]

instance Lift TimeKS where
  lift TimeKS{..} = [| TimeKS _toS |]

instance Lift ChandraTime where
  lift ChandraTime{..} = [| ChandraTime _toUTCTime |]

instance Lift RA where
  lift RA{..} = [| RA _unRA |]

instance Lift Dec where
  lift Dec{..} = [| Dec _unDec |]

instance Lift Double where
  -- lift d = return $ SigE (LitE (RationalL (toRational d))) (ConT GHC.Types.Double)
  lift d = return (LitE (RationalL (toRational d))) -- do I need the explicit typing? probably so

-- UTCTime, Day, DiffTime have Data instances

instance Lift UTCTime where
  lift = dataToExpQ (const Nothing)

{-
instance Lift UTCTime where
  lift UTCTime{..} = [| UTCTime utctDay utctDayTime |]

instance Lift Day where
  lift ModifiedJulianDay{..} = [| ModifiedJulianDay toModifiedJulianDay |]

instance Lift DiffTime where
  lift = dataToExpQ (const Nothing)
-}

-- Looks like there is a problem with the Lift instances, since it seems to cause
-- an infinte loop.
--
-- parseSTS :: String -> Either ParseError [STS]
-- STS = (ScheduleItem, Maybe NonScienceObs)
--
-- Now, there's Lift instances for (a,b), [a], and Maybe a
--
stsExpr :: String -> TH.Q TH.Exp
stsExpr s = 
  case parseSTS s of -- NOTE: no location info
    Left e -> error $ show e
    Right v -> lift v -- [| v |]

dummyParse :: QuasiQuoter
dummyParse = QuasiQuoter dummyExpr (error "quotePat DUMMY") (error "quoteType DUMMY") (error "QuoteDec DUMMY")

dummyExpr :: String -> TH.Q TH.Exp
dummyExpr s = [| map length $ map (filter (=='x')) $ lines s |]

