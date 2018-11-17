{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-

Usage:
  getproposals [nmax]

    nmax defaults to 20

Aim:

Use the CXC interface to get the proposal abstract:

  https://cda.cfa.harvard.edu/srservices/propAbstract.do

with obsid=xxx, propNum=xxx, or both. The response is HTML.

The earlier approach used the NASA ADS service, but it ended up being
more complicated than I wanted (getting at the data is harder since
it is based on bibcode, which is not a value tracked by ocat).

It looks like leading 0's need to be given if the propNum argument
is given, since

 https://cda.cfa.harvard.edu/srservices/propAbstract.do?propNum=2200112

fails, but

 https://cda.cfa.harvard.edu/srservices/propAbstract.do?propNum=02200112

succeeds.

Note that, unlike ADS, propNum=02200112 has the title
"HRC-I CALIBRATION OBSERVATIONS OF ARLAC", whereas ADS has both
"Hrc-I Calibration Observations of Arlac" and
"Hrc-S Calibration Observations of Arlac".

-}

module Main where

import qualified Data.Set as S
import qualified Data.Text.IO as T

import Control.Monad (forM_)

import Database.Groundhog.Postgresql (Cond(CondEmpty), Order(Asc)
                                     , orderBy
                                     , project)

import Data.Monoid ((<>))

import Numeric.Natural

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.Read (readMaybe)

import Database (runDb)
import Types (PropNum
              , fromPropNum
              , Field(PaNumField, PropNumField, MpNumField)
              )

import OCAT (addProposal, showInt)

usage :: String -> IO ()
usage progName = do
  hPutStrLn stderr ("Usage: " ++ progName ++ " [nmax]\n")
  hPutStrLn stderr "Grab proposal abstracts and titles from the CXC.\n"
  hPutStrLn stderr "The default for nmax is 20."
  exitFailure

-- What are the missing proposals?
--
missingProposals ::
  Natural
  -- ^ Maximum number of proposals to return
  -> IO [PropNum]
  -- ^ This is ordered by proposal number, but is for the
  --   highest proposal numbers present.
missingProposals nmax = do

  let dbAct = do
        -- paNum and propNum are unique, so no need to worry about
        -- duplicates in these queries
        propHave <- let p = PaNumField
                    in project p (CondEmpty `orderBy` [Asc p])
        propAll <- let p = PropNumField
                   in project p (CondEmpty `orderBy` [Asc p])
        propNoData <- let p = MpNumField
                      in project p (CondEmpty `orderBy` [Asc p])
        return (propAll, propHave, propNoData)

  (pAll, pHave, pNoData) <- runDb dbAct
  
  -- let Set sort this out
  let sAll = S.fromDistinctAscList pAll
      sHave = S.fromDistinctAscList pHave
      sNoData = S.fromDistinctAscList pNoData
      s = sHave `S.union` sNoData
      wanted = S.toAscList (sAll `S.difference` s)

  let nmiss = length wanted
      ndrop = nmiss - fromIntegral nmax
  T.putStrLn ("There are " <> showInt nmiss <> " missing abstracts.")
  return (drop ndrop wanted)

  
{-
Identify those proposals which do not have an abstract, and then
query CXC for them.

This only works with a local database
-}
runSearch :: Natural -> IO ()
runSearch nmax = do

  props <- missingProposals nmax
  let nprops = length props
  if nprops == 0
    then T.putStrLn "There are no missing proposals."
    else do
      T.putStrLn ("Found " <> showInt nprops <> " missing proposals")

      flags <- mapM (runDb . addProposal) props
      let nfound = length (filter id flags)

      if nfound == nprops
        then T.putStrLn "  all added"
        else do
          T.putStrLn "  The following were not found:"
          let missing = filter (not . fst) (zip flags props)
          forM_ missing (T.putStrLn . ("    " <>) .
                         showInt . fromPropNum . snd)


main :: IO ()
main = do
  pName <- getProgName
  args <- getArgs
  case args of
    [] -> runSearch 20
    [s] -> case readMaybe s of
      Just nmax -> runSearch nmax
      _ -> usage pName
      
    _ -> usage pName
