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

module Main (main) where

import qualified Data.Set as S
import qualified Data.Text.IO as T

import qualified Rel8

import Control.Monad (forM_)

import Hasql.Connection (Connection, release)

import Numeric.Natural

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.Read (readMaybe)

import Database (getConnection, runDb)
import Types (PropNum
              , fromPropNum
              , paNum
              , propNum
              , mpNum
              , proposalAbstractSchema
              , proposalSchema
              , missingProposalAbstractSchema
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
  Connection
  -> Natural
  -- ^ Maximum number of proposals to return
  -> IO [PropNum]
  -- ^ This is ordered by proposal number, but is for the
  --   highest proposal numbers present.
missingProposals conn nmax = do

  -- paNum and propNum are unique, so no need to worry about
  -- duplicates in these queries
  let get schema proj = Rel8.orderBy Rel8.asc (proj <$> Rel8.each schema)
      propHave = get proposalAbstractSchema paNum
      propAll = get proposalSchema propNum
      propNoData = get missingProposalAbstractSchema mpNum

  pAll <- runDb conn (Rel8.select propAll)
  pHave <- runDb conn (Rel8.select propHave)
  pNoData <- runDb conn (Rel8.select propNoData)
  
  -- let Set sort this out
  let sAll = S.fromDistinctAscList pAll
      sHave = S.fromDistinctAscList pHave
      sNoData = S.fromDistinctAscList pNoData
      s = sHave `S.union` sNoData
      wanted = S.toAscList (sAll `S.difference` s)

  let nmiss = length wanted
      ndrop = nmiss - fromIntegral nmax
  T.putStrLn ("There are " <> showInt nmiss <> " missing abstracts.")
  pure (drop ndrop wanted)

  
{-
Identify those proposals which do not have an abstract, and then
query CXC for them.

This only works with a local database
-}
runSearch :: Natural -> IO ()
runSearch nmax = do

  conn <- getConnection
  props <- missingProposals conn nmax
  let nprops = length props
  if nprops == 0
    then T.putStrLn "There are no missing proposals."
    else do
      T.putStrLn ("Found " <> showInt nprops <> " missing proposals")

      flags <- mapM (addProposal conn) props
      let nfound = length (filter id flags)

      if nfound == nprops
        then T.putStrLn "  all added"
        else do
          T.putStrLn "  The following were not found:"
          let missing = filter (not . fst) (zip flags props)
          forM_ missing (T.putStrLn . ("    " <>) .
                         showInt . fromPropNum . snd)

  release conn


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
