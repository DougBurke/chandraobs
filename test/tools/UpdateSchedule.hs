{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{-

Usage:
    ./updateschedule [obsid]

Aim:

Review the current database to see if there are any ObsIds
that may need updating and, if there are, query OCAT for
the new data, updating the database if there has been a change.

If called with a single argument, it's an obsid to re-query
(any checks are ignored in this case, other than that it is
an invalid obsid)

TODO:

The current scheme for identifying observations to query is
not ideal, since it looks like it includes already-observed
data. This needs to be reviewed (and perhaps concentrate on
observations that have just beem, or are about to be,
observed, rather than in reverse start-time order).

-}

module Main (main) where

-- import qualified Data.ByteString.Char8 as B8
-- import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Hasql.Session as S
import qualified Rel8 as R

import Control.Monad (forM, unless, void, when)

import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Time (UTCTime, getCurrentTime)

import Formatting (int, sformat)

import Hasql.Connection (Connection)

import Rel8 ((<.), (==.)
            , Rel8able, Serializable, Statement
            , TableSchema, Expr, Query
            , Delete(..), Insert(..)
            , OnConflict(..), Returning(..)
            , Name, Result, run, run1
            , delete, insert, select
            , desc, each, lit, values
            , isNonNull, nullsLast
            , limit, orderBy
            , in_, where_
            , countRows
            )

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (stderr)

import Database (getConnection
                , runDb_, runDb1, runDbMaybe
                , updateLastModified
                , updateLastModifiedS
                )
import OCAT (OCAT, queryOCAT
            , isScienceObsE
            , ocatToScience, ocatToNonScience)
import Types (ChandraTime, ScienceObs(..), NonScienceObs(..)
             , InvalidObsId(..)
             , ObsIdVal
             , fromObsIdVal
             , ObsIdStatus(..)
             , Proposal(propNum)
             , scienceObsSchema, nonScienceObsSchema, invalidObsIdSchema
             , proposalSchema
             , toChandraTime
             , fromPropNum
             , showCTime
             , toObsIdValStr)


showInt :: Int -> T.Text
showInt = sformat int

showT :: Show a => a -> T.Text
showT = T.pack . show

errOut :: T.Text -> IO a
errOut emsg = T.hPutStrLn stderr emsg >> exitFailure

errOut2 :: T.Text -> T.Text -> IO a
errOut2 hdr msg = do
  T.hPutStrLn stderr hdr
  T.hPutStrLn stderr msg
  exitFailure

-- | What science and non-science observations are there?
--
--   How often should cancelled observations be queried?
--
--   Non-science observations that have not been performed are
--   not queried.
--
--   The responses are ordered by start time, in descending
--   order. A limit is used since a query of OCAT with a large
--   number of obsids failed with an 'invalid request'
--
--   For now, require that a science observation have
--   a start time (which probably just means getting
--   rid of the Unobserved constraint). The idea/hope is that
--   GetSchedule will check these the next time they appear
--   in the STS; they do not need to be updated here.
--
--   Should we remove the "Observed" category from this
--   search, since what could be updated? When does a
--   record go from "Observed" to "Archived"? Note that
--   there are records from 2000 which have an "observed"
--   status - e.g.
--   https://cda.cfa.harvard.edu/chaser/startViewer.do?menuItem=details&obsid=37
--
--   This algorithm needs updating - e.g. may want to occasionally
--   check the observed science observations to see if they
--   have become archived.
--
getUpdateable ::
  Connection
  -> Int
  -- ^ maximum number of each category (may return less
  --   than this)
  -> ChandraTime
  -> IO ([NonScienceObs Result], [ScienceObs Result])
getUpdateable conn nmax tmax = do
  let wantedN = map lit [Unobserved, Scheduled, Observed]
      wantedS = map lit [Unobserved, Scheduled]
      tval = lit (Just tmax)

      nrows = fromIntegral nmax

  let queryN = limit nrows
               $ orderBy (nsStartTime >$< nullsLast desc)
               $ do
        row <- each nonScienceObsSchema
        where_ $ nsStatus row `in_` wantedN
        where_ $ nsStartTime row <. tval
        pure row

      -- Note: no time limit here
      queryS = limit nrows
               $ orderBy (soStartTime >$< nullsLast desc)
               $ do
        row <- each scienceObsSchema
        where_ $ soStatus row `in_` wantedS
        where_ $ isNonNull (soStartTime row)
        pure row

  res1 <- selectDb conn queryN
  res2 <- selectDb conn queryS
  let res = res1 >>= \r1 -> res2 >>= \r2 -> pure (r1, r2)
  case res of
    Right ans -> pure ans
    Left err -> errOut2 "Database error:" (T.pack (show err))


selectDb ::
  Serializable exprs a
  => Connection
  -> Query exprs
  -> IO (Either S.SessionError [a])
selectDb conn sql = S.run (S.statement () (run (select sql))) conn


type OCATMap = [(ObsIdVal, Maybe OCAT)]

{-
Oh joy:

cda query for obsid 17376 has it has observed, and not archived,
even though taken in 2014-11-16

-}

-- | For each observation, if the new OCAT-queried version is different,
--   update the database. If there's no response from OCAT
--   then display a screen message but *do not* change the database.
--
updateObsIds ::
  R.EqTable a
  => Connection
  -> T.Text
  -- ^ label used in screen messages
  -> (a -> ObsIdVal)
  -- ^ project out the ObsId from the database structure
  -> (ObsIdVal -> OCAT -> IO (Maybe a))
  -- ^ Convert OCAT response to structure
  -> (ObsIdVal -> Statement _)
  -- ^ forms the condition for identifying the record to delete
  --   in the database.
  -> (a -> Statement ())
  -- ^ Insert the given record.
  -> OCATMap
  -- ^ The response from querying OCAT
  -> [a]
  -- ^ The information in the database
  -> IO Int
  -- ^ The number of records changed in the database
updateObsIds conn lbl getObsId fromOCAT deleteObs insertObs omap osobs =
  let handle old = do
        let obsid = getObsId old
        case lookup obsid omap of
          Just (Just ocat) -> do
            ans <- fromOCAT obsid ocat
            case ans of
              Just new -> if old == new
                          then pure 0
                          else insNew obsid new
                
              Nothing -> pure 0
          _ -> pure 0

      insNew obsid new = do
        T.putStr $ " " <> showInt (fromObsIdVal obsid)
        let stmt = deleteObs obsid >> insertObs new
        runDb_ conn stmt
        pure 1

      ntot = length osobs
      tailMsg = showInt ntot <> " " <> lbl <> " observations"
        
      report n =
        let msg
              | n == 0 = "No updates for any of the " <> tailMsg
              | n < ntot = "Updated " <> showInt n <> " of the " <>
                           tailMsg
              | otherwise = "Updated all " <> tailMsg
                            
        in T.putStrLn msg
          
  in do
    putStr "Processing:"
    ns <- forM osobs handle
    putStrLn ""
    let n = sum ns
    when (n > 0) (getCurrentTime >>= updateLastModified conn)
    report n
    pure n

{-

-- Unlike the non-science obs, the to-be-queried obsids are dumped
-- to stdout just so that I can check the logic to create this list
-- is sensible
--
updateScience ::
  OCATMap
  -- ^ The response from querying OCAT
  -> [ScienceObs]
  -- ^ The information in the database
  -> IO Int
  -- ^ The number of records changed in the database
updateScience omap sobs =
  let fromOCAT obsid o = do
        ans <- ocatToScience o
        case ans of
          Left e ->
            T.hPutStrLn stderr
            ("Science ObsId: " <> showInt (fromObsIdVal obsid))
            >> T.hPutStrLn stderr e
            >> pure Nothing
          Right (_, x) -> pure (Just x)
        
      del obsid = delete (SoObsIdField ==. obsid)
  in showScienceObs sobs
     >> updateObsIds conn "Science" soObsId fromOCAT del omap sobs

-}


showScienceObs ::
  [ScienceObs Result]
  -> IO ()
showScienceObs = mapM_ (T.putStrLn . textify) . zip [1..]
  where
    textify (i, ScienceObs {..}) =
      let stime = maybe "<no start>" showCTime soStartTime
          pad = "  "
      in pad <> "[" <> showInt i <> "] ObsId: "
         <> showInt (fromObsIdVal soObsId)
         <> " " <> stime

-- For now assume that if a non-science observation can not be found
-- in the OCAT that it should just be left as is.
--
-- It's unclear how to handle the case of "no response" because
-- it is invalid from "no response" because we haven't yet had
-- a ground-system pass, or the OCAT hasn't been updated with the
-- latest results yet.
--
updateNonScience ::
  Connection
  -> OCATMap
  -- ^ The response from querying OCAT
  -> [NonScienceObs Result]
  -- ^ The information in the database
  -> IO Int
  -- ^ The number of records changed in the database
updateNonScience conn =
  let fromOCAT obsid o = case ocatToNonScience o of
        Left e ->
            T.hPutStrLn stderr
            ("Engineering ObsId: " <> showInt (fromObsIdVal obsid)) 
            >> T.hPutStrLn stderr e
            >> pure Nothing
        Right x -> pure (Just x)
                      
      del obsid = delete $ Delete
        { from = nonScienceObsSchema
        , using = pure ()
        , deleteWhere = \_ row -> nsObsId row ==. lit obsid
        , returning = Returning id
        }

      ins a = insert $ Insert
        { into = nonScienceObsSchema
        , rows = values [ lit a ]
        , onConflict = Abort
        , returning = NoReturning
        }
          
  in updateObsIds conn "Engineering" nsObsId fromOCAT del ins


-- | Extract any new schedule information and add it to the
--   system.
--
--   If there's any problem downloading or parsing information
--   then the routine will error out.
--
updateSchedule ::
  Int
  -- ^ Maximum number of non-science or science objects
  -> IO ()
updateSchedule nmax = do

  conn <- getConnection

  -- How about using a limit of only observations newer
  -- than the database last-modified time - some delta
  -- (e.g. a week) - to reduce pointless queries.
  --
  tnow <- toChandraTime <$> getCurrentTime
  (todoNS, todoS) <- getUpdateable conn nmax tnow

  if null todoNS && null todoS
    then T.putStrLn "No updates needed"
    else do

      T.putStrLn ("Non science: " <> showT (length todoNS))
      T.putStrLn ("Science:     " <> showT (length todoS))
      
      let obsidsNS = map nsObsId todoNS
          obsidsS = map soObsId todoS

      {-
      ansS <- queryOCAT obsidsS
      case ansS of
        Right omap -> void (updateScience conn omap todoS)

        Left emsg -> errOut emsg
      -}
  
      ansNS <- queryOCAT obsidsNS
      case ansNS of
        Right omap -> void (updateNonScience conn omap todoNS)

        Left emsg -> errOut emsg
    
  -- This is to help know when I last ran this, so it doesn't matter
  -- if it isn't shown on error
  T.putStrLn ("Query started at: " <> showCTime tnow)


countObsId :: Connection -> ObsIdVal -> IO ()
countObsId conn oi = do
  let count ::
        Rel8able t
        => TableSchema (t Name)
        -> (t Expr -> Expr ObsIdVal)
        -> Query (Expr Int64)
      count schema getObsId = countRows $ do
        row <- each schema
        where_ $ getObsId row ==. lit oi

      query = do
        nInv <- count invalidObsIdSchema ioObsId
        nSci <- count scienceObsSchema soObsId
        nNonSci <- count nonScienceObsSchema nsObsId
        pure (nInv, nSci, nNonSci)

      errVal msg = do
        let emsg = "Error: " <> msg <> ": " <> showInt (fromObsIdVal oi)
        errOut emsg

  res <- S.run (S.statement () (run1 (select query))) conn
  case res of
    Right (nInv, nSci, nNonSci) -> do
      when (nInv < 1) $ errVal "Obsid is not in the invalid table"
      when (nSci > 0) $ errVal "Science Obsid is already known about"
      when (nNonSci > 0) $ errVal "Engineering Obsid is already known about"
        
    Left err -> errOut2 "Database error:" (T.pack (show err))
      
  
-- | It is an error if this obsid is not in the InvalidObsId table.
updateObsId ::
  ObsIdVal
  -- ^ ObsId to query
  -> IO ()
updateObsId oi = do

  T.putStrLn ("Updating ObsId: " <> showInt (fromObsIdVal oi))
  conn <- getConnection
  countObsId conn oi
  
  -- do we assume engineering or science?
  ocat <- queryOCAT [oi]
  case ocat of
    Right omap -> processObsId conn oi omap
    Left emsg -> errOut emsg


-- What is the difference between returning [] and [(xxx, Nothing)] ?
--
processObsId :: Connection -> ObsIdVal -> OCATMap -> IO ()
processObsId conn oi [(oiv, Just ocat)] | oi == oiv = 
  -- check for the type, since this key should always exist
  -- rather than a more permissive "try science, then try
  -- engineering" approach
  case isScienceObsE ocat of
    Left emsg -> errOut emsg
    Right flag -> if flag
                  then processScience conn oi ocat
                  else processEngineering conn oi ocat

  
processObsId _ _ [(_, Just _)] = do
  -- do not expect this to fire so don't do match
  T.hPutStrLn stderr "UNEXPECTED: ObsIds do not match"
  T.hPutStrLn stderr "Database is NOT updated."
  exitFailure
        
processObsId _ _ [(_, Nothing)] = do
  T.hPutStrLn stderr "Unable to parse OCAT response"
  T.hPutStrLn stderr "Database is NOT updated."
  exitFailure

processObsId _ _ [] = do
  T.hPutStrLn stderr "No data retrieved from OCAT query."
  T.hPutStrLn stderr "Database is NOT updated."
  exitFailure

-- This should not happen, so don't bother dealing with it
processObsId _ _ (_:_) = do
  T.hPutStrLn stderr "Errr, multiple responses to OCAT query."
  T.hPutStrLn stderr "Database is NOT updated."
  exitFailure


processScience :: Connection -> ObsIdVal -> OCAT -> IO ()
processScience conn oi ocat =
  let del = delete $ Delete
        { from = invalidObsIdSchema
        , using = pure ()
        , deleteWhere = \_ row -> ioObsId row ==. lit oi
        , returning = NoReturning
        }

      insScience sobs = insert $ Insert
        { into = scienceObsSchema
        , rows = values [ lit sobs ]
        , onConflict = Abort
        , returning = NoReturning
        }

      insProposal prop = insert $ Insert
        { into = proposalSchema
        , rows = values [ lit prop ]
        , onConflict = DoNothing
        , returning = Returning id
        }

      act ::
        UTCTime
        -> ScienceObs Result
        -> Proposal Result
        -> R.Statement (Query (Proposal Expr))
      act now sobs prop = do
        del
        insScience sobs
        ret <- insProposal prop
        updateLastModifiedS now
        pure ret
        
  in do
     ans <- ocatToScience ocat
     case ans of
       Right (prop, sobs) -> do
         now <- getCurrentTime

         flag <- runDbMaybe conn (act now sobs prop)
         
         T.putStrLn "Added science observation"
         when (isJust flag) $
           T.putStrLn ("Also added proposal " <>
                        showInt (fromPropNum (propNum prop)))
         
       Left emsg -> do
         T.hPutStrLn stderr "ERROR converting science obsid"
         errOut emsg
  


-- This doesn't quite match GetSchedule, since in getschedule we have
-- the short-term schedule, so can add in the expected start time
-- from that (if it is missing)
--
processEngineering :: Connection -> ObsIdVal -> OCAT -> IO ()
processEngineering conn oi ocat =
  let del = delete $ Delete
        { from = invalidObsIdSchema
        , using = pure ()
        , deleteWhere = \_ row -> ioObsId row ==. lit oi
        , returning = NoReturning
        }

      ins ns = insert $ Insert
        { into = nonScienceObsSchema
        , rows = values [ lit ns ]
        , onConflict = Abort
        , returning = NoReturning
        }

      act now ns = del >> ins ns >> updateLastModifiedS now
        
  in case ocatToNonScience ocat of
    Left emsg -> do
      T.hPutStrLn stderr "ERROR converting engineering obsid"
      errOut emsg

    Right ns -> do
      now <- getCurrentTime
      runDb_ conn (act now ns)
      T.putStrLn "Added engineering observation"



usage :: IO ()
usage = do
  pName <- T.pack <$> getProgName
  errOut ("Usage: " <> pName <> " [obsid]")


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> updateSchedule 50 -- could be configureable
    [i] | Just oi <- toObsIdValStr (T.pack i) -> updateObsId oi
    _ -> usage
  
