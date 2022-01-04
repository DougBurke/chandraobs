{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-| Very-simple cache. Not at all optimised.

The idea is that we store the text representation of the
JSON version of a schedule in a cache, so that it doesn't
have to be re-calculated. It is tightly coupled to the
database (as the last-updated field of the database is
used to determine whether the cache is valid).

Not all schedules are cached, as this: saves memory; some
schedules are small-enough not to benefit hugely from the
cache; and this lets me roll out the cache support piece-meal.

The keys are going to be the route, and the value the text
representation of the JSON.

An assumption is that it's okay to return old/stale data; that
is, a query for a key that is found to be out of date will
return the old data, and trigger a cache update for that key
*only*; as the updates aren't quick I don't want to slow
the whole system down. This may turn out to be a poor choice.

What monad to use?

Operations

  create empty cache
  ask for a key value

TODO:

 - there's actually two times we need to track
   - last-modified database, used to invalidate the cache
   - when the next item is scheduled, which determines when the
     schedule is out of data and needs re-arranging (but no query
     is needed)

-}

module Cache (Cache
             , CacheKey
             , CacheData
             , fromCacheData
             , getFromCache
             , makeCache
             , toCacheKey) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
-- import qualified Data.Text.IO as T

import Prelude hiding (log)

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, putMVar
                               , readMVar, takeMVar)

import Control.Concurrent (forkIO)
import Control.Monad (void)

import Data.Maybe (fromJust, isNothing)
import Data.Pool (Pool)
import Data.Time (UTCTime, getCurrentTime)

import Database.Groundhog.Core (Action)
import Database.Groundhog.Postgresql (Postgresql, runDbConn)

-- import System.IO (stderr)

import Types (RestrictedSchedule, rrTime, rrUpdateTime)
import Database (updateSchedule, getLastModified)

log :: T.Text -> IO ()
-- log = T.hPutStrLn stderr
log = const (pure ())

-- Do I need the async package, or will this work?
async :: IO () -> IO ()
async = void . forkIO


-- | At the moment this doesn't do very much.
--
--   TBD: how is access to the CacheStore field going to be
--        mediated?
--
data Cache = Cache {
  cacheConn :: Pool Postgresql
  , cacheActions :: M.Map CacheKey (CacheAction RestrictedSchedule)
  , cacheStore :: MVar (M.Map CacheKey (MVar CacheValue))
  }

newtype CacheKey = CK { fromCacheKey :: T.Text }
                 deriving (Eq, Ord)

toCacheKey :: T.Text -> CacheKey
toCacheKey = CK

newtype CacheData = CD { fromCacheData :: RestrictedSchedule }

type CacheAction = Action Postgresql

type LastModTime = UTCTime

-- Stores:
--   - last-modified time of the database at the time the data
--     was created
--   - the schedule itself
--
type CacheValue = (LastModTime, CacheData)

-- | Create a cache.
--
--   Do I want to make this strict?
--
makeCache ::
  Pool Postgresql
  -> [(CacheKey, CacheAction RestrictedSchedule)]
  -> IO Cache
makeCache pool acts = do
  mvar <- newMVar M.empty
  let macts = M.fromList acts
  pure Cache { cacheConn = pool
             , cacheActions = macts
             , cacheStore = mvar}


{-| Retrieve the value from the key, updating the cache if required.

This may return stale data. I am not sure if this is a good idea
any more, but let's see how it works.

Nothing is returned either if
   - the key is unrecognized
     (at the moment not worth type shenanighans to
      make this a compile-time error)
   - there is no data in the database (or, at least,
     there is no last-modified field)

Actions that can happen (allowing for >= in last-modified check
just to fill out the matrix of possibilities, and because of possible
clock skew)

  - data does not exist
    - query for data
    - set off action to update cache with new data
    - return data

  - data exists
    - stored last-modified date is >= current last-modified date
    - update date of schedule is > now
    -> return cache

  - data exists
    - stored last-modified date is >= current last-modified date
    - update date of schedule is <= now
    -> update schedule
       set off action to update the cache with new schedule
       return new schedule

  - data exists
    - stored last-modified date is < current last-modified date
    - update date of schedule is > now
    -> set off action to reload the cache
       return cache (out of date)

  - data exists
    - stored last-modified date is < current last-modified date
    - update date of schedule is <= now
    -> update schedule
       update the cache with the new schedule (in case any other
         queries hit whilst the reload is ongoing); may not be a good idea
       set off action to reload the cache
       return new schedule

-}

getFromCache :: Cache -> CacheKey -> IO (Maybe CacheData)
getFromCache cache@Cache {..} key = do

  -- assume that the cache is mostly going to be used with known
  -- keys, so that checking if the key is valid in cacheActions,
  -- which can be done without waiting for the MVar, is not a
  -- worthwhile optimisation.
  --
  mLastMod <- runDbConn getLastModified cacheConn
  case mLastMod of
    Just lastMod -> do
      store <- readMVar cacheStore
      case M.lookup key store of
        Just mvar -> do
          (keyTime, ans) <- readMVar mvar
          if lastMod <= keyTime
            then Just <$> handleSchedule key mvar keyTime ans
            
            else do
              log ("Invalidating cache for key=" <> fromCacheKey key)
              -- note that we return stale data here
              async (updateCache cache key)
              pure (Just ans)
            
        Nothing -> 
          case M.lookup key cacheActions of
            Just act -> do
              log ("NOTE: empty cache for key=" <> fromCacheKey key)
              cdata <- runDbConn act cacheConn

              -- TODO: is this sensible use of seq?
              -- TODO: note that there is a possibility for cdata
              --       to be newer than lastMod (if the database was
              --       updated inbetween); could return the last-modified
              --       time again, but it shouldn't be a problem if newer
              --       data gets an old time stamp (other than leading
              --       to it being re-requested at the next query).
              --
              let out = CD cdata
              cdata `seq` async (addToCache cache lastMod key out)
              pure (Just out)

            Nothing -> do
              log ("ERROR: unrecognized cache key=" <> fromCacheKey key)
              pure Nothing

    Nothing -> log ("WARNING: cache access for key=" <>
                    fromCacheKey key <> " but no last-modified date in " <>
                    "the database!")
               >> pure Nothing

-- | We have a schedule, does it need updating?
--
--   This relies on the schedule MVar not being changed; perhaps should
--   send in the map mvar, not the value mvar
--
handleSchedule ::
  CacheKey
  -> MVar CacheValue
  -> UTCTime
  -> CacheData
  -> IO CacheData
handleSchedule key mvar keyTime ans = do
  log ("Restoring data from cache for key=" <>
       fromCacheKey key)
  now <- getCurrentTime
  let updateTime = rrUpdateTime (fromCacheData ans)
  if isNothing updateTime || updateTime > Just now
    then pure ans
    else do
      log ("updating the schedule for key=" <> fromCacheKey key)
      let usched = updateSchedule now (fromCacheData ans)
          nans = CD usched

      -- What should we do if the value has changed? Rely on the
      -- time values, although this is not necessarily ideal
      -- (the time stamps are not 100% reliable indicators of
      --  the freshness of the data).
      --
      curr@(_, cans) <- takeMVar mvar
      if now > rrTime (fromCacheData cans)
        then putMVar mvar (keyTime, nans) >> pure nans
        else putMVar mvar curr >> pure cans
         
  
-- | This is for when a key does not exist in the cache, but this
--   constraint is not enforced (which could cause downstream
--   confusion, since existing code may have the old values).
--
addToCache ::
  Cache
  -> UTCTime
  -- ^ last-modified time of the database
  -> CacheKey
  -> CacheData
  -> IO ()
addToCache Cache {..} lastMod ckey cdata = do
  log ("Adding key: " <> fromCacheKey ckey)
  ndata <- newMVar (lastMod, cdata)

  -- as this is a "pure" update, do not bother with modifyMVar_
  store <- takeMVar cacheStore
  putMVar cacheStore (M.insert ckey ndata store)


-- | The cache is stale and needs refreshing. The key must
--   already exist in the store.
--
updateCache ::
  Cache
  -> CacheKey
  -> IO ()
updateCache Cache {..} ckey = do
  log ("Updating key: " <> fromCacheKey ckey)
  case M.lookup ckey cacheActions of
    Just act -> do
      store <- readMVar cacheStore
      case M.lookup ckey store of
        Just mvar -> modifyMVar_ mvar $ \_ -> do

            let dbAct = do
                  a <- getLastModified
                  b <- act
                  pure (a, b)
                  
            (mLastMod, cdata) <- runDbConn dbAct cacheConn

            -- TODO: is this sensible use of seq?
            -- NOTE: the assumption is that if we got here then there
            --       is a last-modified field; should handle this
            --       properly
            --
            let out = (fromJust mLastMod, CD cdata)
            cdata `seq` pure out

        Nothing ->
          -- COULD add to store, but treat as an error
          log ("ERROR: addToCache called with key: " <> fromCacheKey ckey
               <> " but it has no cached value to replace")
          
    Nothing ->
      log ("ERROR: key " <> fromCacheKey ckey <> " is not registered!")


