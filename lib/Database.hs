{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | Simple database access shims.
--
--   Many of these are poorly written (e.g. no use of the database
--   or Groundhog API, such as inner joins or primary keys).
--
module Database ( getCurrentObs
                , getObsInfo
                -- , findObsName
                , getObsId
                , getRecord
                , getSchedule
                , makeSchedule
                , getProposal
                , getProposalFromNumber
                , getProposalObs
                , getRelatedObs
                , getObsFromProposal
                , getProposalInfo
                , reportSize
                , getSimbadInfo
                , fetchSIMBADType
                , fetchObjectTypes
                , fetchConstellation
                , fetchConstellationTypes
                , fetchCategory
                , fetchCategoryTypes
                , fetchProposal
                , fetchInstrument
                , fetchInstrumentTypes
                , insertScienceObs
                , replaceScienceObs
                , insertProposal
                , insertSimbadInfo
                , insertSimbadMatch
                , insertSimbadNoMatch

                , cleanDataBase
                , cleanupDiscarded
                , insertOrReplace
                , addScheduleItem
                , addNonScienceScheduleItem

                , putIO
                , runDb
                , dbConnStr
                ) where

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Control.Applicative ((<$>))
#endif

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (NoLoggingT)

import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (group, groupBy, sortBy)
import Data.Maybe (isNothing, listToMaybe)
import Data.Ord (Down(..), comparing)
import Data.Time (UTCTime(..), Day(..), getCurrentTime, addDays)

import Database.Groundhog.Core (PersistEntity, EntityConstr,
                                DbDescriptor, RestrictionHolder)
import Database.Groundhog.Postgresql

import Types

type DbIO m = (MonadIO m, PersistBackend m)

discarded :: String
discarded = "discarded"

notDiscarded ::
  DbDescriptor db
  => Cond db (RestrictionHolder ScienceObs ScienceObsConstructor)
notDiscarded = SoStatusField /=. discarded

-- | Is this ObsId discarded?
isDiscarded :: PersistBackend m => ObsIdVal -> m Bool
isDiscarded oid = do
  ostatus <- project SoStatusField (SoObsIdField ==. oid)
  case ostatus of
    [status] | status == discarded -> return True
    _ -> return False


-- | Given two lists of observations, which are assumed to have 0 or 1
--   elements in, identify which is the latest (has the largest start
--   time) or earliest.
--
--   These should be consolidated.
--
identifyLatestRecord ::
  [ScienceObs]
  -> [NonScienceObs]
  -> Maybe Record
identifyLatestRecord xs ys =
  -- rely on Ord instance of Maybe for the comparison
  let sobs = listToMaybe xs
      nsobs = listToMaybe ys
      stime = soStartTime <$> sobs
      ntime = nsStartTime <$> nsobs
  in if stime > ntime then Right <$> sobs else Left <$> nsobs

identifyEarliestRecord ::
  [ScienceObs]
  -> [NonScienceObs]
  -> Maybe Record
identifyEarliestRecord xs ys =
  -- rely on Ord instance of Maybe for the comparison
  let sobs = listToMaybe xs
      nsobs = listToMaybe ys
      stime = soStartTime <$> sobs
      ntime = nsStartTime <$> nsobs
  in if stime < ntime then Right <$> sobs else Left <$> nsobs

-- | Return the last observation (science or non-science) to be
--   scheduled before the requested time. The observation can be
--   finished or running. It will not be a discarded observation.
--
findRecord :: (PersistBackend m) => UTCTime -> m (Maybe Record)
findRecord t = do
  let tval = ChandraTime t
  xs <- select (((SoStartTimeField <=. tval) &&. notDiscarded)
                `orderBy` [Desc SoStartTimeField]
                `limitTo` 1)
  ys <- select ((NsStartTimeField <=. tval)
                `orderBy` [Desc NsStartTimeField]
                `limitTo` 1)

  return (identifyLatestRecord xs ys)

-- | Return information on this obsid, if known about. This includes
--   discarded observations.
--
findObsId :: PersistBackend m => ObsIdVal -> m (Maybe Record)
findObsId oi = do
  msobs <- findScience oi
  case msobs of
    Just sobs -> return (Just (Right sobs))
    _ -> do
      ans <- findNonScience oi
      return (Left <$> ans)

findNonScience :: PersistBackend m => ObsIdVal -> m (Maybe NonScienceObs)
findNonScience oi = do
  ans <- select ((NsObsIdField ==. oi) `limitTo` 1)
  return (listToMaybe ans)

-- | This will return a discarded observation.
findScience :: PersistBackend m => ObsIdVal -> m (Maybe ScienceObs)
findScience oi = do
  ans <- select ((SoObsIdField ==. oi) `limitTo` 1)
  return (listToMaybe ans)

-- | Return the current observation (ignoring discarded observations,
--   but in this case it is unlikely that the database will have been
--   updated with the discard information in time).
--
getCurrentObs :: DbIO m => m (Maybe Record)
getCurrentObs = liftIO getCurrentTime >>= findRecord

-- | Return the first observation to start after the given time,
--   excluding discarded observations. If the input observation
--   is discarded then nothing is returned.
--
--   It is not clear whether the obsid value is really needed, but
--   left in in case other parts of the system rely on this
--   explicit check.
--
getNextObs ::
  PersistBackend m
  => ObsIdVal
  -> ChandraTime
  -> m (Maybe Record)
getNextObs oid t = do
  flag <- isDiscarded oid
  if flag
    then return Nothing
    else do
    xs <- select (((SoStartTimeField >. t)
                   &&. (SoObsIdField /=. oid)
                   &&. notDiscarded)
                  `orderBy` [Asc SoStartTimeField]
                  `limitTo` 1)
    ys <- select (((NsStartTimeField >. t)
                   &&. (NsObsIdField /=. oid))
                  `orderBy` [Asc NsStartTimeField]
                  `limitTo` 1)
    return (identifyEarliestRecord xs ys)


-- | Return the last observation to have started before the given time,
--   excluding discarded observations. If the input observation
--   is discarded then nothing is returned.
--
--   See the discussion for `getNextObs`.
--
getPrevObs ::
  PersistBackend m
  => ObsIdVal
  -> ChandraTime
  -> m (Maybe Record)
getPrevObs oid t = do
  flag <- isDiscarded oid
  if flag
    then return Nothing
    else do
    xs <- select (((SoStartTimeField <. t)
                   &&. (SoObsIdField /=. oid)
                   &&. notDiscarded)
                  `orderBy` [Desc SoStartTimeField]
                  `limitTo` 1)
    ys <- select (((NsStartTimeField <. t)
                   &&. (NsObsIdField /=. oid))
                  `orderBy` [Desc NsStartTimeField]
                  `limitTo` 1)
    return (identifyLatestRecord xs ys)


-- | Find the current observation and the previous/next ones.
--
--   TODO: should check for some overlap of the observation date +
--         exposure time to the current date, so can suggest that
--         slewing (or if at end that run out of data...)
--
--   It might be quicker to find the current observation and return
--   the following and preceeding rows in one go, but that's an
--   optimisation.
--
--   TODO: it is likely that we will not have to deal with discarded
--         observations here, since the database will not have been
--         updated with the relevant information until after this
--         code could be running, but need to think about it.
--
getObsInfo ::
  (Functor m, DbIO m)  -- Functor needed before ghc 7.10
  => m (Maybe ObsInfo)
getObsInfo = do
  mobs <- getCurrentObs
  case mobs of
    Just obs -> Just <$> extractObsInfo obs
    Nothing -> return Nothing

-- | Given an observation, extract the previous and next observations.
--
extractObsInfo :: PersistBackend m => Record -> m ObsInfo
extractObsInfo obs = do
  mprev <- getPrevObs (recordObsId obs) (recordStartTime obs)
  mnext <- getNextObs (recordObsId obs) (recordStartTime obs)
  return (ObsInfo obs mprev mnext)

-- | Return information on the given observation, including
--   preceeding and following observations.
--
findObsInfo ::
  (Functor m, PersistBackend m) -- Functor needed before ghc 7.10
  => ObsIdVal
  -> m (Maybe ObsInfo)
findObsInfo oi = do
  mrec <- findObsId oi
  case mrec of
    Just r -> Just <$> extractObsInfo r
    _ -> return Nothing

-- | Return the requested "science" observation,
--   including discarded observations.
--
--   TODO: note that this actually also returns non-science
--         observations, so either the comment or code should change.
getObsId ::
  (Functor m, PersistBackend m)  -- Functor needed for ghc < 7.10
  => ObsIdVal
  -> m (Maybe ObsInfo)
getObsId = findObsInfo 

-- | Return the record of the given observation, if it
--   exists. This includes discarded observations.
--
getRecord :: PersistBackend m => ObsIdVal -> m (Maybe Record)
getRecord = findObsId

-- | TODO: handle the case when the current observation, which has
--   just started, has an exposure time > ndays.
--
--   Discarded observations are assumed to have been removed from
--   the ScheduleItem table.
--
getSchedule ::
  DbIO m
  => Int    -- ^ Number of days to go back/forward
  -> m Schedule
getSchedule ndays = do
  now <- liftIO getCurrentTime

  -- return all observations which have any part of their
  -- observation within the time span tStart to tEnd.
  --
  let dayNow = utctDay now
      dayEnd = addDays (fromIntegral ndays + 1) dayNow
      day = toModifiedJulianDay dayNow - fromIntegral ndays
      dayStart = ModifiedJulianDay day

      tStart = ChandraTime (UTCTime dayStart 0)
      tEnd   = ChandraTime (UTCTime dayEnd 0)

  -- Select the science and non-science observations separately
  -- and then merge them. As the science observations have no
  -- end-time field, post-process the database results here,
  -- rather than having the database do all the filtering.
  --
  xs1 <- select (((SoStartTimeField <. tEnd) &&.
                  notDiscarded)
                 `orderBy` [Asc SoStartTimeField])
  ys1 <- select ((NsStartTimeField <. tEnd)
                 `orderBy` [Asc NsStartTimeField])

  -- should filter by the actual run-time, but the following is
  -- easier
  let filtEnd proj_start proj_runtime o =
        let stime = proj_start o
            rtime = proj_runtime o
            etime = endCTime stime rtime
        in etime < tStart
      xs = map Right (dropWhile (filtEnd soStartTime soApprovedTime) xs1)
      ys = map Left (dropWhile (filtEnd nsStartTime nsTime) ys1)

      res1 = mergeSL recordStartTime (unsafeToSL xs) (unsafeToSL ys)
      res = fromSL res1
      
  -- I used siStart r <= cnow, so keep with that logic for now
  -- (could use siEnd r < cnow)
  let cnow = ChandraTime now
      (aprevs, todo) = span ((<= cnow) . recordStartTime) res
      (mdoing, done) = case reverse aprevs of
        [] -> (Nothing, [])
        (current:cs) -> (Just current, reverse cs)

  return (Schedule now ndays done mdoing todo)

-- | Creates a schedule structure of the list of observations.
--
--   The number-of-days field in the structure is set to 0; this
--   is not ideal!
--
--   Note that any discarded science observations are removed.
--
makeSchedule ::
  DbIO m
  => SortedList StartTimeOrder Record -- ^ no duplicates
  -> m Schedule
makeSchedule rs = do
  now <- liftIO getCurrentTime
  mrec <- findRecord now
  
  let cleanrs = filter removeDiscarded (fromSL rs)
      removeDiscarded (Left _) = True
      removeDiscarded (Right ScienceObs{..}) = soStatus /= discarded

      mobsid = recordObsId <$> mrec
      findNow r = if Just (recordObsId r) == mobsid then Right r else Left r
      (others, nows) = partitionEithers (map findNow cleanrs)

      cnow = ChandraTime now
      (done, todo) = span ((<= cnow) . recordStartTime) others

  return (Schedule now 0 done (listToMaybe nows) todo)

-- | Do we have any SIMBAD information about the target?
--
getSimbadInfo :: 
  PersistBackend m
  => String   -- ^ target name (not the actual SIMBAD search term)
  -> m (Maybe SimbadInfo)
getSimbadInfo target = do
  keys <- project SmmInfoField ((SmmTargetField ==. target) `limitTo` 1)
  case keys of
    [key] -> get key
    _ -> return Nothing

-- | Return all observations of the given SIMBAD type (excluding
--   discarded observations).
--
fetchSIMBADType :: 
  PersistBackend m
  => SimbadType 
  -> m (Maybe (SimbadTypeInfo, SortedList StartTimeOrder ScienceObs))
fetchSIMBADType stype = do
  -- TODO: would be nice to get the database to do this,
  --       or perhaps switch to a graph databse.
  -- 
  -- splitting into two for now
  mtype <- project SmiTypeField ((SmiType3Field ==. stype) `limitTo` 1)
  case mtype of
    [ltype] -> do
               keys <- project AutoKeyField (SmiType3Field ==. stype)
               sos <- forM keys $ \key -> do
                          targets <- project SmmTargetField (SmmInfoField ==. key)
                          obs <- forM targets $
                                 \t -> select ((SoTargetField ==. t &&.
                                                notDiscarded)
                                               `orderBy`
                                               [Asc SoStartTimeField])

                          return (concat obs)

               -- could create sorted lists and then combine them,
               -- but that has issues, so do it manually
               let xs = concat sos
                   ys = toSL soStartTime xs
               return (Just ((stype, ltype), ys))

    _ -> return Nothing

-- | Return information on the object types we have stored.
--
--   The return value includes the number of objects that 
--   have the given type.
--
fetchObjectTypes :: 
  PersistBackend m
  => m [(SimbadTypeInfo, Int)]
fetchObjectTypes = do
  res <- select (CondEmpty `orderBy` [Asc SmiType3Field])
  let srt = groupBy ((==) `on` smiType3) res
      t [] = error "impossible fetchObjectTypes condition occurred"
      t xs@(x:_) = ((smiType3 x, smiType x), length xs)
  return (map t srt)

-- | Return observations which match this constellation, excluding
--   discarded observations.
--
fetchConstellation ::
  PersistBackend m
  => ConShort
  -> m (SortedList StartTimeOrder ScienceObs)
fetchConstellation con = do
  ans <- select ((SoConstellationField ==. con &&. notDiscarded)
                 `orderBy` [Asc SoStartTimeField])
  return (unsafeToSL ans)

-- TODO: use the database to do this computation?

-- | Given a sorted list of values, return the counts of these values,
--   in descending order.
--
countUp :: Eq a => [a] -> [(a, Int)]
countUp xs =
  let gs = group xs
      cts = map length gs
      ids = map head gs
      ys = zip ids cts
  in sortBy (comparing (Down . snd)) ys


-- | Return count of the constellations. Discarded observations are excluded.
--
fetchConstellationTypes :: PersistBackend m => m [(ConShort, Int)]
fetchConstellationTypes = do
  res <- project SoConstellationField
         (notDiscarded `orderBy` [Asc SoConstellationField])
  return (countUp res)
    
-- | Return observations which match this category,
--   excluding discarded observations.
fetchCategory ::
  PersistBackend m
  => String
  -> m (SortedList StartTimeOrder ScienceObs)
fetchCategory cat = do
  propNums <- project PropNumField (PropCategoryField ==. cat)
  sos <- forM propNums $ \pn ->
    select (SoProposalField ==. pn &&. notDiscarded)
  let xs = concat sos
  return (toSL soStartTime xs)

-- | Return information on the category types.
fetchCategoryTypes :: PersistBackend m => m [(String, Int)]
fetchCategoryTypes = do
  res <- project PropCategoryField
         (CondEmpty `orderBy` [Asc PropCategoryField])
  return (countUp res)

-- | Return all the observations which match this proposal,
--   excluding discarded observations.
--
--   See also `getProposal` and `getProposalObs`
fetchProposal ::
  PersistBackend m
  => PropNum
  -> m (Maybe Proposal, SortedList StartTimeOrder ScienceObs)
fetchProposal pn = do
  mprop <- select ((PropNumField ==. pn) `limitTo` 1)
  ms <- select ((SoProposalField ==. pn &&. notDiscarded)
                `orderBy` [Asc SoStartTimeField])
  return (listToMaybe mprop, unsafeToSL ms)

-- | Return all the observations which match this instrument,
--   excluding discarded observations.
--
fetchInstrument ::
  PersistBackend m
  => Instrument
  -> m (SortedList StartTimeOrder ScienceObs)
fetchInstrument inst = do
  ans <- select $ (SoInstrumentField ==. inst &&. notDiscarded)
         `orderBy` [Asc SoStartTimeField]
  return (unsafeToSL ans)

-- | This counts up the individual observations; should it try and group by
--   "proposal", or at least "object per proposal"?
--
--   Discarded observations are excluded.
--
fetchInstrumentTypes :: PersistBackend m => m [(Instrument, Int)]
fetchInstrumentTypes = do
  insts <- project SoInstrumentField (notDiscarded `orderBy`
                                      [Asc SoInstrumentField])
  return (countUp insts)

-- | Return the proposal information for the observation if:
--   a) it's a science observation, and b) we have it.
--   even if the observation was discarded.
--
getProposal :: PersistBackend m => ScienceObs -> m (Maybe Proposal)
getProposal ScienceObs{..} = do
  ans <- select ((PropNumField ==. soProposal) `limitTo` 1)
  return (listToMaybe ans)

-- | Return the proposal information if we have it.
--
getProposalFromNumber :: PersistBackend m => PropNum -> m (Maybe Proposal)
getProposalFromNumber propNum = do
  ans <- select ((PropNumField ==. propNum) `limitTo` 1)
  return (listToMaybe ans)

-- | Find all the other observations in the proposal. This does not return
--   discarded observations, but will return matches if the input observation
--   was discarded.
--
getProposalObs ::
  PersistBackend m
  => ScienceObs
  -> m (SortedList StartTimeOrder ScienceObs)
getProposalObs ScienceObs{..} = do
  -- time sorting probably not needed here
  ans <- select (((SoProposalField ==. soProposal) &&.
                  (SoObsIdField /=. soObsId) &&. notDiscarded)
                 `orderBy` [Asc SoStartTimeField])
  return (unsafeToSL ans)

-- | Find all the other non-discarded observations in the same proposal that
--   are in the database.
--
getRelatedObs ::
  PersistBackend m
  => PropNum
  -> ObsIdVal
  -> m (SortedList StartTimeOrder ScienceObs)
getRelatedObs propNum obsId = do
  ans <- select (((SoProposalField ==. propNum) &&.
                  (SoObsIdField /=. obsId) &&. notDiscarded)
                 `orderBy` [Asc SoStartTimeField])
  return (unsafeToSL ans)

-- | Return the observations we know about for the given proposal
--   (that are not discarded).
getObsFromProposal ::
  PersistBackend m
  => PropNum
  -> m (SortedList StartTimeOrder ScienceObs)
getObsFromProposal propNum = do
  ans <- select ((SoProposalField ==. propNum &&. notDiscarded)
                 `orderBy` [Asc SoStartTimeField])
  return (unsafeToSL ans)

-- | A combination of `getProposal` and `getProposalObs`.
--
getProposalInfo ::
  PersistBackend m
  => Record
  -> m (Maybe Proposal, SortedList StartTimeOrder ScienceObs)
getProposalInfo (Left _) = return (Nothing, emptySL)
getProposalInfo (Right so) = do
  mproposal <- getProposal so
  matches <- getProposalObs so
  return (mproposal, matches)

-- | Report on the observational status of the science observations.
--   The values are ordered in descending order of the counts (the list
--   of options is assumed to be small).
--
--   This INCLUDES discarded observations.
--
findObsStatusTypes :: PersistBackend m => m [(String, Int)]
findObsStatusTypes = do
  statuses <- project SoStatusField (CondEmpty `orderBy` [Asc SoStatusField])
  return (countUp statuses)

putIO :: MonadIO m => String -> m ()
putIO = liftIO . putStrLn

showSize ::
  (DbIO m, PersistEntity v)
  => String
  -> v
  -> m Int
showSize l t = do
  n <- countAll t
  putIO ("Number of " ++ l ++ " : " ++ show n)
  return n

-- | Quick on-screen summary of the database size.
reportSize :: DbIO m => m Int
reportSize = do
  n1 <- showSize "scheduled items  " (undefined :: ScheduleItem)
  n2 <- showSize "science obs      " (undefined :: ScienceObs)
  n3 <- showSize "non-science obs  " (undefined :: NonScienceObs)
  n4 <- showSize "proposals        " (undefined :: Proposal)
  n5 <- showSize "SIMBAD match     " (undefined :: SimbadMatch)
  n6 <- showSize "SIMBAD no match  " (undefined :: SimbadNoMatch)
  n7 <- showSize "SIMBAD info      " (undefined :: SimbadInfo)
  n8 <- showSize "overlap obs      " (undefined :: OverlapObs)

  -- break down the status field of the scheduled observations
  putIO ""
  ns <- findObsStatusTypes
  forM_ ns (\(status, n) ->
             putIO ("  status=" ++ status ++ "  : " ++ show n))
  putIO (" -> total = " ++ show (sum (map snd ns)))
  
  let ntot = sum [n1, n2, n3, n4, n5, n6, n7, n8]
  putIO ("\nNumber of rows              : " ++ show ntot)
  return ntot

-- Need to make sure the following match the constraints on the tables found
-- in Types.hs

-- | Checks that the Science observation is not known about before
--   inserting it.
--
--   If it already exists in the database the new value is ignored;
--   there is no check to make sure that the details match.
--
--   There is no attempt to remove the observation from the ScheduleItem
--   table.
--
insertScienceObs ::
  PersistBackend m
  => ScienceObs
  -> m Bool  -- ^ True if the observation was added to the database
insertScienceObs s = do
  n <- count (SoObsIdField ==. soObsId s)
  let unknown = n == 0
  when unknown (insert_ s)
  return unknown

-- | Replaces the science observation with the new values, if it is different.
--
--   Acts as `insertScienceObs` if the observation is not known about.
--
--   There is no attempt to remove the observation from the ScheduleItem
--   table.
--
replaceScienceObs ::
  PersistBackend m
  => ScienceObs
  -> m ()
replaceScienceObs s = insertOrReplace (SoObsIdField ==. soObsId s) s

-- | Checks that the proposal is not known about before inserting it.
--
--   If it already exists in the database the new value is ignored;
--   there is no check to make sure that the details match.
insertProposal ::
  PersistBackend m
  => Proposal
  -> m Bool  -- ^ True if the proposal was added to the database
insertProposal p = do
  n <- count (PropNumField ==. propNum p)
  let unknown = n == 0
  when unknown (insert_ p)
  return unknown

-- | Checks that the data is not known about before inserting it.
--
--   Returns the key for the item and a flag indicating whether
--   the key already exists (so previous SimbadNoMatch may need
--   to be deleted).
--
insertSimbadInfo ::
  PersistBackend m
  => SimbadInfo
  -> m (AutoKey SimbadInfo, Bool)
insertSimbadInfo sm = do
  ems <- insertByAll sm
  case ems of
    Right newkey -> return (newkey, False)
    Left oldkey -> do
             Just oldsm <- get oldkey
             when (oldsm /= sm) $ error "!!! SimbadInfo does not match !!!" -- TODO: what now?
             return (oldkey, True)

-- | Returns True if the database was updated.
insertSimbadMatch :: PersistBackend m => SimbadMatch -> m Bool
insertSimbadMatch sm = do
  n <- count (SmmTargetField ==. smmTarget sm)
  let unknown = n == 0
  when unknown (insert_ sm)
  return unknown

-- | Returns True if the database was updated.
insertSimbadNoMatch :: PersistBackend m => SimbadNoMatch -> m Bool
insertSimbadNoMatch sm = do
  n <- count (SmnTargetField ==. smnTarget sm)
  let unknown = n == 0
  when unknown (insert_ sm)
  return unknown

-- | If the record is not known - as defined by the condition
--   then add it, otherwise check the stored value and, if
--   different, replace it.
--
--   Note that this does not take advantage of keys for
--   identification or deletion, rather it uses the
--   supplied constraint.
--
--   It is required that the condition only matches 0 or 1 rows,
--   but there is no check that this is true.
--
insertOrReplace ::
  (PersistBackend m, PersistEntity v, Eq v, EntityConstr v c)
  => Cond (PhantomDb m) (RestrictionHolder v c)
  -> v
  -> m ()
insertOrReplace cond newVal = do
  ans <- select (cond `limitTo` 1)
  case ans of
    (oldVal:_) -> when (oldVal /= newVal) $ do
                                delete cond
                                insert_ newVal
    _ -> insert_ newVal

-- | Add a schedule item to the database if there is no known
--   observation for this ObsId. If the obsid already exists
--   then nothing is done (but note that there is no check that
--   the information is valid).
--
--   The return value indicates whether the item was added.
--
--   See also `addNonScienceScheduleItem`; it is not clear if
--   these need to be two separate items now.
addScheduleItem ::
  (Functor m, PersistBackend m) -- ghc 7.8 needs Functor
  => ScheduleItem
  -> m Bool
addScheduleItem si = do
  let obsid = siObsId si
  noRecord <- isNothing <$> getRecord obsid
  -- the assumption is that there is no entry in the ScheduleItem
  -- table for this record; let's see how that goes
  when noRecord (insert_ si)
  return noRecord

addNonScienceScheduleItem ::
  (Functor m, PersistBackend m) -- ghc 7.8 needs Functor
  => NonScienceObs
  -> m Bool
addNonScienceScheduleItem ns = do
  let obsid = nsObsId ns
  noRecord <- isNothing <$> getRecord obsid
  -- the assumption is that there is no entry in the ScheduleItem
  -- table for this record; let's see how that goes
  when noRecord (insert_ ns)
  return noRecord

-- | Remove unwanted information in the database. At present this is:
--
--   1. ScheduleItem entries for which there is a record.
--
--   Should cleanupDiscarded be rolled into this?
cleanDataBase :: PersistBackend m => m ()
cleanDataBase = do
  -- TODO: get the primary key to make deletion easier?
  obsids <- project SiObsIdField CondEmpty
  forM_ obsids $ \obsid -> do
    -- be lazy and make both requests even though could avoid
    -- an extra look-up if the first succeeds
    n1 <- count (SoObsIdField ==. obsid)
    n2 <- count (NsObsIdField ==. obsid)
    unless (n1 == 0 && n2 == 0) (delete (SiObsIdField ==. obsid))

-- | Ensure that any discarded science observations are removed from the
--   ScheduleItem table. It is just easier for me to do this in one place
--   rather than add a check in `insertScienceObs` and `replaceScienceObs`.
--
--   Should this be moved into `cleanDataBase`?
--
cleanupDiscarded :: PersistBackend m => m ()
cleanupDiscarded = do
  obsids <- project SoObsIdField (SoStatusField ==. discarded)
  forM_ obsids (\obsid -> delete (SiObsIdField ==. obsid))

-- | Hard-coded connection string for the database connection.
dbConnStr :: String
dbConnStr = "user=postgres password=postgres dbname=chandraobs host=127.0.0.1"

-- | Run an action against the database. This includes a call to
--   `handleMigration` before the action is run.
--
runDb :: DbPersist Postgresql (NoLoggingT IO) a -> IO a
runDb act =
  withPostgresqlConn dbConnStr (runDbConn (handleMigration >> act))
