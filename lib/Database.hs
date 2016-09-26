{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
                , getScheduleDate
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
                , fetchNoSIMBADType
                , fetchSIMBADDescendentTypes
                , fetchJointMission
                , fetchMissionInfo
                , fetchObjectTypes
                , fetchConstellation
                , fetchConstellationTypes
                , fetchCategory
                , fetchCategorySubType
                , fetchCategoryTypes
                , fetchProposal
                , fetchInstrument
                , fetchGrating
                , fetchIG
                , fetchInstrumentTypes
                , fetchGratingTypes
                , fetchIGTypes
                , fetchTOOs
                , fetchTOO
                , fetchConstraints
                , fetchConstraint
                  
                , findNameMatch
                , findProposalNameMatch
                , findTarget

                , getProposalObjectMapping

                , getExposureBreakdown
                , getNumObsPerDay

                -- , getProposalCategoryBreakdown
                , getProposalTypeBreakdown
                , getProposalType

                  -- Rather experimental
                , getExposureValues

                  -- Highly experimental
                , getTimeline
                  
                , insertScienceObs
                , insertNonScienceObs
                , replaceScienceObs
                , replaceNonScienceObs
                , insertProposal
                , insertSimbadInfo
                , insertSimbadMatch
                , insertSimbadNoMatch

                , cleanDataBase
                , cleanupDiscarded
                , insertOrReplace
                , addScheduleItem

                , updateLastModified
                , getLastModified
                  
                , putIO
                , runDb
                , dbConnStr
                , discarded
                , archived
                , notDiscarded
                , notArchived
                , notCancelled
                , isScheduled

                , SIMKey
                , keyToPair
                  
                  -- * Hack
                  --
                  -- This is needed since ObsCat doesn't store the non-science
                  -- details until after the load has been run. So the data
                  -- from the ScheduleItem table is used as a stop gap until
                  -- it can be replaced. Rather than do the right thing and
                  -- add a field to the NonScienceObs item, I am using the
                  -- nsName field as an indicator: if set to the token
                  -- nsInObsCatName then it's been taken from the
                  -- ObsCat, otherwise it's from the ScheduleItem table
                  -- (where the field value is taken from the column).
                  --
                  , nsInObsCatName
                  , notNsDiscarded
                  , notFromObsCat
                    
                ) where

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Control.Applicative ((<$>))
#endif

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Arrow (first, second)
import Control.Monad (filterM, forM, forM_, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (NoLoggingT)

import Data.Char (toUpper)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (foldl', group, groupBy, nub, sortBy)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Ord (Down(..), comparing)
import Data.Time (UTCTime(..), Day(..), getCurrentTime, addDays)

import Database.Groundhog.Core (PersistEntity, EntityConstr,
                                DbDescriptor, RestrictionHolder)
import Database.Groundhog.Core (distinct)
import Database.Groundhog.Postgresql

import Types

type DbIO m = (MonadIO m, PersistBackend m)

-- | The time used when no last-modified date can be accessed from
--   the database. Ideally would use the current time, as this seems
--   the safest, but just hard code a simple value (as this should not
--   be used anyware).
--
dummyLastMod :: UTCTime
dummyLastMod = UTCTime (ModifiedJulianDay 0) 0

archived :: T.Text
archived = "archived"

discarded :: T.Text
discarded = "discarded"

-- | Bah, the archive goes with US spelling...
cancelled :: T.Text
cancelled = "canceled"

nsInObsCatName :: T.Text
nsInObsCatName = "unknown"

-- | What is the logic to the status fields, in particular
--   unobserved versus scheduled, and does cancelled (or, rather
--   canceled, mean that this ObsId is not going to be re-used?
--
--   Question: should unobserved observations be removed from
--             (most) queries?
--
notArchived ::
  DbDescriptor db
  => Cond db (RestrictionHolder ScienceObs ScienceObsConstructor)
notArchived = SoStatusField /=. archived

notDiscarded ::
  DbDescriptor db
  => Cond db (RestrictionHolder ScienceObs ScienceObsConstructor)
notDiscarded = SoStatusField /=. discarded

notCancelled ::
  DbDescriptor db
  => Cond db (RestrictionHolder ScienceObs ScienceObsConstructor)
notCancelled = SoStatusField /=. cancelled

-- | Reject those observations which have no scheduled time
--   *or* that are labelled as being cancelled.
--
isScheduled ::
  DbDescriptor db
  => Cond db (RestrictionHolder ScienceObs ScienceObsConstructor)
isScheduled = (SoStartTimeField <. futureTime) &&. notCancelled

-- | Identify non-science observations that are not from the
--   ObsCat (i.e. ones that could be queried to see if there
--   is data now). This excludes "discarded" observations
--   (see `notNsDiscarded` for more information on this).
--
notFromObsCat ::
  DbDescriptor db
  => Cond db (RestrictionHolder NonScienceObs NonScienceObsConstructor)
notFromObsCat = (NsNameField /=. nsInObsCatName) &&. notNsDiscarded

-- | Non-science observations that have not been "discarded",
--   that is, the ObsCat field does not have a start date field
--   (have to be careful, since the ObsCat field appears to have
--    no start date when the object hasn't been observed yet as
--    well).
--
notNsDiscarded ::
  DbDescriptor db
  => Cond db (RestrictionHolder NonScienceObs NonScienceObsConstructor)
notNsDiscarded = NsNameField /=. discarded

-- | Is this ObsId discarded (either science or non-science) or
--   unscheduled?
isDiscardedOrUnscheduled ::
  PersistBackend m
  => ObsIdVal
  -> m Bool  -- True is only returned if this is a discarded or unscheduled obs.
isDiscardedOrUnscheduled oid = do
  n1 <- count ((SoObsIdField ==. oid) &&.
               ((SoStatusField ==. discarded) ||. (SoStartTimeField ==. futureTime)))
  n2 <- count ((NsObsIdField ==. oid) &&.
               (NsNameField ==. discarded))
  return ((n1 /= 0) || (n2 /= 0))


-- | Given two lists of observations, which are assumed to have 0 or 1
--   elements in, identify which is the latest (has the largest start
--   time) or earliest.
--
--   These should be consolidated (earliest and latest versions).
--
--   At present, neither variant rejects those science obs with a
--   start date == futureTime. It is assumed that the caller has
--   done any such filtering.
--
identifyLatestRecord ::
  [ScienceObs]
  -> [NonScienceObs]
  -> Maybe Record
identifyLatestRecord = identifyHelper soStartTime nsStartTime (>)

identifyEarliestRecord ::
  [ScienceObs]
  -> [NonScienceObs]
  -> Maybe Record
identifyEarliestRecord = identifyHelper soStartTime nsStartTime (<)

identifyHelper ::
  Ord c
  => (a -> c)
  -> (b -> c)
  -> (Maybe c -> Maybe c -> Bool)
  -- ^ ordering option; if True pick the first (a), otherwise
  --   b
  -> [a]
  -> [b]
  -> Maybe (Either b a)
identifyHelper _ _ _ [] [] = Nothing
identifyHelper _ _ _ (x:_) [] = Just (Right x)
identifyHelper _ _ _ [] (y:_) = Just (Left y)
identifyHelper px py ord xs ys =
  -- rely on Ord instance of Maybe for the comparison
  let mx = listToMaybe xs
      my = listToMaybe ys
      ox = px <$> mx
      oy = py <$> my
  in if ord ox oy then Right <$> mx else Left <$> my

-- | Return the last observation (science or non-science) to be
--   scheduled before the requested time. The observation can be
--   finished or running. It will not be a discarded observation,
--   one with no scheduled time, or labelled as cancelled.
--
findRecord :: (PersistBackend m) => UTCTime -> m (Maybe Record)
findRecord t = do
  let tval = ChandraTime t
  xs <- select (((SoStartTimeField <=. tval)
                 &&. notDiscarded
                 &&. isScheduled)
                `orderBy` [Desc SoStartTimeField]
                `limitTo` 1)
  ys <- select (((NsStartTimeField <=. tval) &&. notNsDiscarded)
                `orderBy` [Desc NsStartTimeField]
                `limitTo` 1)

  return (identifyLatestRecord xs ys)

-- | Return information on this obsid, if known about. This includes
--   discarded and non-scheduled observations.
--
findObsId :: PersistBackend m => ObsIdVal -> m (Maybe Record)
findObsId oi = do
  msobs <- findScience oi
  case msobs of
    Just sobs -> return (Just (Right sobs))
    _ -> do
      ans <- findNonScience oi
      return (Left <$> ans)

-- | Return information on this non-science observation. This includes
--   discarded observations.
--
findNonScience :: PersistBackend m => ObsIdVal -> m (Maybe NonScienceObs)
findNonScience oi = do
  ans <- select ((NsObsIdField ==. oi) `limitTo` 1)
  return (listToMaybe ans)

-- | This will return a discarded or unscheduled observation.
findScience :: PersistBackend m => ObsIdVal -> m (Maybe ScienceObs)
findScience oi = do
  ans <- select ((SoObsIdField ==. oi) `limitTo` 1)
  return (listToMaybe ans)

-- | Return the current observation (ignoring discarded observations,
--   but in this case it is unlikely that the database will have been
--   updated with the discard information in time, or unscheduled
--   observations).
--
getCurrentObs :: DbIO m => m (Maybe Record)
getCurrentObs = liftIO getCurrentTime >>= findRecord

-- | Return the first observation to start after the given time,
--   excluding discarded and unscheduled observations. If the input observation
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
  flag <- isDiscardedOrUnscheduled oid
  if flag
    then return Nothing
    else do
    xs <- select (((SoStartTimeField >. t)
                   &&. (SoObsIdField /=. oid)
                   &&. notDiscarded
                   &&. isScheduled)
                  `orderBy` [Asc SoStartTimeField]
                  `limitTo` 1)
    ys <- select (((NsStartTimeField >. t)
                   &&. (NsObsIdField /=. oid)
                   &&. notNsDiscarded)
                  `orderBy` [Asc NsStartTimeField]
                  `limitTo` 1)
    return (identifyEarliestRecord xs ys)


-- | Return the last observation to have started before the given time,
--   excluding discarded and unscheduled observations. If the input observation
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
  flag <- isDiscardedOrUnscheduled oid
  if flag
    then return Nothing
    else do
    xs <- select (((SoStartTimeField <. t)
                   &&. (SoObsIdField /=. oid)
                   &&. notDiscarded
                   &&. isScheduled)  -- at present isscheduled adds nothing
                  `orderBy` [Desc SoStartTimeField]
                  `limitTo` 1)
    ys <- select (((NsStartTimeField <. t)
                   &&. (NsObsIdField /=. oid)
                   &&. notNsDiscarded)
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
--   including discarded and unscheduled observations.
--
--   TODO: note that this actually also returns non-science
--         observations, so either the comment or code should change.
getObsId ::
  (Functor m, PersistBackend m)  -- Functor needed for ghc < 7.10
  => ObsIdVal
  -> m (Maybe ObsInfo)
getObsId = findObsInfo 

-- | Return the record of the given observation, if it
--   exists. This includes discarded and unscheduled observations.
--
getRecord :: PersistBackend m => ObsIdVal -> m (Maybe Record)
getRecord = findObsId

-- | TODO: handle the case when the current observation, which has
--   just started, has an exposure time > ndays.
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
      -- TODO: why not
      --   dayStart = addDays (- fromIntegral ndays) day
      -- ?
      day = toModifiedJulianDay dayNow - fromIntegral ndays
      dayStart = ModifiedJulianDay day

      tStart = ChandraTime (UTCTime dayStart 0)
      tEnd   = ChandraTime (UTCTime dayEnd 0)

  -- Select the science and non-science observations separately
  -- and then merge them. As the science observations have no
  -- end-time field, post-process the database results here,
  -- rather than having the database do all the filtering.
  --
  -- TODO: not sure this is a good idea any more
  --
  xs1 <- select (((SoStartTimeField <. tEnd)
                  &&. notDiscarded
                  &&. isScheduled) -- the isScheduled check is unlikely to restrict anything
                 `orderBy` [Asc SoStartTimeField])
  ys1 <- select (((NsStartTimeField <. tEnd) &&.
                  notNsDiscarded)
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

  simbad <- getSimbadList res
  return (Schedule now ndays done mdoing todo simbad)

-- | TODO: handle the case when the current observation, which has
--   just started, has an exposure time > ndays.
--
getScheduleDate ::
  DbIO m
  => Day    -- ^ center the schedule on this day
  -> Int    -- ^ Number of days to go back/forward
  -> m Schedule
getScheduleDate day ndays = do
  -- return all observations which have any part of their
  -- observation within the time span tStart to tEnd.
  --
  let dayEnd = addDays (fromIntegral ndays + 1) day
      dayStart = addDays (- fromIntegral ndays) day

      tStart = ChandraTime (UTCTime dayStart 0)
      tEnd   = ChandraTime (UTCTime dayEnd 0)
      
  -- Select the science and non-science observations separately
  -- and then merge them. As the science observations have no
  -- end-time field, post-process the database results here,
  -- rather than having the database do all the filtering.
  --
  -- TODO: not sure this is a good idea any more
  --
  xs1 <- select (((SoStartTimeField <. tEnd)
                  &&. notDiscarded
                  &&. isScheduled) -- the isScheduled check is unlikely to restrict anything
                 `orderBy` [Asc SoStartTimeField])
  ys1 <- select (((NsStartTimeField <. tEnd) &&.
                  notNsDiscarded)
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
      
  simbad <- getSimbadList res

  -- TODO: this logic is more general than getSchedule, perhaps
  --       it should be used there too?
  --       HOWEVER, have to be careful; in getSchedule I have
  --       labelled the "next" observation as current - i.e.
  --       there's always expected to be a current observation
  --       (unless not updated the database), whereas here
  --       it is less clear how to match that logic cleanly
  --
  now <- liftIO getCurrentTime
  let cnow = ChandraTime now

  let sched
        | tStart > cnow = Schedule now ndays [] Nothing res simbad
        | tEnd < cnow   = Schedule now ndays res Nothing [] simbad
        | otherwise     = Schedule now ndays done mdoing todo simbad

      (aprevs, todo) = span ((<= cnow) . recordStartTime) res
      (mdoing, done) = case reverse aprevs of
        [] -> (Nothing, [])
        (current:cs) -> (Just current, reverse cs)

  return sched

-- | Creates a schedule structure of the list of observations.
--
--   The number-of-days field in the structure is set to 0; this
--   is not ideal!
--
--   Note that any discarded observations are removed.
--   Unscheduled ones are left in
--
makeSchedule ::
  DbIO m
  => SortedList StartTimeOrder Record -- ^ no duplicates
  -> m Schedule
makeSchedule rs = do
  now <- liftIO getCurrentTime
  mrec <- findRecord now
  
  let cleanrs = filter keep (fromSL rs)
      keep (Left NonScienceObs{..}) = nsName /= discarded
      keep (Right ScienceObs{..}) = soStatus /= discarded
                                    -- && (soStartTime < futureTime)

      mobsid = recordObsId <$> mrec
      findNow r = if Just (recordObsId r) == mobsid then Right r else Left r
      (others, nows) = partitionEithers (map findNow cleanrs)

      cnow = ChandraTime now
      (done, todo) = span ((<= cnow) . recordStartTime) others

  simbad <- getSimbadList cleanrs
  return (Schedule now 0 done (listToMaybe nows) todo simbad)


-- | Find the SIMBAD records for the input science observations.
--
getSimbadList ::
  PersistBackend m
  => [Record]  -- ^ records; assumed to be filtered
  -> m (M.Map T.Text SimbadInfo)
getSimbadList rs = do
  let getName = either (const Nothing) (Just . soTarget)
      tnames = nub (mapMaybe getName rs)

  -- Is it best to index on target name or ObsId or ...?
  -- Could probably do this in one call, using `in_`, and then
  -- returning (SmmTargetField, SmmInfoField), so that it can
  -- be used to create the output map
  mtargets <- forM tnames $ \tname -> do
    ans <- project SmmInfoField ((SmmTargetField ==. tname) `limitTo` 1)
    case ans of
      [key] -> do
        val <- get key
        return ((tname, ) <$> val)
      _ -> return Nothing

  return (M.fromList (catMaybes mtargets))



-- | Do we have any SIMBAD information about the target?
--
getSimbadInfo :: 
  PersistBackend m
  => T.Text   -- ^ target name (not the actual SIMBAD search term)
  -> m (Maybe SimbadInfo)
getSimbadInfo target = do
  keys <- project SmmInfoField ((SmmTargetField ==. target) `limitTo` 1)
  case keys of
    [key] -> get key
    _ -> return Nothing

-- | Return all observations of the given SIMBAD type (excluding
--   discarded). This restricts to the type only (i.e. no descendents).
--
--   There is NO special-case handling to support listing those objects
--   with no SIMBAD info (i.e. if stype == noSimbadType).
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
                                 \t -> select ((SoTargetField ==. t
                                                &&. notDiscarded
                                                -- &&. isScheduled
                                               )
                                               `orderBy` [Asc SoStartTimeField])

                          return (concat obs)

               -- could create sorted lists and then combine them,
               -- but that has issues, so do it manually
               let xs = concat sos
                   ys = toSL soStartTime xs
               return (Just ((stype, ltype), ys))

    _ -> return Nothing

-- | Identify those observations with no SIMBAD mapping.
fetchNoSIMBADType :: 
  PersistBackend m
  => m (SimbadTypeInfo, SortedList StartTimeOrder ScienceObs)
fetchNoSIMBADType = do

  -- TODO: perhaps should just query SmnTargetField and then
  --       do a reverse lookup to get the sources?
  --
  sobs <- select (CondEmpty `orderBy` [Asc SoStartTimeField])
  sinfo <- project SmmTargetField (distinct CondEmpty)

  let inSimbad = S.fromList sinfo
      noSimbad ScienceObs{..} = soTarget `S.notMember` inSimbad

      out = unsafeToSL (filter noSimbad sobs)

  return ((noSimbadType, noSimbadLabel), out)


{-
TODO: should we return the count of number of distinct objects (rather than observations)
in the simbad-related queries

TODO: support "parent" queries
-}

-- | Return all observations of the given SIMBAD type and any
--   'children' of this type (excluding discarded observations).
--
--   TODO: perhaps this should be sent in a list of types to return
--         so that the caller can decide (and let them easily
--         check if they got all the types they asked for).
--
fetchSIMBADDescendentTypes :: 
  PersistBackend m
  => SimbadType 
  -> m ([SimbadTypeInfo], SortedList StartTimeOrder ScienceObs)
  -- ^ SymbadTypeInfo list is ordered by SimbadCode setting, and
  --   the first element is for the parent, even if no observations
  --   match it.
fetchSIMBADDescendentTypes parent = do

  -- can we let the database do the time sorting?
  let children = map _2 (findChildTypes parent)
      cons = map (SmiType3Field ==.) (parent : children)
      constraint = foldl1 (||.) cons

  sinfos <- project (SmiType3Field, SmiTypeField) (distinct constraint)
  keys <- project AutoKeyField constraint
  if null keys
    then return ([], emptySL)
    else do
      -- can use foldl1 below because have checked that keys is not empty
      let keycons = map (SmmInfoField ==.) keys
          keyconstraint = foldl1 (||.) keycons

      targets <- project SmmTargetField keyconstraint

      let targcons = map (SoTargetField ==.) targets
          targconstraint = foldl1 (||.) targcons

      obs <- select ((targconstraint &&. notDiscarded)
                     `orderBy` [Asc SoStartTimeField])

      let addCode sinfo =
            case simbadTypeToCode (fst sinfo) of
              Just scode -> Just (scode, sinfo)
              _ -> Nothing
          sorted = sortBy (compare `on` fst) (mapMaybe addCode sinfos)

          ptype = (parent, fromMaybe "" (simbadTypeToDesc parent))
          out = case map snd sorted of
            xs@(p:_) | p == ptype -> xs
            xs -> ptype : xs
      
      return (out, unsafeToSL obs)


-- | Return all observations that are joint with the given mission.
fetchJointMission :: 
  PersistBackend m
  => JointMission
  -> m (SortedList StartTimeOrder ScienceObs)
fetchJointMission jm = do

  {- It would be nice if we could use this, but there are
     joint proposals, e.g. CXO-HST, for which the
     joint observatory time is 0.

  let emptyField = Nothing :: Maybe TimeKS
  sobs <- select (((missionSelectorField jm /=. emptyField)
                  &&. notDiscarded)
                  `orderBy` [Asc SoStartTimeField])

  return (unsafeToSL sobs)
  -}

  {-
  I'd like to say

  sobs <- select (SoJointWith `like` "%HST%")

  but I don't know how to "lift" into the Maybe String type,
  so do this manually instead.
  -}

  sobs <- select ((Not (isFieldNothing SoJointWithField)
                   &&. notDiscarded)
                  `orderBy` [Asc SoStartTimeField])

  let hasMission ScienceObs{..} = case soJointWith of
        Just ms -> includesMission jm ms
        Nothing -> False

  return (unsafeToSL (filter hasMission sobs))
  
-- | Return basic information on the joint-with observations.
--
--   Note that the time fields are a bit confusing, so don't have
--   them for all observations, so just report number of observations.
--   Would be nice perhaps to have number of targets.
--
fetchMissionInfo ::
  (Functor m, PersistBackend m) -- Functor needed before ghc 7.10
  => m [(JointMission, Int)]
  -- ^ Number of observations for each mission
fetchMissionInfo = do
  jws <- map fromJust <$>
         project SoJointWithField
         (Not (isFieldNothing SoJointWithField)
          &&. notDiscarded)

  let toks = map (,1) (concatMap splitToMission jws)
  return (M.toList (M.fromListWith (+) toks))

-- | Return information on the object types we have stored.
--
--   The return value includes the number of objects that 
--   have the given type.
--
fetchObjectTypes :: 
  PersistBackend m
  => m [(SimbadTypeInfo, Int)]
  -- ^ Simbad information and the number of objects that match
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
  ans <- select ((SoConstellationField ==. con
                  &&. notDiscarded
                  -- &&. isScheduled
                 )
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


-- | Return count of the constellations.
--   Discarded observations are excluded.
--
fetchConstellationTypes ::
  PersistBackend m
  => m [(ConShort, TimeKS)]
  -- ^ returns the total exposure time spent on targets
  --   in the constellation.
fetchConstellationTypes = do
  res <- project (SoConstellationField
                 , (SoApprovedTimeField, SoObservedTimeField))
         (notDiscarded -- &&. isScheduled
          `orderBy` [Asc SoConstellationField])

  let ms = map (second (uncurry fromMaybe)) res
      ts = M.fromListWith addTimeKS ms
  return (M.toAscList ts)
    
-- | Return observations which match this category,
--   excluding discarded observations.
--
fetchCategory ::
  PersistBackend m
  => PropCategory
  -> m (SortedList StartTimeOrder ScienceObs)
fetchCategory cat = do
  propNums <- project PropNumField (PropCategoryField ==. cat)
  -- TODO: could use in_?
  sos <- forM propNums $ \pn ->
    select (SoProposalField ==. pn &&. notDiscarded) --  &&. isScheduled)
  let xs = concat sos
  return (toSL soStartTime xs)

-- | Return observations which match this category and have
--   the given SIMBAD type (which can also be unidentified).
--
fetchCategorySubType ::
  (Functor m, PersistBackend m, SqlDb (PhantomDb m)) -- ghc 7.8 needs Functor
  => PropCategory  -- ^ proposal category
  -> Maybe SimbadType
  -- ^ If Nothing, use the Unidentified type
  -> m (SortedList StartTimeOrder ScienceObs)
fetchCategorySubType cat mtype = do

  propNums <- project PropNumField (PropCategoryField ==. cat)
  sobs <- select (((SoProposalField `in_` propNums)
                   &&. notDiscarded)
                  `orderBy` [Asc SoStartTimeField])

  -- How much of this logic is in fetchSIMBADType and
  -- fetchNoSIMBADType?
  --
  let matchSIMBAD ScienceObs{..} = do
        mkey <- listToMaybe <$> project SmmInfoField (SmmTargetField ==. soTarget)
        case mkey of
          Just key -> case mtype of
            Nothing -> return False
            Just _ -> do
              otype <- listToMaybe
                       <$> project SmiType3Field (AutoKeyField ==. key)
              return (otype == mtype)
                        
          Nothing -> return (isNothing mtype)

  out <- filterM matchSIMBAD sobs
  return (unsafeToSL out)


-- | Return information on the category types.
--
fetchCategoryTypes ::
  PersistBackend m
  => m [(PropCategory, Int)]
  -- ^ proposal category and the number of proposals that match
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
--   excluding discarded.
--
fetchInstrument ::
  PersistBackend m
  => Instrument
  -> m (SortedList StartTimeOrder ScienceObs)
fetchInstrument inst = do
  ans <- select $ (SoInstrumentField ==. inst &&. notDiscarded) --  &&. isScheduled)
         `orderBy` [Asc SoStartTimeField]
  return (unsafeToSL ans)

-- | Return all the observations which match this grating,
--   excluding discarded.
--
fetchGrating ::
  PersistBackend m
  => Grating
  -> m (SortedList StartTimeOrder ScienceObs)
fetchGrating grat = do
  ans <- select $ (SoGratingField ==. grat &&. notDiscarded) --  &&. isScheduled)
         `orderBy` [Asc SoStartTimeField]
  return (unsafeToSL ans)

-- | Return all the observations which match this instrument and grating,
--   excluding discarded.
--
fetchIG ::
  PersistBackend m
  => (Instrument, Grating)
  -> m (SortedList StartTimeOrder ScienceObs)
fetchIG (inst, grat) = do
  ans <- select $ ((SoInstrumentField ==. inst)
                   &&. (SoGratingField ==. grat)
                   &&. notDiscarded) --  &&. isScheduled)
         `orderBy` [Asc SoStartTimeField]
  return (unsafeToSL ans)

-- | This counts up the individual observations; should it try and group by
--   "proposal", or at least "object per proposal"?
--
--   Discarded observations are excluded.
--
fetchInstrumentTypes ::
  PersistBackend m
  => m [(Instrument, Int)]
  -- ^ instrument and the number of observations that match
fetchInstrumentTypes = do
  insts <- project SoInstrumentField (notDiscarded `orderBy`
                                      [Asc SoInstrumentField])
  return (countUp insts)

-- | As `fetchInstrumentTypes` but for gratings.
--
--   Discarded observations are excluded.
--
fetchGratingTypes ::
  PersistBackend m
  => m [(Grating, Int)]
  -- ^ grating and the number of observations that match
fetchGratingTypes = do
  grats <- project SoGratingField (notDiscarded `orderBy`
                                   [Asc SoGratingField])
  return (countUp grats)

-- | A combination of `fetchInstrumentTypes` and `fetchGratingTypes`.
--
--   Discarded observations are excluded.
--
fetchIGTypes ::
  PersistBackend m
  => m [((Instrument, Grating), Int)]
  -- ^ instrument + grating combo and the number of observations that match
fetchIGTypes = do
  igs <- project (SoInstrumentField, SoGratingField)
         (notDiscarded `orderBy`
          [Asc SoInstrumentField, Asc SoGratingField])
  return (countUp igs)


-- | What is the breakdown of TOOs?
--
--   TODO: give some indication of the total exposure time
--         (or some other metric) so that we can view as
--         a fraction?
--
fetchTOOs ::
  PersistBackend m
  => m ([(TOORequestTime, TimeKS)], TimeKS)
  -- ^ The TOO period and the associated time; the
  --   second component is the time for those observations
  --   with no constraint.
fetchTOOs = do
  res <- project (SoTOOField,
                  (SoApprovedTimeField, SoObservedTimeField))
         (notDiscarded `orderBy` [Asc SoTOOField])
  let (nones, toos) = partitionEithers (map convField res)

      getTExp = uncurry fromMaybe
      convField (Just a, b) = Right (a, getTExp b)
      convField (Nothing, b) = Left (getTExp b)

      noneTime = foldl' addTimeKS zeroKS nones

      ms = map (first trType) toos
      ts = M.fromListWith addTimeKS ms
      
  return (M.toAscList ts, noneTime)

{-
-- | The set of TOO labels in the database. This
--   does not include "none".
--
getTOOLabels :: PersistBackend m => m [TOORequestTime]
getTOOLabels = do
  ans <- catMaybes <$> project SoTOOField (distinct CondEmpty)
  -- have to remove duplicates since the distinct check above
  -- is not going to consolidate multiple time ranges within a
  -- period
  return (map trType (nub ans))
-}


-- | Return the schedule for a given TOO period.
--
fetchTOO ::
  PersistBackend m
  => Maybe TOORequestTime
  -- Nothing means "no TOO"
  -> m (SortedList StartTimeOrder ScienceObs)
fetchTOO Nothing = do
  ans <- select ((isFieldNothing SoTOOField
                  &&. notDiscarded)
                 `orderBy` [Asc SoStartTimeField])
  return (unsafeToSL ans)
  
fetchTOO too = do
  -- It would be nice to say something like
  --      (SoTOOField ==. Just (rtToRequest too))
  -- but this doesn't work, since the DB representation
  -- is the low-level string type.
  --
  -- Ideally the filtering would all be done in the DB,
  -- but it's easier to do this in Haskell. I do not
  -- want to hard-code the possible mappings from
  -- too to the string representation, as that is fragile.
  --
  allAns <- select ((Not (isFieldNothing SoTOOField)
                     &&. notDiscarded)
                    `orderBy` [Asc SoStartTimeField])
  let ans = filter isTOO allAns
      isTOO ScienceObs{..} = trType `fmap` soTOO == too
  return (unsafeToSL ans)


-- | What is the breakdown of the constraints?
--
fetchConstraints ::
  PersistBackend m
  => m ([(ConstraintKind, TimeKS)], TimeKS)
  -- ^ The constraint type period and the associated time; the
  --   second component is the time for those observations
  --   with no constraint. Note that because an observation
  --   can have multiple constraints, the sum of all these
  --   times can exceed the actual observing time.
fetchConstraints = do
  res <- project ((SoTimeCriticalField, SoMonitorField, SoConstrainedField),
                  (SoApprovedTimeField, SoObservedTimeField))
         notDiscarded
  let (nones, cs) = partitionEithers (concatMap convField res)

      getTExp = uncurry fromMaybe
      convField ((NoConstraint, NoConstraint, NoConstraint), t)
        = [Left (getTExp t)]
      convField ((a,b,c), t) =
        let tout = getTExp t
            go (_, NoConstraint) = []
            go (l, _) = [Right (l, tout)]
        in concatMap go [ (TimeCritical, a)
                        , (Monitor, b)
                        , (Constrained, c) ]

      noneTime = foldl' addTimeKS zeroKS nones

      ts = M.fromListWith addTimeKS cs
      
  return (M.toAscList ts, noneTime)


getCon ::
  DbDescriptor db
  => Maybe ConstraintKind
  -> Cond db (RestrictionHolder ScienceObs ScienceObsConstructor)
getCon Nothing =
  (SoTimeCriticalField ==. NoConstraint)
  &&. (SoMonitorField ==. NoConstraint)
  &&. (SoConstrainedField ==. NoConstraint)
getCon (Just TimeCritical) = SoTimeCriticalField /=. NoConstraint
getCon (Just Monitor) = SoMonitorField /=. NoConstraint
getCon (Just Constrained) = SoConstrainedField /=. NoConstraint
  
-- | Return the schedule for a given constraint.
--
fetchConstraint ::
  PersistBackend m
  => Maybe ConstraintKind
  -- Nothing means "no constraint"
  -> m (SortedList StartTimeOrder ScienceObs)
fetchConstraint mcs = do
  ans <- select ((getCon mcs
                  &&. notDiscarded)
                 `orderBy` [Asc SoStartTimeField])
  return (unsafeToSL ans)

  
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
--   was discarded. It will return unscheduled observations.
--
getProposalObs ::
  PersistBackend m
  => ScienceObs
  -> m (SortedList StartTimeOrder ScienceObs)
getProposalObs ScienceObs{..} = do
  -- time sorting probably not needed here
  ans <- select (((SoProposalField ==. soProposal)
                  &&. (SoObsIdField /=. soObsId)
                  &&. notDiscarded)
                 `orderBy` [Asc SoStartTimeField])
  return (unsafeToSL ans)

-- | Find all the other non-discarded observations in the same proposal that
--   are in the database (including unscheduled observations).
--
getRelatedObs ::
  PersistBackend m
  => PropNum
  -> ObsIdVal
  -> m (SortedList StartTimeOrder ScienceObs)
getRelatedObs propNum obsId = do
  ans <- select (((SoProposalField ==. propNum)
                  &&. (SoObsIdField /=. obsId)
                  &&. notDiscarded)
                 `orderBy` [Asc SoStartTimeField])
  return (unsafeToSL ans)

-- | Return the observations we know about for the given proposal
--   (that are not discarded). This includes unscheduled observations.
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
--   This INCLUDES discarded and unscheduled observations.
--
findObsStatusTypes :: PersistBackend m => m [(T.Text, Int)]
findObsStatusTypes = do
  statuses <- project SoStatusField (CondEmpty `orderBy` [Asc SoStatusField])
  return (countUp statuses)


-- | Try supporting "name matching". This is complicated by the fact that there
--   are both the target names (soTarget) and the SIMBAD-matched names (smmTarget)
--   that could be searched. 
--
--   TODO: SIMBAD names can have multiple spaces (e.g. NGC family, so a query of
--         'NGC 3' should probably map to '%NGC% %3%' - but then this matches
--         'NGC 623' which might not be the expected result).
--
--         Also, should protect/remove the search-specific terms.
--
findNameMatch ::
  (PersistBackend m, SqlDb (PhantomDb m))
  => String
  -- ^ a case-insensitive match is made for this string; an empty string matches
  --   everything
  -> m ([T.Text], [T.Text])
  -- ^ object names - first the target names, then the "also known as" from SIMBAD
  --   - names in the database.
findNameMatch instr = do
  let matchStr = '%' : map toUpper instr ++ "%"
  targets <- project SoTargetField (distinct (upper SoTargetField `like` matchStr))
  simbads <- project SmiNameField (distinct (upper SmiNameField `like` matchStr))
  return (targets, simbads)

-- | Find proposals whose titles match the given string
findProposalNameMatch ::
  (PersistBackend m, SqlDb (PhantomDb m))
  => String
  -- ^ a case-insensitive match is made for this string; an empty string matches
  --   everything
  -> m [(T.Text, PropNum)]
  -- ^ Matching proposal titles and numbers. The ordering is not set (in that I
  --   haven't decided if it's worth enforcing an ordering).
findProposalNameMatch instr = 
  let matchStr = '%' : map toUpper instr ++ "%"
  in project (PropNameField, PropNumField)
     ((upper PropNameField `like` matchStr) `orderBy` [Asc PropNameField])


-- | Find observations of the given target. The input name is searched - using a
--   case-insensitive match - against both the observation target name and
--   Simbad matches.
--
--   It is related to `findNameMatch`.
--
--   TODO:
--     improve space matching
--     filter out % and _ ?
--
findTarget ::
  (PersistBackend m, SqlDb (PhantomDb m))
  => T.Text
  -> m (SortedList StartTimeOrder ScienceObs, [TargetName])
  -- ^ Returns a list of matching observations and the list of
  --   "SIMBAD" names, that is, the names that are considered
  --   the primary values for the source. I would hope that
  --   there can only be one name here, but need to convince
  --   myself of that, so leaving as a list.
  --
findTarget target = do
  -- have to do a direct search on SoTargetField, and an
  -- indirect search in case the match is against the SmiNameField.
  --
  -- TODO: need a better story with spaces - would like to ignore
  --       them completely
  let searchTerm = T.toUpper target
  direct <- select ((upper SoTargetField ==. searchTerm)
                    `orderBy` [Asc SoStartTimeField])

  -- find the "Simbad" name for these targets
  let tnames = map soTarget direct
  skeys <- project SmmInfoField (SmmTargetField `in_` tnames)
  sfields <- project (AutoKeyField, SmiNameField) (AutoKeyField `in_` skeys)
  let (sauto, snames) = unzip sfields

  -- find those names that map to the "Simabd" names
  smatches <- project SmmTargetField (SmmInfoField `in_` sauto)

  -- remove those names we already have data for
  let onames = S.fromList smatches `S.difference` S.fromList tnames
  indirect <- select ((SoTargetField `in_` S.toList onames)
                      `orderBy` [Asc SoStartTimeField])

  -- hopefully there are no repeats in these two lists
  let sobs = mergeSL soStartTime (unsafeToSL direct) (unsafeToSL indirect)
  return (sobs, snames)

type NumSrc = Int
type NumObs = Int

-- | Only used to provide an Ord instance, which is just the
--   ordering of the first element (since it is taken that there
--   is a unique mapping between the two components of the
--   tuple).
--
newtype SIMKey = SK (SIMCategory, SimbadType)

instance Eq SIMKey where
  (SK (a1, _)) == (SK (a2, _)) = a1 == a2
  
instance Ord SIMKey where
  compare (SK (a1, _)) (SK (a2, _)) = compare a1 a2

keyToPair :: SIMKey -> (SIMCategory, SimbadType)
keyToPair (SK a) = a

unidentifiedKey :: SIMKey
unidentifiedKey = SK (noSimbadLabel, noSimbadType)

-- | Return the number of object types per proposal category.
--
--   TODO: can this be made more efficient?
--         (it appears to be better than it was, but could
--         probably be improved)
--
getProposalObjectMapping ::
  (Functor m, PersistBackend m) -- ghc 7.8 needs Functor
  => m (M.Map (PropCategory, SIMKey) (TimeKS, NumSrc, NumObs),
        UTCTime)
  -- ^ The keys are the proposal category and SIMBAD type.
  --   The values are the total time, the number of objects,
  --   and the number of observations of these objects.
  --
  --   There is a special SIMKey value - namely the vakue
  --   unidentifiedKey - which is used for those sources with
  --   no SIMBAD information.
  --
  --   The time is the last-modified time for the database
getProposalObjectMapping = do

  -- Ideally a lot of this aggregation could be done by the database
  -- but for now it is simpler to do it here. It makes sense to
  -- get "all" the information here, since all the science obs,
  -- proposals, and simbad types should be used (bar the odd case
  -- where maybe the only type or proposal is related to a discarded
  -- observation, but this should be a small fraction of the data).
  --
  -- The assumption is that the target field value is "unique",
  -- in that repeated values refer to the same object. This is
  -- obviously not true (e.g. two observers can use 'field 1')
  -- but live with the ambiguity for now, in part because the
  -- ambiguity is embedded into the database schema.
  --
  -- I have added explicit type annotations to make it clearer
  -- what is going on.
  --
  -- The ordering used by the database and Haskell should be the
  -- same for the proposal number, so I can use fromAscList.
  --
  propsDb <- project (PropNumField, PropCategoryField)
             (CondEmpty `orderBy` [Asc PropNumField])
  let propMap :: M.Map PropNum PropCategory
      propMap = M.fromAscList propsDb

  -- The assumption is that the mapping from target field to SIMBAD info
  -- is unique.
  --
  simList <- project (SmmTargetField, SmmInfoField) (distinct CondEmpty)
  simDb <- forM simList $ \(target, key) -> do
    ans <- listToMaybe <$> project
           (SmiTypeField, SmiType3Field)
           ((AutoKeyField ==. key) `limitTo` 1)
    return ((target,) . SK <$> ans)
    
  let simMap :: M.Map TargetName SIMKey
      simMap = M.fromList (catMaybes simDb)

  -- Process each source, skipping it if it has no proposal category
  -- (which it should have but there may be a few for which this
  -- information is missing) but including a special "unidentified"
  -- type for those with no SIMBAD type (which many do not have).
  --
  obsDb <- project (SoTargetField, SoProposalField
                   , SoApprovedTimeField, SoObservedTimeField)
           (notDiscarded &&. isScheduled)
  let convert (a, b, aTime, oTime) = do
        let scat = fromMaybe unidentifiedKey (M.lookup a simMap)
        pcat <- M.lookup b propMap
        return (a, [((pcat, scat), (fromMaybe aTime oTime, 1))])

      obsMap1 :: M.Map TargetName [((PropCategory, SIMKey), (TimeKS, NumObs))]
      obsMap1 = M.fromListWith (++) (mapMaybe convert obsDb)

      -- converting from a list of times to the total time and the number
      -- of observations (the latter probably isn't interesting, but
      -- calculate it for now).
      addElem :: (TimeKS, NumObs) -> (TimeKS, NumObs) -> (TimeKS, NumObs)
      addElem (t1,c1) (t2,c2) = (addTimeKS t1 t2, c1+c2)

      obsMap2 :: M.Map TargetName (M.Map (PropCategory, SIMKey) (TimeKS, NumObs))
      obsMap2 = M.map (M.fromListWith addElem) obsMap1

      -- Have a set of keys for a given target, so can add in
      -- the number of sources
      addNSrc ::
         M.Map k (TimeKS, NumObs)
         -> M.Map k (TimeKS, NumSrc, NumObs)
      addNSrc = M.map (\(t, nobs) -> (t, 1, nobs))

      obsMap3 :: M.Map TargetName (M.Map (PropCategory, SIMKey) (TimeKS, NumSrc, NumObs))
      obsMap3 = M.map addNSrc obsMap2

      -- could be more polymorphic, but force the types for now
      combine ::
         (TimeKS, NumSrc, NumObs)
         -> (TimeKS, NumSrc, NumObs)
         -> (TimeKS, NumSrc, NumObs)
      combine (t1, ns1, no1) (t2, ns2, no2) = (addTimeKS t1 t2, ns1+ns2, no1+no2)

      obsMap :: M.Map (PropCategory, SIMKey) (TimeKS, NumSrc, NumObs)
      obsMap = M.unionsWith combine (M.elems obsMap3)

  -- The output is a map from
  --    (Proposal category, SIMBAD type)
  -- where both are human-readable strings, to
  --    { exposureTime: ..., numberSources: ..., numberObs: ...}
  --
  addLastMod obsMap
  
-- | Work out a timeline of the data.
--
--   It looks like there are some non-science observations that
--   have been cancelled or removed, but are still in the system.
--   These can be identified as having nsName /= "unknown" but
--   in the past. A query like
--   http://cda.cfa.harvard.edu/chaser/startViewer.do?menuItem=details&obsid=52472
--   returns no data, but it does for
--   http://cda.cfa.harvard.edu/chaser/startViewer.do?menuItem=details&obsid=53718
--
--   For now, a simple solution is used to filter them out,
--   which is to remove any record which has a start time before
--   the last-modified time of the database and has nsName /= "unknown".
--   Should be cleaned up *in* the database.
--
getTimeline ::
  PersistBackend m
  => m (SortedList StartTimeOrder ScienceObs,
        SortedList StartTimeOrder NonScienceObs,
        M.Map TargetName SimbadInfo,
        [Proposal])
{-  
  => m ((SortedList StartTimeOrder ScienceObs,
         SortedList StartTimeOrder NonScienceObs,
         [Proposal]), UTCTime)
-}
getTimeline = do

  lastMod <- (ChandraTime . fromMaybe dummyLastMod) <$> getLastModified
  
  obs <- select ((notDiscarded &&. isScheduled)
                 `orderBy` [Asc SoStartTimeField])

  let unknown :: T.Text
      unknown = "unknown"
      
  ns <- select ((NsStartTimeField >. discardedTime &&.
                 (NsNameField ==. unknown ||.
                  NsStartTimeField >. lastMod))
                `orderBy` [Asc NsStartTimeField])

  -- SIMBAD info: the returned information is enough to create
  -- a map from the soTargetname field of a Science Obs and
  -- the SIMBAD information. Note that there are expected to be
  -- multiple matches to the same SIMBAD source (e.g. with different
  -- names), but I am just going to use a simple mapping for now.
  --
  -- The key returned by selectAll does not have an ord instance,
  -- which means my simple plan of using this as a key in a map
  -- falls over without some work.
  --
  -- So for now looping over each record and querying for the
  -- answer, which is not sensible!
  --
  matchInfo <- project (SmmTargetField, SmmInfoField) CondEmpty
  simbadInfo <- forM matchInfo (\(name, key) -> do
                                   Just si <- get key
                                   return (name, si))

  -- TODO: should simbadMap be evaluated?
  let simbadMap = M.fromList simbadInfo

  props <- map snd <$> selectAll
  -- addLastMod (unsafeToSL obs, unsafeToSL ns, props)
  return (unsafeToSL obs, unsafeToSL ns, simbadMap, props)


-- | Grab the last-modified date from the database and include
--   it in the return value.
--
addLastMod ::
  PersistBackend m
  => a
  -> m (a, UTCTime)
  -- ^ The time value is set to @dummyLastMod@ if no last-modified
  --   field is found in the database.
addLastMod out = do
  lastMod <- fromMaybe dummyLastMod <$> getLastModified
  return (out, lastMod)
  
-- | Work out a timeline based on instrument configuration; that is,
--   start times, exposure lengths, and instruments.
--
--   This is not perfect, since it allocates the full exposure
--   length to the day the observation starts, which is obviously
--   wrong for lengths > a day or if the observations spans
--   midnight.
--
getExposureBreakdown ::
  PersistBackend m
  => Day
  -- ^ The upper limit for when to search; this is so that items
  --   in the long-term schedule do not "show up" in the output.
  --   It is assumed that this is before `futureTime`.
  -> m (M.Map (Instrument, Grating) TimeKS
       , M.Map Day (M.Map (Instrument, Grating) TimeKS))
getExposureBreakdown maxDay = do
  let maxTime = ChandraTime (UTCTime maxDay 0)
  ans <- project (SoStartTimeField, SoApprovedTimeField
                 , SoObservedTimeField, SoInstrumentField
                 , SoGratingField)
         (((SoStartTimeField <=. maxTime) &&. notDiscarded)
          `orderBy` [Asc SoStartTimeField])

  let conv (startTime, approvTime, observTime, inst, grat) =
        let expTime = fromMaybe approvTime observTime
            key = utctDay (_toUTCTime startTime)
            val = M.singleton (inst,grat) expTime
        in (key, val)

      days = map conv ans
      
      -- the second element is guaranteed to be a singleton, so
      -- the following could probably be more efficient, but this
      -- is easy to write
      addElem = M.unionWith addTimeKS

      perDay = M.fromAscListWith addElem days
      total = M.foldl' addElem M.empty perDay
        
  return (total, perDay)


-- | Get the number of science observations per day.
--
--   This is likely horribly inefficient, as I couldn't find an easy way
--   to get the database to do this with Groundhog.
--
getNumObsPerDay ::
  PersistBackend m
  => Day
  -- ^ The upper limit for when to search; this is so that items
  --   in the long-term schedule do not "show up" in the output.
  --   It is assumed that this is before `futureTime`.
  -> m (M.Map Day Int)
  -- ^ The return value is the number of science observations
  --   in each day. For dates in the future it uses the planned
  --   observations.
getNumObsPerDay maxDay = do
  let maxTime = ChandraTime (UTCTime maxDay 0)
  times <- project SoStartTimeField
           (((SoStartTimeField <=. maxTime) &&. notDiscarded)
            `orderBy` [Asc SoStartTimeField])

  let days = map (\t -> ((utctDay . _toUTCTime) t, 1)) times
  return (M.fromAscListWith (+) days)

{-
-- | Return some hopefully-interesting statistics about the
--   proposals.
--
--   Examples include:
--    - could break down by category, type (GO vs CAL vs DDT ...),
--      cycle
--      - number of targets per proposal
--      - total proposal exposure time

getProposalCategoryBreakdown ::
  PersistBackend m
  => m ?
getProposalCategoryBreakdown = do
  props <- project PropNumField CondEmpty

-}


getProposalTypeBreakdown ::
  PersistBackend m
  => m (M.Map PropType (Int, Int, TimeKS))
  -- ^ Return the number of proposals, number of targets
  --   in this proposal (if the same target is observed in
  --   different proposals then it counts multiple times; it
  --   is the "safest" thing to do since the target mapping
  --   is not perfect; similarly, different target names
  --   for the same object are not merged: that is, this
  --   gives an idea of how many targets per proposal,
  --   not unique targets per proposal), and total exposure time.
  --
  --   *NOTE*: the number of targets is WRONG; it is currently
  --           the number of observations, not targets.
  --
getProposalTypeBreakdown = do
  propInfo <- project (PropTypeField, PropNumField) CondEmpty

  -- Is it best to read in all the target info and process it
  -- here, or do repeated calls to the database (slower, but
  -- likely less memory)
  obsInfo <- project (SoProposalField,
                      (SoApprovedTimeField, SoObservedTimeField))
             (notDiscarded `orderBy` [Asc SoProposalField])

  let getTexp = uncurry fromMaybe

      -- Number of targets per proposal, and the exposure time
      -- for these objects. The assumption is that the
      -- ordering returned by the database for the PropNum
      -- type matches the Haskell ordering.
      --
      getObs (pnum, texp) = (pnum, (1, getTexp texp))
      addObs (a1, b1) (a2, b2) = (a1 + a2, addTimeKS b1 b2)
      obsMap = M.fromAscListWith addObs (map getObs obsInfo)

      -- There should be no failures here
      convType (t, xs) = case toPropType t of
        Just tt -> Just (tt, xs)
        Nothing -> Nothing
      propInfo2 = mapMaybe convType propInfo
      propTmp = map (second (:[])) propInfo2
      propMap = M.fromListWith (++) propTmp

      propAdd =
        let addProp orig@(np,nt,te) pn = case M.lookup pn obsMap of
              Just (ntarg, texp) -> (np+1, nt + ntarg, addTimeKS te texp)
              Nothing -> orig -- this should not happen
            e0 = (0, 0, zeroKS)
        in foldl' addProp e0
           
  -- The output is a count of the number of proposals,
  -- the number of targets in the proposals, and the
  -- exposure time of the proposals for each proposal
  -- type.
  --
  return (M.map propAdd propMap)

-- | This does not include discarded observatons.  
getProposalType ::
  (PersistBackend m, SqlDb (PhantomDb m))
  => PropType
  -> m (SortedList StartTimeOrder ScienceObs)
getProposalType ptype = do
  let ptypeStr = fromPropType ptype
  pnums <- project PropNumField (PropTypeField ==. ptypeStr)
  sobs <- select (((SoProposalField `in_` pnums)
                   &&. notDiscarded)
                  `orderBy` [Asc SoStartTimeField])
  return (unsafeToSL sobs)

-- | Information on the exposure times.
--
--   Unclear at present what information to return; a
--   histogram would probably be better but much-more
--   work to create.
--
getExposureValues ::
  PersistBackend m
  => m [(T.Text, SortedList ExposureTimeOrder TimeKS)]
  -- ^ Return the lists for each cycle, with cycle=="all"
  --   for all data.
getExposureValues = do

  props <- project (PropNumField, PropCycleField) CondEmpty

  -- could `orderBy` [Asc SoApprovedTime] to get approximate
  -- ordering, but is it worth it?
  sobs <- project (SoProposalField,
                   (SoApprovedTimeField, SoObservedTimeField))
          (notDiscarded &&. isScheduled)

  -- Is it best to insert into an ordered list (so changing the
  -- list structure each iteration) or sort at the end?
  -- If iterate over a sorted (descending) list then the output should be
  -- okay.
  --
  let stimes = sortBy (compare `on` (Down . snd))
               (map (second (uncurry fromMaybe)) sobs)

      propMap = M.fromList props
  
      addElem orig@(omap, oall) (pnum, time) =
        case M.lookup pnum propMap of
          Just cyc ->
            let nmap = M.insertWith (++) cyc [time] omap
            in (nmap, time : oall)
          Nothing -> orig -- SHOULD NOT HAPPEN
          
      (map2, all2) = foldl' addElem (M.empty, []) stimes

      out = ("all", unsafeToSL all2) :
            map (second unsafeToSL) (M.toList map2)

  return out

  
putIO :: MonadIO m => T.Text -> m ()
putIO = liftIO . T.putStrLn

showSize ::
  (DbIO m, PersistEntity v)
  => String
  -> v
  -> m Int
showSize l t = do
  n <- countAll t
  putIO ("Number of " <> T.pack l <> " : " <> T.pack (show n))
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
             putIO ("  status=" <> status <> "  : " <> T.pack (show n)))
  putIO (" -> total = " <> T.pack (show (sum (map snd ns))))
  putIO ""

  -- unscheduled observations
  nuns <- count (SoStartTimeField ==. futureTime)
  putIO ("  un-scheduled science obs      = " <> T.pack (show nuns))
  
  -- non-science breakdown
  ns1 <- count notFromObsCat
  ns2 <- count (Not notNsDiscarded)
  putIO ("  non-science (not from obscat) = " <> T.pack (show ns1))
  putIO ("  non-science discarded         = " <> T.pack (show ns2))
  
  let ntot = sum [n1, n2, n3, n4, n5, n6, n7, n8]
  putIO ""
  putIO ("Number of rows              : " <> T.pack (show ntot))
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
insertScienceObs s = insertIfUnknown s (SoObsIdField ==. soObsId s)

-- | Checks that the observation is not known about before
--   inserting it.
--
--   If it already exists in the database the new value is ignored;
--   there is no check to make sure that the details match.
--
--   There is no attempt to remove the observation from the ScheduleItem
--   table.
--
insertNonScienceObs ::
  PersistBackend m
  => NonScienceObs
  -> m Bool  -- ^ True if the observation was added to the database
insertNonScienceObs ns = insertIfUnknown ns (NsObsIdField ==. nsObsId ns)

-- | Update the data in the archive with the new version; errors out
--   if the obsid is not already in the database
replaceNonScienceObs ::
  PersistBackend m
  => NonScienceObs
  -> m ()
replaceNonScienceObs ns = do
  n <- count (NsObsIdField ==. nsObsId ns)
  if n == 0
    then fail ("internal error: not in database " ++ show ns)
    else do
      -- rely on database constraint to make sure only one match
      -- really should be using the unique key for this deletion
      delete (NsObsIdField ==. nsObsId ns)
      insert_ ns

  
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
insertProposal p = insertIfUnknown p (PropNumField ==. propNum p)

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
insertSimbadMatch sm = insertIfUnknown sm (SmmTargetField ==. smmTarget sm)

-- | Returns True if the database was updated.
insertSimbadNoMatch :: PersistBackend m => SimbadNoMatch -> m Bool
insertSimbadNoMatch sm = insertIfUnknown sm (SmnTargetField ==. smnTarget sm)

-- | If there is no entity that matches the condition then
--   insert the item.
--
insertIfUnknown ::
  (PersistEntity v, PersistBackend m, EntityConstr v c)
  => v
  -> Cond (PhantomDb m) (RestrictionHolder v c)
  -> m Bool
insertIfUnknown o cond = do
  n <- count cond
  let unknown = n == 0
  when unknown (insert_ o)
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
--   then nothing is done (note that there is *no* check that
--   the information is valid).
--
--   For Non-Science observations *only*, a NonScienceObs
--   item is also added if it does not already exist. This
--   item has the nsName field set to the value from the
--   input item. This lets down-stream determine that it
--   needs to be updated with the obscat record, once it
--   is available.
--
addScheduleItem ::
  (Functor m, PersistBackend m) -- ghc 7.8 needs Functor
  => (ScheduleItem, Maybe NonScienceObs)
  -> m Bool  -- ^ True if the item was added to the database
addScheduleItem (si, mns) = do
  let obsid = siObsId si
  noRecord1 <- isNothing <$> getRecord obsid

  -- although there are constraints on the obsid field,
  -- let's do an explicit check.
  noRecord <- if noRecord1
              then do
                n <- count (SiObsIdField ==. obsid)
                return (n == 0)
              else return False

  -- by this point we know there's no NonScienceObs
  -- version of the data.
  when noRecord $ do
    insert_ si
    case mns of
      Just ns -> do
        when (nsName ns == nsInObsCatName)
          (fail ("*** Internal invariant not maintained: " ++ show ns))
        insert_ ns
      _ -> return ()
    
  return noRecord


-- | Remove unwanted information in the database. At present this is:
--
--   1. ScheduleItem entries for which there is a record (science or non-science).
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
--   Do the same for the non-science observations.
--
--   Should this be moved into `cleanDataBase`?
--
cleanupDiscarded :: PersistBackend m => m ()
cleanupDiscarded = do
  sobsids <- project SoObsIdField (Not notDiscarded)
  forM_ sobsids (\obsid -> delete (SiObsIdField ==. obsid))
  
  nsobsids <- project NsObsIdField (Not notNsDiscarded)
  forM_ nsobsids (\obsid -> delete (SiObsIdField ==. obsid))

-- | Update the last-modified field with the time.
--
--   Some would say that this should be done by Postgres itself, with
--   triggers, and they'd be right.
--
updateLastModified :: PersistBackend m => UTCTime -> m ()
updateLastModified lastMod = do
  let new = MetaData { mdLastModified = lastMod }
  deleteAll (undefined :: MetaData)
  insert_ new
  
-- | When was the database last modified?
getLastModified :: PersistBackend m => m (Maybe UTCTime)
getLastModified = do
  lmods <- project MdLastModifiedField (CondEmpty
                                        `orderBy` [Desc MdLastModifiedField]
                                        `limitTo` 1)
  return (listToMaybe lmods)
  
-- | Hard-coded connection string for the database connection.
dbConnStr :: String
dbConnStr = "user=postgres password=postgres dbname=chandraobs host=127.0.0.1"

-- | Run an action against the database. This includes a call to
--   `handleMigration` before the action is run.
--
runDb :: DbPersist Postgresql (NoLoggingT IO) a -> IO a
runDb act =
  withPostgresqlConn dbConnStr (runDbConn (handleMigration >> act))
