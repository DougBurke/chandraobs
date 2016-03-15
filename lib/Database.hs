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
                , fetchSIMBADDescendentTypes
                , fetchObjectTypes
                , fetchConstellation
                , fetchConstellationTypes
                , fetchCategory
                , fetchCategoryTypes
                , fetchProposal
                , fetchInstrument
                , fetchGrating
                , fetchIG
                , fetchInstrumentTypes
                , fetchGratingTypes
                , fetchIGTypes

                , findNameMatch
                , findTarget
                  
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

                , putIO
                , runDb
                , dbConnStr
                , discarded
                , archived
                , notDiscarded
                , notArchived
                , isScheduled
                  
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

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (NoLoggingT)

import Data.Char (toUpper)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (group, groupBy, nub, sortBy)
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Ord (Down(..), comparing)
import Data.Time (UTCTime(..), Day(..), getCurrentTime, addDays)

import Database.Groundhog.Core (PersistEntity, EntityConstr,
                                DbDescriptor, RestrictionHolder)
import Database.Groundhog.Core (distinct)
import Database.Groundhog.Postgresql

import Types

type DbIO m = (MonadIO m, PersistBackend m)

archived :: String
archived = "archived"

discarded :: String
discarded = "discarded"

nsInObsCatName :: String
nsInObsCatName = "unknown"

notArchived ::
  DbDescriptor db
  => Cond db (RestrictionHolder ScienceObs ScienceObsConstructor)
notArchived = SoStatusField /=. archived

notDiscarded ::
  DbDescriptor db
  => Cond db (RestrictionHolder ScienceObs ScienceObsConstructor)
notDiscarded = SoStatusField /=. discarded

-- | Reject those observations which have no scheduled time.
isScheduled ::
  DbDescriptor db
  => Cond db (RestrictionHolder ScienceObs ScienceObsConstructor)
isScheduled = SoStartTimeField <. futureTime

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
--   that is, the ObsCat field does not have a start date field.
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
identifyLatestRecord [] [] = Nothing
identifyLatestRecord (x:_) [] = Just (Right x)
identifyLatestRecord [] (y:_) = Just (Left y)
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
identifyEarliestRecord [] [] = Nothing
identifyEarliestRecord (x:_) [] = Just (Right x)
identifyEarliestRecord [] (y:_) = Just (Left y)
identifyEarliestRecord xs ys =
  -- rely on Ord instance of Maybe for the comparison
  let sobs = listToMaybe xs
      nsobs = listToMaybe ys
      stime = soStartTime <$> sobs
      ntime = nsStartTime <$> nsobs
  in if stime < ntime then Right <$> sobs else Left <$> nsobs

-- | Return the last observation (science or non-science) to be
--   scheduled before the requested time. The observation can be
--   finished or running. It will not be a discarded observation
--   or those with no scheduled time.
--
findRecord :: (PersistBackend m) => UTCTime -> m (Maybe Record)
findRecord t = do
  let tval = ChandraTime t
  xs <- select (((SoStartTimeField <=. tval)
                 &&. notDiscarded
                 &&. isScheduled) -- at present the isScheduled does not add anything
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
      day = toModifiedJulianDay dayNow - fromIntegral ndays
      dayStart = ModifiedJulianDay day

      tStart = ChandraTime (UTCTime dayStart 0)
      tEnd   = ChandraTime (UTCTime dayEnd 0)

  -- Select the science and non-science observations separately
  -- and then merge them. As the science observations have no
  -- end-time field, post-process the database results here,
  -- rather than having the database do all the filtering.
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
      keep (Right ScienceObs{..}) = (soStatus /= discarded)
                                    -- && (soStartTime < futureTime)

      mobsid = recordObsId <$> mrec
      findNow r = if Just (recordObsId r) == mobsid then Right r else Left r
      (others, nows) = partitionEithers (map findNow cleanrs)

      cnow = ChandraTime now
      (done, todo) = span ((<= cnow) . recordStartTime) others

  simbad <- getSimbadList cleanrs
  return (Schedule now 0 done (listToMaybe nows) todo simbad)


getSimbadList ::
  PersistBackend m
  => [Record]  -- ^ records; assumed to be filtered
  -> m (M.Map String SimbadInfo)
getSimbadList rs = do
  let getName = either (const Nothing) (Just . soTarget)
      tnames = nub (mapMaybe getName rs)

  -- Is it best to index on target name or ObsId or ...?
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
  => String   -- ^ target name (not the actual SIMBAD search term)
  -> m (Maybe SimbadInfo)
getSimbadInfo target = do
  keys <- project SmmInfoField ((SmmTargetField ==. target) `limitTo` 1)
  case keys of
    [key] -> get key
    _ -> return Nothing

-- | Return all observations of the given SIMBAD type (excluding
--   discarded). This restricts to the type only (i.e. no descendents).
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
  keys <- project (AutoKeyField) constraint
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
fetchConstellationTypes :: PersistBackend m => m [(ConShort, Int)]
fetchConstellationTypes = do
  res <- project SoConstellationField
         (notDiscarded -- &&. isScheduled
          `orderBy` [Asc SoConstellationField])
  return (countUp res)
    
-- | Return observations which match this category,
--   excluding discarded observations.
--
fetchCategory ::
  PersistBackend m
  => String
  -> m (SortedList StartTimeOrder ScienceObs)
fetchCategory cat = do
  propNums <- project PropNumField (PropCategoryField ==. cat)
  sos <- forM propNums $ \pn ->
    select (SoProposalField ==. pn &&. notDiscarded) --  &&. isScheduled)
  let xs = concat sos
  return (toSL soStartTime xs)

-- | Return information on the category types.
--
fetchCategoryTypes ::
  PersistBackend m
  => m [(String, Int)]
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
findObsStatusTypes :: PersistBackend m => m [(String, Int)]
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
  -> m ([String], [String])
  -- ^ object names - first the target names, then the "also known as" from SIMBAD
  --   - names in the database.
findNameMatch instr = do
  let matchStr = '%' : (map toUpper instr) ++ "%"
  targets <- project SoTargetField (distinct ((upper SoTargetField) `like` matchStr))
  -- do not use SmmTargetField
  simbads <- project SmiNameField (distinct ((upper SmiNameField) `like` matchStr))
  return (targets, simbads)


-- | Find observations of the given target. The input name is searched - using a
--   case-insensitive match - against both the observation target name and
--   Simbad matches.
--
--   It is related to `findNameMatch`.
--
--   TODO:
--     add in SIMBAD search
--     improve space matching
--     filter out % and _ ?
--
findTarget ::
  (PersistBackend m, SqlDb (PhantomDb m))
  => String
  -> m (SortedList StartTimeOrder ScienceObs)
findTarget target = do
  -- need to think about how the SIMBAD search is going to work
  -- simbads <- select ((upper SmiNameField) =. (upper target))
  let searchTerm = map toUpper target
  sobs <- select ((upper SoTargetField ==. searchTerm)
                  `orderBy` [Asc SoStartTimeField])
  return (unsafeToSL sobs)
  
  
  
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
  putIO ""

  -- unscheduled observations
  nuns <- count (SoStartTimeField ==. futureTime)
  putIO ("  un-scheduled science obs      = " ++ show nuns)
  
  -- non-science breakdown
  ns1 <- count notFromObsCat
  ns2 <- count (Not notNsDiscarded)
  putIO ("  non-science (not from obscat) = " ++ show ns1)
  putIO ("  non-science discarded         = " ++ show ns2)
  
  let ntot = sum [n1, n2, n3, n4, n5, n6, n7, n8]
  putIO ""
  putIO ("Number of rows              : " ++ show ntot)
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

-- | Hard-coded connection string for the database connection.
dbConnStr :: String
dbConnStr = "user=postgres password=postgres dbname=chandraobs host=127.0.0.1"

-- | Run an action against the database. This includes a call to
--   `handleMigration` before the action is run.
--
runDb :: DbPersist Postgresql (NoLoggingT IO) a -> IO a
runDb act =
  withPostgresqlConn dbConnStr (runDbConn (handleMigration >> act))
