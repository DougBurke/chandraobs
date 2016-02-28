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

                , cleanupDiscarded
                , insertOrReplace

                , putIO
                , runDb
                , dbConnStr
                ) where

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Control.Applicative ((<$>))
#endif

import Control.Monad (forM, forM_, liftM, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (NoLoggingT)

import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (group, groupBy, sortBy)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ord (Down(..), comparing)
import Data.Time (UTCTime(..), Day(..), getCurrentTime, addDays)

import Database.Groundhog.Core (PersistEntity, EntityConstr,
                                DbDescriptor, RestrictionHolder)
import Database.Groundhog.Postgresql

import Types

type DbIO m = (MonadIO m, PersistBackend m)

{-

# How best to remove discarded observations?

cshould just remove them from ScheduleItem, so that the schedule stuff
doesn't find them, and then deal with them directly - as now done -
for the ScienceObs table.

So, when code detects that a status has changed to discarded, it
should remove it from the ScheduleItem table. Will also need a
pass to remove already-set items.

-}

discarded :: String
discarded = "discarded"

noDiscarded ::
  DbDescriptor db
  => Cond db (RestrictionHolder ScienceObs ScienceObsConstructor)
noDiscarded = SoStatusField /=. discarded

-- | Return the last ScheduleItem to be defined before the given time.
--
findSI :: (PersistBackend m) => UTCTime -> m (Maybe ScheduleItem)
findSI t = do
  res <- select $ (SiStartField <=. ChandraTime t)
                  `orderBy` [Desc SiStartField]
                  `limitTo` 1
  return (listToMaybe res)

-- | Given a ScheduleItem, return infomation on the observation it
--   refers to.
findItem :: PersistBackend m => ScheduleItem -> m (Maybe Record)
findItem si = 
  let obsid = siObsId si
  in if siScienceObs si
     then (Right `liftM`) `liftM` findScience obsid
     else (Left `liftM`) `liftM` findNonScience obsid

-- | Return information on this obsid, if known about. The
--   boolean flag is True if this is a "scheduled" observation;
--   i.e. was found in the ScheduledItem list. If not, it is
--   likely an "overlap observation".
--
--   TODO: should this also flag if the observation is discarded
--         (i.e. move from Bool to an enumeration)?
--
findObsId :: PersistBackend m => ObsIdVal -> m (Maybe (Record, Bool))
findObsId oi = do
  ans <- select $ (SiObsIdField ==. oi) `limitTo` 1
  case ans of
    (si:_) -> do
      rval <- findItem si
      return $ (,True) `fmap` rval
    _ -> do
      ans2 <- select $ (SoObsIdField ==. oi) `limitTo` 1
      case ans2 of
        (so:_) -> return (Just (Right so, False))
        _ -> return Nothing

findNonScience :: PersistBackend m => ObsIdVal -> m (Maybe NonScienceObs)
findNonScience oi = do
  ans <- select ((NsObsIdField ==. oi) `limitTo` 1)
  return (listToMaybe ans)

-- | This will return a discarded observation.
findScience :: PersistBackend m => ObsIdVal -> m (Maybe ScienceObs)
findScience oi = do
  ans <- select ((SoObsIdField ==. oi) `limitTo` 1)
  return (listToMaybe ans)

-- | Given a list of schedule items, return the record of the
--   first element, or @Nothing@ if the list is empty.
--
--   TODO: how should this handle discarded observations?
extractRecord:: PersistBackend m => [ScheduleItem] -> m (Maybe Record)
extractRecord [] = return Nothing
extractRecord (si:_) = findItem si

-- | Return the current observation. The assumption here is that it
--   is not a discarded observation (the only way we can find this out
--   is to query the OCAT and this is not going to be updated in
--   near-real time).
--
getCurrentObs :: DbIO m => m (Maybe Record)
getCurrentObs = do
  now <- liftIO getCurrentTime
  msi <- findSI now
  case msi of
    Just si -> findItem si
    _ -> return Nothing

-- | Return the first observation to start after the given time.
--
--   Hmm, this is tricky now that we have science times that do not
--   necessarily agree with the schedule times, which is why the
--   obsid is given too
--
getNextObs ::
  PersistBackend m
  => ObsIdVal
  -> ChandraTime
  -> m (Maybe Record)
getNextObs oid t = do
  res <- select $ ((SiStartField >. t) &&. (SiObsIdField /=. oid)) 
                  `orderBy` [Asc SiStartField]
                  `limitTo` 1
  extractRecord res

-- | Return the last observation to have started before the given time that is not
--   discarded. If the input observation is discarded then nothing is returned.
--
--   Perhaps this should check on @SiEndTime@.
--
--   See the discussion for `getNextObs`.
--
--   TODO: this does not actually handle the discard check as described above!
--
getPrevObs ::
  PersistBackend m
  => ObsIdVal
  -> ChandraTime
  -> m (Maybe Record)
getPrevObs oid t = do
  {-
  ostatus <- project SoStatusField (SiObsIdField ==. oid)
  case ostatus of
    [status] | status /= discarded -> return (Just ())
    _ -> return Nothing
  -}

  res <- select $ ((SiStartField <. t) &&. (SiObsIdField /=. oid))
                  `orderBy` [Desc SiStartField]
                  `limitTo` 1
  extractRecord res

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
getObsInfo :: DbIO m => m (Maybe ObsInfo)
getObsInfo = do
  mobs <- getCurrentObs
  case mobs of
    Just obs -> extractObsInfo obs
    Nothing -> return Nothing

-- | Given an observation, extract the previous and next observations.
--
--   TODO: it should not do anything for discarded observations,
--         and should not have prev/next ones that are discarded.
--
extractObsInfo :: PersistBackend m => Record -> m (Maybe ObsInfo)
extractObsInfo obs = do
  mprev <- getPrevObs (recordObsId obs) (recordStartTime obs)
  mnext <- getNextObs (recordObsId obs) (recordStartTime obs)
  return (Just (ObsInfo obs mprev mnext))

findObsInfo :: PersistBackend m => ObsIdVal -> m (Maybe ObsInfo)
findObsInfo oi = do
  res <- findObsId oi
  case res of
    Just (obs,f) | f         -> extractObsInfo obs
                 | otherwise -> return (Just (ObsInfo obs Nothing Nothing))
    _ -> return Nothing
  
-- | Return the requested "science" observation, including discarded observations.
getObsId :: PersistBackend m => ObsIdVal -> m (Maybe ObsInfo)
getObsId = findObsInfo 

-- | Return the record of the given observation, if it
--   exists. This includes discarded obsrevations.
getRecord :: PersistBackend m => ObsIdVal -> m (Maybe Record)
getRecord oid = (liftM . fmap) fst (findObsId oid)

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

  -- This works on the schedule item list, so can not use the
  -- noDiscarded constraint. The idea is that all the discarded items
  -- will have been removed from this particular table.
  res <- select $ ((SiStartField <. tEnd) &&. (SiEndField >=. tStart))
                  `orderBy` [Asc SiStartField]

  -- I used siStart r <= cnow, so keep with that logic for now
  -- (could use siEnd r < cnow)
  let cnow = ChandraTime now
      (aprevs, nexts) = span ((<= cnow) . siStart) res
      (mobs, prevs) = case reverse aprevs of
        [] -> (Nothing, [])
        (current:cs) -> (Just current, reverse cs)

  done <- catMaybes `liftM` mapM findItem prevs
  todo <- catMaybes `liftM` mapM findItem nexts
  mdoing <- case mobs of
              Just obs -> findItem obs
              _ -> return Nothing

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
  msi <- findSI now
  
  let cleanrs = filter removeDiscarded (fromSL rs)
      removeDiscarded (Left _) = True
      removeDiscarded (Right ScienceObs{..}) = soStatus /= discarded

      mobsid = siObsId <$> msi
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
                                                noDiscarded)
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
  ans <- select ((SoConstellationField ==. con &&. noDiscarded)
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
         (noDiscarded `orderBy` [Asc SoConstellationField])
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
    select ((SoProposalField ==. pn &&. noDiscarded) `limitTo` 1)
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
  ms <- select ((SoProposalField ==. pn &&. noDiscarded)
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
  ans <- select $ (SoInstrumentField ==. inst &&. noDiscarded)
         `orderBy` [Asc SoStartTimeField]
  return (unsafeToSL ans)

-- | This counts up the individual observations; should it try and group by
--   "proposal", or at least "object per proposal"?
--
--   Discarded observations are excluded.
--
fetchInstrumentTypes :: PersistBackend m => m [(Instrument, Int)]
fetchInstrumentTypes = do
  insts <- project SoInstrumentField (noDiscarded `orderBy`
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
                  (SoObsIdField /=. soObsId) &&. noDiscarded)
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
                  (SoObsIdField /=. obsId) &&. noDiscarded)
                 `orderBy` [Asc SoStartTimeField])
  return (unsafeToSL ans)

-- | Return the observations we know about for the given proposal
--   (that are not discarded).
getObsFromProposal ::
  PersistBackend m
  => PropNum
  -> m (SortedList StartTimeOrder ScienceObs)
getObsFromProposal propNum = do
  ans <- select ((SoProposalField ==. propNum &&. noDiscarded)
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

-- | Checks that the Science observation is not known about before inserting it.
--
--   If it already exists in the database the new value is ignored;
--   there is no check to make sure that the details match.
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
replaceScienceObs ::
  PersistBackend m
  => ScienceObs
  -> m ()
replaceScienceObs s = insertOrReplace (SoObsIdField ==. soObsId s) s

-- | Ensure that any discarded science observations are removed from the
--   ScheduleItem table. It is just easier for me to do this in one place
--   rather than add a check in `insertScienceObs` and `replaceScienceObs`.
--
cleanupDiscarded :: PersistBackend m => m ()
cleanupDiscarded = do
  obsids <- project SoObsIdField (SoStatusField ==. discarded)
  forM_ obsids (\obsid -> delete (SiObsIdField ==. obsid))

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

insertSimbadMatch :: PersistBackend m => SimbadMatch -> m ()
insertSimbadMatch sm = do
  n <- count (SmmTargetField ==. smmTarget sm)
  when (n == 0) (insert_ sm)

insertSimbadNoMatch :: PersistBackend m => SimbadNoMatch -> m ()
insertSimbadNoMatch sm = do
  n <- count (SmnTargetField ==. smnTarget sm)
  when (n == 0) (insert_ sm)

-- | If the record is not known - as defined by the condition
--   then add it, otherwise check the stored value and, if
--   different, replace it.
--
--   Note that this does not take advantage of keys for
--   identification or deletion, rather it uses the
--   supplied constraint.
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

-- | Hard-coded connection string for the database connection.
dbConnStr :: String
dbConnStr = "user=postgres password=postgres dbname=chandraobs host=127.0.0.1"

-- | Run an action against the database. This includes a call to
--   `handleMigration` before the action is run.
--
runDb :: DbPersist Postgresql (NoLoggingT IO) a -> IO a
runDb act =
  withPostgresqlConn dbConnStr (runDbConn (handleMigration >> act))
