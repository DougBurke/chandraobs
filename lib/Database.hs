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
                , getProposalObs
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

                , insertOrReplace
                ) where

import Control.Applicative ((<$>))
import Control.Monad (forM, liftM, when)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (group, groupBy, sortBy)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ord (comparing)
import Data.Time (UTCTime(..), Day(..), getCurrentTime, addDays)

import Database.Groundhog.Core (PersistEntity, EntityConstr, RestrictionHolder)
import Database.Groundhog.Postgresql

import Types

-- | Return the last ScheduleItem to be defined before the given time.
findSI :: (PersistBackend m) => UTCTime -> m (Maybe ScheduleItem)
findSI t = do
  res <- select $ (SiStartField <=. ChandraTime t)
                  `orderBy` [Desc SiStartField]
                  `limitTo` 1
  return $ listToMaybe res

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
        (so:_) -> return $ Just (Right so, False)
        _ -> return Nothing

findNonScience :: PersistBackend m => ObsIdVal -> m (Maybe NonScienceObs)
findNonScience oi = do
  ans <- select $ (NsObsIdField ==. oi) `limitTo` 1
  return $ listToMaybe ans

findScience :: PersistBackend m => ObsIdVal -> m (Maybe ScienceObs)
findScience oi = do
  ans <- select $ (SoObsIdField ==. oi) `limitTo` 1
  return $ listToMaybe ans

-- | Given a list of schedule items, return the record of the
--   first element, or @Nothing@ if the list is empty.
--
extractRecord:: PersistBackend m => [ScheduleItem] -> m (Maybe Record)
extractRecord [] = return Nothing
extractRecord (si:_) = findItem si

-- | Return the current observation
getCurrentObs :: (MonadIO m, PersistBackend m) => m (Maybe Record)
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
getNextObs :: (MonadIO m, PersistBackend m) => ObsIdVal -> ChandraTime -> m (Maybe Record)
getNextObs oid t = do
  res <- select $ ((SiStartField >. t) &&. (SiObsIdField /=. oid)) 
                  `orderBy` [Asc SiStartField]
                  `limitTo` 1
  extractRecord res

-- | Return the last observation to have started before the given time.
--
--   Perhaps this should check on @SiEndTime@.
--
--   See the discussion for `getNextObs`.
--
getPrevObs :: (MonadIO m, PersistBackend m) => ObsIdVal -> ChandraTime -> m (Maybe Record)
getPrevObs oid t = do
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
getObsInfo :: (MonadIO m, PersistBackend m) => m (Maybe ObsInfo)
getObsInfo = do
  mobs <- getCurrentObs
  case mobs of
    Just obs -> extractObsInfo obs
    Nothing -> return Nothing

-- | Given an observation, extract the previous and next observations.
extractObsInfo :: (MonadIO m, PersistBackend m) => Record -> m (Maybe ObsInfo)
extractObsInfo obs = do
  mprev <- getPrevObs (recordObsId obs) (recordStartTime obs)
  mnext <- getNextObs (recordObsId obs) (recordStartTime obs)
  return $ Just $ ObsInfo obs mprev mnext

findObsInfo :: (MonadIO m, PersistBackend m) => ObsIdVal -> m (Maybe ObsInfo)
findObsInfo oi = do
  res <- findObsId oi
  case res of
    Just (obs,f) | f         -> extractObsInfo obs
                 | otherwise -> return $ Just $ ObsInfo obs Nothing Nothing 
    _ -> return Nothing
  
-- | Return the requested "science" observation.
getObsId :: (MonadIO m, PersistBackend m) => ObsIdVal -> m (Maybe ObsInfo)
getObsId = findObsInfo 

-- | Return the record of the given observation, if it
--   exists.
getRecord :: PersistBackend m => ObsIdVal -> m (Maybe Record)
getRecord oid = (liftM . fmap) fst $ findObsId oid

-- | TODO: handle the case when the current observation, which has
--   just started, has an exposure time > ndays.
--
getSchedule ::
  (MonadIO m, PersistBackend m)
  => Int    -- ^ Number of days to go back/forward
  -> m Schedule
getSchedule ndays = do
  now <- liftIO getCurrentTime

  -- return all observations which have any part of their
  -- observation within the time span tStart to tEnd.
  --
  let dayNow = utctDay now
      dayEnd = addDays (fromIntegral ndays + 1) dayNow
      dayStart = ModifiedJulianDay $ toModifiedJulianDay dayNow - fromIntegral ndays

      tStart = ChandraTime $ UTCTime dayStart 0
      tEnd   = ChandraTime $ UTCTime dayEnd 0

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

  return $ Schedule now ndays done mdoing todo

-- | Creates a schedule structure of the list of observations.
--
--   The number-of-days field in the structure is set to 0; this
--   is not ideal!
--
makeSchedule :: (MonadIO m, PersistBackend m) => 
  [Record]      -- it is assumed that this list contains no duplicates
  -> m Schedule
makeSchedule rs = do
  now <- liftIO getCurrentTime
  msi <- findSI now
  
  -- Unlike getSchedule, the records are not guaranteed to
  -- be in time order, and we are working with records, not
  -- the schedule. However, given that we end up sorting
  -- the records, we should be able to have a similar scheme.
  -- However, getSchedule assumes that we have a set of
  -- records that cover the given time period, so we can
  -- identify the current observation as the last observation
  -- with a start time <= now, but here we can not.
  --
  -- I am hoping that the compiler can fuse these multiple
  -- traversals together.
  --
  let mobsid = siObsId <$> msi
      findNow r = if Just (recordObsId r) == mobsid then Right r else Left r
      (others, nows) = partitionEithers $ map findNow rs

      sorted = sortBy (comparing recordStartTime) others
      cnow = ChandraTime now
      (done, todo) = span ((<= cnow) . recordStartTime) sorted      

  return $ Schedule now 0 done (listToMaybe nows) todo

-- | Do we have any SIMBAD information about the target?
--
getSimbadInfo :: 
  (MonadIO m, PersistBackend m) 
  => String   -- ^ target name (not the actual SIMBAD search term)
  -> m (Maybe SimbadInfo)
getSimbadInfo target = do
  keys <- project SmmInfoField $ (SmmTargetField ==. target) `limitTo` 1
  case keys of
    [key] -> get key
    _ -> return Nothing
{-
  case listToMaybe keys of
    Just key -> get key
    _ -> return Nothing
-}

-- | Return all observations of the given SIMBAD type.
--
fetchSIMBADType :: 
  (MonadIO m, PersistBackend m) 
  => SimbadType 
  -> m (Maybe (SimbadTypeInfo, [ScienceObs]))
fetchSIMBADType stype = do
  -- TODO: would be nice to be able to use the database to do this,
  --       or perhaps switch to a graph databse.
  -- 
  -- splitting into two for now
  mtype <- project SmiTypeField $ (SmiType3Field ==. stype) `limitTo` 1
  case mtype of
    [ltype] -> do
               keys <- project AutoKeyField (SmiType3Field ==. stype)
               sos <- forM keys $ \key -> do
                          targets <- project SmmTargetField (SmmInfoField ==. key)
                          obs <- forM targets $ \t -> select (SoTargetField ==. t)
                          return $ concat obs
               return $ Just ((stype, ltype), concat sos)

    _ -> return Nothing

-- | Return information on the object types we have stored.
--
--   The return value includes the number of objects that 
--   have the given type.
--
fetchObjectTypes :: 
  (MonadIO m, PersistBackend m) 
  => m [(SimbadTypeInfo, Int)]
fetchObjectTypes = do
  res <- select $ CondEmpty `orderBy` [Asc SmiType3Field]
  let srt = groupBy ((==) `on` smiType3) res
      t [] = error "impossible fetchObjectTypes condition occurred"
      t xs@(x:_) = ((smiType3 x, smiType x), length xs)
  return $ map t srt 

-- | Return observations which match this constellation, in time order.
fetchConstellation :: (MonadIO m, PersistBackend m) => ConShort -> m [ScienceObs]
fetchConstellation con = 
  select $ (SoConstellationField ==. con) `orderBy` [Asc SoStartTimeField]

-- | Return count of the constellations.
fetchConstellationTypes :: (MonadIO m, PersistBackend m) => m [(ConShort, Int)]
fetchConstellationTypes = do
  res <- project SoConstellationField $ CondEmpty `orderBy` [Asc SoConstellationField]
  let srt = group res
      t [] = error "impossible fetchConstellationTypes condition occurred"
      t xs@(x:_) = (x, length xs)
  return $ map t srt 
    
-- | Return observations which match this category, in time order.
fetchCategory :: (MonadIO m, PersistBackend m) => String -> m [ScienceObs]
fetchCategory cat = do
  propNums <- project PropNumField (PropCategoryField ==. cat)
  sos <- forM propNums $ \pn -> select $ (SoProposalField ==. pn) `limitTo` 1
  return $ concat sos

-- | Return information on the category types.
fetchCategoryTypes :: (MonadIO m, PersistBackend m) => m [(String, Int)]
fetchCategoryTypes = do
  res <- project PropCategoryField $ CondEmpty `orderBy` [Asc PropCategoryField]
  let srt = group res
      t [] = error "impossible fetchCategoryTypes condition occurred"
      t xs@(x:_) = (x, length xs)
  return $ map t srt 

-- | Return all the observations which match this proposal, in time order.
--
--   See also `getProposal` and `getProposalObs`
fetchProposal :: (MonadIO m, PersistBackend m) => PropNum -> m (Maybe Proposal, [ScienceObs])
fetchProposal pn = do
  mprop <- select $ (PropNumField ==. pn) `limitTo` 1
  ms <- select $ (SoProposalField ==. pn) `orderBy` [Asc SoStartTimeField]
  return (listToMaybe mprop, ms)

-- | Return all the observations which match this instrument, in time order.
--
fetchInstrument :: (MonadIO m, PersistBackend m) => Instrument -> m [ScienceObs]
fetchInstrument inst = 
  select $ (SoInstrumentField ==. inst) `orderBy` [Asc SoStartTimeField]

-- | This counts up the individual observations; should it try and group by
--   "proposal", or at least "object per proposal"?
fetchInstrumentTypes :: (MonadIO m, PersistBackend m) => m [(Instrument, Int)]
fetchInstrumentTypes = do
  res <- select $ CondEmpty `orderBy` [Asc SoInstrumentField]
  let srt = groupBy ((==) `on` soInstrument) res
      t [] = error "impossible fetchInstrumentTypes condition occurred"
      t xs@(x:_) = (soInstrument x, length xs)
  return $ map t srt 

-- | Return the proposal information for the observation if:
--   a) it's a science observation, and b) we have it.
--
getProposal :: (MonadIO m, PersistBackend m) => ScienceObs -> m (Maybe Proposal)
getProposal ScienceObs{..} = do
  ans <- select $ (PropNumField ==. soProposal) `limitTo` 1
  return $ listToMaybe ans

-- | Find all the other observations in the proposal.
--
getProposalObs :: (MonadIO m, PersistBackend m) => ScienceObs -> m [ScienceObs]
getProposalObs ScienceObs{..} = 
  -- time sorting probably not needed here
  select $ ((SoProposalField ==. soProposal) &&. (SoObsIdField /=. soObsId))
           `orderBy` [Asc SoStartTimeField]

-- | A combination of `getProposal` and `getProposalObs`.
--
getProposalInfo :: (MonadIO m, PersistBackend m) => Record -> m (Maybe Proposal, [ScienceObs])
getProposalInfo (Left _) = return (Nothing, [])
getProposalInfo (Right so) = do
  mproposal <- getProposal so
  matches <- getProposalObs so
  return (mproposal, matches)

-- | Quick on-screen summary of the database size.
reportSize :: (MonadIO m, PersistBackend m) => m ()
reportSize = do
  nall <- countAll (undefined :: ScheduleItem)
  ns <- countAll (undefined :: ScienceObs)
  nn <- countAll (undefined :: NonScienceObs)
  np <- countAll (undefined :: Proposal)
  liftIO $ putStrLn $ "Number of scheduled items   : " ++ show nall
  liftIO $ putStrLn $ "Number of science obs       : " ++ show ns
  liftIO $ putStrLn $ "Number of non-science obs   : " ++ show nn
  liftIO $ putStrLn $ "Number of proposals         : " ++ show np

-- Need to make sure the following match the constraints on the tables found
-- in Types.hs

-- | Checks that the Science observation is not known about before inserting it.
--
--   If it already exists in the database the new value is ignored; there is no check to
--   make sure that the details match.
insertScienceObs :: (MonadIO m, PersistBackend m) => ScienceObs -> m ()
insertScienceObs s = do
  n <- count (SoObsIdField ==. soObsId s)
  when (n == 0) $ insert_ s

-- | Replaces the science observation with the new values, if it is different.
--
--   Acts as `insertScienceObs` if the observation is not known about.
replaceScienceObs :: (MonadIO m, PersistBackend m) => ScienceObs -> m ()
replaceScienceObs s = insertOrReplace (SoObsIdField ==. soObsId s) s

-- | Checks that the proposal is not known about before inserting it.
--
--   If it already exists in the database the new value is ignored; there is no check to
--   make sure that the details match.
insertProposal :: (MonadIO m, PersistBackend m) => Proposal -> m ()
insertProposal p = do
  n <- count (PropNumField ==. propNum p)
  when (n == 0) $ insert_ p

-- | Checks that the data is not known about before inserting it.
--
--   Returns the key for the item and a flag indicating whether
--   the key already exists (so previous SimbadNoMatch may need
--   to be deleted).
--
insertSimbadInfo :: (MonadIO m, PersistBackend m) => SimbadInfo -> m (AutoKey SimbadInfo, Bool)
insertSimbadInfo sm = do
  ems <- insertByAll sm
  case ems of
    Right newkey -> return (newkey, False)
    Left oldkey -> do
             Just oldsm <- get oldkey
             when (oldsm /= sm) $ error "!!! SimbadInfo does not match !!!" -- TODO: what now?
             return (oldkey, True)

insertSimbadMatch :: (MonadIO m, PersistBackend m) => SimbadMatch -> m ()
insertSimbadMatch sm = do
  n <- count (SmmTargetField ==. smmTarget sm)
  when (n == 0) $ insert_ sm

insertSimbadNoMatch :: (MonadIO m, PersistBackend m) => SimbadNoMatch -> m ()
insertSimbadNoMatch sm = do
  n <- count (SmnTargetField ==. smnTarget sm)
  when (n == 0) $ insert_ sm

-- | If the record is not known - as defined by the condition
--   then add it, otherwise check the stored value and, if
--   different, replace it.
--
--   Note that this does not take advantage of keys for
--   identification or deletion, rather it uses the
--   supplied constraint.
--
insertOrReplace ::
    (MonadIO m, PersistBackend m, PersistEntity v, Eq v,
     EntityConstr v c)
    => Cond (PhantomDb m) (RestrictionHolder v c)
    -> v
    -> m ()
insertOrReplace cond newVal = do
  ans <- select $ cond `limitTo` 1
  case ans of
    (oldVal:_) -> when (oldVal /= newVal) $ do
                                delete cond
                                insert_ newVal
    _ -> insert_ newVal

