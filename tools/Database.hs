{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
                , fetchConstellation
                , fetchCategory
                , fetchProposal
                , fetchInstrument
                ) where

import Control.Applicative ((<$>))
import Control.Monad (forM, liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Either (partitionEithers)
import Data.List (sortBy)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ord (comparing)
import Data.Time (UTCTime(..), Day(..), getCurrentTime, addDays)

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

findObsId :: PersistBackend m => ObsIdVal -> m (Maybe Record)
findObsId oi = do
  ans <- select $ (SiObsIdField ==. oi) `limitTo` 1
  case listToMaybe ans of
    Just si -> findItem si
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
--   Allow for the possibility of there being no observation; e.g.
--   because the data base hasn't been updated.
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
  res <- select $ (SiObsIdField ==. oi) `limitTo` 1
  mobs <- extractRecord res
  case mobs of
    Just obs -> extractObsInfo obs
    _ -> return Nothing
  
-- | Return the requested "science" observation.
getObsId :: (MonadIO m, PersistBackend m) => ObsIdVal -> m (Maybe ObsInfo)
getObsId = findObsInfo 

-- | Return the record of the given observation, if it
--   exists.
getRecord :: PersistBackend m => ObsIdVal -> m (Maybe Record)
getRecord = findObsId

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
getSimbadInfo :: (MonadIO m, PersistBackend m) => String -> m (Maybe SimbadInfo)
getSimbadInfo tgt = do
  ans <- select $ (SiTargetField ==. tgt)
  return $ listToMaybe ans

-- | Return all observations of the given SIMBAD type.
fetchSIMBADType :: (MonadIO m, PersistBackend m) => SimbadType -> m (SimbadTypeInfo, [ScienceObs])
fetchSIMBADType stype = do
  -- TODO: use a join, or at least have a relationship between the
  --       two tables to make use of the database, since the following
  --       is not nice!
  -- 
  -- Why can I not just use project here?
  -- lans <- project SiTypeField $ (SiType3Field ==. Just stype) `limitTo` 1

  lans <- select $ (SiType3Field ==. Just stype) `limitTo` 1
  -- the ugliness here is down to my poor data modelling
  let sinfo = case lans of
                [x] -> case siType x of
                        Just y -> (stype, y)
                        _ -> (stype, error "*db error - Simbad Type has short form but not long*")
                _ -> (stype, error "*internal error - matchSIMBADType*")

  names <- project SiTargetField $ (SiType3Field ==. Just stype)
  ans <- forM names $ \n -> select $ (SoTargetField ==. n)
  return $ (sinfo, concat ans)

-- | Return observations which match this constellation, in time order.
fetchConstellation :: (MonadIO m, PersistBackend m) => ConShort -> m [ScienceObs]
fetchConstellation con = 
  select $ (SoConstellationField ==. con) `orderBy` [Asc SoStartTimeField]
    
-- | Return observations which match this category, in time order.
fetchCategory :: (MonadIO m, PersistBackend m) => String -> m [ScienceObs]
fetchCategory cat = do
  propNums <- project PropNumField $ (PropCategoryField ==. cat)
  sos <- forM propNums $ \pn -> select $ (SoProposalField ==. pn) `limitTo` 1
  return $ concat sos

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
