{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

--
-- Initialize the database using the manually-curated information
-- stored in HackData. At present this is to the local Postgres
-- instance.
--
-- Moving to a system where it will add to, or replace,
-- the existing database, to allow new data to be added. Note that
-- it's not a foolproof system, since if there was
--    obs1, obs2, obs3
-- and then obs2 gets removed and obs1/3 are changed, the checks
-- will only notice the obs1/3 changes, not the deletion of obs2,
-- so the old value will be left in the data base.
--

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)

import Database.Groundhog.Postgresql

import Database (insertOrReplace, reportSize, putIO, runDb)
import HackData
import Types

addSI ::
    (MonadIO m, PersistBackend m)
    => ScheduleItem
    -> m ()
addSI si = insertOrReplace (SiObsIdField ==. siObsId si) si

addNS ::
    (MonadIO m, PersistBackend m)
    => NonScienceObs
    -> m ()
addNS ns = insertOrReplace (NsObsIdField ==. nsObsId ns) ns

main :: IO ()
main =
  runDb $ do
    _ <- reportSize

    o1 <- countAll (undefined :: ScheduleItem)
    o2 <- countAll (undefined :: ScienceObs)
    o3 <- countAll (undefined :: NonScienceObs)
    o4 <- countAll (undefined :: Proposal)

    putIO "Inserting schedule"
    mapM_ addSI testSchedule

    -- putIO "Inserting science obs"
    -- mapM_ insert testScience

    putIO "Inserting non-science obs"
    mapM_ addNS testNonScience

    _ <- reportSize

    n1 <- countAll (undefined :: ScheduleItem)
    n2 <- countAll (undefined :: ScienceObs)
    n3 <- countAll (undefined :: NonScienceObs)
    n4 <- countAll (undefined :: Proposal)

    when (o1 /= n1) $ putIO ("# Number of scheduled items increased by " ++ show (n1-o1))
    when (o2 /= n2) $ putIO ("# Number of science obs     increased by " ++ show (n2-o2))
    when (o3 /= n3) $ putIO ("# Number of non-science obs increased by " ++ show (n3-o3))
    when (o4 /= n4) $ putIO ("# Number of proposals       increased by " ++ show (n4-o4))
