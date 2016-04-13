{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

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
-- so the old value will be left in the database.
--
-- With the move to using the ScheduleItem table as a source
-- of the actual information (i.e. the data needs to be processed
-- by ObsCat to access the useful information and insert it into
-- the database), the above concern is reduced. There are also
-- now explicit checks for discarded observations, which
-- is not ideal, but it's useful to keep discarded obs in the
-- database to avoid broken links. This is only for science
-- observations; discarded non-science observations are not
-- added to the database, which means that they will be
-- re-added to the scheduleitem table by the next call to InitDB,
-- and then removed by ObsCat. How many such items are there?
-- If a status field were added to NonScience this could be
-- avoided.
--
-- ARGH: obscat doesn't have any info for non-science obs until after
-- they have been observed. Which means need to add items in from the
-- ScheduleItem table, but then need to identiy these so that they
-- can be searched/over-written by ObsCat. Probably use the nsName
-- field - if not nsInObsCatName then it's a scheduleitem data set...
--

import qualified Data.Text as T

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Functor (void)
import Database.Groundhog.Postgresql
import Data.Monoid ((<>))

import Database (addScheduleItem
                , cleanDataBase
                , reportSize, putIO, runDb)
import HackData (STS, stsList)
import Types

-- | Given a schedule item, add it to the database if there is
--   no matching observation already in the database.
--
addSTS ::
    (Functor m, MonadIO m, PersistBackend m) -- ghc 7.8 needs Functor
    => STS
    -> m ()
addSTS sts = do
  flag <- addScheduleItem sts
  let obsid = fromObsId (siObsId (fst sts))
  when flag (liftIO (putStrLn (" - inserted si: " ++ show obsid)))

main :: IO ()
main = 
  runDb $ do
    void reportSize
    putIO ""
    putIO "-- starting"
    putIO ""

    o1 <- countAll (undefined :: ScheduleItem)
    o2 <- countAll (undefined :: ScienceObs)
    o3 <- countAll (undefined :: NonScienceObs)
    o4 <- countAll (undefined :: Proposal)

    putIO "Inserting schedule"
    mapM_ addSTS stsList
    putIO ""

    cleanDataBase

    void reportSize
    putIO ""
    
    n1 <- countAll (undefined :: ScheduleItem)
    n2 <- countAll (undefined :: ScienceObs)
    n3 <- countAll (undefined :: NonScienceObs)
    n4 <- countAll (undefined :: Proposal)

    let report lbl na nb =
          let ndiff = T.pack (show (nb - na))
          in when (na /= nb)
             (putIO ("# Number of " <> lbl <> " increased by " <> ndiff))

    report "scheduled items" o1 n1
    report "science obs    " o2 n2
    report "non-science obs" o3 n3
    report "proposals"       o4 n4
