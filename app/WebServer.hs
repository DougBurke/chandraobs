{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- TODO:
--   provide cache information for JSON responses; could have a last-updated
--   field in the database which is used to seed the last-Modified header,
--   as a simple case
--
--   Actually, need to decide what data:
--     - can be cached until the last-update has changed
--     - until some "indeterminate time" (e.g. next observation)
--     - those settings that could be cached but need a time value
--       to be applied to the cache
--

-- | A test webserver.
-- 
module Main (main) where

import qualified Data.Map.Strict as M

import qualified Data.Set as S

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import qualified Text.Blaze.Html5 as H5
import qualified Text.Blaze.Html5.Attributes as A5

import qualified About
import qualified Views.Index as Index
import qualified Views.NotFound as NotFound
import qualified Views.Proposal as Proposal
import qualified Views.Record as Record
import qualified Views.Search.Calendar as Calendar
import qualified Views.Search.Category as Category
import qualified Views.Search.Constellation as Constellation
import qualified Views.Search.Cycle as Cycle
import qualified Views.Search.Constraint as Constraint
import qualified Views.Search.ExposureRanges as ExposureRanges
import qualified Views.Search.Instrument as Instrument
import qualified Views.Search.Mapping as Mapping
import qualified Views.Search.Mission as Mission
import qualified Views.Search.PropType as PropType
import qualified Views.Search.SubArrays as SubArrays
import qualified Views.Search.Target as Target
import qualified Views.Search.TOO as TOO
import qualified Views.Search.Types as SearchTypes
import qualified Views.Schedule as Schedule

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar)

import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (lift, reader,runReaderT, ask)

import Data.Aeson (ToJSON, Value, (.=), encode, object)
import Data.Default (def)
import Data.List (foldl', nub)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import Data.Pool (Pool)
import Data.Time (UTCTime(utctDay), NominalDiffTime, addDays, addUTCTime, diffUTCTime, getCurrentTime)

import Database.Groundhog.Postgresql (Postgresql(..)
                                     , Conn
                                     , PersistBackend
                                     , SqlDb
                                     , (==.)
                                     , runDbConn
                                     , withPostgresqlPool)

-- import Network.HTTP.Date (HTTPDate, epochTimeToHTTPDate, formatHTTPDate)
import Network.HTTP.Types (StdMethod(HEAD)
                          , notModified304
                          , notFound404
                          , serviceUnavailable503)
-- import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static (CacheContainer
                                     , CachingStrategy(PublicStaticCaching)
                                     , (>->)
                                     , addBase, initCaching, noDots
                                     , staticPolicyWithOptions
                                     , defaultOptions, cacheContainer)
import Network.Wai.Handler.Warp (defaultSettings, setPort)

import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hFlush, stderr)

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Read (readMaybe)

import Web.Scotty.Trans

import Cache (Cache, CacheKey
             , fromCacheData, getFromCache, makeCache, toCacheKey)
import Database (NumObs, NumSrc, SIMKey
                , findRecord
                , getCurrentObs
                , getObsInfo
                , getObsId
                , getSchedule
                , getScheduleDate
                  
                , makeScheduleRestricted
                  
                , getProposalInfo
                -- , getProposalFromNumber
                -- , getRelatedObs
                -- , getObsFromProposal
                , getSimbadInfo
                , getTimeline

                  -- , findObsId
                , fetchSIMBADType
                , fetchNoSIMBADType
                , fetchSIMBADDescendentTypes
                , fetchObjectTypes 
                , fetchConstellation
                , fetchConstellationTypes
                , fetchCycle
                , fetchCycles

                , fetchCategory
                  
                , fetchCategorySubType
                , fetchCategoryTypes
                , fetchJointMission
                , fetchMissionInfo
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
                , fetchSubArrays

                , findNameMatch
                , findProposalNameMatch
                , findTarget
                  
                , getProposalObjectMapping
                , keyToPair
                  
                , getNumObsPerDay
                , getExposureBreakdown
                , getProposalTypeBreakdown
                , getProposalType
                  
                , getExposureValues
                , fetchExposureRanges
                , fetchExposureRange
                
                -- , findNearbyObs
                , findAllObs
                -- , NormSep
                -- , fromNormSep
                , getLastModifiedFixed

                , maybeProject
                
                , getDataBaseInfo
                  
                , dbConnStr
                )

import Git (gitCommitId)

import Layout (getFact, renderObsIdDetails)
import Sorted (SortedList
              , nullSL, emptySL, fromSL, mergeSL, unsafeToSL, lengthSL
              , StartTimeOrder, ExposureTimeOrder)

import Types (Record, SimbadInfo(..), Proposal(..), ProposalAbstract
             , PropNum
             , fromPropNum
             , NonScienceObs(..)
             , ScienceObs(..)
             , ObsInfo(..)
             , unsafeToObsIdVal
             , fromObsId

             , ObsIdVal
             , RA
             , Dec
             , ObsIdStatus
             , fromRA
             , fromDec
             , fromObsIdStatus
             
             -- , PropType(..)
             , SIMCategory
             , SimbadTypeInfo
             , TargetName(..)
             , TimeKS
             , unsafeToTimeKS
             , fromTimeKS
             , addTimeKS
             , zeroKS
             
             , ChandraTime
             , toChandraTime
             , fromChandraTime

             , Instrument(..)
             , Grating(..)
             , PropType(..)
             , PropCategory
             , TOORequestTime
             , ConstraintKind(TimeCritical)
               
             , RestrictedRecord
             , RestrictedSO
             , RestrictedSchedule
             , ScienceTimeline
             , EngineeringTimeline

             , Field(PaAbstractField, PaNumField)

             , fromSimbadType
             , toSimbadType
             , showExpTime
             , handleMigration
             , labelToRT
             , rtToLabel
             , labelToCS
             , csToLC
             , csToLabel
             , getConstellationNameStr
             , fromInstrument
             , fromGrating
             , fromPropType
             , recordObsId
             , recordTarget
             , recordStartTime
             , recordEndTime

             , fromConShort
             , fromConLong
             , getConstellationName

             , fromCycle

             , getMissionInfo

             , rsoObsId
             , rsoTarget
             , rsoRA
             , rsoDec
             , rsoExposureTime
             
             )
import Utils (ActionM, ScottyM
             , DBInfo
             , ChandraData(..), newReader
             , fromBlaze, standardResponse
             , timeToRFC1123
             , showInt
             , isChandraImageViewable
             , publicImageURL
             , ETag
             , makeETag
             , fromETag
             , HtmlContext(DynamicHtml)
             )




production :: 
  Int  -- ^ The port number to use
  -> Options
production p = def { verbose = 0
                   , settings = setPort p defaultSettings }

development :: Options
development = def

uerror :: T.Text -> IO ()
uerror msg = do
  T.hPutStrLn stderr ("ERROR: " <> msg)
  hFlush stderr
  exitFailure

-- The behavior depends on the presence of two environment variables,
-- that are handled separately:
--
--   PORT - the port number to run on (if not given it defaults to
--          3000)
--   DATABASE_URL  - the database to use (if on Heroku) otherwise
--                   use a local setup.
--
-- The Scotty documentation suggests calling setFdCacheDuration on the
-- settings field, to change the value from 0, but do not really
-- explain the implications of why it is set to 0 in the first place.
--
main :: IO ()
main = do
  mports <- lookupEnv "PORT"
  let eopts = case mports of
        Just ports -> case readMaybe ports of
          Just port ->  Right (production port)
          _ -> Left ("Invalid PORT argument: " <> T.pack ports)

        _ -> Right development

  connStr <- dbConnStr

  case eopts of
    Left emsg -> uerror emsg
    Right opts -> 
        do
          scache <- initCaching PublicStaticCaching
          withPostgresqlPool connStr 5 $ \pool -> do
            runDbConn handleMigration pool

            -- Invent a different caching strategy than used below for the
            -- observation-info data.
            --
            chandraApp <- setupCache pool
            makeAnApp <- setupLongCache pool

            let activeGalaxiesAndQuasars = "ACTIVE GALAXIES AND QUASARS"
                clustersOfGalaxies = "CLUSTERS OF GALAXIES"
                snSnrIsolatedNS = "SN, SNR AND ISOLATED NS"
                starsAndWD = "STARS AND WD"
                bhAndBinaries = "BH AND NS BINARIES"

                -- f is the db query, s is the "show" function
                pair ::
                  (PersistBackend m, SqlDb (Conn m))
                  => (a -> m (SortedList StartTimeOrder RestrictedSO))
                  -> (a -> T.Text)
                  -> a
                  -> (CacheKey, m RestrictedSchedule)
                pair f s k = (toCacheKey (s k),
                              fetchSchedule (f k))

                inst ::
                  (PersistBackend m, SqlDb (Conn m))
                  => Instrument
                  -> (CacheKey, m RestrictedSchedule)
                inst = pair fetchInstrument fromInstrument

                grat ::
                  (PersistBackend m, SqlDb (Conn m))
                  => Grating
                  -> (CacheKey, m RestrictedSchedule)
                grat = pair fetchGrating fromGrating
                
                igrat ::
                  (PersistBackend m, SqlDb (Conn m))
                  => (Instrument, Grating)
                  -> (CacheKey, m RestrictedSchedule)
                igrat = pair fetchIG fromIG
                
                prop ::
                  (PersistBackend m, SqlDb (Conn m))
                  => PropType
                  -> (CacheKey, m RestrictedSchedule)
                prop = pair getProposalType fromPropType
                
                cat ::
                  (PersistBackend m, SqlDb (Conn m))
                  => PropCategory
                  -> (CacheKey, m RestrictedSchedule)
                cat = pair fetchCategory id

                turnaround ::
                  (PersistBackend m, SqlDb (Conn m))
                  => Maybe TOORequestTime
                  -> (CacheKey, m RestrictedSchedule)
                turnaround = pair fetchTOO mRtToLabel
                
                conkind ::
                  (PersistBackend m, SqlDb (Conn m))
                  => Maybe ConstraintKind
                  -> (CacheKey, m RestrictedSchedule)
                conkind = pair fetchConstraint mCSToLC
                
            cache <- makeCache pool
                     [ inst HRCI
                     , inst HRCS
                     , inst ACISI
                     , inst ACISS

                     , grat NONE
                     , grat LETG
                     , grat HETG
                       
                        -- do not cache all the Instrument + Grating
                        -- combos
                     , igrat (ACISI, NONE)
                     , igrat (ACISS, NONE)
                       
                       -- how many proposal types should we cache?
                     , prop GTO
                     , prop GO
                       
                       -- this is unfortunately not a fixed list
                     , cat activeGalaxiesAndQuasars
                     , cat clustersOfGalaxies
                     , cat snSnrIsolatedNS
                     , cat starsAndWD
                     , cat bhAndBinaries

                       -- at the moment the vast majority of data has
                       -- no turnaround constraint, so not worth
                       -- caching the others
                     , turnaround Nothing

                     , conkind Nothing
                     , conkind (Just TimeCritical)
                     ]

            let wrap r = runReaderT r chandraApp
            scottyOptsT opts wrap (webapp pool scache cache makeAnApp)


-- | Create one of the caches used by the app.
--
--   This is used via a reader environment, and updates the cache every
--   60 seconds.
--
setupCache :: Pool Postgresql -> IO ChandraData
setupCache pool = do

  let cacheData = do
        let getData = do
              a <- getObsInfo
              b <- case a of
                     Just obs -> getDBInfo (oiCurrentObs obs)
                     Nothing -> pure (Nothing, (Nothing, emptySL))
              c <- getCurrentObs
              d <- getSchedule 3
              e <- getLastModifiedFixed
              return (a, b, c, d, e)

        (mobs, dbInfo, mRec, sched, timeData) <- runDbConn getData pool
        let obsData = case mobs of
                 Just obs ->
                   let t1 = "Success" :: T.Text
                       t2 = object [ "current" .= simpleObject (oiCurrentObs obs)
                                   , "previous" .= (simpleObject <$> oiPrevObs obs)
                                   , "next" .= (simpleObject <$> oiNextObs obs)
                                   ]
                       ans = t2 `seq` encode (t1, t2)
                   in ans
                 Nothing -> encode ("Failed" :: T.Text)

        now <- getCurrentTime
        pure (mobs, obsData, dbInfo, mRec, sched, timeData, now)

  -- We could just use a single MVar to access this info. This would
  -- be **much** better since it then ensures that all the values are
  -- coherent, and not that a subset of fields have just been updates
  -- while processing a page.
  --
  (mobs1, obsData1, dbInfo1, mRec1, sched1, timeData1, now1) <- cacheData
  obsInfoCache <- newMVar mobs1
  obsInfoJSONCache <- newMVar obsData1
  dbInfoCache <- newMVar dbInfo1
  currentObsCache <- newMVar mRec1
  schedule3Cache <- newMVar sched1
  lastModCache <- newMVar timeData1
  lastUpdatedCache <- newMVar now1

  _ <- forkIO $ forever $ do
    threadDelay 60000000
    (mobs, obsData, dbInfo, mRec, sched, timeData, now) <- cacheData

    -- I am randomly adding seq for fun here.
    --
    void $ mobs `seq` swapMVar obsInfoCache mobs
    void $ obsData `seq` swapMVar obsInfoJSONCache obsData
    void $ dbInfo `seq` swapMVar dbInfoCache dbInfo
    void $ mRec `seq` swapMVar currentObsCache mRec
    void $ sched `seq` swapMVar schedule3Cache sched
    void $ timeData `seq` swapMVar lastModCache timeData
    void $ now `seq` swapMVar lastUpdatedCache now

  pure $ newReader obsInfoCache obsInfoJSONCache dbInfoCache currentObsCache schedule3Cache lastModCache lastUpdatedCache


-- Grab the timeline data, but at a significantly lower cadence than
-- setupCache. Also, since the initial query takes a long time to
-- run we handle the cache setup differently.
--

type TimelineCacheData =
  (SortedList StartTimeOrder ScienceTimeline,
   SortedList StartTimeOrder EngineeringTimeline,
   M.Map TargetName SimbadInfo,
   [Proposal])

setupLongCache :: Pool Postgresql -> IO (MVar (TimelineCacheData, UTCTime, NominalDiffTime))
setupLongCache pool = do

  -- For the "empty" case could we use the current observation?
  now1 <- getCurrentTime
  let empty = (emptySL, emptySL, M.empty, [])
  mvar <- newMVar (empty, now1, 0.0)

  _ <- forkIO $ forever $ do
    t1 <- getCurrentTime
    cd <- runDbConn getTimeline pool
    t2 <- getCurrentTime

    let dt = diffUTCTime t2 t1

    -- now <- getCurrentTime
    let now = t2

    -- I am randomly adding seq for fun here.
    --
    void $ cd `seq` dt `seq` swapMVar mvar (cd, now, dt)

    threadDelay (60000000 * 5)

  pure mvar


-- Hack; needs cleaning up
getDBInfo :: 
  (MonadIO m, PersistBackend m, SqlDb (Conn m)) 
  => Record 
  -> m DBInfo
getDBInfo r = do
  as <- either (const (pure Nothing)) (getSimbadInfo . soTarget) r
  bs <- getProposalInfo r
  return (as, bs)


fromIG :: (Instrument, Grating) -> T.Text
fromIG (i, g) = fromInstrument i <> "-" <> fromGrating g

-- Fortunately TOORequestTime and ConstraintKind use a different
-- textual label to indicate "no data" - "Nothing" vs "none" -
-- so we can use this for the cache without worrying about a
-- name clash, or confusion over the key.
--
mRtToLabel :: Maybe TOORequestTime -> T.Text
mRtToLabel Nothing = "Nothing"
mRtToLabel (Just r) = rtToLabel r
      
-- use the lower-case representation      
mCSToLC :: Maybe ConstraintKind -> T.Text      
mCSToLC Nothing = "none"
mCSToLC (Just ck) = csToLC ck


-- TODO: could add a generic ETag for any non-static resource, based on git commit,
--       path element, and last-modified time from the DB. Which would allow a generic
--       304-handler for any resource.
--
webapp ::
  Pool Postgresql
  -> CacheContainer
  -> Cache
  -> MVar (TimelineCacheData, UTCTime, NominalDiffTime)
  -> ScottyM ()
webapp cm scache cache timeCache = do

    let liftSQL a = liftAndCatchIO (runDbConn a cm)
        getCache f = lift (reader f) >>= liftAndCatchIO . readMVar

    defaultHandler errHandle

    -- Need to find out how the static directory gets copied
    -- over by cabal; seems to be okay
    --
    -- middleware logStdoutDev
    -- middleware (staticPolicy (noDots >-> addBase "static"))
    -- middleware (staticPolicy' scache (noDots >-> addBase "static"))

    let cacheOpts = defaultOptions { cacheContainer = scache }
    middleware (staticPolicyWithOptions cacheOpts (noDots >-> addBase "static"))

    let {-
        dbQuery :: Parsable p
                   => L.Text
                   -> (p -> DbPersist Postgresql (NoLoggingT IO) v)
                   -> ActionM (p, v)
        -}
        dbQuery name act = do
          pval <- param name
          ans <- liftSQL (act pval)
          return (pval, ans)

        -- queryObsidParam :: ActionM (Int, Maybe ObsInfo)
        queryObsidParam = dbQuery "obsid" (getObsId . unsafeToObsIdVal)

    -- TEST
    get "/cache" $ do
      r <- lift ask
      let cget f = liftAndCatchIO (readMVar (f r))
      lastUpdated <- cget cdLastUpdatedCache
      lastMod <- cget cdLastModCache
      mCurrent <- cget cdCurrentObsCache
      dbInfo <- cget cddbInfoCache

      ((sObs, nsObs, tgs, props), timeLineNow, dt) <- liftAndCatchIO (readMVar timeCache)

      let page = H5.div ("Cache updated: " <> (H5.toHtml . timeToRFC1123) lastUpdated)
                 <>
                 H5.div ("Last modified: " <> (H5.toHtml . timeToRFC1123) lastMod)
                 <>
                 H5.div ("Observation: " <> H5.toHtml obs <> " - " <> H5.toHtml tgt)
                 <>
                 H5.div ("Proposal: " <> H5.toHtml prop)
                 <>
                 H5.div ("Timeline: nScience=" <> count sObs <>
                        " nEngineering=" <> count nsObs <>
                        " targets=" <> (H5.toHtml (show (M.size tgs))) <>
                        " proposals=" <> (H5.toHtml (show (length props))) <>
                        H5.br <>
                        H5.toHtml (timeToRFC1123 timeLineNow) <>
                        H5.br <>
                        "Runtime: " <> H5.toHtml (showInt (round dt :: Int)) <> "s")

          count = H5.toHtml . showInt . lengthSL

          obs = maybe "none" (showInt . fromObsId . recordObsId) mCurrent
          tgt = maybe "none" (fromTargetName . recordTarget) mCurrent

          prop = maybe "none"
            (\p -> showInt (fromPropNum (propNum p)) <> ": " <> propName p)
            (fst (snd dbInfo))

      fromBlaze $ H5.docTypeHtml H5.! A5.lang "en-US" $
        H5.head (H5.title "Cache")
        <>
        H5.body page


    get "/api/allfov" (apiAllFOV
                        (getCache cdLastModCache)
                        (liftSQL findAllObs))

    -- is adding HEAD support, and including modified info, sensible?
    addroute HEAD "/api/allfov"
      (do
          lastMod <- getCache cdLastModCache
          let toETag = makeETag gitCommitId "/api/allfov"
          setCacheHeaders toETag lastMod)

    -- For now always return JSON; need a better success/failure
    -- set up.
    --
    get "/api/current" $ do
      do
        -- We use the time the cache was last updated as the query time,
        -- which means it's going to be invalidated every minute, which
        -- is a bit short, but it should be "safe".
        let toEtag = makeETag gitCommitId "/api/current"
            getData = do
              jdata <- getCache cdObsInfoJSONCache
              lastMod <- getCache cdLastUpdatedCache
              -- since storing JSON stored as a string we can not use json
              --- but have to manually recreate it
              setHeader "Content-Type" "application/json; charset=utf-8"
              pure (jdata, lastMod)

        cacheApiQueryRaw raw toEtag (getCache cdLastUpdatedCache) getData

    -- TODO: this is completely experimental
    --       add support for cache
    --
    --       NOTE: we don't want this result to be cached for longer than
    --       a minute (as the page contents get updated on a minute
    --       time scale).
    --
    get "/api/page/:obsid" $ do
      obsid <- param "obsid"

      -- Is there a way to get the URL without having to recreate it?
      let toETag = makeETag gitCommitId ("/api/page/" <> showInt (fromObsId obsid))

      -- TODO: rethink this once I know what information I actually need
      --
      let getData = do
            cTime <- liftIO getCurrentTime
      
            -- note that we could use the cached data for the current observation
            --
            dbans <- liftSQL (do
                                 mobs <- getObsId obsid
                                 -- do I need all of findRecord
                                 b <- findRecord cTime
                                 c <- case mobs of
                                   Just o -> Just <$> getDBInfo (oiCurrentObs o)
                                   Nothing -> return Nothing
                                 pure (mobs, b, c))

            let (mobs, mCurrent, mDbInfo) = dbans
                mCurrentObsId = recordObsId <$> mCurrent

            case (mobs, mDbInfo) of
              (Just obs, Just dbInfo) -> do

                let thisObs = oiCurrentObs obs
                    (msimbad, (mprop, matches)) = dbInfo

                mpropText <- case thisObs of
                  Left _ -> pure Nothing
                  Right so -> liftSQL (maybeProject PaAbstractField (PaNumField ==. soProposal so))

                let obshtml = Record.renderStuff DynamicHtml cTime thisObs dbInfo

                    mDetails = either (const Nothing) Just
                               (renderObsIdDetails DynamicHtml mprop msimbad <$> thisObs)

                    -- could add number of observations we know about this proposal
                    mProposal = case mpropText of
                      Just prop -> Just ((H5.div H5.! A5.class_ "abstract")
                                         (H5.p (H5.toHtml prop)))
                      _ -> Nothing

                    mRelated = case thisObs of
                      Left _ -> Nothing
                      Right so -> Record.renderRelatedObs (soTarget so) matches

                    mKV k v = [k .= renderHtml v]

                    objItems = ["status" .= ("success" :: T.Text)
                               , "overview" .= renderHtml obshtml
                               , "ra" .= fromRA (either nsRa soRA thisObs)
                               , "dec" .= fromDec (either nsDec soDec thisObs)
                               , "isCurrent" .= (mCurrentObsId == Just obsid)
                               , "observation" .= simpleObject thisObs
                               , "previous" .= (simpleObject <$> oiPrevObs obs)
                               , "next" .= (simpleObject <$> oiNextObs obs)
                               ]
                               ++ maybe [] (mKV "details") mDetails
                               ++ maybe [] (mKV "related") mRelated
                               ++ maybe [] (mKV "proposal") mProposal

                pure (object objItems)

              _  -> do
                fact <- liftIO getFact
                let nohtml = Index.noDataDiv fact
                pure (object ["status" .= ("error" :: T.Text)
                             , "error" .= renderHtml nohtml])


          getData' = getCache cdLastUpdatedCache
                     >>= \a -> getData
                     >>= \b -> pure (b, a)

      -- NOTE: we use the cache update time as the "last-modified" time,
      --       even though nothing is cached here, as we just want something
      --       to say "don't update it less than every minute".
      --
      cacheApiQuery toETag (getCache cdLastUpdatedCache) getData'

    addroute HEAD "/api/page/:obsid" $ do
      obsid <- param "obsid"
      lastMod <- getCache cdLastUpdatedCache  -- use time of last update
      let toETag = makeETag gitCommitId ("/api/page/" <> showInt (fromObsId obsid))
      setCacheHeaders toETag lastMod

    -- support "category" searches (e.g. ACIS-I observations or
    -- related proposals)
    --
    -- For now have specific search pages, which replicates some/all
    -- of the "static" versions.
    --
    let skyview ::
          T.Text
          -- ^ Title
          -> Maybe T.Text
          -- ^ Optional description
          -> SortedList a RestrictedSO
          -> Value
        skyview title mdesc xs = 
          let times = map rsoExposureTime (fromSL xs)
              etime = foldl' addTimeKS zeroKS times
              exptime = showExpTime etime
              
              pairs = [ "observations" .= rsoListToJSON xs
                      , "nobs" .= lengthSL xs
                      , "exptime" .= exptime
                      , "title" .= title
                      ] <>
                      [ "description" .= t | Just t <- [mdesc]]
                      
          in object pairs

        -- drop non-science observations
        --
        {-
        skyview2 :: T.Text -> Maybe T.Text -> RestrictedSchedule -> Value
        skyview2 title mdesc rs =
          -- rely on time ordering applied when creating these
          -- schedules, which is not encoded in the types
          let xs = rrDone rs <> doing <> rrToDo rs
              doing = maybe [] (:[]) (rrDoing rs)
              (_, ys) = partitionEithers xs
          in skyview title mdesc (unsafeToSL ys)
         -}
    
    {-  copied from later as guidance
        let searchResultsRestricted getData isNull page = do
          (xs, matches) <- getData
          when (nullSL matches || isNull xs) next
          sched <- liftSQL (makeScheduleRestricted (fmap Right matches))
          fromBlaze (page xs sched)
    -}

    -- TODO: get from cache
    get "/api/skyview/category/:category"
      (do
          (propCat, xs) <- dbQuery "category" fetchCategory
          let title = "Category search: " <> propCat
          json (skyview title Nothing xs)
      )

    get "/api/skyview/constellation/:constellation"
      (do
          (con, xs) <- dbQuery "constellation" fetchConstellation
          let title = "Constellation search: " <> fromConShort con
              longName = fromConLong <$> getConstellationName con
          json (skyview title longName xs)
      )

    -- TODO: what case do we want the 'none' parameter to be?
    --
    get "/api/skyview/constraints/none"
      (do
          xs <- liftSQL (fetchConstraint Nothing)
          let title = "No constraint"
          json (skyview title Nothing xs)
      )
    
    get "/api/skyview/constraints/:cs"
      (do
          csStr <- param "cs"
          case labelToCS csStr of
            Just cs -> do
              xs <- liftSQL (fetchConstraint (Just cs))
              let title = "Constraint: " <> csStr
                  desc = csToLabel cs
              json (skyview title (Just desc) xs)

            Nothing -> status notFound404
      )
      
    get "/api/skyview/cycle/:cycle"
      (do
          (cycleVal, xs) <- dbQuery "cycle" fetchCycle
          let title = "Cycle: " <> v
              desc = "Chandra Cycle " <> v
              v = fromCycle cycleVal
          json (skyview title (Just desc) xs)
      )
      
    -- TODO: use cache
    get "/api/skyview/grating/:grating"
      (do
          (grat, xs) <- dbQuery "grating" fetchGrating
          let title = "Grating: " <> v
              desc = v
              v = fromGrating grat
          json (skyview title (Just desc) xs)
      )

    {-
    get "/api/skyview/instrument/:instrument"
      (do
          let getDB = fetchInstrument
                
          inst <- param "instrument"
          let key = toCacheKey (toText inst)
          mcdata <- liftIO (getFromCache cache key)
          case mcdata of
            Just cdata -> skyview2 (fromCacheData cdata)
            Nothing -> do
              matches <- liftSQL (getDB inst)
              when (nullSL matches) status notFound404
              skyview matches
      )
    -}

    -- TODO: use cache
    get "/api/skyview/instrument/:instrument"
      (do
          (inst, xs) <- dbQuery "instrument" fetchInstrument
          let title = "Instrument: " <> v
              desc = v
              v = fromInstrument inst
          json (skyview title (Just desc) xs)
      )

    get "/api/skyview/joint/:mission"
      (do
          (mission, xs) <- dbQuery "mission" fetchJointMission
          
          case getMissionInfo mission of
            Just (sName, lName, _) ->
              let title = "Joint mission: " <> sName
              in json (skyview title (Just lName) xs)

            Nothing -> do
              pval <- param "mission" -- can I re-request it?
              let title = "Joint mission: " <> pval
              json (skyview title Nothing xs)
      )

    -- TODO: do we need the query parameter? Just make it the path
    --
    get "/api/skyview/name"
      (do
          name <- param "target"
          (xs, tnames) <- liftSQL (findTarget name)
          if nullSL xs
            then status notFound404
            else do
              let title = "Name search: " <> fromTargetName name
                  nameList = T.intercalate ", " (map fromTargetName tnames)
                  desc = case tnames of
                    [] -> "Target: " <> fromTargetName name
                    [_] -> "Target: " <> nameList
                    _ -> "Targets: " <> nameList

              json (skyview title (Just desc) xs)
      )

    get "/api/skyview/proposal/:propnum"
      (do
          (propNum, ans) <- dbQuery "propnum" fetchProposal
          let (mprop, _, xs) = ans
              title = "Proposal: " <> showInt (fromPropNum propNum)
              desc = propName <$> mprop

          json (skyview title desc xs)
      )

    -- TODO: use cache
    get "/api/skyview/proptype/:proptype"
      (do
          (propType, xs) <- dbQuery "proptype" getProposalType
          let title = "Proposal Type: " <> v
              v = fromPropType propType
          json (skyview title Nothing xs)
      )

    -- TODO: what case do we want the 'none' parameter to be?
    --
    get "/api/skyview/turnaround/none"
      (do
          xs <- liftSQL (fetchTOO Nothing)
          let title = "Not a TOO"
          json (skyview title Nothing xs)
      )
    
    get "/api/skyview/turnaround/:too"
      (do
          tooStr <- param "too"
          case labelToRT tooStr of
            Just too -> do
              xs <- liftSQL (fetchTOO (Just too))
              let title = "TOO request: " <> tooStr
                  desc = rtToLabel too
              json (skyview title (Just desc) xs)

            Nothing -> status notFound404
      )
      
    get "/api/skyview/type/unidentified"
      (do
          (_, xs) <- liftSQL fetchNoSIMBADType
          let title = "SIMBAD search: unidentified"
              desc = "Targets with no SIMBAD match"
          json (skyview title (Just desc) xs)
      )
      
    get "/api/skyview/type/:type"
      (do
          mres <- snd <$> dbQuery "type" fetchSIMBADType
          case mres of
            Just (sti, xs) ->
              let (stype, sdesc) = sti
                  title = "SIMBAD search: " <> fromSimbadType stype
              in json (skyview title (Just sdesc) xs)

            Nothing -> status notFound404
      )

    -- this includes "children" of this type
    -- TODO: improve information on the children being shown
    --
    -- include after /type/ to show commonalities
    --
    get "/api/skyview/dtype/:type"
      (do
          (stis, xs) <- snd <$> dbQuery "type" fetchSIMBADDescendentTypes

          -- guaranteed that stis is not empty
          let (stype, sdesc) = head stis
              title = "SIMBAD search: " <> fromSimbadType stype
                      <> if length stis > 1 then " and children" else mempty

              -- TODO: need to add in info about all the descendants
          json (skyview title (Just sdesc) xs)
      )


    {-

    get "/api/obsid/:obsid" (apiObsId queryObsidParam)

    -- break down the monolithic queries into separate ones, which may or may not
    -- be a good idea
    --
    -- Note that for Simbad names we may have a / in them, so we use
    -- a regex
    get (regex "^/api/simbad/name/(.+)$") (apiSimbadName
                                           (dbQuery "1" getSimbadInfo))
      
    get "/api/proposal/:propnum" (apiProposal
                                  (dbQuery "propnum" getProposalFromNumber))

    get "/api/related/:propnum/:obsid" $ do
      propNum <- param "propnum"
      obsid <- param "obsid"
      apiRelatedPropNumObsId
        (liftSQL (getRelatedObs propNum obsid))

    get "/api/related/:propnum" (apiRelatedPropNum
                                 (dbQuery "propnum" getObsFromProposal))

    get "/api/search/dtype" (apiSearchDtype (liftSQL fetchObjectTypes))

    -}


    -- note that this is different from /api/simbad/name since it
    -- is a search, rather than exact match
    --
    -- also, the handling of spaces is not ideal, since it is
    -- likely that I just want to ignore all spaces: possible
    -- that there are some cases this is not wanted, but it
    -- seems like it is the most useful behavior. Unfortunately,
    -- not easy to search the db in this manner with the current
    -- set up.
    --
    get "/api/search/name" (apiSearchName
                            (dbQuery "term" findNameMatch))


    get "/api/search/proposal" (apiSearchProposal
                                (dbQuery "term" findProposalNameMatch))

    -- How to best serialize the mapping data? For now go with a
    -- form that is closely tied to the visualization.
    --
    get "/api/mappings" (apiMappings
                          (getCache cdLastModCache)
                          (liftSQL getProposalObjectMapping))

    -- HIGHLY EXPERIMENTAL: explore a timeline visualization
    --
    -- Looks like the "new" version is ~ 5 times faster at the moment
    -- (on my local machine; 10s vs 2s, approx) so switch to it for
    -- now (turns out not to be as big a saving on heroku :-(
    --
    get "/api/timeline" (apiTimeline (liftSQL getTimeline))
    
    get "/api/timeline2" $ do
      (cd, tnow, _) <- liftAndCatchIO (readMVar timeCache)
      apiTimeline2 cd tnow

    -- highly experimental
    get "/api/exposures" (apiExposures
                           (getCache cdLastModCache)
                           (liftSQL getExposureValues))
    
    get "/" (redirect "/index.html")

    get "/about.html" (redirect "/about/index.html")
    get "/about" (redirect "/about/index.html")
    get "/about/index.html"
      (liftSQL getDataBaseInfo >>= fromBlaze . About.aboutPage)
      
    -- WWT redirects: I could mark these as 410, moved permenantly,
    -- but for now leave as 304's.
    --
    let redirectObsid = do
          -- note: this is treating obsid as a string and not an
          -- integer; we can let the redirected handler deal with
          -- invalid values rather than convert to a number and
          -- then back to text.
          --
          obsid <- param "obsid"
          redirect ("/obsid/" <> obsid)
                    
    get "/wwt.html" (redirect "/index.html")
    get "/obsid/:obsid/wwt" redirectObsid

    get "/index.html" $ do
      mobs <- getCache cdObsInfoCache
      db <- getCache cddbInfoCache
      case (mobs, db) of
        (Just obs, dbInfo) -> do
          cTime <- liftIO getCurrentTime
          fromBlaze (Index.introPage cTime obs dbInfo)

        _  -> liftIO getFact >>= fromBlaze . Index.noDataPage


    get "/obsid/:obsid" (obsidOnly
                         (snd <$> queryObsidParam)
                         (getCache cdCurrentObsCache)
                         -- (liftSQL getCurrentObs)
                         (liftSQL . getDBInfo . oiCurrentObs)
                        )

    -- TODO: head requests
    get "/proposal/:propnum" (proposal
                              (snd <$> dbQuery "propnum" fetchProposal)
                              (liftSQL . makeScheduleRestricted))
      
    let -- querySchedule 3 = queryScheduleCache
        querySchedule n = do
          sched <- liftSQL (getSchedule n)
          fromBlaze (Schedule.schedPage sched)
          
        queryScheduleDate date n = do
          sched <- liftSQL (getScheduleDate date n)
          fromBlaze (Schedule.schedDatePage date sched)

        queryScheduleTime name mul = do
          val <- param name
          when (val <= 0) next  -- TODO: better error message
          querySchedule (mul * val)

        -- just cache the 3-day result
        queryScheduleCache = getCache cdSchedule3Cache >>= fromBlaze . Schedule.schedPage

    get "/schedule" (redirect "/schedule/index.html")
    get "/schedule/index.html" queryScheduleCache
    get "/schedule/day" (querySchedule 1)
    get "/schedule/week" (querySchedule 7)
    get "/schedule/day/:ndays" (queryScheduleTime "ndays" 1)
    get "/schedule/week/:nweeks" (queryScheduleTime "nweeks" 7)

    -- allow the schedule to be centered on a date
    --
    -- TODO: when should the validity of the input date be checked?
    get "/schedule/date/:date/:ndays" $ do
      ndays <- param "ndays"
      when (ndays <= 0) next  -- TODO: better error message
      -- there is no Parsable instance for Date, so rather than have
      -- an orphan instance, deal with conversion manually
      dateText <- param "date"
      case readEither dateText of
        Left _ -> next -- TODO: better error message
        Right date -> queryScheduleDate date ndays

    -- TODO: also need a HEAD request version
    get "/search/" (redirect "/search/index.html")

    -- TODO: what does a cache miss mean here? Should we fill it in or
    --       not?
    --
    let fromCacheInt pval toText getDB toHtml = do
          let key = toCacheKey (toText pval)
          mcdata <- liftIO (getFromCache cache key)
          case mcdata of
            Just cdata ->
              fromBlaze (toHtml pval (fromCacheData cdata))
          
            Nothing -> do
              matches <- liftSQL (getDB pval)
              when (nullSL matches) next
              sched <- liftSQL (makeScheduleRestricted (fmap Right matches))
              fromBlaze (toHtml pval sched)

        fromCache pname toText getDB toHtml = do
          pval <- param pname
          fromCacheInt pval toText getDB toHtml

    -- TODO: also need a HEAD request version
    -- This returns only those observations that match this
    -- type; contrast with /seatch/dtype/:type
    --
    get "/search/type/unidentified" (searchTypeUnId
                                     (liftSQL fetchNoSIMBADType)
                                     (liftSQL . makeScheduleRestricted))

    -- TODO: use the Cache, Luke
    get "/search/type/:type" (searchType
                              (snd <$> dbQuery "type" fetchSIMBADType)
                              (liftSQL . makeScheduleRestricted))

    -- TODO: also need a HEAD request version
    get "/search/type/" (searchTypeNone (liftSQL fetchObjectTypes))

    -- TODO: also need a HEAD request version
    --     FOR TESTING
    --
    get "/search/dtype/" (searchDTypeNone (liftSQL fetchObjectTypes))

    let searchResultsRestricted getData isNull page = do
          (xs, matches) <- getData
          when (nullSL matches || isNull xs) next
          sched <- liftSQL (makeScheduleRestricted (fmap Right matches))
          fromBlaze (page xs sched)

    -- This is still useful for the category/simbad search, since
    -- we probably don't need to cache any of these, and it's
    -- not clear whether the API of maybeSearchResultsRestricted
    -- will work here (since the key needs to know the proposal
    -- category and it needs some work to pass through). Actually,
    -- can probably handle it, just need a "Maybe _ -> Text" routine
    -- that is created on the fly to include the category.
    --
    let maybeSearchResultsRestrictedOld mval getData page =
          case mval of
            Just val -> do
              matches <- getData val
              when (nullSL matches) next
              sched <- liftSQL (makeScheduleRestricted (fmap Right matches))
              fromBlaze (page val sched)
            Nothing -> next

    let maybeSearchResultsRestricted mval toText getData page =
          case mval of
            Just val -> fromCacheInt val toText getData page
            Nothing -> next
            
    -- This returns those observations that match this
    -- type and any "sub types"; contrast with /seatch/type/:type
    -- TODO: also need a HEAD request version
    --
    -- TODO: use the Cache, Luke
    get "/search/dtype/:type"
      (searchResultsRestricted
       (snd <$> dbQuery "type" fetchSIMBADDescendentTypes)
       null SearchTypes.matchDependencyPage)

    -- TODO: also need a HEAD request version
    --
    -- This does not need to use the cache just yet, I think
    get "/search/constellation/:constellation"
      (searchResultsRestricted
       (dbQuery "constellation" fetchConstellation)
       (const False) Constellation.matchPage)
    
    -- TODO: also need a HEAD request version
    get "/search/constellation/"
      (liftSQL fetchConstellationTypes
       >>= fromBlaze . Constellation.indexPage)
          

    -- TODO: also need a HEAD request version
    --
    -- The "none" type needs to use the cache
    --
    get "/search/turnaround/:too" $ do
      -- as I do not have a "none" type in TOORequestTime, parse
      -- this parameter as a string rather than as a TOORequestTime
      --
      -- perhaps should have two separate routes; one where the
      -- parameter can be parsed automatically and one for none
      tooParam <- param "too"
      let mans = if T.toLower tooParam == "none"
                 then Just Nothing
                 else case labelToRT tooParam of
                        Nothing -> Nothing
                        a -> Just a

      maybeSearchResultsRestricted mans mRtToLabel
        fetchTOO TOO.matchPage
        
    -- TODO: also need a HEAD request version
    get "/search/turnaround/" $ do
      (matches, noneTime) <- liftSQL fetchTOOs
      fromBlaze (TOO.indexPage matches noneTime)

    -- TODO: also need a HEAD request version
    --
    -- This does not need to use the cache just yet, I think
    get "/search/cycle/:cycle"
      (searchResultsRestricted (dbQuery "cycle" fetchCycle)
       (const False) Cycle.matchPage)

    get "/search/cycle/" $ do
      cycles <- liftSQL fetchCycles
      fromBlaze (Cycle.indexPage cycles)

    -- TODO: also need a HEAD request version
    --
    -- The "none" type needs to use the cache
    --
    get "/search/constraints/:cs" $ do
      -- as I do not have a "none" type in ConstraintKind, parse
      -- this parameter as a string rather than as a ConstraintKind
      csParam <- param "cs"
      let mans = if T.toLower csParam == "none"
                 then Just Nothing
                 else case labelToCS csParam of
                        Nothing -> Nothing
                        a -> Just a

      maybeSearchResultsRestricted mans mCSToLC
        fetchConstraint Constraint.matchPage
      
    -- TODO: also need a HEAD request version
    get "/search/constraints/" $ do
      (matches, noneTime) <- liftSQL fetchConstraints
      fromBlaze (Constraint.indexPage matches noneTime)

    -- TODO: also need a HEAD request version
    get "/search/category/:category/:type" $ do
      -- TODO: perhaps should do a check on category, as can assume a
      --       relatively static set of options
      cat <- param "category"
      stypeUser <- param "type"
      -- oh, this is ugly
      let mtype = if stypeUser == "unidentified"
                  then Just Nothing
                  else case toSimbadType stypeUser of
                    Just s -> Just (Just s)
                    _ -> Nothing

      maybeSearchResultsRestrictedOld mtype
        (liftSQL . fetchCategorySubType cat)
        (Category.categoryAndTypePage cat)

    {-
    get "/search/category/:category"
      (searchResultsRestricted (dbQuery "category" fetchCategory)
       (const False) Category.matchPage)
    -}
    
    get "/search/category/:category"
      (fromCache "category" id fetchCategory
       Category.matchPage)

    -- TODO: also need a HEAD request version
    get "/search/category/" $ do
      matches <- liftSQL fetchCategoryTypes
      fromBlaze (Category.indexPage matches)

    get "/search/exposurerange/:range"
      (searchResultsRestricted (dbQuery "range" fetchExposureRange)
       (const False) ExposureRanges.matchPage)
    
    get "/search/exposurerange/" $ do
      matches <- liftSQL fetchExposureRanges
      fromBlaze (ExposureRanges.indexPage matches)

    -- TODO: also need a HEAD request version
    {-
    get "/search/instrument/:instrument"
      (searchResultsRestricted
       (dbQuery "instrument" fetchInstrument) (const False)
       Instrument.matchInstPage)
    -}

    get "/search/instrument/:instrument"
      (fromCache "instrument" fromInstrument fetchInstrument
                 Instrument.matchInstPage)

    -- TODO: also need a HEAD request version
    {-
    get "/search/grating/:grating"
      (searchResultsRestricted
       (dbQuery "grating" fetchGrating) (const False)
       Instrument.matchGratPage)
    -}
    
    get "/search/grating/:grating"
      (fromCache "grating" fromGrating fetchGrating
                 Instrument.matchGratPage)

    -- TODO: also need a HEAD request version
    {-
    get "/search/instgrat/:ig"
      (searchResultsRestricted
       (dbQuery "ig" fetchIG) (const False)
       Instrument.matchIGPage)
    -}

    get "/search/instgrat/:ig"
      (fromCache "ig" fromIG fetchIG
                 Instrument.matchIGPage)
    
    let igsearch = do
          (imatches, gmatches, igmatches) <- liftSQL (
            do
              xs <- fetchInstrumentTypes
              ys <- fetchGratingTypes
              zs <- fetchIGTypes
              return (xs, ys, zs))
          fromBlaze (Instrument.indexPage imatches gmatches igmatches)
          
    -- TODO: also need a HEAD request version
    get "/search/instrument/" igsearch
    get "/search/grating/" igsearch
    get "/search/instgrat/" igsearch

    -- TODO: can this use makeScheduleRestricted?
    get "/search/name" $ do
      (target, (matches, matchNames)) <- dbQuery "target" findTarget
      -- TODO: set an error code if no match? Once have sorted out search info
      if nullSL matches
        then fromBlaze (Target.noMatchPage target)
        else do
          sched <- liftSQL (makeScheduleRestricted (fmap Right matches))
          fromBlaze (Target.targetPage target matchNames sched)

    -- Try displaying a "calendar" view
    -- TODO: the choice of 21 days is somewhat arbitrary, but the idea
    --       is to support STS elements, which should be about 2 weeks
    --       of data; however, I've seen a case where there's a few more
    --       days, so bump up to 21. This may begin to include LTS
    --       data, but live with that for now.
    --
    let dayLimit = 21 :: Integer
    
    get "/search/calendar" $ do
      now <- liftIO getCurrentTime
      let maxDay = addDays dayLimit (utctDay now)
      cts <- liftSQL (getNumObsPerDay maxDay)
      fromBlaze (Calendar.indexPage cts)

    -- TODO: need better endpoint name
    get "/search/breakdown" $ do
      now <- liftIO getCurrentTime
      let maxDay = addDays dayLimit (utctDay now)
      (total, perDay) <- liftSQL (getExposureBreakdown maxDay)
      fromBlaze (Instrument.breakdownPage total perDay)

    -- highly experimental
    -- for now use a static file
    get "/search/exposures/" (redirect "/search/exposures/index.html")
    get "/search/timeline/" (redirect "/search/timeline.html")

    get "/search/proptype" $ do
      propInfo <- liftSQL getProposalTypeBreakdown
      fromBlaze (PropType.indexPage propInfo)

    {-
    get "/search/proptype/:proptype"
      (searchResultsRestricted
       (dbQuery "proptype" getProposalType) (const False)
       PropType.matchPage)
    -}
    
    get "/search/proptype/:proptype"
      (fromCache "proptype" fromPropType getProposalType
                 PropType.matchPage)

    
    -- map between proposal category and SIMBAD object types.
    get "/search/mappings" (fromBlaze Mapping.indexPage)

    get "/search/joint/:mission"
      (searchResultsRestricted
       (dbQuery "mission" fetchJointMission) (const False)
       Mission.matchPage)
    
    get "/search/joint/"
      (liftSQL fetchMissionInfo >>= fromBlaze . Mission.indexPage)

    get "/search/subarrays/" $ do
      (matches, noSubTime) <- liftSQL fetchSubArrays
      fromBlaze (SubArrays.indexPage matches noSubTime)

    -- HEAD requests
    -- TODO: is this correct for HEAD; or should it just 
    --       set the redirect header?
    addroute HEAD "/" (standardResponse >> redirect "/index.html")
    addroute HEAD "/index.html" standardResponse

    addroute HEAD "/wwt.html" (standardResponse >> redirect "/index.html")

    addroute HEAD "/about" (standardResponse >> redirect "/about/index.html")

    -- TODO: does the staticPolicy middleware deal with this?
    -- addroute HEAD "/about/index.html" standardResponse
    -- addroute HEAD "/about/instruments.html" standardResponse
    -- addroute HEAD "/about/views.html" standardResponse

    addroute HEAD "/obsid/:obsid" $ do
      mobs <- snd <$> queryObsidParam
      case mobs of
        Just _ -> standardResponse
        _      -> next -- status notFound404

    -- TODO: is this actually correct?
    addroute HEAD "/obsid/:obsid/wwt" redirectObsid

    {-
    get "/404" $ redirect "/404.html"
    get "/404.html" $ do
      fact <- liftIO getFact
      fromBlaze $ NotFound.notFoundPage fact
      status notFound404

    notFound $ redirect "/404.html"
    -}

    notFound $ do
      fact <- liftIO getFact
      status notFound404
      fromBlaze (NotFound.notFoundPage fact)


-- | Exception handler. We should log the error.
errHandle :: L.Text -> ActionM ()
errHandle txt = do
  liftIO (L.putStrLn ("Error string: " <> txt))
  -- Can we change the HTTP status code? The following does not seem to
  -- work.
  status serviceUnavailable503
  fromBlaze NotFound.errPage


{-
-- | Log a message to stderr
logMsg :: T.Text -> ActionM ()
-- logMsg = liftIO . T.hPutStrLn stderr
logMsg = const (return ())
-}


-- | Set up the cache headers (last-modified and etag).
--
setCacheHeaders ::
  (UTCTime -> ETag)
  -> UTCTime
  -> ActionM ()
setCacheHeaders toETag lastMod = do
  let etag = toETag lastMod
  setHeader "Last-Modified" (timeToRFC1123 lastMod)
  setHeader "ETag" (fromETag etag)


-- | Attempt to support cache controll access to the JSON data
--   in this resource.
--
--   If the query has an ETag then use it, otherwise check on the
--   last-modified date. This means that the last-modified date
--   can be the same but the cache hit fails (if the ETag is different).
--   This is different to my reading of wai-middleware-cache.
--
cacheApiQueryRaw ::
  (a -> ActionM ())
  -- ^ Convert the data into a response.
  -> (UTCTime -> ETag)
  -- ^ Create the ETag for this resource, based on the last-updated
  --   value from the database.
  -> ActionM UTCTime
  -- ^ Returns the last-modified time to use
  -> ActionM (a, UTCTime)
  -- ^ Returns the data to convert and the last-modified time (which
  --   could have changed since the original query).
  -> ActionM ()
cacheApiQueryRaw toData toETag getLastMod getData = do
  hdrs <- headers
  let readHeader = flip lookup hdrs
      qryLastMod = readHeader "If-Modified-Since"
      qryETag = readHeader "If-None-Match"

  lastModUTC <- getLastMod
  let -- isNotModified = (Just etag == qryETag) || (Just lastMod == qryLastMod)

      lastMod = timeToRFC1123 lastModUTC
      isNotModified = case qryETag of
                        Just e -> e == fromETag (toETag lastModUTC)
                        Nothing -> Just lastMod == qryLastMod

      -- I was setting the cache headers here, but a bit pointless
      notModified = status notModified304

      modified = do
        -- since the lastMod date could have changed since we last requested
        -- it, requery for it.
        --
        (ans, lastMod') <- getData

        -- The times here are longer than the expected update time of 60 seconds,
        -- but it's not terrible if things are cached for a few minutes.
        --
        setHeader "Cache-Control" "no-transform,public,max-age=300,s-maxage=900"
        setHeader "Vary" "Accept-Encoding"
        setCacheHeaders toETag lastMod'
        toData ans

  if isNotModified then notModified else modified


cacheApiQuery ::
  ToJSON a
  => (UTCTime -> ETag)
  -- ^ Create the ETag for this resource, based on the last-updated
  --   value from the database.
  -> ActionM UTCTime
  -- ^ Returns the last-modified time to use
  -> ActionM (a, UTCTime)
  -- ^ Returns the data to convert and the last-modified time (which
  --   could have changed since the original query).
  -> ActionM ()
cacheApiQuery = cacheApiQueryRaw json


-- TODO: define a data type to represent the JSON response, so that
--       there's a "defined" success/error reporting structure.
--

type Roll = Double

-- Do we really gain much from enforcing a 304 here?
-- In tests with chromium we get a 304 from the initial run but then
-- subsequent requests return the data from the on-disk cache but still
-- report a 200
--
-- Useful resources
--   https://www.mnot.net/cache_docs/
--   http://dev.mobify.com/blog/beginners-guide-to-http-cache-headers/
--
apiAllFOV ::
  ActionM UTCTime
  -> ActionM ([(RA, Dec, Roll, Instrument, TargetName, ObsIdVal, ObsIdStatus)]
             , UTCTime)
  -> ActionM ()
apiAllFOV getLastMod getData =
  let toETag = makeETag gitCommitId "/api/allfov"

      getData' = do
        (ans, lastMod) <- getData

        -- need to convert to a JSON map
        let conv (ra, dec, roll, inst, tname, obsid, ostatus) =
              object [ "ra" .= fromRA ra
                     , "dec" .= fromDec dec
                     , "roll" .= roll
                     , "instrument" .= fromInstrument inst
                     , "name" .= tname  -- has a ToJSON instance we can use
                     , "status" .= fromObsIdStatus ostatus
                     , "obsid" .= fromObsId obsid
                     ]

        return (map conv ans, lastMod)

  in cacheApiQuery toETag getLastMod getData'


{-

debug :: T.Text -> ActionM ()
debug msg = liftAndCatchIO (T.putStrLn ("<< " <> msg <> " >>"))

-}

-- | Represent an observation as an object containing the following
--   fields:
--
--      obsid
--      target
--
simpleObject :: Record -> Value
simpleObject r =
  let mtimes = do
        s <- recordStartTime r
        e <- recordEndTime r
        Just (s, e)

  in object ([ "obsid" .= fromObsId (recordObsId r)
             , "target" .= recordTarget r
             ] ++ case mtimes of
                    Just (stime, etime) ->
                      [ "start" .= fromChandraTime stime
                      , "end" .= fromChandraTime etime
                      ]
                    Nothing -> [])



{-
apiObsId :: ActionM (Int, Maybe ObsInfo) -> ActionM ()
apiObsId getData = do

  (obsid, mobs) <- getData
  case mobs of
    Just v -> json ("Success" :: T.Text, v)
    _ -> json ("Unknown ObsId" :: T.Text, obsid)

-}


{-

apiSimbadName :: ActionM (TargetName, Maybe SimbadInfo) -> ActionM ()
apiSimbadName getData = do

  debug "/api/simbad/name/"

  (name, msim) <- getData
  case msim of
    Just sim -> json ("Success" :: T.Text, sim)
    _ -> json ("Unknown Target" :: T.Text, name)


apiProposal :: ActionM (PropNum, Maybe Proposal) -> ActionM ()
apiProposal getData = do

  debug "/api/proposal/"

  (propNum, mres) <- getData
  case mres of
    Just res -> json ("Success" :: T.Text, res)
    _ -> json ("Unknown Proposal Number" :: T.Text, propNum)


apiRelatedPropNumObsId ::
  ActionM (SortedList StartTimeOrder ScienceObs)
  -> ActionM ()
apiRelatedPropNumObsId getData = do

  debug "/api/related/:propnum/:obsid"

  res <- getData 
  -- hmmm, can't tell between an unknown propnum/obsid
  -- pair and an observation with no related observations.
  json ("Success" :: T.Text, fromSL res)


apiRelatedPropNum ::
  ActionM (PropNum, SortedList StartTimeOrder ScienceObs)
  -> ActionM ()
apiRelatedPropNum getData = do

  debug "/api/related/:propnum"

  res <- snd <$> getData
  -- hmmm, can't tell between an unknown propnum
  -- and an observation with no related observations.
  json ("Success" :: T.Text, fromSL res)


apiSearchDtype :: ActionM [(SimbadTypeInfo, Int)] -> ActionM ()
apiSearchDtype getData = do

  debug "/api/search/dtype"

  matches <- getData
  json (SearchTypes.renderDependencyJSON matches)

-}


-- For now we map the results to a dictionary which may or may not have
-- a location, and we remove matches from the "other" response with
-- a case-insensitive match to extractAll.
--
-- Although not enforced by the type, the TargetName,Ra,Dec tuple
-- should not contain multiple target name fields (other than
-- case or space differences)
--
apiSearchName ::
  ActionM (String, ([(TargetName, RA, Dec)], [TargetName]))
  -> ActionM ()
apiSearchName getData = do
  (_, (exactAll, other)) <- getData
  let getName = T.toLower . fromTargetName
      names = S.fromList (map (\(a, _, _) -> getName a) exactAll)
      otherFilt = filter (\n -> getName n `S.notMember` names) other

      t1 = map (\(n, ra, dec) -> object ["name" .= n, "ra" .= fromRA ra, "dec" .= fromDec dec]) exactAll
      t2 = map (\n -> object ["name" .= n]) otherFilt

  json (t1 ++ t2)


apiSearchProposal ::
  ActionM (String, [(T.Text, PropNum)])
  -> ActionM ()
apiSearchProposal getData = do
  (_, matches) <- getData
  -- for now, explicitly convert the PropNum field to an integer
  -- for easy serialization, but maybe this should be the default
  -- ToJSON serialization?
  let out = fmap conv matches
      conv (title, pnum) = object [ "title" .= title
                                  , "number" .= fromPropNum pnum ]
  json out


apiMappings ::
  ActionM UTCTime
  -> ActionM (M.Map (SIMCategory, SIMKey) (TimeKS, NumSrc, NumObs), UTCTime)
  -> ActionM ()
apiMappings getLastMod getData =
  let toETag = makeETag gitCommitId "/api/mappings"

      getData' = do
        (mapping, lastMod) <- getData

        let names = M.keys mapping
            propNames = nub (map fst names)
            simKeys = nub (map (keyToPair . snd) names)
            simNames = map fst simKeys

            -- remove the object that would be created by the
            -- ToJSON instance of SimbadType
            toPair (k, v) = k .= fromSimbadType v
            symbols = map toPair simKeys
      
            -- Need to provide unique numeric identifiers than
            -- can be used to index into the 'nodes' array.
            --
            nprop = length propNames
            zero = 0 :: Int
            propMap = M.fromList (zip propNames [zero..])
            simMap = M.fromList (zip simNames [nprop, nprop+1..])
  
            makeName n = object [ "name" .= n ]
            getVal n m = fromJust (M.lookup n m)
      
            makeLink ((prop,skey), (texp, nsrc, nobs)) =
              let stype = fst (keyToPair skey)
              in object [ "source" .= getVal prop propMap
                        , "target" .= getVal stype simMap
                        , "totalExp" .= fromTimeKS texp
                        , "numSource" .= nsrc
                        , "numObs" .= nobs ]

            out = object [
              "nodes" .= map makeName (propNames ++ simNames)
              , "links" .= map makeLink (M.toList mapping)
              , "proposals" .= propNames
              , "simbadNames" .= simNames
              , "simbadMap" .= object symbols
              ]

        return (out, lastMod)

  in cacheApiQuery toETag getLastMod getData'
 

-- IF we remove the current time from the conversion of
-- the ScienceTimeline items (i.e. push this logic into the
-- javascript that receives this data) then we can cache
-- the data (based on the last-update time).
--
-- Unfortunately, the data is loaded with a direct query - e.g.
--    <link href="/api/timeline" type="application/json"
--          rel="exhibit-data" />
-- which means that I need to find out how to insert a shim layer
-- that post-processes this data.
--
-- Alternatively, we cache the data here, then post-process it
-- and send the converted data. This may be the best intermediate
-- approach.
--
-- Can we return a failure case if the timeline data is "empty",
-- so the exhibit page can say "hey, no data yet"?
--
apiTimeline ::
  ActionM TimelineCacheData
  -> ActionM ()
apiTimeline getData = do

  -- debug "/api/timeline"

  (stline, nstline, simbadMap, props) <- getData
  tNow <- liftIO getCurrentTime

  -- What information do we want - e.g. Simbad type?
  --
  let propMap = M.fromList (map (\p -> (propNum p, p)) props)
      fromSO = fromScienceObs propMap simbadMap tNow

      sitems = fmap fromSO stline
      nsitems = fmap fromNonScienceObs nstline

      -- need to convert SortedList StartTimeorder (Maybe (ChandraTime, Value))
      -- to remove the Maybe.
      --
      noMaybe :: SortedList f (Maybe a) -> SortedList f a
      noMaybe = unsafeToSL . catMaybes . fromSL

      -- Use the time value, already pulled out by from*Science,
      -- to merge the records. This saves having to query the
      -- JSON itself.
      --
      items = fmap snd (mergeSL fst
                        (noMaybe sitems)
                        (noMaybe nsitems))

  json (object ["items" .= fromSL items])


apiTimeline2 ::
  TimelineCacheData
  -> UTCTime
  -> ActionM ()
apiTimeline2 getData tNow = do

  -- debug "/api/timeline"

  hdrs <- headers
  let readHeader = flip lookup hdrs
      qryLastMod = readHeader "If-Modified-Since"
      qryETag = readHeader "If-None-Match"

      toETag = makeETag gitCommitId "/api/timeline"
  
  let lastMod = timeToRFC1123 tNow
      isNotModified = case qryETag of
                        Just e -> e == fromETag (toETag tNow)
                        Nothing -> Just lastMod == qryLastMod

      -- I was setting the cache headers here, but a bit pointless
      notModified = status notModified304

      modified = do

        -- (stline, nstline, simbadMap, props) <- getData
        -- tNow <- liftIO getCurrentTime

        let (stline, nstline, simbadMap, props) = getData

        -- What information do we want - e.g. Simbad type?
        --
        let propMap = M.fromList (map (\p -> (propNum p, p)) props)
            fromSO = fromScienceObs propMap simbadMap tNow

            sitems = fmap fromSO stline
            nsitems = fmap fromNonScienceObs nstline

            -- need to convert SortedList StartTimeorder (Maybe (ChandraTime, Value))
            -- to remove the Maybe.
            --
            noMaybe :: SortedList f (Maybe a) -> SortedList f a
            noMaybe = unsafeToSL . catMaybes . fromSL

            -- Use the time value, already pulled out by from*Science,
            -- to merge the records. This saves having to query the
            -- JSON itself.
            --
            items = fmap snd (mergeSL fst
                              (noMaybe sitems)
                              (noMaybe nsitems))

        -- TODO: change these times !!!
        setHeader "Cache-Control" "no-transform,public,max-age=300,s-maxage=900"
        setHeader "Vary" "Accept-Encoding"
        setCacheHeaders toETag tNow
        json (object ["items" .= fromSL items])

  if isNotModified then notModified else modified


apiExposures ::
  ActionM UTCTime
  -> ActionM [(T.Text, SortedList ExposureTimeOrder TimeKS)]
  -> ActionM ()
apiExposures getLastMod getData =
  let toETag = makeETag gitCommitId "/api/exposures"

      getData' = do
        -- TODO: exposure query should return the last-modified date
        pairs <- getData
        lastMod <- getLastMod

        let toPair (cyc, vals) =
              let ts = map fromTimeKS (fromSL vals)
                  allTime = showExpTime (unsafeToTimeKS (sum ts))
              in cyc .= object
                 [ "cycle" .= cyc
                 , "units" .= ("ks" :: T.Text)
                 , "length" .= length ts
                 , "totalTime" .= allTime
                 , "times" .= ts
                 ]

            out = object (map toPair pairs)

        return (out, lastMod)

  in cacheApiQuery toETag getLastMod getData'


obsidOnly ::
  ActionM (Maybe ObsInfo)
  -> ActionM (Maybe Record)
  -> (ObsInfo -> ActionM
      (Maybe SimbadInfo, 
       (Maybe Proposal, SortedList StartTimeOrder ScienceObs)))
  -> ActionM ()
obsidOnly getData getObs getDB = do
  mobs <- getData
  case mobs of
    Just obs -> do
      cTime <- liftIO getCurrentTime
      mCurrent <- getObs
      dbInfo <- getDB obs
      fromBlaze (Record.recordPage cTime mCurrent obs dbInfo)
    _ -> liftIO getFact >>= fromBlaze . Index.noObsIdPage


proposal ::
  ActionM (Maybe Proposal, Maybe ProposalAbstract, SortedList StartTimeOrder RestrictedSO)
  -> (SortedList StartTimeOrder RestrictedRecord -> ActionM RestrictedSchedule)
  -> ActionM ()
proposal getData getSched = do
  (mprop, mabs, matches) <- getData
  case mprop of
    Just prop -> do
      sched <- getSched (fmap Right matches)
      fromBlaze (Proposal.matchPage prop mabs sched)
    _         -> next -- status notFound404


searchTypeUnId ::
  ActionM (SimbadTypeInfo, SortedList StartTimeOrder RestrictedSO)
  -> (SortedList StartTimeOrder RestrictedRecord -> ActionM RestrictedSchedule)
  -> ActionM ()
searchTypeUnId getData getSched = do
  (typeInfo, ms) <- getData
  sched <- getSched (fmap Right ms)
  fromBlaze (SearchTypes.matchPage typeInfo sched)
      

searchType ::
  ActionM (Maybe (SimbadTypeInfo, SortedList StartTimeOrder RestrictedSO))
  -> (SortedList StartTimeOrder RestrictedRecord -> ActionM RestrictedSchedule)
  -> ActionM ()
searchType getData getSched = do
  matches <- getData
  case matches of
    Just (typeInfo, ms) -> do
      sched <- getSched (fmap Right ms)
      fromBlaze (SearchTypes.matchPage typeInfo sched)
          
    _ -> next -- status notFound404
  

searchTypeNone :: ActionM [(SimbadTypeInfo, Int)] -> ActionM ()
searchTypeNone getData = getData >>= fromBlaze . SearchTypes.indexPage


searchDTypeNone :: ActionM [(SimbadTypeInfo, Int)] -> ActionM ()
searchDTypeNone getData = getData >>= fromBlaze . SearchTypes.dependencyPage

                         
-- Conversion routines for the timeline API.
--

-- | Convert to hours.
--
--   Perhaps should return (number of hours, number of minutes)?
--   The problem is that this is used to generate the "duration"
--   filter in the timeline view, and this needs a number.
--   So, can the conversion be done in js (I'd rather not send
--   both a number and a text label in the JSON if I can help it).
--
toHours :: TimeKS -> Double
toHours ks = fromTimeKS ks / 3.6

{-
toHours ks =
  let h = fromTimeKS ks / 3.6
      h10 = round (h * 10)
  in fromInteger h10 / 10
-}     

-- | Assumes TimeKS >= 0.
--
toHoursLabel :: TimeKS -> T.Text
toHoursLabel ks =
  let th = toHours ks
      nd, nh, nm :: Int
      nd = floor th `div` 24
      nh = floor (th - fromIntegral (nd * 24))

      -- don't need this much gymnastics
      nm = round (60 * (th - fromIntegral (floor th :: Int)))

      plural x = if x > 1 then "s" else ""
      u x unit = if x > 0
                 then Just (showInt x <> " " <> unit <> plural x)
                 else Nothing

      lbls = catMaybes [u nd "day", u nh "hour", u nm "minute"]
      
  in case lbls of
    [t1, t2, t3] -> t1 <> ", " <> t2 <> ", and " <> t3
    [t1, t2] -> t1 <> " and " <> t2
    [t1] -> t1
    _ -> "unknown"
  

-- | Convert a science observation into a JSON dictionary,
--   using the "Science" schema for the Exhibit timeline.
--
--   TODO: add "is joint with" and "simultaneous with" fields
--
fromScienceObs ::
  M.Map PropNum Proposal
  -- ^ The known proposals, used to enrich the ScienceObs values
  --   with extra information.
  -> M.Map TargetName SimbadInfo
  -- ^ The known targets with SIMBAD information; note that multiple
  --   target names can map to the same SIMBAD object.
  -> UTCTime
  -- ^ The current time (used to determine if an observation is
  --   now public).
  -> ScienceTimeline
  -- ^ The observation to convert
  -> Maybe (ChandraTime, Value)
  -- ^ The start time of the observation and a JSON dictionary
  --   following the Exhibit schema for the Science type.
  --   The result will be Nothing if the observation has no
  --   start time.
fromScienceObs propMap simbadMap tNow so =
  let (obsidVal, propnum, targetName, mStartTime, mPublicDate,
       approvedTime, mObservedTime, instrument, grating,
       mDataMode, mTOO, con) = so

      obsid = fromObsId obsidVal

      -- The SIMBAD info could be stored separately, and use cross-linking,
      -- but for now just encode all the infomation we want in the science
      -- target structure.
      --
      msimbad = case M.lookup targetName simbadMap of
        Just SimbadInfo {..} ->
          ["aka" .= smiName | smiName /= targetName] ++
          ["simbadType" .= smiType, "simbadCode" .= fromSimbadType smiType3]
        _ -> []

      isBool :: Bool -> T.Text
      isBool True = "yes"
      isBool _ = "no"

      -- Note, this extends isPublic with a check to see if this is
      -- CC-mode data. This means that the isPublic check has been
      -- repeated.
      --
      -- NOTE: isPublic/Viewable should be added by JS in the
      --       page itself (so can cache the JSON)
      --
      isViewable = isChandraImageViewable mPublicDate
                   mDataMode instrument tNow

      -- Isn't this the logic of the Ord typeclass for Maybe?
      isPublic = case mPublicDate of
        Just pDate -> pDate < tNow
        Nothing -> False

      -- It would be good not to encode this along with isPublic,
      -- but I dont' see a sensible way of encoding the URL
      -- from within Exhibit (the 0-padded obsid value)
      -- without help from here. I guess could have a "label-ified"
      -- version of the obsid field
      --
      imgURL = publicImageURL obsidVal instrument

      -- TODO: do not include the item if the value is
      --       not known
      fromProp :: (Proposal -> T.Text) -> T.Text
      fromProp f = maybe "unknown" f (M.lookup propnum propMap)

      obsLen = fromMaybe approvedTime mObservedTime
    
      objs startTime endTime = [
        "type" .= ("Science" :: T.Text),
        -- need a unique label
        "label" .=
        (fromTargetName targetName <> " - ObsId " <> showInt obsid),
        "object" .= targetName,
        "obsid" .= obsid,
        "start" .= fromChandraTime startTime,
        "end" .= fromChandraTime endTime,
      
        -- includling isPublic means that the data can't
        -- be easily cached *OR* would have to identify
        -- the time until the next obsid is public -- which
        -- need not be the next item in the time-ordered
        -- list -- and then use that as the cache-until
        -- date
        "isPublic" .= isBool isPublic,
    
        -- observation length, in hours; since I can't work out a simple
        -- way to get exhibit to do the time calculation (when creating
        -- a lens), do it here (which is wasteful).
        "length" .= toHours obsLen,
        "lengthLabel" .= toHoursLabel obsLen,
      
        "instrument" .= fromInstrument instrument,
        "grating" .= fromGrating grating,
        "isTOO" .= isBool (isJust mTOO),
        "constellation" .= getConstellationNameStr con,
      
        "cycle" .= fromProp propCycle,
        "category" .= fromProp propCategory,
        "proptype" .= fromProp propType
    
        ]
      
      go (startTime, endTime) = 
        (startTime, object (objs startTime endTime
                            ++ ["imgURL" .= imgURL | isViewable]
                            ++ msimbad
                           ))

      -- based on Utils.getTimes
      getSTLTimes = 
        case mStartTime of
          Just t -> let sTime = fromChandraTime t

                        tlen = fromMaybe approvedTime mObservedTime
                        expTime = (fromInteger . ceiling) (1000 * fromTimeKS tlen)
                        
                        eTime = addUTCTime expTime sTime
                    in Just (toChandraTime sTime, toChandraTime eTime)
          Nothing -> Nothing
      
  in go <$> getSTLTimes
     


-- | Convert a non-science observation into a JSON dictionary,
--   using the "Engineering" schema for the Exhibit timeline.
--
fromNonScienceObs ::
  EngineeringTimeline
  -- ^ The observation to convert
  -> Maybe (ChandraTime, Value)
  -- ^ The start time of the observation and a JSON dictionary
  --   following the Exhibit schema for the NonScience type.
fromNonScienceObs ns =
  let (obsidVal, targetName, mStartTime, obsLen) = ns

      obsid = fromObsId obsidVal

      -- only include end time if > start time
      objs startTime endTime = [
        "type" .= ("Engineering" :: T.Text),
        -- need a unique label
        "label" .= targetName,
        "obsid" .= obsid,
        "start" .= fromChandraTime startTime,
        -- "end" .= fromChandraTime endTime,
        
        -- observation length, in hours
        "length" .= toHours obsLen
      
        ] ++ ["end" .= fromChandraTime endTime | endTime > startTime]
                 
      go (startTime, endTime) =
        (startTime, object (objs startTime endTime))

      -- based on Utils.getTimes
      getETLTimes =
        case mStartTime of
          Just t -> let sTime = fromChandraTime t

                        expTime = (fromInteger . ceiling) (1000 * fromTimeKS obsLen)
                        
                        eTime = addUTCTime expTime sTime
                    in Just (toChandraTime sTime, toChandraTime eTime)
          Nothing -> Nothing
        
  in go <$> getETLTimes

-- cache code

-- | Return a schedule for a query that only returns
--   science observations (e.g. ACIS-I observations).
--
fetchSchedule ::
  (PersistBackend m, SqlDb (Conn m))
  => m (SortedList StartTimeOrder RestrictedSO)
  -> m RestrictedSchedule
fetchSchedule act = do
  matches <- act
  -- note: there is no check for no matches
  makeScheduleRestricted (fmap Right matches)


-- Convert a restricted science observation to an object; very-limited
-- at the moment.
--
rsoToJSON :: RestrictedSO -> Value
rsoToJSON rso =
  object [ "obsid" .= fromObsId (rsoObsId rso)
         , "target" .= rsoTarget rso
         , "ra" .= fromRA (rsoRA rso)
         , "dec" .= fromDec (rsoDec rso)
         , "expks" .= fromTimeKS (rsoExposureTime rso)
         ]
  
-- Technically not converted to JSON, but to something that can be
-- converted to JSON
rsoListToJSON :: SortedList a RestrictedSO -> [Value]
rsoListToJSON = map rsoToJSON . fromSL
