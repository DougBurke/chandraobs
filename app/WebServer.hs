{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# Language RecordWildCards #-}

-- TODO:
--   provide cache information for JSON responses; could have a last-updated
--   field in the database which is used to seed the last-Modified header,
--   as a simple case
--

-- | A test webserver.
-- 
module Main where

import qualified Data.ByteString.Base64.Lazy as B64
-- import qualified Data.Conduit as C
-- import qualified Data.Conduit.Combinators as CC

import qualified Data.Map.Strict as M

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Conduit as NHC

import qualified Views.Index as Index
import qualified Views.NotFound as NotFound
import qualified Views.Proposal as Proposal
import qualified Views.Record as Record
import qualified Views.Search.Calendar as Calendar
import qualified Views.Search.Category as Category
import qualified Views.Search.Constellation as Constellation
import qualified Views.Search.Cycle as Cycle
import qualified Views.Search.Constraint as Constraint
import qualified Views.Search.Instrument as Instrument
import qualified Views.Search.Mapping as Mapping
import qualified Views.Search.Mission as Mission
import qualified Views.Search.PropType as PropType
import qualified Views.Search.Target as Target
import qualified Views.Search.TOO as TOO
import qualified Views.Search.Types as SearchTypes
import qualified Views.Schedule as Schedule

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Aeson(Value, (.=), object)
import Data.Default (def)
import Data.List (nub)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import Data.Monoid ((<>))
import Data.Pool (Pool)
import Data.Time (UTCTime(utctDay), addDays, getCurrentTime)

import Database.Groundhog.Postgresql (Postgresql(..)
                                     , Conn
                                     , PersistBackend
                                     , SqlDb
                                     , runDbConn
                                     , withPostgresqlPool)

-- import Network.HTTP.Date (HTTPDate, epochTimeToHTTPDate, formatHTTPDate)
import Network.HTTP.Types (StdMethod(HEAD)
                          , hLastModified
                          , status404, status503)
-- import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static (CacheContainer, CachingStrategy(PublicStaticCaching)
                                     , (>->)
                                     , addBase, initCaching, noDots, staticPolicy')
import Network.Wai.Handler.Warp (defaultSettings, setPort)

import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hFlush, stderr)

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Read (readMaybe)

import Web.Heroku (dbConnParams)
import Web.Scotty

import Database (NumObs, NumSrc, SIMKey
                , findRecord
                , getCurrentObs, getObsInfo
                , getObsId
                , getSchedule
                , getScheduleDate
                  
                , makeSchedule
                , makeScheduleRestricted
                  
                , getProposalInfo
                , getProposalFromNumber
                , getRelatedObs
                , getObsFromProposal
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
                   
                , dbConnStr
                )
-- import Git (gitCommitId)

import Layout (getFact, renderLinks)
import Types (Record, SimbadInfo(..), Proposal(..)
             , PropNum(..)
             , NonScienceObs(..), ScienceObs(..)
             , ObsInfo(..), ObsIdVal(..)
             -- , PropType(..)
             , Schedule
             , Sequence(..)
             , SIMCategory
             , SimbadTypeInfo
             , SortedList, StartTimeOrder, ExposureTimeOrder
             , TargetName(..)
             , TimeKS(..)
             , ChandraTime(..)
             , fromSimbadType
             , toSimbadType
             , nullSL, fromSL, mergeSL, unsafeToSL
             , showExpTime
             , handleMigration
             , labelToRT
             , labelToCS
             , getConstellationNameStr
             , fromInstrument
             , fromGrating
             , recordObsId
             )
import Utils (HtmlContext(..)
             , fromBlaze, standardResponse
             , timeToRFC1123
             , getTimes
             , showInt
             , isChandraImageViewable
             , publicImageURL
             -- , makeETag
             )

production :: 
  Int  -- ^ The port number to use
  -> Options
production p = def { verbose = 0
                   , settings = setPort p defaultSettings }

development :: Options
development = def

-- | @True@ for production/heroku, otherwise local
getDbConnStr :: Bool -> IO String
getDbConnStr False = return dbConnStr
getDbConnStr _ = do
  cparams <- dbConnParams
  return $ T.unpack $ foldr (\(k,v) s ->
                        s <> (k <> "=" <> v <> " ")) "" cparams

uerror :: T.Text -> IO ()
uerror msg = do
  T.hPutStrLn stderr ("ERROR: " <> msg)
  hFlush stderr
  exitFailure

-- I use the presence of the PORT environment variable to decide
-- between production/heroku and test environments. This is for
-- *both* the port to run Scotty on and the database to use, so
--
--   env var PORT exists: use this port number and use DATABASE_URL
--     env var for database (assumed to be on Heroku)
--
--   otherwise, run on port=3000 and use the local postgresql instance
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

  connStr <- getDbConnStr (isJust mports)
 
  case eopts of
    Left emsg -> uerror emsg
    Right opts -> 
      -- TODO: what is a sensible number for the pool size?
        {-
      withPostgresqlPool connStr 5 $ 
        scottyOpts opts . webapp
        -}

        -- Given that the manager is meant to exist for as long
        -- as the application, there seems no need to use
        -- withManager
        do
          mgr <- NHC.newManager Client.defaultManagerSettings
          scache <- initCaching PublicStaticCaching
          withPostgresqlPool connStr 5 $ \pool -> do
            runDbConn handleMigration pool
            scottyOpts opts (webapp pool mgr scache)

-- Hack; needs cleaning up
getDBInfo :: 
  (MonadIO m, PersistBackend m, SqlDb (Conn m)) 
  => Record 
  -> m (Maybe SimbadInfo, (Maybe Proposal, SortedList StartTimeOrder ScienceObs))
getDBInfo r = do
  as <- either (const (return Nothing)) (getSimbadInfo . soTarget) r
  bs <- getProposalInfo r
  return (as, bs)

webapp ::
    Pool Postgresql
    -> NHC.Manager
    -> CacheContainer
    -> ScottyM ()
webapp cm mgr scache = do
    let liftSQL a = liftAndCatchIO (runDbConn a cm)

    defaultHandler errHandle

    -- Need to find out how the static directory gets copied
    -- over by cabal; seems to be okay
    --
    -- middleware logStdoutDev
    -- middleware (staticPolicy (noDots >-> addBase "static"))
    middleware (staticPolicy' scache (noDots >-> addBase "static"))

    -- proxy requests to the DSS/RASS/PSPC images so we can
    -- access them via AJAX. *experimental*
    --
    let proxyImg imgType = do
          seqVal <- param "sequence"
          obsid <- param "obsid"
          proxy mgr imgType seqVal obsid
          
    get "/proxy/dss/:sequence/:obsid" (proxyImg PTDSS)
    get "/proxy/rass/:sequence/:obsid" (proxyImg PTRASS)
    get "/proxy/pspc/:sequence/:obsid" (proxyImg PTPSPC)

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
        queryObsidParam = dbQuery "obsid" (getObsId . ObsIdVal)

    -- for now always return JSON; need a better success/failure
    -- set up.
    --
    -- the amount of information returned by getObsId is
    -- excessive here; probably just need the preceeding and
    -- next obsid values (if any), but leave as is for now.
    --
    get "/api/current" (apiCurrent (liftSQL getCurrentObs))

    -- TODO: this is completely experimental
    --
    get "/api/page/:obsid" $ do
      obsid <- param "obsid"

      cTime <- liftIO getCurrentTime
      dbans <- liftSQL (do
                           a <- getObsId obsid
                           -- do I need all of findRecord
                           b <- findRecord cTime
                           c <- case a of
                             Just o -> Just <$> getDBInfo (oiCurrentObs o)
                             Nothing -> return Nothing
                           return (a, b, c))

      let (mobs, mCurrent, mDbInfo) = dbans
          mCurrentObsId = recordObsId <$> mCurrent
      
      jsobj <- case (mobs, mDbInfo) of
        (Just obs, Just dbInfo) -> do
          let thisObs = oiCurrentObs obs
              -- oiCurrentObs is badly named; it is really the
              -- "focus" observation.
              --
              obshtml = Record.renderStuff DynamicHtml cTime thisObs dbInfo
              (msimbad, (mprop, _)) = dbInfo

              imgLinks = either
                         (const mempty)
                         (renderLinks cTime mprop msimbad)
                         thisObs

              navBar = Record.obsNavBar DynamicHtml (Just thisObs) obs

          return (object ["status" .= ("success" :: T.Text)
                         , "observation" .= renderHtml obshtml
                         , "imglinks" .= renderHtml imgLinks
                         , "navbar" .= renderHtml navBar
                         , "isCurrent" .= (mCurrentObsId == Just obsid)
                         ])
      
        _  -> do
          fact <- liftIO getFact
          let nohtml = Index.noDataDiv fact
          return (object ["status" .= ("error" :: T.Text)
                         , "error" .= renderHtml nohtml])

      json jsobj
      
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
    get "/api/mappings" (apiMappings (liftSQL getProposalObjectMapping))

    -- HIGHLY EXPERIMENTAL: explore a timeline visualization
    --
    get "/api/timeline" (apiTimeline (liftSQL getTimeline))

    -- highly experimental
    get "/api/exposures" (apiExposures (liftSQL getExposureValues))
    
    get "/" (redirect "/index.html")
    get "/about.html" (redirect "/about/index.html")
    get "/about" (redirect "/about/index.html")

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
      mobs <- liftSQL getObsInfo
      cTime <- liftIO getCurrentTime
      case mobs of
        Just obs -> do
          dbInfo <- liftSQL (getDBInfo (oiCurrentObs obs))
          fromBlaze (Index.introPage cTime obs dbInfo)
        _  -> liftIO getFact >>= fromBlaze . Index.noDataPage

    get "/obsid/:obsid" (obsidOnly
                         (snd <$> queryObsidParam)
                         (liftSQL getCurrentObs)
                         (liftSQL . getDBInfo . oiCurrentObs)
                        )

    -- TODO: head requests
    get "/proposal/:propnum" (proposal
                              (snd <$> dbQuery "propnum" fetchProposal)
                              (liftSQL .makeSchedule))
      
    let querySchedule n = do
          sched <- liftSQL (getSchedule n)
          fromBlaze (Schedule.schedPage sched)
          
        queryScheduleDate date n = do
          sched <- liftSQL (getScheduleDate date n)
          fromBlaze (Schedule.schedDatePage date sched)

        queryScheduleTime name mul = do
          val <- param name
          when (val <= 0) next  -- TODO: better error message
          querySchedule (mul * val)
          
    get "/schedule" (redirect "/schedule/index.html")
    get "/schedule/index.html" (querySchedule 3)
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
    -- This returns only those observations that match this
    -- type; contrast with /seatch/dtype/:type
    --
    get "/search/type/unidentified" (searchTypeUnId
                                     (liftSQL fetchNoSIMBADType)
                                     (liftSQL . makeSchedule))

    get "/search/type/:type" (searchType
                              (snd <$> dbQuery "type" fetchSIMBADType)
                              (liftSQL . makeSchedule))

    -- TODO: also need a HEAD request version
    get "/search/type/" (searchTypeNone (liftSQL fetchObjectTypes))

    -- TODO: also need a HEAD request version
    --     FOR TESTING
    get "/search/dtype/" (searchDTypeNone (liftSQL fetchObjectTypes))

    let searchResults getData isNull page = do
          (xs, matches) <- getData
          when (nullSL matches || isNull xs) next
          sched <- liftSQL (makeSchedule (fmap Right matches))
          fromBlaze (page xs sched)

    let searchResultsRestricted getData isNull page = do
          (xs, matches) <- getData
          when (nullSL matches || isNull xs) next
          sched <- liftSQL (makeScheduleRestricted (fmap Right matches))
          fromBlaze (page xs sched)

    let maybeSearchResults mval getData page =
          case mval of
            Just val -> do
              matches <- getData val
              when (nullSL matches) next
              sched <- liftSQL (makeSchedule (fmap Right matches))
              fromBlaze (page val sched)
            Nothing -> next
            
    let maybeSearchResultsRestricted mval getData page =
          case mval of
            Just val -> do
              matches <- getData val
              when (nullSL matches) next
              sched <- liftSQL (makeScheduleRestricted (fmap Right matches))
              fromBlaze (page val sched)
            Nothing -> next
            
    -- This returns those observations that match this
    -- type and any "sub types"; contrast with /seatch/type/:type
    -- TODO: also need a HEAD request version
    get "/search/dtype/:type"
      (searchResults (snd <$> dbQuery "type" fetchSIMBADDescendentTypes)
       null SearchTypes.matchDependencyPage)

    -- TODO: also need a HEAD request version
    get "/search/constellation/:constellation"
      (searchResults (dbQuery "constellation" fetchConstellation)
       (const False) Constellation.matchPage)
    
    -- TODO: also need a HEAD request version
    get "/search/constellation/"
      (liftSQL fetchConstellationTypes >>= fromBlaze . Constellation.indexPage)
          

    -- TODO: also need a HEAD request version
    get "/search/turnaround/:too" $ do
      -- as I do not have a "none" type in TOORequestTime, parse
      -- this parameter as a string rather than as a TOORequestTime
      tooParam <- param "too"
      let mans = if T.toLower tooParam == "none"
                 then Just Nothing
                 else case labelToRT tooParam of
                        Nothing -> Nothing
                        a -> Just a

      maybeSearchResults mans (liftSQL . fetchTOO) TOO.matchPage
        
    -- TODO: also need a HEAD request version
    get "/search/turnaround/" $ do
      (matches, noneTime) <- liftSQL fetchTOOs
      fromBlaze (TOO.indexPage matches noneTime)

    -- TODO: also need a HEAD request version
    get "/search/cycle/:cycle"
      (searchResults (dbQuery "cycle" fetchCycle)
       (const False) Cycle.matchPage)

    get "/search/cycle/" $ do
      cycles <- liftSQL fetchCycles
      fromBlaze (Cycle.indexPage cycles)

    -- TODO: also need a HEAD request version
    get "/search/constraints/:cs" $ do
      -- as I do not have a "none" type in ConstraintKind, parse
      -- this parameter as a string rather than as a ConstraintKind
      csParam <- param "cs"
      let mans = if T.toLower csParam == "none"
                 then Just Nothing
                 else case labelToCS csParam of
                        Nothing -> Nothing
                        a -> Just a
                      
      maybeSearchResults mans (liftSQL . fetchConstraint) Constraint.matchPage
      
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

      maybeSearchResultsRestricted mtype (liftSQL . fetchCategorySubType cat)
        (Category.categoryAndTypePage cat)
      
    get "/search/category/:category"
      (searchResultsRestricted (dbQuery "category" fetchCategory)
       (const False) Category.matchPageRestricted)

    -- TODO: also need a HEAD request version
    get "/search/category/" $ do
      matches <- liftSQL fetchCategoryTypes
      fromBlaze (Category.indexPage matches)

    -- TODO: also need a HEAD request version
    get "/search/instrument/:instrument"
      (searchResultsRestricted
       (dbQuery "instrument" fetchInstrument) (const False)
       Instrument.matchInstPage)

    -- TODO: also need a HEAD request version
    get "/search/grating/:grating"
      (searchResultsRestricted
       (dbQuery "grating" fetchGrating) (const False)
       Instrument.matchGratPage)
    
    -- TODO: also need a HEAD request version
    get "/search/instgrat/:ig"
      (searchResultsRestricted
       (dbQuery "ig" fetchIG) (const False)
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

    get "/search/name" $ do
      (target, (matches, matchNames)) <- dbQuery "target" findTarget
      -- TODO: set an error code if no match? Once have sorted out search info
      if nullSL matches
        then fromBlaze (Target.noMatchPage target)
        else do
          sched <- liftSQL (makeSchedule (fmap Right matches))
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

    get "/search/proptype/:proptype"
      (searchResults (dbQuery "proptype" getProposalType) (const False)
       PropType.matchPage)
    
    -- map between proposal category and SIMBAD object types.
    get "/search/mappings" (fromBlaze Mapping.indexPage)

    get "/search/joint/:mission"
      (searchResults (dbQuery "mission" fetchJointMission) (const False)
       Mission.matchPage)
    
    get "/search/joint/"
      (liftSQL fetchMissionInfo >>= fromBlaze . Mission.indexPage)

    -- TODO: also need a HEAD request version
    get "/search/" (redirect "/search/index.html")

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
        _      -> next -- status status404

    -- TODO: is this actually correct?
    addroute HEAD "/obsid/:obsid/wwt" (redirectObsid)

    {-
    get "/404" $ redirect "/404.html"
    get "/404.html" $ do
      fact <- liftIO getFact
      fromBlaze $ NotFound.notFoundPage fact
      status status404

    notFound $ redirect "/404.html"
    -}

    notFound $ do
      fact <- liftIO getFact
      status status404
      fromBlaze (NotFound.notFoundPage fact)

-- | Exception handler. We should log the error.
errHandle :: L.Text -> ActionM ()
errHandle txt = do
  liftIO (L.putStrLn ("Error string: " <> txt))
  -- Can we change the HTTP status code? The following does not seem to
  -- work.
  status status503
  fromBlaze NotFound.errPage


-- TODO: define a data type to represent the JSON response, so that
--       there's a "defined" success/error reporting structure.
--
apiCurrent :: ActionM (Maybe Record) -> ActionM ()
apiCurrent getData = do
  -- note: this is creating/throwing away a bunch of info that could be useful
  mrec <- getData
  let rval o = json ("Success" :: T.Text, fromObsId o)
  case mrec of
    Just (Left ns) -> rval (nsObsId ns)
    Just (Right so) -> rval (soObsId so)
    _ -> json ("Failed" :: T.Text)


apiObsId :: ActionM (Int, Maybe ObsInfo) -> ActionM ()
apiObsId getData = do
  (obsid, mobs) <- getData
  case mobs of
    Just v -> json ("Success" :: T.Text, v)
    _ -> json ("Unknown ObsId" :: T.Text, obsid)


apiSimbadName :: ActionM (TargetName, Maybe SimbadInfo) -> ActionM ()
apiSimbadName getData = do
  (name, msim) <- getData
  case msim of
    Just sim -> json ("Success" :: T.Text, sim)
    _ -> json ("Unknown Target" :: T.Text, name)


apiProposal :: ActionM (PropNum, Maybe Proposal) -> ActionM ()
apiProposal getData = do
  (propNum, mres) <- getData
  case mres of
    Just res -> json ("Success" :: T.Text, res)
    _ -> json ("Unknown Proposal Number" :: T.Text, propNum)


apiRelatedPropNumObsId ::
  ActionM (SortedList StartTimeOrder ScienceObs)
  -> ActionM ()
apiRelatedPropNumObsId getData = do
  res <- getData 
  -- hmmm, can't tell between an unknown propnum/obsid
  -- pair and an observation with no related observations.
  json ("Success" :: T.Text, fromSL res)


apiRelatedPropNum ::
  ActionM (PropNum, SortedList StartTimeOrder ScienceObs)
  -> ActionM ()
apiRelatedPropNum getData = do     
  res <- snd <$> getData
  -- hmmm, can't tell between an unknown propnum
  -- and an observation with no related observations.
  json ("Success" :: T.Text, fromSL res)


apiSearchDtype :: ActionM [(SimbadTypeInfo, Int)] -> ActionM ()
apiSearchDtype getData = do
  matches <- getData
  json (SearchTypes.renderDependencyJSON matches)


apiSearchName ::
  ActionM (String, ([TargetName], [TargetName]))
  -> ActionM ()
apiSearchName getData = do
  (_, (exact, other)) <- getData
  -- for now, flatten out the response
  -- TODO: should also remove excess spaces, but this requires some
  --       thought on how the search functionality should work
  json (nub (exact ++ other))


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
                                  , "number" .= _unPropNum pnum ]
  json out


apiMappings ::
  ActionM (M.Map (SIMCategory, SIMKey) (TimeKS, NumSrc, NumObs), UTCTime)
  -> ActionM ()
apiMappings getData = do
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
                  , "totalExp" .= _toKS texp
                  , "numSource" .= nsrc
                  , "numObs" .= nobs ]

      out = object [
        "nodes" .= map makeName (propNames ++ simNames)
        , "links" .= map makeLink (M.toList mapping)
        , "proposals" .= propNames
        , "simbadNames" .= simNames
        , "simbadMap" .= object symbols
        ]

  -- If the code is not changed then the last-modified date
  -- of the database is sufficient. However, there's no guarantee
  -- that there isn't a change in the serialization code (e.g. above
  -- or at a lower level, such as a type or type class), so I
  -- should use an ETag. This also helps to handle the case where
  -- the database is updated within a second (which, given the
  -- current set up, is only an issue for the test server since
  -- the "production" one on Heroku is not updated in the
  -- same manner).
  --
  -- However, initial testing suggests that setting the ETag
  -- doesn't work, whereas Last-Modified does. So for now go
  -- with the working-but-technically-broken version.
  --
  -- Should I add some middleware that handles these checks for
  -- these resources?
  setHeader "Last-Modified" (timeToRFC1123 lastMod)
  -- setHeader "ETag" (makeETag gitCommitId "/api/mappings" lastMod)
  
  json out
 

apiTimeline ::
  ActionM (SortedList StartTimeOrder ScienceObs,
           SortedList StartTimeOrder NonScienceObs,
           M.Map TargetName SimbadInfo,
           [Proposal])
  -> ActionM ()
apiTimeline getData = do

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
      
      --
      -- Use the time value, already pulled out by from*Science,
      -- to merge the records. This saves having to query the
      -- JSON itself.
      --
      items = fmap snd (mergeSL fst
                        (noMaybe sitems)
                        (noMaybe nsitems))
      
  -- As we keep changing the structure of the JSON, using the
  -- last-modified date in the header does not work well (although
  -- it's use does at least validate that the caching was doing
  -- something). The inclusion of the isPublic field complicates
  -- the cacheing, so turn it off for now.
  --
  -- setHeader "Last-Modified" (timeToRFC1123 lastMod)
  -- setHeader "ETag" (makeETag gitCommitId "/api/timeline" lastMod)

  json (object ["items" .= fromSL items])


apiExposures ::
  ActionM [(T.Text, SortedList ExposureTimeOrder TimeKS)]
  -> ActionM ()
apiExposures getData = do
  pairs <- getData
  let toPair (cyc, vals) =
        let ts = map _toKS (fromSL vals)
            allTime = showExpTime (TimeKS (sum ts))
        in 
          cyc .= object
          [ "cycle" .= cyc
          , "units" .= ("ks" :: T.Text)
          , "length" .= length ts
          , "totalTime" .= allTime
          , "times" .= ts
          ]

      out = object (map toPair pairs)
  json out


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
  ActionM (Maybe Proposal, SortedList StartTimeOrder ScienceObs)
  -> (SortedList StartTimeOrder Record -> ActionM Schedule)
  -> ActionM ()
proposal getData getSched = do
  (mprop, matches) <- getData
  case mprop of
    Just prop -> do
      sched <- getSched (fmap Right matches)
      fromBlaze (Proposal.matchPage prop sched)
    _         -> next -- status status404


searchTypeUnId ::
  ActionM (SimbadTypeInfo, SortedList StartTimeOrder ScienceObs)
  -> (SortedList StartTimeOrder Record -> ActionM Schedule)
  -> ActionM ()
searchTypeUnId getData getSched = do
  (typeInfo, ms) <- getData
  sched <- getSched (fmap Right ms)
  fromBlaze (SearchTypes.matchPage typeInfo sched)
      

searchType ::
  ActionM (Maybe (SimbadTypeInfo, SortedList StartTimeOrder ScienceObs))
  -> (SortedList StartTimeOrder Record -> ActionM Schedule)
  -> ActionM ()
searchType getData getSched = do
  matches <- getData
  case matches of
    Just (typeInfo, ms) -> do
      sched <- getSched (fmap Right ms)
      fromBlaze (SearchTypes.matchPage typeInfo sched)
          
    _ -> next -- status status404
  

searchTypeNone :: ActionM [(SimbadTypeInfo, Int)] -> ActionM ()
searchTypeNone getData = getData >>= fromBlaze . SearchTypes.indexPage


searchDTypeNone :: ActionM [(SimbadTypeInfo, Int)] -> ActionM ()
searchDTypeNone getData = getData >>= fromBlaze . SearchTypes.dependencyPage

                         
-- TODO: move into a separate module once it works
-- TODO: should cache the http manager
-- TODO: use a streaming solution/base64 converter from
--       conduit-extra
--

data ProxyType = PTDSS | PTRASS | PTPSPC
               deriving Eq

instance Show ProxyType where
    show PTDSS  = "dss"
    show PTRASS = "rass"
    show PTPSPC = "pspc"

plainText :: L.Text
plainText = "text/plain; charset=utf-8"

{-   
proxy1 :: a -> ProxyType -> Sequence -> ObsIdVal -> ActionM ()
proxy1 _ pt seqVal obsid = do
  let seqStr = show $ _unSequence seqVal
      obsStr = show $ fromObsId obsid
      url = "http://asc.harvard.edu/targets/" ++
            seqStr ++ "/" ++ seqStr ++ "." ++
            obsStr ++ ".soe." ++ show pt ++ ".gif"
  -- request <- liftIO $ NHC.parseUrl url
  liftIO $ putStrLn $ "--> " ++ url
  bdy <- liftIO $ NHC.simpleHttp url
  liftIO $ putStrLn $ "<-- " ++ url
  setHeader "Content-Type" plainText
  raw $ B64.encode bdy

-}

-- TODO: handle possible errors 
proxy2 :: 
    NHC.Manager
    -> ProxyType
    -> Sequence 
    -> ObsIdVal 
    -> ActionM ()
proxy2 mgr pt seqVal obsid = do
  let seqStr = show (_unSequence seqVal)
      obsStr = show (fromObsId obsid)
      url = "http://asc.harvard.edu/targets/" 
            <> seqStr <> "/" <> seqStr <> "."
            <> obsStr <> ".soe." <> show pt <> ".gif"
            
  req <- liftIO (NHC.parseRequest url)
  
  -- liftIO $ putStrLn $ "--> " ++ url
  rsp <- liftIO (NHC.httpLbs req mgr)
  -- liftIO $ putStrLn $ "<-- " ++ url
  setHeader "Content-Type" plainText

  -- copy over etags/last-modified headers to see if that helps
  -- with the caching; if may have done.
  --
  -- Since the ETag header is opaque it should be okay
  -- to just copy it over, since the assumption is that the
  -- base64 encoding is not going to change. Except that it
  -- also depends on what my code does, so if there's ever
  -- any change here I might want to change the ETag,
  -- so perhaps there should be some addition to it.
  --
  let rhdrs = NHC.responseHeaders rsp
      mLastMod = cText <$> lookup hLastModified rhdrs
      mETag = cText <$> lookup "ETag" rhdrs
  
      cText =  L.fromStrict . TE.decodeUtf8

      runM f x = fromMaybe (return ()) (f <$> x)

  runM (setHeader "Last-Modified") mLastMod
  runM (setHeader "ETag") mETag

  raw (B64.encode (NHC.responseBody rsp))

{-

can we stream the base-64 encoding?

proxy3 :: 
    NHC.Manager
    -> ProxyType
    -> Sequence 
    -> ObsIdVal 
    -> ActionM ()
proxy3 mgr pt seqVal obsid = do
  let seqStr = show $ _unSequence seqVal
      obsStr = show $ fromObsId obsid
      url = "http://asc.harvard.edu/targets/" ++
            seqStr ++ "/" ++ seqStr ++ "." ++
            obsStr ++ ".soe." ++ show pt ++ ".gif"
  liftIO $ putStrLn $ "--> " ++ url
  req <- liftIO $ NHC.parseUrl url
  rsp <- liftIO $ NHC.http req mgr
  liftIO $ putStrLn $ "<-- " ++ url
  setHeader "Content-Type" plainText
  -- how to take advantage of stream, rather than raw,
  -- to allow a streaming solution?
  -- b64 <- liftIO (NHC.responseBody rsp C.$$+- CC.encodeBase64)

want to take the response, encode it, then convert it into a form that
I can use stream :: StreamingBody -> ActionM () with
  type StreamingBody = (Builder -> IO ()) -> IO () -> IO ()

  b64 <- liftIO (NHC.responseBody rsp C.$$+- CC.encodeBase64)
  raw b64

-}

proxy :: 
    NHC.Manager
    -> ProxyType
    -> Sequence 
    -> ObsIdVal 
    -> ActionM ()
proxy = proxy2


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
toHours ks = _toKS ks / 3.6

{-
toHours ks =
  let h = _toKS ks / 3.6
      h10 = round (h * 10)
  in fromInteger h10 / 10
-}     

-- | Assumes TimeKS >= 0.
--
toHoursLabel :: TimeKS -> T.Text
toHoursLabel ks =
  let th = toHours ks
      nd, nh, nm :: Int
      nd = (floor th) `div` 24
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
  -> ScienceObs
  -- ^ The observation to convert
  -> Maybe (ChandraTime, Value)
  -- ^ The start time of the observation and a JSON dictionary
  --   following the Exhibit schema for the Science type.
  --   The result will be Nothing if the observation has no
  --   start time.
  
fromScienceObs propMap simbadMap tNow so@ScienceObs {..} =
  let go (startTime, endTime) = 
        (startTime, object (objs
                            ++ [ "imgURL" .= imgURL | isViewable]
                            ++ msimbad
                           ))
        where
          obsid = fromObsId soObsId
    
          -- The SIMBAD info could be stored separately, and use cross-linking,
          -- but for now just encode all the infomation we want in the science
          -- target structure.
          --
          msimbad = case M.lookup soTarget simbadMap of
            Just SimbadInfo {..} ->
              ["aka" .= smiName | smiName /= soTarget] ++
              ["simbadType" .= smiType, "simbadCode" .= fromSimbadType smiType3]
            _ -> []

          isBool :: Bool -> T.Text
          isBool True = "yes"
          isBool _ = "no"

          -- Note, this extends isPublic with a check to see if this is
          -- CC-mode data. This means that the isPublic check has been
          -- repeated.
          --
          isViewable = isChandraImageViewable soPublicRelease
                       soDataMode soInstrument tNow

          -- Isn't this the logic of the Ord typeclass for Maybe?
          isPublic = case soPublicRelease of
            Just pDate -> pDate < tNow
            Nothing -> False

          -- It would be good not to encode this aling with isPublic,
          -- but I dont' see a sensible way of encoding the URL
          -- from within Exhibit (the 0-padded obsid value)
          -- without help from here. I guess could have a "label-ified"
          -- version of the obsid field
          --
          imgURL = publicImageURL soObsId soInstrument

          -- TODO: do not include the item if the value is
          --       not known
          fromProp :: (Proposal -> T.Text) -> T.Text
          fromProp f = fromMaybe "unknown"
                       (f <$> M.lookup soProposal propMap)

          obsLen = fromMaybe soApprovedTime soObservedTime
    
          objs = [
            "type" .= ("Science" :: T.Text),
            -- need a unique label
            "label" .=
            (fromTargetName soTarget <> " - ObsId " <> showInt obsid),
            "object" .= soTarget,
            "obsid" .= obsid,
            "start" .= _toUTCTime startTime,
            "end" .= _toUTCTime endTime,
      
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
      
            "instrument" .= fromInstrument soInstrument,
            "grating" .= fromGrating soGrating,
            "isTOO" .= isBool (isJust soTOO),
            "constellation" .= getConstellationNameStr soConstellation,
      
            "cycle" .= fromProp propCycle,
            "category" .= fromProp propCategory,
            "proptype" .= fromProp propType
    
            ]

  in go <$> getTimes (Right so) 


-- | Convert a non-science observation into a JSON dictionary,
--   using the "Engineering" schema for the Exhibit timeline.
--
fromNonScienceObs ::
  NonScienceObs
  -- ^ The observation to convert
  -> Maybe (ChandraTime, Value)
  -- ^ The start time of the observation and a JSON dictionary
  --   following the Exhibit schema for the NonScience type.
fromNonScienceObs ns@NonScienceObs {..} =
  let go (startTime, endTime) =
        (startTime, object objs)
        where
          obsid = fromObsId nsObsId

          -- only include end time if > start time
          objs = [
            "type" .= ("Engineering" :: T.Text),
            -- need a unique label
            "label" .= nsTarget,
            "obsid" .= obsid,
            "start" .= _toUTCTime startTime,
            -- "end" .= _toUTCTime endTime,
            
            -- observation length, in hours
            "length" .= toHours nsTime
      
            ] ++ ["end" .= _toUTCTime endTime | endTime > startTime]

  in go <$> getTimes (Left ns)
