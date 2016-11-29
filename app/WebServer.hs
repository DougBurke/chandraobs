{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# Language RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

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
import qualified Views.Search.Constraint as Constraint
import qualified Views.Search.Instrument as Instrument
import qualified Views.Search.Mapping as Mapping
import qualified Views.Search.Mission as Mission
import qualified Views.Search.PropType as PropType
import qualified Views.Search.Target as Target
import qualified Views.Search.TOO as TOO
import qualified Views.Search.Types as SearchTypes
import qualified Views.Schedule as Schedule
import qualified Views.WWT as WWT

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Control.Applicative ((<$>))
#endif

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Aeson(Value, (.=), object)
import Data.Default (def)
import Data.List (nub)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Monoid ((<>))
import Data.Pool (Pool)
import Data.Time (UTCTime(utctDay), addDays, getCurrentTime)

import Database.Groundhog.Postgresql (Postgresql(..)
                                     , PersistBackend
                                     , runDbConn
                                     , withPostgresqlPool)

import Formatting hiding (now)

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

import Web.Heroku (dbConnParams)
import Web.Scotty

import Database (NumObs, NumSrc, SIMKey
                 , getCurrentObs, getRecord, getObsInfo
                 , getObsId
                 , getSchedule
                 , getScheduleDate
                 , makeSchedule
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
import Types (Record, SimbadInfo(..), Proposal(..)
             , PropNum(..)
             , NonScienceObs(..), ScienceObs(..)
             , ObsInfo(..), ObsIdVal(..)
             -- , PropType(..)
             , Sequence(..)
             , SIMCategory
             , SortedList, StartTimeOrder, ExposureTimeOrder
             , TargetName
             , TimeKS(..)
             , ChandraTime(..)
             , Instrument(..)
             , fromSimbadType
             , toSimbadType
             , nullSL, fromSL, mergeSL
             , showExpTime
             , handleMigration
             , labelToRT
             , labelToCS
             , getConstellationNameStr
             , fromInstrument
             , fromGrating
             )
import Utils (fromBlaze, standardResponse, getFact
             , timeToRFC1123
             , getTimes
             , showInt
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
        Just ports -> case reads ports of
          [(port,[])] ->  Right (production port)
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
  (MonadIO m, PersistBackend m) 
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

        -- queryRecord :: ActionM (Maybe (Either NonScienceObs ScienceObs))
        queryRecord = snd <$> dbQuery "obsid" (getRecord . ObsIdVal)

    -- for now always return JSON; need a better success/failure
    -- set up.
    --
    -- the amount of information returned by getObsId is
    -- excessive here; probably just need the preceeding and
    -- next obsid values (if any), but leave as is for now.
    --
    get "/api/current" (apiCurrent (liftSQL getCurrentObs))

    get "/api/obsid/:obsid" (apiObsId queryObsidParam)

    -- break down the monolithic queries into separate ones, which may or may not
    -- be a good idea
    --
    -- Note that for Simbad names we may have a / in them, so we use
    -- a regex
    get (regex "^/api/simbad/name/(.+)$") $ do
      (name, msim) <- dbQuery "1" getSimbadInfo
      case msim of
        Just sim -> json ("Success" :: T.Text, sim)
        _ -> json ("Unknown Target" :: T.Text, name)

    get "/api/proposal/:propnum" (apiProposal
                                  (dbQuery "propnum" getProposalFromNumber))

    get "/api/related/:propnum/:obsid" $ do
      propNum <- param "propnum"
      obsid <- param "obsid"
      res <- liftSQL (getRelatedObs propNum (ObsIdVal obsid))
      -- hmmm, can't tell between an unknown propnum/obsid
      -- pair and an observation with no related observations.
      json ("Success" :: T.Text, fromSL res)

    get "/api/related/:propnum" $ do
      res <- snd <$> dbQuery "propnum" getObsFromProposal
      -- hmmm, can't tell between an unknown propnum
      -- and an observation with no related observations.
      json ("Success" :: T.Text, fromSL res)

    get "/api/search/dtype" $ do
      matches <- liftSQL fetchObjectTypes
      json (SearchTypes.renderDependencyJSON matches)

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

    get "/index.html" $ do
      mobs <- liftSQL getObsInfo
      cTime <- liftIO getCurrentTime
      case mobs of
        Just obs -> do
          dbInfo <- liftSQL (getDBInfo (oiCurrentObs obs))
          fromBlaze (Index.introPage cTime obs dbInfo)
        _  -> liftIO getFact >>= fromBlaze . Index.noDataPage

    -- TODO: send in proposal details
    get "/wwt.html" $ do
      mobs <- liftSQL getCurrentObs
      case mobs of 
        Just (Right so) -> fromBlaze (WWT.wwtPage True so)
        _ -> liftIO getFact >>= fromBlaze . Index.noDataPage
        
    get "/obsid/:obsid" $ do
      mobs <- snd <$> queryObsidParam
      case mobs of
        Just obs -> do
          cTime <- liftIO getCurrentTime
          mCurrent <- liftSQL getCurrentObs
          dbInfo <- liftSQL (getDBInfo (oiCurrentObs obs))
          fromBlaze (Record.recordPage cTime mCurrent obs dbInfo)
        _ -> liftIO getFact >>= fromBlaze . Index.noObsIdPage

    -- TODO: send in proposal details
    get "/obsid/:obsid/wwt" $ do
      mobs <- queryRecord
      case mobs of
        Just (Right so) -> fromBlaze (WWT.wwtPage False so)
        _               -> next -- status status404

    -- TODO: head requests
    get "/proposal/:propnum" $ do
      (mprop, matches) <- snd <$> dbQuery "propnum" fetchProposal
      case mprop of
        Just prop -> do
          sched <- liftSQL (makeSchedule (fmap Right matches))
          fromBlaze (Proposal.matchPage prop sched)
        _         -> next -- status status404

    let querySchedule n = do
          sched <- liftSQL (getSchedule n)
          fromBlaze (Schedule.schedPage sched)
          
        queryScheduleDate date n = do
          sched <- liftSQL (getScheduleDate date n)
          fromBlaze (Schedule.schedDatePage date sched)
          
    get "/schedule" (redirect "/schedule/index.html")
    get "/schedule/index.html" (querySchedule 3)
    get "/schedule/day" (querySchedule 1)
    get "/schedule/week" (querySchedule 7)

    get "/schedule/day/:ndays" $ do
      ndays <- param "ndays"
      when (ndays <= 0) next  -- TODO: better error message
      querySchedule ndays

    get "/schedule/week/:nweeks" $ do
      nweeks <- param "nweeks"
      when (nweeks <= 0) next  -- TODO: better error message
      querySchedule (7 * nweeks)

    -- allow the schedule to be centered on a date
    --
    -- TODO: when should the validity of the input date be checked?
    get "/schedule/date/:date/:ndays" $ do
      -- there is no Parsable instance for Date, so rather than have
      -- an orphan instance, deal with conversion manually
      dateText <- param "date"
      ndays <- param "ndays"
      when (ndays <= 0) next  -- TODO: better error message
      case readEither dateText of
        Left _ -> next -- TODO: better error message
        Right date -> queryScheduleDate date ndays

    -- TODO: also need a HEAD request version
    -- This returns only those observations that match this
    -- type; contrast with /seatch/dtype/:type
    --
    --
    get "/search/type/unidentified" $ do
      (typeInfo, ms) <- liftSQL fetchNoSIMBADType
      sched <- liftSQL (makeSchedule (fmap Right ms))
      fromBlaze (SearchTypes.matchPage typeInfo sched)
      
    get "/search/type/:type" $ do
      matches <- snd <$> dbQuery "type" fetchSIMBADType
      case matches of
        Just (typeInfo, ms) -> do
          sched <- liftSQL (makeSchedule (fmap Right ms))
          fromBlaze (SearchTypes.matchPage typeInfo sched)
          
        _ -> next -- status status404
  
    -- TODO: also need a HEAD request version
    get "/search/type/" $ do
      matches <- liftSQL fetchObjectTypes
      fromBlaze (SearchTypes.indexPage matches)

    -- TODO: also need a HEAD request version
    --     FOR TESTING
    get "/search/dtype/" $ do
      matches <- liftSQL fetchObjectTypes
      fromBlaze (SearchTypes.dependencyPage matches)

    -- This returns those observations that match this
    -- type and any "sub types"; contrast with /seatch/type/:type
    -- TODO: also need a HEAD request version
    get "/search/dtype/:type" $ do
      (types, matches) <- snd <$> dbQuery "type" fetchSIMBADDescendentTypes
      if null types || nullSL matches
        then next -- status status404
        else do
          -- TODO: want a slightly different match page
          sched <- liftSQL (makeSchedule (fmap Right matches))
          fromBlaze (SearchTypes.matchDependencyPage types sched)

    -- TODO: also need a HEAD request version
    get "/search/constellation/:constellation" $ do
      (con, matches) <- dbQuery "constellation" fetchConstellation
      if nullSL matches
        then next
        else do
        sched <- liftSQL (makeSchedule (fmap Right matches))
        fromBlaze (Constellation.matchPage con sched)

    -- TODO: also need a HEAD request version
    get "/search/constellation/" $ do
      matches <- liftSQL fetchConstellationTypes
      fromBlaze (Constellation.indexPage matches)

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
                      
      case mans of
        Just mtoo -> do
          matches <- liftSQL (fetchTOO mtoo)
          if nullSL matches
            then next
            else do
              sched <- liftSQL (makeSchedule (fmap Right matches))
              fromBlaze (TOO.matchPage mtoo sched)
        Nothing -> next
    
    -- TODO: also need a HEAD request version
    get "/search/turnaround/" $ do
      (matches, noneTime) <- liftSQL fetchTOOs
      fromBlaze (TOO.indexPage matches noneTime)

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
                      
      case mans of
        Just mcs -> do
          matches <- liftSQL (fetchConstraint mcs)
          if nullSL matches
            then next
            else do
              sched <- liftSQL (makeSchedule (fmap Right matches))
              fromBlaze (Constraint.matchPage mcs sched)
        Nothing -> next
    
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

      case mtype of
        Just stype -> do
          matches <- liftSQL (fetchCategorySubType cat stype)
          if nullSL matches
            then next
            else do
              sched <- liftSQL (makeSchedule (fmap Right matches))
              fromBlaze (Category.categoryAndTypePage cat stype sched)
        Nothing -> next
        
    get "/search/category/:category" $ do
      (cat, matches) <- dbQuery "category" fetchCategory
      if nullSL matches
        then next
        else do
        sched <- liftSQL (makeSchedule (fmap Right matches))
        fromBlaze (Category.matchPage cat sched)

    -- TODO: also need a HEAD request version
    get "/search/category/" $ do
      matches <- liftSQL fetchCategoryTypes
      fromBlaze (Category.indexPage matches)

    -- TODO: also need a HEAD request version
    get "/search/instrument/:instrument" $ do
      (inst, matches) <- dbQuery "instrument" fetchInstrument
      if nullSL matches
        then next -- status status404
        else do
        sched <- liftSQL (makeSchedule (fmap Right matches))
        fromBlaze (Instrument.matchInstPage inst sched)

    -- TODO: also need a HEAD request version
    get "/search/grating/:grating" $ do
      (inst, matches) <- dbQuery "grating" fetchGrating
      if nullSL matches
        then next -- status status404
        else do
        sched <- liftSQL (makeSchedule (fmap Right matches))
        fromBlaze (Instrument.matchGratPage inst sched)

    -- TODO: also need a HEAD request version
    get "/search/instgrat/:ig" $ do
      (ig, matches) <- dbQuery "ig" fetchIG
      if nullSL matches
        then next -- status status404
        else do
        sched <- liftSQL (makeSchedule (fmap Right matches))
        fromBlaze (Instrument.matchIGPage ig sched)

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

    get "/search/proptype/:proptype" $ do
      (propType, matches) <- dbQuery "proptype" getProposalType
      if nullSL matches
        then next
        else do
          sched <- liftSQL (makeSchedule (fmap Right matches))
          fromBlaze (PropType.matchPage propType sched)

    -- map between proposal category and SIMBAD object types.
    get "/search/mappings" (fromBlaze Mapping.indexPage)

    get "/search/joint/:mission" $ do
      (mission, matches) <- dbQuery "mission" fetchJointMission
      if nullSL matches
        then next
        else do
          sched <- liftSQL (makeSchedule (fmap Right matches))
          fromBlaze (Mission.matchPage mission sched)

    get "/search/joint/" $ do
      missions <- liftSQL fetchMissionInfo
      fromBlaze (Mission.indexPage missions)

    -- TODO: also need a HEAD request version
    {-
    get "/search/" $ do
      fromBlaze $ ?.indexPage
    -}
    get "/search/" (redirect "/search/index.html")
                
    -- HEAD requests
    -- TODO: is this correct for HEAD; or should it just 
    --       set the redirect header?
    addroute HEAD "/" (standardResponse >> redirect "/index.html")
    addroute HEAD "/index.html" standardResponse

    addroute HEAD "/wwt.html" standardResponse

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

    addroute HEAD "/obsid/:obsid/wwt" $ do
      mobs <- queryRecord
      case mobs of
        Just (Right _) -> standardResponse
        _              -> next -- status status404

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


apiProposal :: ActionM (PropNum, Maybe Proposal) -> ActionM ()
apiProposal getData = do
  (propNum, mres) <- getData
  case mres of
    Just res -> json ("Success" :: T.Text, res)
    _ -> json ("Unknown Proposal Number" :: T.Text, propNum)


apiSearchName ::
  ActionM (String, ([T.Text], [T.Text]))
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

      -- Use the time value, already pulled out by from*Science,
      -- to merge the records. This saves having to query the
      -- JSON itself.
      items = fmap snd (mergeSL fst sitems nsitems)
      
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
            
#if defined(MIN_VERSION_http_client) && MIN_VERSION_http_client(0,4,31)
  req <- liftIO $ NHC.parseRequest url
#else
  req <- liftIO $ NHC.parseUrl url
#endif
  
  -- liftIO $ putStrLn $ "--> " ++ url
  rsp <- liftIO $ NHC.httpLbs req mgr
  -- liftIO $ putStrLn $ "<-- " ++ url
  setHeader "Content-Type" plainText

  -- copy over etags/last-modified headers to see if that helps
  -- with the caching; if may have done.
  --
  -- Since the ETag header is opaque it should be okay
  -- to just copy it over, since the assumption is that the
  -- base64 encoding is not going to change.
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
toHours :: TimeKS -> Double
toHours ks = _toKS ks / 3.6

-- | Convert a science observation into a JSON dictionary,
--   using the "Science" schema for the Exhibit timeline.
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
  -> (ChandraTime, Value)
  -- ^ The start time of the observation and a JSON dictionary
  --   following the Exhibit schema for the Science type.
fromScienceObs propMap simbadMap tNow so@ScienceObs {..} =
  (startTime, object (objs
                      ++ [ "imgURL" .= imgURL | isPublic && notCC]
                      ++ msimbad
                     ))

  where
    (startTime, endTime) = getTimes (Right so)
    obsid = fromObsId soObsId
    obsidTxt = sformat (left 5 '0' %. int) obsid
    
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

    -- If it is HRC the soDataMode will be Nothing. This is okay.
    -- I am not sure if we can have ACIS data with soDataMode equal to
    -- Nothing, so add a check here, just in case.
    --
    notCC = case soDataMode of
      Just mode -> not ("CC" `T.isPrefixOf` mode)
      Nothing -> soInstrument `elem` [HRCI, HRCS]

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
    -- TODO: how to find the correct version number (i.e.
    -- 'N00x' value)? One option would be to provide an
    -- endpoint (e.g. /api/image/:obsid) which would
    -- do the navigation, but leave that for the (possible)
    -- future.
    --
    imgURL :: T.Text
    imgURL = "http://cda.cfa.harvard.edu/chaser/viewerImage.do?obsid="
             <> obsidTxt <> "&filename=" <> instTxt
             <> "f" <> obsidTxt
             <> "N001_full_img2.jpg&filetype=loresimg_jpg"

    instTxt = case soInstrument of
      ACISI -> "acis"
      ACISS -> "acis"
      HRCI -> "hrc"
      HRCS -> "hrc"

    -- TODO: do not include the item if the value is
    --       not known
    fromProp :: (Proposal -> T.Text) -> T.Text
    fromProp f = fromMaybe "unknown"
                 (f <$> M.lookup soProposal propMap)

    objs = [
      "type" .= ("Science" :: T.Text),
      -- need a unique label
      "label" .= (soTarget <> " - ObsId " <> showInt obsid),
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
    
      -- observation length, in hours
      "length" .= toHours (fromMaybe soApprovedTime soObservedTime),
      
      "instrument" .= fromInstrument soInstrument,
      "grating" .= fromGrating soGrating,
      "isTOO" .= isBool (isJust soTOO),
      "constellation" .= getConstellationNameStr soConstellation,
      
      "cycle" .= fromProp propCycle,
      "category" .= fromProp propCategory,
      "proptype" .= fromProp propType
    
      ]

-- | Convert a non-science observation into a JSON dictionary,
--   using the "Engineering" schema for the Exhibit timeline.
--
fromNonScienceObs ::
  NonScienceObs
  -- ^ The observation to convert
  -> (ChandraTime, Value)
  -- ^ The start time of the observation and a JSON dictionary
  --   following the Exhibit schema for the NonScience type.
fromNonScienceObs ns@NonScienceObs {..} =
  (startTime, object objs)
  where
    (startTime, endTime) = getTimes (Left ns)
    obsid = fromObsId nsObsId
    -- do not use the nsName field as it is set from the STS value,
    -- but then is later removed/replaced by values from ObsCat.
    --

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

