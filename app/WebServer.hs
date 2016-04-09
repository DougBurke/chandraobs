{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-
TODO: when all tables are removed, get errors like

% curl http://localhost:3000/index.html
<h1>500 Internal Server Error</h1>user error (Postgresql.withStmt': bad result status FatalError ("PGRES_FATAL_ERROR"). Error message: ERROR:  relation "ScheduleItem" does not exist
LINE 1: ...siScienceObs","siStart","siEnd","siDuration" FROM "ScheduleI...

but it returns a 200 status !                                                             ^
% curl -I http://localhost:3000/index.html
HTTP/1.1 200 OK
Date: Wed, 11 Jun 2014 19:56:35 GMT
Server: Warp/2.1.5.2

-}

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
import qualified Views.Search.Instrument as Instrument
import qualified Views.Search.Mapping as Mapping
import qualified Views.Search.Mission as Mission
import qualified Views.Search.PropType as PropType
import qualified Views.Search.Target as Target
import qualified Views.Search.Types as SearchTypes
import qualified Views.Schedule as Schedule
import qualified Views.WWT as WWT

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Control.Applicative ((<$>))
#endif

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
-- import Control.Monad.Logger (NoLoggingT)

import Data.Aeson((.=), object)
import Data.Default (def)
import Data.List (nub)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Monoid ((<>))
import Data.Pool (Pool)
import Data.Time (UTCTime(utctDay), addDays, getCurrentTime)

-- import Database.Groundhog.Core (DbPersist)
import Database.Groundhog.Postgresql (Postgresql(..)
                                     , PersistBackend
                                     , runDbConn
                                     , withPostgresqlPool)

import Network.HTTP.Types (StdMethod(HEAD)
                          , hLastModified
                          , status404, status503)
-- import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Handler.Warp (defaultSettings, setPort)

import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStrLn, stderr)

import Web.Heroku (dbConnParams)
import Web.Scotty

import Database (getCurrentObs, getRecord, getObsInfo
                 , getObsId
                 , getSchedule
                 , getScheduleDate
                 , makeSchedule
                 , getProposalInfo
                 , getProposalFromNumber
                 , getRelatedObs
                 , getObsFromProposal
                 , getSimbadInfo
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
import Types (Record, SimbadInfo, Proposal
             , PropNum(..)
             , NonScienceObs(..), ScienceObs(..)
             , ObsInfo(..), ObsIdVal(..)
             -- , PropType(..)
             , Sequence(..)
             , SortedList, StartTimeOrder
             , TimeKS(..)
             , fromSimbadType
             , toSimbadType
             , nullSL, fromSL
             , showExpTime
             , handleMigration
             )
import Utils (fromBlaze, standardResponse, getFact)

readInt :: String -> Maybe Int
readInt s = case reads s of
              [(v,[])] -> Just v
              _ -> Nothing

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

uerror :: String -> IO ()
uerror msg = do
  hPutStrLn stderr ("ERROR: " ++ msg)
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
                Just ports -> case readInt ports of
                                Just port -> Right (production port)
                                _ -> Left ("Invalid PORT argument: " ++ ports)

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
          withPostgresqlPool connStr 5 $ \pool -> do
            runDbConn handleMigration pool
            scottyOpts opts (webapp pool mgr)

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
    -> ScottyM ()
webapp cm mgr = do
    let liftSQL a = liftIO (runDbConn a cm)

    defaultHandler errHandle

    -- Need to find out how the static directory gets copied
    -- over by cabal; seems to be okay
    --
    -- middleware logStdoutDev
    middleware (staticPolicy (noDots >-> addBase "static"))

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
    get "/api/current" $ do
              -- note: this is creating/throwing away a bunch of info that could be useful
              mrec <- liftSQL getCurrentObs
              let rval o = json ("Success" :: T.Text, fromObsId o)
              case mrec of
                Just (Left ns) -> rval (nsObsId ns)
                Just (Right so) -> rval (soObsId so)
                _ -> json ("Failed" :: T.Text)

    get "/api/obsid/:obsid" $ do
      (obsid, mobs) <- queryObsidParam
      case mobs of
        Just v -> json ("Success" :: T.Text, v)
        _ -> json ("Unknown ObsId" :: T.Text, obsid)

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

    get "/api/proposal/:propnum" $ do
      (propNum, mres) <- dbQuery "propnum" getProposalFromNumber
      case mres of
        Just res -> json ("Success" :: T.Text, res)
        _ -> json ("Unknown Proposal Number" :: T.Text, propNum)

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
    {-
    get "/api/search/name" $ do
      (query, (exact, other)) <- dbQuery "term" findNameMatch
      json (object [ "query" .= query
                   , "exact" .= exact
                   , "other" .= other])
    -}
    get "/api/search/name" $ do
      (_, (exact, other)) <- dbQuery "term" findNameMatch
      -- for now, flatten out the response
      -- TODO: should also remove excess spaces, but this requires some
      --       thought on how the search functionality should work
      json (nub (exact ++ other))
    
    get "/api/search/proposal" $ do
      (_, matches) <- dbQuery "term" findProposalNameMatch
      -- for now, explicitly convert the PropNum field to an integer
      -- for easy serialization, but maybe this should be the default
      -- ToJSON serialization?
      let out = fmap conv matches
          conv (title, pnum) = object [ "title" .= title
                                      , "number" .= _unPropNum pnum ]
      json out

    -- How to best serialize the mapping data? For now go with a
    -- form that is closely tied to the visualization.
    --
    get "/api/mappings" $ do
      mapping <- liftSQL getProposalObjectMapping

      let names = M.keys mapping
          propNames = nub (map fst names)
          simKeys = nub (map (keyToPair . snd) names)
          simNames = map fst simKeys

          -- remove the object that would be created by the
          -- ToJSON instance of SimbadType
          toPair (k, v) = T.pack k .= fromSimbadType v
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

      json out

    -- highly experimental
    get "/api/exposures" $ do
      pairs <- liftSQL getExposureValues
      let toPair (cyc, vals) =
            let ts = map _toKS (fromSL vals)
                allTime = showExpTime (TimeKS (sum ts))
            in 
              T.pack cyc .= object
              [ "cycle" .= cyc
              , "units" .= ("ks" :: T.Text)
              , "length" .= length ts
              , "totalTime" .= allTime
              , "times" .= ts
              ]

          out = object (map toPair pairs)
      json out

    
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
      (target, matches) <- dbQuery "target" findTarget
      -- TODO: set an error code if no match? Once have sorted out search info
      if nullSL matches
        then fromBlaze (Target.noMatchPage target)
        else do
          sched <- liftSQL (makeSchedule (fmap Right matches))
          fromBlaze (Target.targetPage target sched)

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
      fromBlaze (NotFound.notFoundPage fact)
      status status404

-- | Exception handler. We should log the error.
errHandle :: L.Text -> ActionM ()
errHandle txt = do
  liftIO (L.putStrLn ("Error string: " <> txt))
  fromBlaze NotFound.errPage
  -- Can we change the HTTP status code? The following does not
  -- work.
  status status503

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
  req <- liftIO $ NHC.parseUrl url
  -- liftIO $ putStrLn $ "--> " ++ url
  rsp <- liftIO $ NHC.httpLbs req mgr
  -- liftIO $ putStrLn $ "<-- " ++ url
  setHeader "Content-Type" plainText

  -- copy over etags/last-modified headers to see if that helps
  -- with the caching; if may have done.
  --
  -- Since the ETag header is opaque it should be okay
  -- to just cioy it over, since the assumption is that the
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
