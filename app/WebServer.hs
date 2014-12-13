{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE FlexibleContexts #-} -- needed for webapp signature

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
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC

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
import qualified Views.Search.Category as Category
import qualified Views.Search.Constellation as Constellation
import qualified Views.Search.Instrument as Instrument
import qualified Views.Search.Types as SearchTypes
import qualified Views.Schedule as Schedule
import qualified Views.WWT as WWT

import qualified Web.Scotty.Trans as Trans

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (runResourceT)

import Data.Default (def)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Time (getCurrentTime)

import Database.Groundhog.Core (ConnectionManager(..))
import Database.Groundhog.Postgresql (Postgresql(..), PersistBackend, runDbConn, withPostgresqlPool)

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
                 , getObsId, getSchedule, makeSchedule
                 , getProposalInfo
                 , getProposalFromNumber
                 , getRelatedObs
                 , getObsFromProposal
                 , getSimbadInfo
                 -- , findObsId
                 , fetchSIMBADType
                 , fetchObjectTypes 
                 , fetchConstellation
                 , fetchConstellationTypes
                 , fetchCategory
                 , fetchCategoryTypes
                 , fetchProposal
                 , fetchInstrument
                 , fetchInstrumentTypes
                 )
import Types (Record, SimbadInfo, Proposal
             , NonScienceObs(..), ScienceObs(..)
             , ObsInfo(..), ObsIdVal(..), Sequence(..)
             , handleMigration)
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
getDbConnStr False = return "user=postgres password=postgres dbname=chandraobs host=127.0.0.1"
getDbConnStr _ = do
  cparams <- dbConnParams
  return $ T.unpack $ foldr (\(k,v) s ->
                        s <> (k <> "=" <> v <> " ")) "" cparams

uerror :: String -> IO ()
uerror msg = do
  hPutStrLn stderr $ "ERROR: " ++ msg
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
                                Just port -> Right $ production port
                                _ -> Left $ "Invalid PORT argument: " ++ ports

                _ -> Right development

  connStr <- getDbConnStr $ isJust mports
 
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
          withPostgresqlPool connStr 5 $ \pool ->
              scottyOpts opts $ webapp pool mgr

-- Hack; needs cleaning up
getDBInfo :: 
  (MonadIO m, PersistBackend m) 
  => Record 
  -> m (Maybe SimbadInfo, (Maybe Proposal, [ScienceObs]))
getDBInfo r = do
  as <- either (const (return Nothing)) (getSimbadInfo . soTarget) r
  bs <- getProposalInfo r
  return (as, bs)

webapp :: 
    ConnectionManager cm Postgresql 
    => cm 
    -> NHC.Manager
    -> ScottyM ()
webapp cm mgr = do
    let liftSQL a = liftIO $ runDbConn a cm

    defaultHandler errHandle
    liftSQL handleMigration

    -- Need to find out how the static directory gets copied
    -- over by cabal; seems to be okay
    --
    -- middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "static")

    -- proxy requests to the DSS/RASS/PSPC images so we can
    -- access them via AJAX. *experimental*
    --
    get "/proxy/dss/:sequence/:obsid" $ do
              seqVal <- param "sequence"
              obsid <- param "obsid"
              proxy mgr PTDSS seqVal obsid

    get "/proxy/rass/:sequence/:obsid" $ do
              seqVal <- param "sequence"
              obsid <- param "obsid"
              proxy mgr PTRASS seqVal obsid

    get "/proxy/pspc/:sequence/:obsid" $ do
              seqVal <- param "sequence"
              obsid <- param "obsid"
              proxy mgr PTPSPC seqVal obsid

    -- for now always return JSON; need a better success/failure
    -- set up.
    --
    -- the amount of information returned by getObsId is
    -- excessive here; probably just need the preceeding and
    -- next obsid values (if any), but leave as is for now.
    --
    get "/api/current" $ do
              -- note: this is creating/throwing away a bunch of info that could be useful
              mrec <- liftSQL $ getCurrentObs
              let rval o = json ("Success" :: T.Text, fromObsId o)
              case mrec of
                Just (Left ns) -> rval $ nsObsId ns
                Just (Right so) -> rval $ soObsId so
                _ -> json ("Failed" :: T.Text)

    get "/api/obsid/:obsid" $ do
              obsid <- param "obsid"
              -- mobs <- liftSQL $ findObsId $ ObsIdVal obsid
              mobs <- liftSQL $ getObsId $ ObsIdVal obsid
              case mobs of
                -- Just v -> json ("Success" :: T.Text, v) -- NOTE: v is (Record, Bool) from findObsId
                Just v -> json ("Success" :: T.Text, v)
                _ -> json ("Unknown ObsId" :: T.Text, obsid)

    -- break down the monolithic queries into separate ones, which may or may not
    -- be a good idea
    --
    -- Note that for Simbad names we may have a / in them, so we use
    -- a regex
    get (regex "^/api/simbad/name/(.+)$") $ do
              name <- param "1"
              msim <- liftSQL $ getSimbadInfo name
              case msim of
                Just sim -> json ("Success" :: T.Text, sim)
                _ -> json ("Unknown Target" :: T.Text, name)

    get "/api/proposal/:propnum" $ do
              propNum <- param "propnum"
              mres <- liftSQL $ getProposalFromNumber propNum
              case mres of
                Just res -> json ("Success" :: T.Text, res)
                _ -> json ("Unknown Proposal Number" :: T.Text, propNum)

    get "/api/related/:propnum/:obsid" $ do
              propNum <- param "propnum"
              obsid <- param "obsid"
              res <- liftSQL $ getRelatedObs propNum $ ObsIdVal obsid
              -- hmmm, can't tell between an unknown propnum/obsid
              -- pair and an observation with no related observations.
              json ("Success" :: T.Text, res)

    get "/api/related/:propnum" $ do
              propNum <- param "propnum"
              res <- liftSQL $ getObsFromProposal propNum
              -- hmmm, can't tell between an unknown propnum
              -- and an observation with no related observations.
              json ("Success" :: T.Text, res)

    get "/" $ redirect "/index.html"
    get "/about.html" $ redirect "/about/index.html"
    get "/about" $ redirect "/about/index.html"

    get "/index.html" $ do
      mobs <- liftSQL getObsInfo
      cTime <- liftIO getCurrentTime
      case mobs of
        Just obs -> do
          dbInfo <- liftSQL $ getDBInfo $ oiCurrentObs obs
          fromBlaze $ Index.introPage cTime obs dbInfo
        _        -> do
          fact <- liftIO getFact
          fromBlaze $ Index.noDataPage fact

    -- TODO: send in proposal details
    get "/wwt.html" $ do
      mobs <- liftSQL getCurrentObs
      case mobs of 
        Just (Right so) -> fromBlaze (WWT.wwtPage True so)
        _ -> do
          fact <- liftIO getFact
          fromBlaze $ Index.noDataPage fact

    get "/obsid/:obsid" $ do
      obsid <- param "obsid"
      mobs <- liftSQL $ getObsId $ ObsIdVal obsid
      mCurrent <- liftSQL getCurrentObs
      cTime <- liftIO getCurrentTime
      case mobs of
        Just obs -> do
          dbInfo <- liftSQL $ getDBInfo $ oiCurrentObs obs
          fromBlaze $ Record.recordPage cTime mCurrent obs dbInfo
        _ -> next -- status status404

    -- TODO: send in proposal details
    get "/obsid/:obsid/wwt" $ do
      obsid <- param "obsid"
      mobs <- liftSQL $ getRecord $ ObsIdVal obsid
      case mobs of
        Just (Right so) -> fromBlaze $ WWT.wwtPage False so
        _               -> next -- status status404

    -- TODO: head requests
    get "/proposal/:propnum" $ do
      pNum <- param "propnum"
      (mprop, matches) <- liftSQL $ fetchProposal pNum
      case mprop of
        Just prop -> do
          sched <- liftSQL $ makeSchedule $ map Right matches
          fromBlaze $ Proposal.matchPage prop sched
        _         -> next -- status status404

    get "/schedule" $ redirect "/schedule/index.html"
    get "/schedule/index.html" $ do
      sched <- liftSQL $ getSchedule 3
      fromBlaze $ Schedule.schedPage sched

    get "/schedule/day" $ do
      sched <- liftSQL $ getSchedule 1
      fromBlaze $ Schedule.schedPage sched

    get "/schedule/day/:ndays" $ do
      ndays <- param "ndays"
      when (ndays <= 0) next  -- TODO: better error message
      sched <- liftSQL $ getSchedule ndays
      fromBlaze $ Schedule.schedPage sched

    get "/schedule/week" $ do
      sched <- liftSQL $ getSchedule 7
      fromBlaze $ Schedule.schedPage sched

    get "/schedule/week/:nweeks" $ do
      nweeks <- param "nweeks"
      when (nweeks <= 0) next  -- TODO: better error message
      sched <- liftSQL $ getSchedule (7 * nweeks)
      fromBlaze $ Schedule.schedPage sched

    -- TODO: also need a HEAD request version
    get "/search/type/:type" $ do
      simbadType <- param "type"
      matches <- liftSQL $ fetchSIMBADType simbadType
      case matches of
        Just (typeInfo, ms) -> do
           sched <- liftSQL $ makeSchedule $ map Right ms
           fromBlaze $ SearchTypes.matchPage typeInfo sched
        _ -> next -- status status404

    -- TODO: also need a HEAD request version
    get "/search/type/" $ do
      matches <- liftSQL $ fetchObjectTypes
      fromBlaze $ SearchTypes.indexPage matches

    -- TODO: also need a HEAD request version
    get "/search/constellation/:constellation" $ do
      con <- param "constellation"
      matches <- liftSQL $ fetchConstellation con
      case matches of
        [] -> next -- status status404
        _ -> do
           sched <- liftSQL $ makeSchedule $ map Right matches
           fromBlaze $ Constellation.matchPage con sched

    -- TODO: also need a HEAD request version
    get "/search/constellation/" $ do
      matches <- liftSQL $ fetchConstellationTypes
      fromBlaze $ Constellation.indexPage matches

    -- TODO: also need a HEAD request version
    get "/search/category/:category" $ do
      cat <- param "category"
      matches <- liftSQL $ fetchCategory cat
      case matches of
        [] -> next -- status status404
        _ -> do
           sched <- liftSQL $ makeSchedule $ map Right matches
           fromBlaze $ Category.matchPage cat sched

    -- TODO: also need a HEAD request version
    get "/search/category/" $ do
      matches <- liftSQL $ fetchCategoryTypes
      fromBlaze $ Category.indexPage matches

    -- TODO: also need a HEAD request version
    get "/search/instrument/:instrument" $ do
      inst <- param "instrument"
      matches <- liftSQL $ fetchInstrument inst
      case matches of
        [] -> next -- status status404
        _ -> do
           sched <- liftSQL $ makeSchedule $ map Right matches
           fromBlaze $ Instrument.matchPage inst sched

    -- TODO: also need a HEAD request version
    get "/search/instrument/" $ do
      matches <- liftSQL $ fetchInstrumentTypes
      fromBlaze $ Instrument.indexPage matches

    -- TODO: also need a HEAD request version
    {-
    get "/search/" $ do
      fromBlaze $ ?.indexPage
    -}
    get "/search/" $ redirect "/search/index.html"
                
    -- HEAD requests
    -- TODO: is this correct for HEAD; or should it just 
    --       set the redirect header?
    addroute HEAD "/" $ standardResponse >> redirect "/index.html"
    addroute HEAD "/index.html" standardResponse

    addroute HEAD "/wwt.html" standardResponse

    addroute HEAD "/about" $ standardResponse >> redirect "/about/index.html"

    -- TODO: does the staticPolicy middleware deal with this?
    -- addroute HEAD "/about/index.html" standardResponse
    -- addroute HEAD "/about/instruments.html" standardResponse
    -- addroute HEAD "/about/views.html" standardResponse

    addroute HEAD "/obsid/:obsid" $ do
      obsid <- param "obsid"
      mobs <- liftSQL $ getObsId $ ObsIdVal obsid
      case mobs of
        Just _ -> standardResponse
        _      -> next -- status status404

    addroute HEAD "/obsid/:obsid/wwt" $ do
      obsid <- param "obsid"
      mobs <- liftSQL $ getRecord $ ObsIdVal obsid
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
      fromBlaze $ NotFound.notFoundPage fact
      status status404

-- | Exception handler. We should log the error.
errHandle :: L.Text -> ActionM ()
errHandle txt = do
  liftIO $ L.putStrLn $ "Error string: " <> txt
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
  let seqStr = show $ _unSequence seqVal
      obsStr = show $ fromObsId obsid
      url = "http://asc.harvard.edu/targets/" ++
            seqStr ++ "/" ++ seqStr ++ "." ++
            obsStr ++ ".soe." ++ show pt ++ ".gif"
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

  case mLastMod of
    Just lastMod -> setHeader "Last-Modified" lastMod
    _ -> return ()

  case mETag of
    Just eTag -> setHeader "ETag" eTag
    _ -> return ()

  raw $ B64.encode $ NHC.responseBody rsp

{-

can we stream the base-64 encoding?

-}

{-

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

NOPITY NOPE NOPE

  runResourceT $ do
    rsp <- NHC.http req mgr
    liftIO $ putStrLn $ "<-- " ++ url
    Trans.setHeader "Content-Type" plainText

    let src = NHC.responseBody rsp C.$=+ CC.encodeBase64

  -- how to take advantage of stream, rather than raw,
  -- to allow a streaming solution?
  -- b64 <- liftIO (NHC.responseBody rsp C.$$+- CC.encodeBase64)

  {-
  want to take the response, encode it, then convert it into a form that
  I can use stream :: StreamingBody -> ActionM () with
  type StreamingBody = (Builder -> IO ()) -> IO () -> IO ()

  b64 <- liftIO (NHC.responseBody rsp C.$$+- CC.encodeBase64)
  raw b64

  -}

    Trans.raw "FOO"
-}

proxy :: 
    NHC.Manager
    -> ProxyType
    -> Sequence 
    -> ObsIdVal 
    -> ActionM ()
proxy = proxy2
