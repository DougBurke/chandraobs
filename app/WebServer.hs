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

-- | A test webserver.
-- 
module Main where

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

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

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Default (def)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Time (getCurrentTime)

import Database.Groundhog.Core (ConnectionManager(..))
import Database.Groundhog.Postgresql (Postgresql(..), PersistBackend, runDbConn, withPostgresqlPool)

import Network.HTTP.Types (StdMethod(HEAD), status404, status503)
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
                 , getSimbadInfo
                 , fetchSIMBADType
                 , fetchConstellation
                 , fetchCategory
                 , fetchProposal
                 , fetchInstrument
                 )
import Types (Record, SimbadInfo, Proposal, ScienceObs(..), ObsInfo(..), ObsIdVal(..), handleMigration)
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
      withPostgresqlPool connStr 5 $ 
        scottyOpts opts . webapp

-- Hack; needs cleaning up
getDBInfo :: (MonadIO m, PersistBackend m) => Record -> m (Maybe SimbadInfo, (Maybe Proposal, [ScienceObs]))
getDBInfo r = do
  as <- case r of
          Right x -> getSimbadInfo (soTarget x)
          _ -> return Nothing
  bs <- getProposalInfo r
  return (as, bs)

webapp :: ConnectionManager cm Postgresql => cm -> ScottyM ()
webapp cm = do

    let liftSQL a = liftIO $ runDbConn a cm

    defaultHandler errHandle
    liftSQL handleMigration

    -- Need to find out how the static directory gets copied
    -- over by cabal; seems to be okay
    --
    -- middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "static")

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
        (_, []) -> next -- status status404
        (typeInfo, ms) -> do
           sched <- liftSQL $ makeSchedule $ map Right ms
           fromBlaze $ SearchTypes.matchPage typeInfo sched

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
    get "/search/category/:category" $ do
      cat <- param "category"
      matches <- liftSQL $ fetchCategory cat
      case matches of
        [] -> next -- status status404
        _ -> do
           sched <- liftSQL $ makeSchedule $ map Right matches
           fromBlaze $ Category.matchPage cat sched

    -- TODO: also need a HEAD request version
    get "/search/instrument/:instrument" $ do
      inst <- param "instrument"
      matches <- liftSQL $ fetchInstrument inst
      case matches of
        [] -> next -- status status404
        _ -> do
           sched <- liftSQL $ makeSchedule $ map Right matches
           fromBlaze $ Instrument.matchPage inst sched

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
  fromBlaze $ NotFound.errPage
  -- Can we change the HTTP status code? The following does not
  -- work.
  status status503

