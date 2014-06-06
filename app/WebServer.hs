{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE FlexibleContexts #-} -- needed for webapp signature

-- | A test webserver.
-- 
module Main where

import qualified Views.Index as Index
import qualified Views.NotFound as NotFound
import qualified Views.Record as Record
import qualified Views.Schedule as Schedule
import qualified Views.WWT as WWT

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import Data.Default (def)
import Data.Time (getCurrentTime)

import Database.Groundhog.Core (ConnectionManager(..))
import Database.Groundhog.Postgresql (Postgresql(..), runDbConn, withPostgresqlConn)

import Network.HTTP.Types (StdMethod(HEAD), status404)
-- import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Handler.Warp (defaultSettings, setPort)

import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStrLn, stderr)

import Web.Scotty

import Database (getCurrentObs, getRecord, getObsInfo,
                 getObsId, getSchedule,
                 getProposalInfo)
import Types (ObsInfo(..), ObsIdVal(..), handleMigration)
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

uerror :: String -> IO ()
uerror msg = do
  hPutStrLn stderr $ "ERROR: " ++ msg
  hFlush stderr
  exitFailure

-- I use the presence of the PORT environment variable to decide
-- between production and test environments. The Scotty documentations
-- suggest calling setFdCacheDuration on the settings field, to change
-- the value from 0, but do not really explain the implications of why
-- it is set to 0 in the first place.
--
main :: IO ()
main = do
  mports <- lookupEnv "PORT"
  let eopts = case mports of
                Just ports -> case readInt ports of
                                Just port -> Right $ production port
                                _ -> Left $ "Invalid PORT argument: " ++ ports

                _ -> Right development
 
  case eopts of
    Left emsg -> uerror emsg
    Right opts -> do
      withPostgresqlConn "user=postgres password=postgres dbname=chandraobs host=127.0.0.1" $ \cm -> 
        scottyOpts opts (webapp cm)

{-
      webapp :: forall cm.
                Database.Groundhog.Core.ConnectionManager cm SQL.Postgresql =>
                cm
                -> scotty-0.7.3:Web.Scotty.Types.ScottyT
                     Data.Text.Internal.Lazy.Text IO ()
-}
webapp :: ConnectionManager cm Postgresql => cm -> ScottyM ()
webapp cm = do

    let liftSQL a = liftIO $ runDbConn a cm

    liftSQL handleMigration

    -- Need to find out how the static directory gets copied
    -- over by cabal
    --
    -- middleware logStdoutDev-- An invalid port number foe
    middleware $ staticPolicy (noDots >-> addBase "static")

    get "/" $ redirect "/index.html"
    get "/about.html" $ redirect "/about/index.html"
    get "/about" $ redirect "/about/index.html"

    get "/index.html" $ do
      mobs <- liftSQL getObsInfo
      cTime <- liftIO getCurrentTime
      case mobs of
        Just obs -> do
          propInfo <- liftSQL $ getProposalInfo $ oiCurrentObs obs
          fromBlaze $ Index.introPage cTime obs propInfo
        _        -> fromBlaze Index.noDataPage

    -- TODO: send in proposal details
    get "/wwt.html" $ do
      mobs <- liftSQL getCurrentObs
      case mobs of 
        Just (Right so) -> fromBlaze (WWT.wwtPage True so)
        _ -> fromBlaze Index.noDataPage

    get "/obsid/:obsid" $ do
      obsid <- param "obsid"
      mobs <- liftSQL $ getObsId $ ObsIdVal obsid
      mCurrent <- liftSQL getCurrentObs
      cTime <- liftIO getCurrentTime
      case mobs of
        Just obs -> do
          propInfo <- liftSQL $ getProposalInfo $ oiCurrentObs obs
          fromBlaze $ Record.recordPage cTime mCurrent obs propInfo
        _        -> status status404

    -- TODO: send in proposal details
    get "/obsid/:obsid/wwt" $ do
      obsid <- param "obsid"
      mobs <- liftSQL $ getRecord $ ObsIdVal obsid
      case mobs of
        Just (Right so) -> fromBlaze $ WWT.wwtPage False so
        _               -> status status404

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
        _      -> status status404

    addroute HEAD "/obsid/:obsid/wwt" $ do
      obsid <- param "obsid"
      mobs <- liftSQL $ getRecord $ ObsIdVal obsid
      case mobs of
        Just (Right _) -> standardResponse
        _              -> status status404

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

