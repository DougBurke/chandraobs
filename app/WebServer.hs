{-# LANGUAGE OverloadedStrings #-}

-- | A test webserver.
-- 
-- As this is a test server, all the HTML is crammed
-- into this module. Once I have worked out what I
-- want, things will get separated out and cleaned
-- up.
--
module Main where

import qualified Views.Index as Index
import qualified Views.Record as Record
import qualified Views.Schedule as Schedule
import qualified Views.WWT as WWT

import Control.Monad.IO.Class (liftIO)

import Data.Default (def)
import Data.Time (getCurrentTime)

import Network.HTTP.Types (StdMethod(HEAD), status404)
-- import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Handler.Warp (defaultSettings, setPort)

import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStrLn, stderr)

import Web.Scotty

import Database (getCurrentObs, getRecord, getObsInfo, getObsId, getSpecialObs, getSchedule)
import Types (ObsName(..))
import Utils (ObsInfo(..), fromBlaze, standardResponse)

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
    Right opts -> scottyOpts opts webapp

webapp :: ScottyM ()
webapp = do

    -- Need to find out how the static directory gets copied
    -- over by cabal
    --
    -- middleware logStdoutDev-- An invalid port number foe
    middleware $ staticPolicy (noDots >-> addBase "static")

    get "/" $ redirect "/index.html"
    get "/about.html" $ redirect "/about/index.html"
    get "/about" $ redirect "/about/index.html"

    get "/index.html" $ do
      mobs <- liftIO getObsInfo
      cTime <- liftIO getCurrentTime
      case mobs of
        Just obs -> fromBlaze $ Index.introPage cTime obs
        _        -> fromBlaze Index.noDataPage

    get "/wwt.html" $ do
      mobs <- liftIO getObsInfo
      case mobs of 
        Just obs -> fromBlaze (WWT.wwtPage True (oiCurrentObs obs))
        _ -> fromBlaze Index.noDataPage

    get "/obs/:special" $ do
      sobs <- param "special"
      mobs <- liftIO $ getSpecialObs sobs
      mCurrent <- liftIO getCurrentObs
      cTime <- liftIO getCurrentTime
      case mobs of
        Just obs -> fromBlaze $ Record.recordPage cTime mCurrent obs
        _        -> status status404 -- TODO: want an error page

    get "/obsid/:obsid" $ do
      obsid <- param "obsid"
      mobs <- liftIO $ getObsId obsid
      mCurrent <- liftIO getCurrentObs
      cTime <- liftIO getCurrentTime
      case mobs of
        Just obs -> fromBlaze $ Record.recordPage cTime mCurrent obs
        _        -> status status404 -- TODO: want an error page

    get "/obsid/:obsid/wwt" $ do
      obsid <- param "obsid"
      mrecord <- liftIO $ getRecord (ObsId obsid)
      case mrecord of
        Just record -> fromBlaze $ WWT.wwtPage False record
        _           -> status status404 -- TODO: want an error page

    get "/schedule" $ redirect "/schedule/index.html"
    get "/schedule/index.html" $ do
      sched <- liftIO $ getSchedule 3
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

    addroute HEAD "/obs/:special" $ do
      sobs <- param "special"
      mobs <- liftIO $ getSpecialObs sobs
      case mobs of
        Just _ -> standardResponse
        _      -> status status404

    addroute HEAD "/obsid/:obsid" $ do
      obsid <- param "obsid"
      mobs <- liftIO $ getObsId obsid
      case mobs of
        Just _ -> standardResponse
        _      -> status status404

    addroute HEAD "/obsid/:obsid/wwt" $ do
      obsid <- param "obsid"
      mrecord <- liftIO $ getRecord (ObsId obsid)
      case mrecord of
        Just _ -> standardResponse
        _      -> status status404
