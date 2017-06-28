{-# LANGUAGE OverloadedStrings #-}

{-

Redirect from chandraobs-devel.herokuapp.com to
              chandraobservatory.herokuapp.com

This could be made more configureable, but let's keep it simple.

-}

module Main (main) where

import Data.Monoid ((<>))

import Network.HTTP.Types.Header (hLocation)
import Network.HTTP.Types.Status (status301)
import Network.Wai (Application
                   , isSecure
                   , rawPathInfo
                   , rawQueryString
                   , responseLBS)
import Network.Wai.Handler.Warp (runEnv)

-- picks up the port from the PORT environment variable, using a
-- default of 3001 if not set.
main :: IO ()
main = runEnv 3001 redirect
    

-- For now I am going to use rawPathInfo/QueryString rather than
-- pathInfo and queryString, as this results in less work for me.
--
redirect :: Application
redirect req resp =
  let hdrs = [(hLocation, newURI)]
      oldPath = rawPathInfo req <> rawQueryString req
      scheme = if isSecure req then "https" else "http"
      newURI = scheme <> "://chandraobservatory.herokuapp.com" <> oldPath
  in resp (responseLBS status301 hdrs "")
