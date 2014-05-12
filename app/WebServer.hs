{-# LANGUAGE OverloadedStrings #-}

-- | A test webserver.
-- 
-- Try the Heroku Haskell buildpack from
--
module Main where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Default (def)
import Data.Monoid ((<>))

import Network.HTTP.Types (StdMethod(HEAD))
-- import Network.Wai.Middleware.RequestLogger
import Network.Wai.Handler.Warp (defaultSettings, setPort)

import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStrLn, stderr)

import Text.Blaze.Html.Renderer.Text

import Web.Scotty

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

    -- middleware logStdoutDev-- An invalid port number foe


    get "/" $ redirect "/index.html"
    get "/index.html" $ fromBlaze intro
    -- TODO: is this correct for HEAD; or should it just 
    --       set the redirect header?
    addroute HEAD "/" $ standardResponse >> redirect "/index.html"
    addroute HEAD "/index.html" standardResponse

    get "/about.html" $ fromBlaze about
    addroute HEAD "/about.html" standardResponse

fromBlaze :: H.Html -> ActionM ()
fromBlaze = html . renderHtml

-- A placeholder in case we want to set up any 
-- response settings.
standardResponse :: ActionM ()
standardResponse = return ()

-- The uninformative landing page
intro :: H.Html
intro =
  H.docTypeHtml $
    H.head (H.title "Welcome")
    <>
    H.body (H.p "Hello world!")

-- The uninformative about page  
about :: H.Html
about = 
  H.docTypeHtml $
   let txt = "This is " <> H.em "not" <> 
             " the space-ninja rocket ship start-up you were looking for."
       lnk = "Back " <> (H.a H.! A.href "/index.html" $ "home") <> "."
   in H.body $ 
      H.p txt <> H.p lnk
