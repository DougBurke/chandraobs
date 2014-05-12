{-# LANGUAGE OverloadedStrings #-}

-- | A test webserver.
-- 
-- Try the Heroku Haskell buildpack from
--
module Main where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad (liftM)

import Data.Default (def)
import Data.Monoid ((<>))

import Network.HTTP.Types (StdMethod(HEAD))
-- import Network.Wai.Middleware.RequestLogger
import Network.Wai.Handler.Warp (defaultSettings, setPort)

import System.Environment (lookupEnv)

import Text.Blaze.Html.Renderer.Text

import Web.Scotty

main :: IO ()
main = do
  -- Use presence of PORT argument to decide between production and
  -- test environments. The Scotty documentations suggest calling
  -- setFdCacheDuration on the settings field, to change the value from
  -- 0, but do not really explain the implications of why it is
  -- set to 0 in the first place.
  --
  mports <- lookupEnv "PORT"
  let opts = case read `liftM` mports of
               Just port -> def { verbose = 0
                                , settings = setPort port defaultSettings }
               _ -> def
 
  scottyOpts opts $ do

    -- middleware logStdoutDev

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
