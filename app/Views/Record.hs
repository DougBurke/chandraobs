{-# LANGUAGE OverloadedStrings #-}

-- | The record page.

module Views.Record (recordPage) where

import qualified Prelude as P
import Prelude (($))

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Maybe (isJust)
import Data.Monoid ((<>), mempty)

import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (title)

import PersistentTypes
import Utils (ObsInfo(..), defaultMeta, obsURI, renderRecord)

-- The specific page for this observation. At present I have not
-- worked out how this interacts with the top-level page; i.e.
-- the current observation (i.e. should the current observation
-- be flagged as such when using this view?)
--
recordPage :: ObsInfo -> Html
recordPage (ObsInfo currentObs mPrevObs mNextObs) =
  let initialize = "initialize()"

      obsName = recordObsname currentObs

      prevLink = case mPrevObs of
        P.Just prevObs -> a ! href (toValue (obsURI prevObs))
                            $ "Previous observation."
        _ -> mempty

      nextLink = case mNextObs of
        P.Just nextObs -> a ! href (toValue (obsURI nextObs))
                            $ "Next observation."
        _ -> mempty

      navLinks = let cts = prevLink <> nextLink
                 in if isJust mPrevObs P.|| isJust mNextObs
                    then p cts
                    else mempty

  in docTypeHtml $
    head (H.title ("Chandra observation: " <> toHtml obsName) <>
            defaultMeta <>
            (script ! src "/js/main.js") "" <>
            link ! href   "/css/main.css"
                 ! type_  "text/css" 
                 ! rel    "stylesheet"
                 ! A.title  "Default"
                 ! media  "all"
            )
    <>
    (body ! onload initialize)
     (-- p "Hello world!" <>
      navLinks <>
      -- p "The current observation is:" <>
      renderRecord P.False currentObs
     )
