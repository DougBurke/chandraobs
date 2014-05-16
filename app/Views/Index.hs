{-# LANGUAGE OverloadedStrings #-}

-- | The index page.

module Views.Index (introPage, noDataPage) where

import qualified Prelude as P
import Prelude (($))

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Maybe (isJust)
import Data.Monoid ((<>), mempty)

import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (title)

import Utils (ObsInfo(..), defaultMeta, obsURI, renderRecord)

-- The uninformative error page
noDataPage :: Html
noDataPage =
  docTypeHtml $
    head (H.title "Welcome" <>
          defaultMeta
          )
    <>
    body
     (p "Hello world!" <>
      p ("Unfortunately there is no new observation found in my database, " <>
         "which likely means that something has gone wrong somewhere.")
     )

-- The uninformative landing page; TODO: avoid duplication with recordPage
introPage :: ObsInfo -> Html
introPage (ObsInfo currentObs mPrevObs mNextObs) =
  let initialize = "initialize()"

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
    head (H.title "What is Chandra doing?" <>
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
     (p "Hello world!" <>
      navLinks <>
      p "The current observation is:" <>
      renderRecord P.True currentObs <>
      p ("Information on " <> 
         (a ! href "http://burro.cwru.edu/Academics/Astr306/Coords/coords.html") "Astronomical coordinate systems" <>
         ".")
     )
