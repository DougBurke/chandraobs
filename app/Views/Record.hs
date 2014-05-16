{-# LANGUAGE OverloadedStrings #-}

-- | The record page.

module Views.Record (recordPage) where

import qualified Prelude as P
import Prelude (($))

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>))

import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (title)

import PersistentTypes
import Utils (ObsInfo(..), defaultMeta, navLinks, renderRecord)

-- The specific page for this observation. At present I have not
-- worked out how this interacts with the top-level page; i.e.
-- the current observation (i.e. should the current observation
-- be flagged as such when using this view?)
--
recordPage :: ObsInfo -> Html
recordPage oi@(ObsInfo currentObs _ _) =
  let initialize = "initialize()"

      obsName = recordObsname currentObs

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
      navLinks P.False oi <>
      -- p "The current observation is:" <>
      renderRecord P.False currentObs
     )
