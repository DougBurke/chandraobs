{-# LANGUAGE OverloadedStrings #-}

-- | The index page.

module Views.Index (introPage, noDataPage) where

-- import qualified Prelude as P
import Prelude (($), Bool(..), Maybe(..), const, either)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>), mconcat, mempty)
import Data.Time (UTCTime)

import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (ScienceObs, Proposal, ObsInfo(..))
import Utils (defaultMeta, renderLinks, renderFooter)
import Views.Record (CurrentPage(..), renderStuff, renderTwitter
                    , mainNavBar, obsNavBar)

noDataPage :: Html
noDataPage =
  docTypeHtml ! lang "en-US" $
    head (H.title "What is Chandra doing? I am not sure!" <>
          defaultMeta <>
          (script ! src "/js/tour.js") "" <>
           link ! href   "/css/main.css"
                ! type_  "text/css" 
                ! rel    "stylesheet"
                ! A.title  "Default"
                ! media  "all"
          )
    <>
    body
     (mainNavBar CPIndex
      <> (div ! class_ "error")  
        ("Unfortunately there is no new observation found in my database, " <>
         "which hopefully means that the database is being updated, so " <>
         "please wait a few minutes and try again. If there is still " <>
         "a problem, try contacting " <>
         (a ! href "http://twitter.com/doug_burke" $ "@doug_burke") <>
         " and reporting the problem.") -- TODO: add in an amazing fact...
      <> renderTwitter
      <> renderFooter
     )

tourElements :: Html
tourElements =
  mconcat [
    (script ! src "http://code.jquery.com/jquery-1.11.1.min.js") ""
    , (script ! src "/js/bootstrap-tour-standalone-0.9.3.min.js") ""
    , link ! href   "/css/bootstrap-tour-standalone-0.9.3.min.css"
           ! type_  "text/css" 
           ! rel    "stylesheet"
           ! A.title  "Default"
           ! media  "all"
    , (script ! src "/js/tour.js") ""
    ]

-- | TODO: this should be merged with Views.Record.recordPage
introPage :: 
  UTCTime     -- current time
  -> ObsInfo 
  -> (Maybe Proposal, [ScienceObs])  -- other observations in the proposal
  -> Html
introPage cTime oi@(ObsInfo currentObs _ _) propInfo =
  let initialize = "initialize(); addTour();"

      imgLinks = either (const mempty) (renderLinks True) currentObs

  in docTypeHtml ! lang "en-US" $
    head (H.title "What is Chandra doing now?" <>
          defaultMeta <>
          tourElements <>
          (script ! src "/js/main.js") "" <>
           link ! href   "/css/main.css"
                ! type_  "text/css" 
                ! rel    "stylesheet"
                ! A.title  "Default"
                ! media  "all"
          )
    <>
    (body ! onload initialize)
     (mainNavBar CPIndex
      <> obsNavBar (Just currentObs) oi
      <> (div ! id "mainBar") 
         (renderStuff cTime currentObs propInfo
          <> imgLinks)
      <> (div ! id "otherBar") renderTwitter)
      <> renderFooter
