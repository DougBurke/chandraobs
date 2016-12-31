{-# LANGUAGE OverloadedStrings #-}

-- | The index page.

module Views.Index (introPage, noDataPage, noObsIdPage) where

-- import qualified Prelude as P
import Prelude (($), Bool(..), Maybe(..), const, either)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>), mempty)
import Data.Time (UTCTime)

import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (title)

import API (jsScript, cssLink)
import Layout (defaultMeta, jqueryMeta, renderLinks, renderFooter)
import Types (SimbadInfo, ScienceObs, Proposal, ObsInfo(..)
             , SortedList, StartTimeOrder)
import Views.Record (CurrentPage(..), renderStuff, twitterDiv
                    , mainNavBar, obsNavBar
                    , noObsIdParas)

noObsIdPage :: Html -> Html
noObsIdPage fact =
  docTypeHtml ! lang "en-US" $
    head (H.title "Unknown observation" <>
          defaultMeta <>
          (cssLink "/css/main.css" ! A.title "Default")
          )
    <>
    body
     (mainNavBar CPOther
      <> noObsIdDiv fact
      <> twitterDiv)
    <> renderFooter

noObsIdDiv :: Html -> Html
noObsIdDiv fact = (div ! id "mainBar") (noObsIdParas fact)

noDataPage :: Html -> Html
noDataPage fact =
  docTypeHtml ! lang "en-US" $
    head (H.title "What is Chandra doing? I am not sure!" <>
          defaultMeta <>
          (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    body
     (mainNavBar CPOther
      <> noDataDiv fact
      <> twitterDiv)
    <> renderFooter

noDataDiv :: Html -> Html
noDataDiv fact = (div ! id "mainBar") (noDataParas fact)

noDataParas :: Html -> Html
noDataParas fact = 
  (p ! class_ "error")
  ("Unfortunately there doesn't seem to be any observations in my database, " <>
   "which hopefully means that the database is being updated, so " <>
   "please wait a few minutes and try again. If there is still " <>
   "a problem, try reporting the problem to either " <>
   (a ! href "http://twitter.com/doug_burke" $ "@doug_burke") <>
   " or the " <>
   (a ! href "https://bitbucket.org/doug_burke/chandraobs/issues?status=new&status=open" $ "issue tracker") <>
   ". Whilst you are waiting, how about this fun Chandra fact:")
  <> (p ! class_ "fact") fact


tourElements :: Html
tourElements =
  jqueryMeta
  <> jsScript "/js/bootstrap-tour-standalone-0.9.3.min.js"
  <> (cssLink "/css/bootstrap-tour-standalone-0.9.3.min.css"
      ! A.title  "Default")
  <> jsScript "/js/tour.js"

-- | TODO: this should be merged with Views.Record.recordPage
introPage :: 
  UTCTime     -- current time
  -> ObsInfo 
  -> (Maybe SimbadInfo, (Maybe Proposal, SortedList StartTimeOrder ScienceObs))
  -- other observations in the proposal
  -> Html
introPage cTime oi@(ObsInfo currentObs _ _) dbInfo =
  let initialize = "initialize(); addTour();"

      (msimbad, (mprop, _)) = dbInfo
      imgLinks = either (const mempty) (renderLinks True mprop msimbad) currentObs

  in docTypeHtml ! lang "en-US" $
    head (H.title "What is Chandra doing now?" <>
          defaultMeta <>
          tourElements <>
          jsScript "/js/image-switch.js" <>
          jsScript "/js/main.js" <>
          (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    (body ! onload initialize)
     (mainNavBar CPIndex
      <> obsNavBar (Just currentObs) oi
      <> (div ! id "mainBar") 
         (renderStuff cTime currentObs dbInfo
          <> imgLinks)
      <> twitterDiv)
      <> renderFooter
