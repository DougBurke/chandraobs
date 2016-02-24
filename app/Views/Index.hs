{-# LANGUAGE OverloadedStrings #-}

-- | The index page.

module Views.Index (introPage, noDataPage) where

-- import qualified Prelude as P
import Prelude (($), Bool(..), Maybe(..), const, either, fst, snd)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>), mempty)
import Data.Time (UTCTime)

import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (SimbadInfo, ScienceObs, Proposal, ObsInfo(..))
import Utils (defaultMeta, jsScript, cssLink, renderLinks, renderFooter)
import Views.Record (CurrentPage(..), renderStuff, renderTwitter
                    , mainNavBar, obsNavBar)

noDataPage :: Html -> Html
noDataPage fact =
  docTypeHtml ! lang "en-US" $
    head (H.title "What is Chandra doing? I am not sure!" <>
          defaultMeta <>
          jsScript "/js/tour.js" <>
          (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    body
     (mainNavBar CPIndex
      <> (div ! id "mainBar") (
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
          )
      <> (div ! id "otherBar") renderTwitter)
    <> renderFooter

tourElements :: Html
tourElements =
  jsScript "https://code.jquery.com/jquery-1.11.1.min.js"
  <> jsScript "/js/bootstrap-tour-standalone-0.9.3.min.js"
  <> (cssLink "/css/bootstrap-tour-standalone-0.9.3.min.css" ! A.title  "Default")
  <> jsScript "/js/tour.js"

-- | TODO: this should be merged with Views.Record.recordPage
introPage :: 
  UTCTime     -- current time
  -> ObsInfo 
  -> (Maybe SimbadInfo, (Maybe Proposal, [ScienceObs]))  -- other observations in the proposal
  -> Html
introPage cTime oi@(ObsInfo currentObs _ _) dbInfo =
  let initialize = "initialize(); addTour();"

      mprop = fst $ snd dbInfo
      imgLinks = either (const mempty) (renderLinks True mprop) currentObs

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
      <> (div ! id "otherBar") renderTwitter)
      <> renderFooter
