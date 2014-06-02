{-# LANGUAGE OverloadedStrings #-}

-- | The index page.

module Views.Index (introPage, noDataPage) where

-- import qualified Prelude as P
import Prelude (($), Bool(..), Maybe(..))

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>), mconcat)
import Data.Time (UTCTime)

import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (title)

import Utils (ObsInfo(..), defaultMeta, renderLinks)
import PersistentTypes
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
         "which likely means that something has gone wrong somewhere.")
      <> renderTwitter
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

introPage :: 
  UTCTime     -- current time
  -> ObsInfo 
  -> [Record]  -- records with the same sequence number
  -> Html
introPage cTime oi@(ObsInfo currentObs _ _) matches =
  let initialize = "initialize(); addTour();"

      {-
           p ("Information on " <> 
              (a ! href "http://burro.cwru.edu/Academics/Astr306/Coords/coords.html") "Astronomical coordinate systems" <>
             ".")
      -}

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
         (renderStuff cTime currentObs matches
          <> renderLinks True currentObs)
      <> (div ! id "otherBar") renderTwitter)
