{-# LANGUAGE OverloadedStrings #-}

-- | Search on proposal categories.

module Views.Search.Category (matchPage) where

-- import qualified Prelude as P
import Prelude (($), String)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>), mconcat)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (ChandraTime(..), Schedule(..))
import Utils (defaultMeta, renderFooter)
import Views.Record (CurrentPage(..), mainNavBar)
import Views.Render (makeSchedule)

jsScript :: AttributeValue -> Html
jsScript uri = script ! src uri $ ""

-- TODO: combine with Schedule.schedPage

matchPage :: 
  String
  -> Schedule  -- the observations that match this category, organized into a "schedule"
  -> Html
matchPage cat sched =
  docTypeHtml ! lang "en-US" $
    head (H.title ("Chandra observations: category " <> H.toHtml cat) <>
          defaultMeta
          <> jsScript "http://code.jquery.com/jquery-1.11.1.min.js"
          <> jsScript "http://d3js.org/d3.v3.min.js"
          <> jsScript "http://d3js.org/d3.geo.projection.v0.min.js"
          <> jsScript "/js/jquery.tablesorter.min.js"
          <> jsScript "/js/table.js"
          <> jsScript "/js/projection.js"
          <> link ! href   "/css/tablesorter.css"
               ! type_  "text/css" 
               ! rel    "stylesheet"
               -- ! A.title  "Default (TableSorter)"
               ! media  "all"
          <> link ! href   "/css/schedule.css"
               ! type_  "text/css" 
               ! rel    "stylesheet"
               -- ! A.title  "Default (TableSorter)"
               ! media  "all"
          <> link ! href   "/css/main.css"
                ! type_  "text/css" 
                ! rel    "stylesheet"
                ! A.title  "Default"
                ! media  "all"
          )
    <>
    (body ! onload "createMap(obsinfo);")
     (mainNavBar CPOther
      <> (div ! id "schedule") 
          (renderMatches cat sched)
      <> renderFooter
     )

-- | TODO: combine table rendering with Views.Schedule
--
renderMatches ::
  String           -- ^ Category name
  -> Schedule      -- ^ non-empty list of matches
  -> Html
renderMatches cat (Schedule cTime _ done mdoing todo) = 
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo

  in div ! A.id "scheduleBlock" $ do
    h2 $ toHtml cat
    p  $ "The current time is: " <> toHtml (ChandraTime cTime) <> "."

    svgBlock

    -- TODO: improve English here
    p $ mconcat
        [ "This page shows Chandra observations of objects from proposals "
        , "in the category "
        , toHtml cat
        , " (since the database only includes a "
        , "small fraction of the mission you will only see a few "
        , "matches). The format is the same as used in the "
        , (a ! href "/schedule") "schedule view"
        , "."
        ]

    tblBlock

