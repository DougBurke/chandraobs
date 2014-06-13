{-# LANGUAGE OverloadedStrings #-}

-- | Search on instrument.

module Views.Search.Instrument (matchPage) where

-- import qualified Prelude as P
import Prelude (($))

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>), mconcat)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (Schedule(..), Instrument(..))
import Utils (defaultMeta, renderFooter, jsScript, instLinkAbout)
import Views.Record (CurrentPage(..), mainNavBar)
import Views.Render (makeSchedule)

-- TODO: combine with Schedule.schedPage

matchPage :: 
  Instrument
  -> Schedule
  -> Html
matchPage inst sched =
  docTypeHtml ! lang "en-US" $
    head (H.title ("Chandra observations with " <> H.toHtml inst) <>
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
          (renderMatches inst sched)
      <> renderFooter
     )

-- | TODO: combine table rendering with Views.Schedule
--
renderMatches ::
  Instrument       
  -> Schedule      -- ^ non-empty list of matches
  -> Html
renderMatches inst (Schedule cTime _ done mdoing todo) = 
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo

  in div ! A.id "scheduleBlock" $ do
    h2 $ toHtml inst

    svgBlock

    p $ mconcat
        [ "This page shows observations of objects that use "
        , "the "
        , instLinkAbout inst
        , " instrument on Chandra "
        , "(since the database only includes a "
        , "small fraction of the mission you will only see a few "
        , "matches). The format is the same as used in the "
        , (a ! href "/schedule") "schedule view"
        , "."
        ]

    tblBlock

