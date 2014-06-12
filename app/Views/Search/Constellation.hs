{-# LANGUAGE OverloadedStrings #-}

-- | Search on constellations.

module Views.Search.Constellation (matchPage) where

-- import qualified Prelude as P
import Prelude (($), (==), String, map, maybe, otherwise)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>), mconcat)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (ChandraTime(..), Schedule(..))
import Types (ConLong(..), ConShort(..), getConstellationName)
import Utils (defaultMeta, renderFooter)
import Views.Record (CurrentPage(..), mainNavBar)
import Views.Render (makeSchedule)

jsScript :: AttributeValue -> Html
jsScript uri = script ! src uri $ ""

-- TODO: combine with Schedule.schedPage

matchPage :: 
  ConShort
  -> Schedule  -- the observations that match this type, organized into a "schedule"
  -> Html
matchPage con sched =
  let lbl = maybe (fromConShort con) fromConLong $ getConstellationName con
  in docTypeHtml ! lang "en-US" $
    head (H.title ("Chandra observations in " <> H.toHtml lbl) <>
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
          (renderMatches lbl sched)
      <> renderFooter
     )

-- | TODO: combine table rendering with Views.Schedule
--
--   TODO: convert the long version of SimbadType to a nice string (may want to send in SimbadType here to match on)
renderMatches ::
  String           -- ^ Constellation name
  -> Schedule      -- ^ non-empty list of matches
  -> Html
renderMatches lbl (Schedule cTime _ done mdoing todo) = 
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo

      conLink cname =
        let clean c | c == ' '  = '_'
                    | c == '\246'  = 'o' -- o umlaut
                    | otherwise = c
            l = map clean cname
        in "http://www.astro.wisc.edu/~dolan/constellations/constellations/" <> (toValue l) <> ".html"

  in div ! A.id "scheduleBlock" $ do
    h2 $ toHtml lbl
    p  $ "The current time is: " <> toHtml (ChandraTime cTime) <> "."

    svgBlock

    -- TODO: improve English here
    p $ mconcat
        [ "This page shows Chandra observations of objects in the constellation "
        , (a ! href (conLink lbl)) (toHtml lbl)
        , " (since the database only includes a "
        , "small fraction of the mission you will only see a few "
        , "matches). The format is the same as used in the "
        , (a ! href "/schedule") "schedule view"
        , "."
        ]

    {-
    p $ mconcat 
        [ "This page shows ", toHtml ndays
        , " days of the Chandra schedule, centered on today. "
        , "The size of the circles indicate the exposure time, and "
        , "the color shows whether the observation has been done, "
        , "is running now, or is in the future; the same colors "
        , "are used in the table below. For repeated observations "
        , "it can be hard to make out what is going on, since the "
        , "circles overlap! "
        , "The points are plotted in the "
        , a ! href "http://en.wikipedia.org/wiki/Equatorial_coordinate_system#Use_in_astronomy" $ "Equatorial coordinate system"
        , ", using the "
        , a ! href "http://en.wikipedia.org/wiki/Aitoff_projection" $ "Aitoff projection"
        , ". See "
        , a ! href "http://burro.astr.cwru.edu/" $ "Chris Mihos'"
        , " page on "
        , a ! href "http://burro.cwru.edu/Academics/Astr306/Coords/coords.html" $ "Astronomical coordinate systems"
        , " for more informaion."
        ]
    -}

    tblBlock

