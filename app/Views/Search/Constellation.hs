{-# LANGUAGE OverloadedStrings #-}

-- | Search on constellations.

module Views.Search.Constellation (indexPage, matchPage) where

import qualified Prelude as P
import Prelude (($), (==), String,
                compare, fst, length, lookup, map, mapM_,
                show, otherwise)

-- import qualified Data.Aeson as Aeson
-- import qualified Data.ByteString.Lazy.Char8 as LB8
-- import qualified Data.Text as T

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- import Data.Aeson ((.=))
import Data.Function (on)
-- import Data.Functor (void)
import Data.List (intercalate, sortBy)
import Data.Maybe (isNothing)
import Data.Monoid ((<>), mconcat)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (Schedule(..), ConShort(..), ConLong(..), TimeKS(..)
              , getConstellationNameStr
              , constellationMap
              , getConstellationNameStr
              , showExpTime)
import Utils (defaultMeta, skymapMeta, renderFooter, cssLink
             , constellationLinkSearch
             , getNumObs
             , getScienceTime)
import Views.Record (CurrentPage(..), mainNavBar)
import Views.Render (makeSchedule)

indexPage :: 
  [(ConShort, TimeKS)]
  -> Html
indexPage cons =
  docTypeHtml ! lang "en-US" $
    head (H.title "Chandra observations"
          <> defaultMeta
          -- <> skymapMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          <> cssLink "/css/constellation.css"
          )
    <>
    -- (body ! onload "showConstellations(coninfo);")
    body
     (mainNavBar CPExplore
      <> (div ! id "schedule") 
          (renderList cons)
      <> renderFooter
     )

-- TODO: combine with Schedule.schedPage

matchPage :: 
  ConShort
  -> Schedule
  -- ^ the observations that match this type, organized into a "schedule"
  -> Html
matchPage con sched =
  let lbl = getConstellationNameStr con
      -- unlike other pages, need to send in additional information
      jsLoad = "conInfo = {'shortName': '"
               <> stringValue (fromConShort con)
               <> "', 'longName': '"
               <> stringValue (getConstellationNameStr con)
               <> "'}; "
               <> "createMap(obsinfo, conInfo);"
  in docTypeHtml ! lang "en-US" $
    head (H.title ("Chandra observations in " <> H.toHtml lbl) <>
          defaultMeta
          <> skymapMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    (body ! onload jsLoad)
     (mainNavBar CPExplore
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
renderMatches lbl (Schedule cTime _ done mdoing todo simbad) = 
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo simbad
      scienceTime = getScienceTime done mdoing todo

      conLink cname =
        let clean c | c == ' '  = '_'
                    | c == '\246'  = 'o' -- o umlaut
                    | otherwise = c
            l = map clean cname
        in mconcat
           [ "http://www.astro.wisc.edu/~dolan/constellations/constellations/"
           , toValue l
           , ".html"
           ]

  in div ! A.id "scheduleBlock" $ do
    h2 $ toHtml lbl

    svgBlock

    -- TODO: improve English here
    p $ mconcat
        [ "This page shows Chandra observations of objects in the constellation "
        , (a ! href (conLink lbl)) (toHtml lbl)
        , scienceTime
        , ". The constellation outline is also shown; the outlines "
        , "were taken from the "
        , (a ! href "https://github.com/ofrohn/d3-celestial/") "d3-celestial"
        , " project by Olaf Frohn. "
          -- assume the schedule is all science observations
        , toHtml (getNumObs done mdoing todo)
        , ". The format is the same as used in the "
        , (a ! href "/schedule") "schedule view"
        , "."
        ]

    tblBlock

-- | Render the list of constellations
renderList ::
  [(ConShort, TimeKS)]
  -> Html
renderList cons = 
  let toRow (con,t) =
        tr $ do
          td (constellationLinkSearch con (conLabel con))
          td (toHtml (showExpTime t))

      conLabel = getConstellationNameStr

      scons = sortBy (compare `on` fst) cons

      -- the assumption is that there's enough data that few,
      -- if any, constellations are missing.
      --
      notIn t = isNothing (lookup t cons)
      missing = [fromConLong clong |
                 (cshort, clong) <- constellationMap, notIn cshort]
                 
      missTxt = case length missing of
        0 -> "and has a target in each constellation"
        1 -> "and is missing one constellation, namely "
             <> toHtml (P.head missing)
        nm -> "and is missing " <> toHtml (show nm)
              <> " constellations: "
              <> toHtml (intercalate "," missing)

      {-
      dataRow (cs, texp) =
        T.pack (fromConShort cs) .= 
          Aeson.object [ "id" .= fromConShort cs
                       , "label" .= getConstellationNameStr cs
                       , "tks" .= _toKS texp ]

      conMap = Aeson.object (P.map dataRow scons)
      -}
      
  in div $ do

    p ("As can be seen, the length of time spent observing targets "
       <> "in a constellation varies strongly with "
       <> "the constellation. This data base only contains a "
       <> (a ! href "/search/calendar/") "small fraction"
       <> " of the output of Chandra, "
       <> missTxt
       <> "."
      )

    -- (div ! id "constellationMap") ""
    
    (table ! class_ "floatable") $ do
             thead $ tr $ do
               th "Constellation"
               th "Observing time"
             tbody (mapM_ toRow scons)

    {-
    To do this, need to work out how to get filled constellation
    outlines.
    script ! type_ "text/javascript" $ do
      void "var coninfo = "
      toHtml (LB8.unpack (Aeson.encode conMap))
      ";"
    -}
