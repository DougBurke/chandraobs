{-# LANGUAGE OverloadedStrings #-}

-- | Search on constellations.

module Views.Search.Constellation (indexPage, matchPage) where

-- import qualified Prelude as P
import Prelude (($), (==), (++), Int, String, compare, fst, length, map, mapM_, show, otherwise)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Function (on)
import Data.List (sortBy)
import Data.Monoid ((<>), mconcat)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (Schedule(..), ConShort(..), getConstellationNameStr)
import Utils (defaultMeta, skymapMeta, renderFooter, cssLink, constellationLinkSearch)
import Views.Record (CurrentPage(..), mainNavBar)
import Views.Render (makeSchedule)

indexPage :: 
  [(ConShort, Int)]
  -> Html
indexPage cons =
  docTypeHtml ! lang "en-US" $
    head (H.title "Chandra observations" <>
          defaultMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    body
     (mainNavBar CPOther
      <> (div ! id "schedule") 
          (renderTypes cons)
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
        , ", and the constellation outline is also shown (these outlines "
        , "were taken from the "
        , (a ! href "https://github.com/ofrohn/d3-celestial/") "d3-celestial"
        , " project by Olaf Frohn). "
        , "Since the database only includes a "
        , "small fraction of the mission you will only see a few "
        , "matches. The format is the same as used in the "
        , (a ! href "/schedule") "schedule view"
        , "."
        ]

    tblBlock

-- | Render the list of constellations
renderTypes ::
  [(ConShort, Int)]
  -> Html
renderTypes cons = 
  let toRow (con,n) = tr $ do
                    td $ constellationLinkSearch con (conLabel con)
                    td $ toHtml n

      conLabel = getConstellationNameStr

      scons = sortBy (compare `on` fst) cons
  in div $ do
    p $ toHtml $ "There are " ++ show (length cons) ++ " constellations with Chandra observations."
    table $ do
             thead $ tr $ do
               th "Constellation"
               th "Number"
             tbody $ mapM_ toRow scons
             
