{-# LANGUAGE OverloadedStrings #-}

-- | Search on SIMBAD object type.

module Views.Search.Types (indexPage, matchPage) where

-- import qualified Prelude as P
import Prelude ((.), ($), (++), Int, String, compare, fst, length, mapM_, show, snd, uncurry)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Function (on)
import Data.List (sortBy)
import Data.Monoid ((<>), mconcat)

import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (Schedule(..), SimbadType(..), SimbadTypeInfo)
import Utils (defaultMeta, skymapMeta, jqueryMeta, renderFooter, cssLink, typeLinkSearch)
import Views.Record (CurrentPage(..), mainNavBar)
import Views.Render (makeSchedule)

indexPage :: 
  [(SimbadTypeInfo, Int)]
  -> Html
indexPage objs =
  docTypeHtml ! lang "en-US" $
    head (H.title "Chandra observations by object type"
          <> defaultMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    body
     (mainNavBar CPOther
      <> (div ! id "schedule") 
          (renderTypes objs)
      <> renderFooter
     )

-- TODO: combine with Schedule.schedPage

matchPage :: 
  SimbadTypeInfo
  -> Schedule  -- the observations that match this type, organized into a "schedule"
  -> Html
matchPage typeInfo sched =
  let lbl = niceType typeInfo
  in docTypeHtml ! lang "en-US" $
    head (H.title ("Chandra observations of " <> H.toHtml lbl)
          <> defaultMeta
          <> jqueryMeta
          <> skymapMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    (body ! onload "createMap(obsinfo);")
     (mainNavBar CPOther
      <> (div ! id "schedule") 
          (renderMatches lbl sched)
      <> renderFooter
     )

niceType :: (SimbadType, String) -> String
niceType (SimbadType "reg", _) = "Area of the sky"
niceType (_, l) = l

-- | TODO: combine table rendering with Views.Schedule
--
--   TODO: convert the long version of SimbadType to a nice string (may want to send in SimbadType here to match on)
renderMatches ::
  String           -- ^ SIMBAD type, as a string
  -> Schedule      -- ^ non-empty list of matches
  -> Html
renderMatches lbl (Schedule cTime _ done mdoing todo) = 
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo

  in div ! A.id "scheduleBlock" $ do
    h2 $ toHtml lbl

    svgBlock

    -- TODO: improve English here
    p $ mconcat
        [ "This page shows the observations of ", toHtml lbl, " "
        , "objects by Chandra (since the database only includes a "
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

-- | Render the list of object types
renderTypes ::
  [(SimbadTypeInfo, Int)]
  -> Html
renderTypes objs = 
  let toRow (sti,n) = tr $ do
                        td $ uncurry typeLinkSearch sti
                        td $ toHtml n

      sobjs = sortBy (compare `on` (snd.fst)) objs
  in div $ do
    p $ do
      toHtml $ "There are " ++ show (length objs) ++ " object types. These types are taken from "
      a ! href "http://cds.u-strasbg.fr/cgi-bin/Otype?X" $ "the SIMBAD database"
      toHtml ("."::String)
    table $ do
             thead $ tr $ do
               th "Object Type"
               th "Number"
             tbody $
               mapM_ toRow sobjs
             
