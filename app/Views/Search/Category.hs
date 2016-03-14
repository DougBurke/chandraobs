{-# LANGUAGE OverloadedStrings #-}

-- | Search on proposal categories.

module Views.Search.Category (indexPage, matchPage) where

-- import qualified Prelude as P
import Prelude (($), (++), Int, String, compare, fst, length, mapM_, show)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.List (sortBy)
import Data.Function (on)
import Data.Monoid ((<>), mconcat)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (Schedule(..))
import Utils (defaultMeta, skymapMeta, renderFooter, cssLink
             , categoryLinkSearch, getNumObs)
import Views.Record (CurrentPage(..), mainNavBar)
import Views.Render (makeSchedule)

indexPage :: 
  [(String, Int)]
  -> Html
indexPage cats =
  docTypeHtml ! lang "en-US" $
    head (H.title "Chandra observations by category" <>
          defaultMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    body
     (mainNavBar CPOther
      <> (div ! id "schedule") 
          (renderTypes cats)
      <> renderFooter
     )

-- TODO: combine with Schedule.schedPage

matchPage :: 
  String
  -> Schedule  -- the observations that match this category, organized into a "schedule"
  -> Html
matchPage cat sched =
  docTypeHtml ! lang "en-US" $
    head (H.title ("Chandra observations: category " <> H.toHtml cat) <>
          defaultMeta
          <> skymapMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
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
renderMatches cat (Schedule cTime _ done mdoing todo simbad) = 
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo simbad
      
  in div ! A.id "scheduleBlock" $ do
    h2 $ toHtml cat

    svgBlock

    -- TODO: improve English here
    p $ mconcat
        [ "This page shows Chandra observations of objects from proposals "
        , "in the category "
        , toHtml cat
        , ". "
          -- assume the schedule is all science observations
        , toHtml (getNumObs done mdoing todo)
        , ". The format is the same as used in the "
        , (a ! href "/schedule") "schedule view"
        , "."
        ]

    tblBlock

-- | Render the list of categories.
--
--   The input list is probably sorted already, given the
--   way it is constructed, but at the moment do not enforce
--   this in the types.
--
renderTypes ::
  [(String, Int)]
  -> Html
renderTypes cats = 
  let toRow (cat, n) = tr $ do
        td (categoryLinkSearch cat cat)
        td (toHtml n)

      scats = sortBy (compare `on` fst) cats
  in div $ do
    p (toHtml ("There are " ++ show (length cats) ++ " categories."))
    table $ do
             thead $ tr $ do
               th "Category"
               th "Number of proposals"
             tbody (mapM_ toRow scats)
             
