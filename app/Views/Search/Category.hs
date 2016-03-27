{-# LANGUAGE OverloadedStrings #-}

-- | Search on proposal categories.

module Views.Search.Category (indexPage
                             , matchPage
                             , categoryAndTypePage
                             ) where

-- import qualified Prelude as P
import Prelude (($), (++), Maybe(..), Int, String
               , compare, fst, length, mapM_, show)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.List (sortBy)
import Data.Function (on)
import Data.Monoid ((<>), mconcat)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (Schedule(..), SimbadType
             , simbadTypeToDesc)
import Utils (defaultMeta, skymapMeta, renderFooter, cssLink
             , basicTypeLinkSearch
             , categoryLinkSearch
             , getNumObs)
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
     (mainNavBar CPExplore
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
     (mainNavBar CPExplore
      <> (div ! id "schedule") (renderMatches cat sched)
      <> renderFooter
     )

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
             

getSimbadHtml :: Maybe SimbadType -> Html
getSimbadHtml Nothing = "Unidentified"
getSimbadHtml (Just s) =
  case simbadTypeToDesc s of
    Just l -> H.toHtml l
    _ -> "Unknown"

getComboTitle :: String -> Maybe SimbadType -> Html
getComboTitle cat mtype = 
  H.toHtml cat <> " → " <> getSimbadHtml mtype

-- TODO: 
categoryAndTypePage :: 
  String  -- ^ propoal category
  -> Maybe SimbadType
  -- ^ If Nothing, the it is the Unidentified category.
  -> Schedule
  -> Html
categoryAndTypePage cat mtype sched =
  docTypeHtml ! lang "en-US" $
    head (H.title ("Chandra observations: "
                   <> getComboTitle cat mtype
                  )
          <> defaultMeta
          <> skymapMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    (body ! onload "createMap(obsinfo);")
     (mainNavBar CPExplore
      <> (div ! id "schedule") (renderComboMatches cat mtype sched)
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

renderComboMatches ::
  String           -- ^ Category name
  -> Maybe SimbadType
  -- ^ If Nothing then the unidentified matches.
  -> Schedule      -- ^ non-empty list of matches
  -> Html
renderComboMatches cat mtype (Schedule cTime _ done mdoing todo simbad) = 
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo simbad

  in div ! A.id "scheduleBlock" $ do
    h2 (getComboTitle cat mtype)

    svgBlock

    -- TODO: improve English here
    p $ mconcat
        [ "This page shows Chandra observations of objects from proposals "
        , "in the category "
        , categoryLinkSearch cat cat
        , " that also have the SIMBAD type of "
        , basicTypeLinkSearch mtype
        , ". "
          -- assume the schedule is all science observations
        , toHtml (getNumObs done mdoing todo)
        , ". The format is the same as used in the "
        , (a ! href "/schedule") "schedule view"
        , "."
        ]

    tblBlock

