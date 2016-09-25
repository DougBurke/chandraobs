{-# LANGUAGE OverloadedStrings #-}

-- | Search on proposal categories.

module Views.Search.Category (indexPage
                             , matchPage
                             , categoryAndTypePage
                             ) where

-- import qualified Prelude as P
import Prelude (($), Maybe(..), Int
               , compare, fst, mapM_)

import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.List (sortBy)
import Data.Function (on)
import Data.Monoid ((<>))

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (Schedule, ObsIdVal(..), SimbadType
             , PropCategory
             , simbadTypeToDesc)
import Utils (defaultMeta, renderFooter, cssLink
             , abstractLink
             , basicTypeLinkSearch
             , categoryLinkSearch
             , getNumObs
             , getScienceTime
             , floatableTable
             )
import Views.Record (CurrentPage(..), mainNavBar)
import Views.Render (standardSchedulePage)

indexPage :: 
  [(PropCategory, Int)]
  -> Html
indexPage cats =
  docTypeHtml ! lang "en-US" $
    head (H.title "Chandra observations by category" <>
          defaultMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          <> cssLink "/css/category.css"
          )
    <>
    body
     (mainNavBar CPExplore
      <> (div ! id "schedule") 
          (renderTypes cats)
      <> renderFooter
     )

matchPage :: 
  PropCategory
  -> Schedule
  -- the observations that match this category, organized into a "schedule"
  -> Html
matchPage cat sched =
  let hdrTitle = "Chandra observations: category " <> H.toHtml cat
      (pageTitle, mainBlock) = renderMatches cat sched
  in standardSchedulePage sched CPExplore hdrTitle pageTitle mainBlock


-- | Render the list of categories.
--
--   The input list is probably sorted already, given the
--   way it is constructed, but at the moment do not enforce
--   this in the types.
--
renderTypes ::
  [(PropCategory, Int)]
  -> Html
renderTypes cats = 
  let toRow (cat, n) = tr $ do
        td (categoryLinkSearch cat cat)
        (td ! A.title (toValue lbl)) (toHtml n)

      lbl = "Number of proposals" :: T.Text

      scats = sortBy (compare `on` fst) cats
  in div $ do
    p ("The Chandra X-ray Centre has a yearly proposal deadline, "
       <> "where Astronomers submit their desired targets, their "
       <> "reasons for making the observations - in other words, "
       <> "what Science do we think we'll learn from the "
       <> "data" <> sup "1" <> " - and a technical justification, "
       <> "in order to justify the observation length and choice "
       <> "of "
       <> (a ! href "/about/instruments.html")
       "Chandra instrumentation"
       <> ". The Astronomer choses which category best fits her "
       <> "science case, and submits the proposal; it is this "
       <> "category that is shown below. There is often a choice "
       <> "of category, since a proposal can attempt to answer "
       <> "many questions, so it is only a rough guide to what "
       <> "Chandra is being asked to study."
      )

    p ("Once the proposal deadline has passed, the staff at the "
       <> "Chandra X-ray Center swing into action, setting up the "
       <> "review panels - inviting experts from around the world "
       <> "to come to a hotel near the Boston Airport, sit in "
       <> "window-less rooms for several days, and come up with a "
       <> "ranked set of proposals. This is not quite the end of the "
       <> "story, since the proposal list has to go through a final "
       <> "check before it can finally be released to the community. "
       <> "The over-subscription rate is high enough that only around "
       <> "twenty percent of the proposed observations make it through, "
       <> "so there are often many more unhappy than happy Astronomers "
       <> "when the results are announced! The abstracts for successful "
       <> "proposals can be read by following the "
       <> (a ! href "/proposal/14400832") "proposal link"
       <> " for an observation, and then selecting the proposal title "
       <> "- in this example I chose "
       <> (a ! href (abstractLink (ObsIdVal 14662)))
       "An X-ray binary candidate with potential extended emission"
       <> "."
      )

    -- hr

    floatableTable $ do
             thead $ tr $ do
               th "Category"
               th (toHtml lbl)
             tbody (mapM_ toRow scats)

    -- TODO: there should be a link back/forth between the subscipt
    --       text and the caller, but that's for later
    (p ! class_ "footnote")
      (sup "1" <> " Since Chandra data eventually becomes public - "
       <> "normally a year after the data was taken, but sometimes much "
       <> "sooner - and most observations contain many X-ray emitting "
       <> "sources, the data from a proposal can often end up producing "
       <> "more results from Astronomers using the data for Archival "
       <> "studies than come from the original proposal. In fact, "
       <> "Astronomers can ask to analyze archival data as part of the "
       <> "proposal process."
       )


getSimbadHtml :: Maybe SimbadType -> Html
getSimbadHtml Nothing = "Unidentified"
getSimbadHtml (Just s) =
  case simbadTypeToDesc s of
    Just l -> H.toHtml l
    _ -> "Unknown"

getComboTitle :: PropCategory -> Maybe SimbadType -> Html
getComboTitle cat mtype = 
  H.toHtml cat <> " → " <> getSimbadHtml mtype

categoryAndTypePage :: 
  PropCategory  -- ^ propoal category
  -> Maybe SimbadType
  -- ^ If Nothing, the it is the Unidentified category.
  -> Schedule
  -> Html
categoryAndTypePage cat mtype sched =
  let hdrTitle = "Chandra observations: " <> getComboTitle cat mtype
      (pageTitle, mainBlock) = renderComboMatches cat mtype sched
  in standardSchedulePage sched CPExplore hdrTitle pageTitle mainBlock

-- | TODO: combine table rendering with Views.Schedule
--
renderMatches ::
  PropCategory     -- ^ Category name
  -> Schedule      -- ^ non-empty list of matches
  -> (Html, Html)
renderMatches cat sched = 
  let scienceTime = getScienceTime sched

      -- TODO: improve English here
      matchBlock = p (
        "This page shows Chandra observations of objects from proposals "
        <> "in the category "
        <> toHtml cat
        <> scienceTime
        <> ". "
        -- assume the schedule is all science observations
        <> toHtml (getNumObs sched)
        <> ". The format is the same as used in the "
        <> (a ! href "/schedule") "schedule view"
        <> "."
        )
        
  in (toHtml cat, matchBlock)

renderComboMatches ::
  PropCategory           -- ^ Category name
  -> Maybe SimbadType
  -- ^ If Nothing then the unidentified matches.
  -> Schedule      -- ^ non-empty list of matches
  -> (Html, Html)
renderComboMatches cat mtype sched = 
  let scienceTime = getScienceTime sched

      -- TODO: improve English here
      matchBlock = p (
        "This page shows Chandra observations of objects from proposals "
        <> "in the category "
        <> categoryLinkSearch cat cat
        <> " that observe targets with the SIMBAD type of "
        <> basicTypeLinkSearch mtype
        <> scienceTime
        <> ". "
        -- assume the schedule is all science observations
        <> toHtml (getNumObs sched)
        <> ". The format is the same as used in the "
        <> (a ! href "/schedule") "schedule view"
        <> "."
        )

  in (getComboTitle cat mtype, matchBlock)
