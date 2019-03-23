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

import API (abstractLink
           , basicTypeLinkSearch
           , categoryLinkSearch)
import Layout (floatableTable)
import Types (SimbadType
             , PropCategory
             , RestrictedSchedule
             , unsafeToObsIdVal
             , simbadTypeToDesc)
import Utils (HtmlContext(StaticHtml)
             , toLink
             , extLink
             , getNumObsRestricted
             , getScienceTimeRestricted
             )
import Views.Record (CurrentPage(..))
import Views.Render (standardRestrictedSchedulePage
                    , standardExplorePage)

indexPage :: 
  [(PropCategory, Int)]
  -> Html
indexPage cats =
  let hdrTitle = "Chandra observations by category"
      css = Just "/css/category.css"
      bodyBlock = renderTypes cats
  in standardExplorePage css hdrTitle bodyBlock Nothing

     
matchPage :: 
  PropCategory
  -> RestrictedSchedule
  -- the observations that match this category, organized into a "schedule"
  -> Html
matchPage cat sched =
  let hdrTitle = "Chandra observations: category " <> H.toHtml cat
      scienceTime = getScienceTimeRestricted sched
      nobs = getNumObsRestricted sched
      (pageTitle, mainBlock) = renderMatches cat scienceTime nobs
  in standardRestrictedSchedulePage sched CPExplore hdrTitle pageTitle mainBlock


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
        td (categoryLinkSearch StaticHtml cat cat)
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
       <> toLink StaticHtml "/about/instruments.html"
       ("Chandra instrumentation" :: Html)
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
       <> toLink StaticHtml "/proposal/14400832" ("proposal link" :: Html)
       <> " for an observation, and then selecting the proposal title "
       <> "- in this example I chose "
       <> extLink StaticHtml (abstractLink (unsafeToObsIdVal 14662))
       ("An X-ray binary candidate with potential extended emission" :: Html)
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
  -> RestrictedSchedule
  -> Html
categoryAndTypePage cat mtype sched =
  let hdrTitle = "Chandra observations: " <> getComboTitle cat mtype
      (pageTitle, mainBlock) = renderComboMatches cat mtype sched
  in standardRestrictedSchedulePage sched CPExplore hdrTitle pageTitle mainBlock

-- | TODO: combine table rendering with Views.Schedule
--
renderMatches ::
  PropCategory     -- ^ Category name
  -> Html          -- ^ exposure time value for the schedule
  -> T.Text        -- ^ number of observations in the schedule
  -> (Html, Html)
renderMatches cat scienceTime nobs = 
  let -- TODO: improve English here
      matchBlock = p (
        "This page shows Chandra observations of objects from proposals "
        <> "in the category "
        <> toHtml cat
        <> scienceTime
        <> ". "
        -- assume the schedule is all science observations
        <> toHtml nobs
        <> ". The format is the same as used in the "
        <> toLink StaticHtml "/schedule" ("schedule view" :: Html)
        <> "."
        )
        
  in (toHtml cat, matchBlock)

renderComboMatches ::
  PropCategory           -- ^ Category name
  -> Maybe SimbadType
  -- ^ If Nothing then the unidentified matches.
  -> RestrictedSchedule      -- ^ list of matches, could be empty
  -> (Html, Html)
renderComboMatches cat mtype sched = 
  let scienceTime = getScienceTimeRestricted sched

      -- TODO: improve English here
      matchBlock = p (
        "This page shows Chandra observations of objects from proposals "
        <> "in the category "
        <> categoryLinkSearch StaticHtml cat cat
        <> " that observe targets with the SIMBAD type of "
        <> basicTypeLinkSearch mtype
        <> scienceTime
        <> ". "
        -- assume the schedule is all science observations
        <> toHtml (getNumObsRestricted sched)
        <> ". The format is the same as used in the "
        <> toLink StaticHtml "/schedule" ("schedule view" :: Html)
        <> "."
        )

  in (getComboTitle cat mtype, matchBlock)
