{-# LANGUAGE OverloadedStrings #-}

-- | Search on TOO, discuss scheduling constraints.

module Views.Search.TOO (indexPage, matchPage) where

-- import qualified Prelude as P
import Prelude (Maybe(..), ($), (.), (>>), compare, fst, mapM_)

import qualified Text.Blaze.Html5 as H

import Control.Arrow (first)
import Data.Function (on)
import Data.List (sortBy)
import Data.Monoid ((<>))

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import API (tooLinkSearch)
import Types (Schedule, TimeKS, TOORequestTime(..), rtToLabel, showExpTime)
import Utils (getScienceTime
             , dquote, standardTable
             )
import Views.Record (CurrentPage(..))
import Views.Render (standardSchedulePage
                     , standardExplorePage)

indexPage :: 
  [(TOORequestTime, TimeKS)]
  -- ^ The TOO categories
  -> TimeKS
  -- ^ Time for the "no TOO" category
  -> Html
indexPage toos noneTime =
  let hdrTitle = "Chandra observations by turnaround time"
      bodyBlock = renderTOOs toos noneTime
      mid = Just "explorebox"
  in standardExplorePage Nothing hdrTitle bodyBlock mid


-- | Render the results for a single class of TOOs.
matchPage :: 
  Maybe TOORequestTime
  -> Schedule
  -> Html
matchPage mtoo sched =
  let hdrTitle = "Chandra observations: " <> lbl
      lbl = H.toHtml (case mtoo of
                       Just too -> rtToLabel too <> " turnaround time"
                       Nothing -> "No constraint")

      mainBlock = renderMatches mtoo sched
      
  in standardSchedulePage sched CPExplore hdrTitle lbl mainBlock


proplink :: H.Html
proplink =
  (a ! href "/search/proptype/")
  "Target of Opportunity (TOO) or Director's Discretionary Time (DDT)"

-- | Render the list of TOOs.
--
renderTOOs ::
  [(TOORequestTime, TimeKS)]
  -> TimeKS
  -- ^ How much time is in the "no TOO" category
  -> Html
renderTOOs objs noneTime= 
  let toRow (mtoo, ts) = tr $ do
        td (tooLinkSearch mtoo)
        td (toHtml (showExpTime ts))

      sobjs = sortBy (compare `on` fst) objs

  in div $ do
    h2 "Proposal turnaround time for TOO and DDT observations"
    
    p ("For most objects observed by Chandra it does not matter "
       <> "when it is observed - perhaps because it varies so slowly, "
       -- TODO: use a wrapper
       <> (a ! href "/search/constraints/") "predictably"
       <> " (such as the "
       <> (a ! href "http://chandra.si.edu/press/13_releases/press_072913.html")
       "occultation of a star by one of its planets"
       <> "), or even unpredictably - but for some Science "
       <> "goals, it is vital that an observation be performed at certain "
       <> "times. Examples of when time is of the essence include studying the "
       <> (a ! href "https://en.wikipedia.org/wiki/Gamma-ray_burst#Afterglow")
       "afterglow of a Gamma-Ray burst"
       <> " or the unexpected behavior of the "
       <> (a ! href "http://chandra.si.edu/press/15_releases/press_010515.html")
       "black hole in the center of our Galaxy"
       <> ". In these cases, Astronomers will have written a proposal to "
       <> "study such a case, either a "
       <> proplink
       <> " proposal, and then they wait (the trick is therefore being able to "
       <> "detect the source in some way, either from its behavior in other "
       <> "wave bands or it being observed by a different X-Ray satellite). "
       <> "Once they have a target, they inform the Chandra X-ray Center "
       <> "about how quickly they need the observation (or observations) to happen "
       <> "(this is referred to as the turnaround time in the table below). "
       <> "The times correspond roughly to: less than a week (Immediate), "
       <> "one to two weeks (Quick), two weeks to a month (Intermediate), "
       <> "more than a month (Slow), and the None category is for those "
       <> "observations with no such constraint."
       )

    p ("The reason that the turnaround time is important is because the "
       <> "Chandra schedule is created in two-week blocks, so that any "
       <> "change requires a re-plan, and is a time-consuming task. "
       <> "It is also complicated by the fact that the Chandra X-ray Center "
       <> "is not in constant contact with the satellite - this differs to "
       <> "the "
       <> (a ! href "http://www.esa.int/Our_Activities/Operations/XMM-Newton_operations")
       "XMM-Newton satellite"
       <> ", for instance - and so there is often a minimum delay before an "
       <> "observation can be uploaded to Chandra" <> sup "1" <> "."
      )

    standardTable $ do
             thead $ tr $ do
               th "Turnaround time"
               th "Exposure time"
             tbody
               (mapM_ (toRow . first Just) sobjs
                >> toRow (Nothing, noneTime)
               )

    (p ! class_ "footnote")
      (sup "1" <> " NASA communicates with its satellites using the "
       <> "Deep Space Network, and has a really cool site showing "
       <> "a real-time view of "
       <> (a ! href "https://eyes.nasa.gov/dsn/dsn.html")
       "what missions are using the DSN"
       <> "."
      )

explain :: Maybe TOORequestTime -> H.Html -> H.Html
explain Nothing scienceTime =
  "This is a view of those observations which had no "
  <> dquote "turnaround time"
  <> scienceTime
  <> ". Just because "
  <> "they had no constraints does not mean that they weren't "
  <> "time critical - that is, the observations had to be done "
  <> "at certain times - it just means that they were not a "
  <> proplink
  <> " observation."

explain (Just Immediate) scienceTime =
  "These observations have a turn-around time of less than a week"
  <> scienceTime
  <> "."

explain (Just Quick) scienceTime =
  "These observations have a turn-around time between one and "
  <> "two weeks"
  <> scienceTime
  <> "."

explain (Just Intermediate) scienceTime =
  "These observations have a turn-around time between two weeks and "
  <> "one month"
  <> scienceTime
  <> "."

explain (Just Slow) scienceTime =
  "These observations have a turn-around time of more than a month"
  <> scienceTime
  <> "."

-- | TODO: combine table rendering with Views.Schedule
--
renderMatches ::
  Maybe TOORequestTime
  -> Schedule          -- ^ non-empty list of matches
  -> Html
renderMatches mtoo sched = 
  let scienceTime = getScienceTime sched
  in p (explain mtoo scienceTime)

