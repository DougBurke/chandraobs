{-# LANGUAGE OverloadedStrings #-}

-- | Search on cycle.

module Views.Search.Cycle (indexPage, matchPage) where

import qualified Prelude as P
import Prelude (Int, ($), (==),
                compare, fst, mapM_)

-- import qualified Data.Text as T

import qualified Text.Blaze.Html5 as H

import Data.Function (on)
import Data.List (sortBy)
import Data.Monoid ((<>))

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import API (cycleLinkSearch)
import Layout (standardTable)
import Types (RestrictedSchedule
             , Cycle, fromCycle, allCycles)
import Utils (getNumObsRestricted
             , getScienceTimeRestricted)
import Views.Record (CurrentPage(..))
import Views.Render (standardRestrictedSchedulePage
                    , standardExplorePage)

indexPage :: 
  [(Cycle, Int)]
  -> Html
indexPage cycles =
  let cssPage = P.Nothing
      bodyBlock = renderList cycles
      mid = P.Just "explorebox"
  in standardExplorePage cssPage
     "Chandra observations by Cycle" bodyBlock mid

     
matchPage :: 
  Cycle
  -> RestrictedSchedule
  -> Html
matchPage cycle sched =
  let cycleHtml = if cycle == allCycles
                  then "all proposal cycles"
                  else "Cycle " <> H.toHtml (fromCycle cycle)
      hdrTitle = "Chandra observations: " <> cycleHtml
      pageTitle = "Observations in " <> cycleHtml

      mainBlock = renderMatches cycle sched
      
  in standardRestrictedSchedulePage sched CPExplore hdrTitle pageTitle mainBlock


-- | TODO: add some text explaining what a cycle is, or at least link to
--         somewhere on the site explaining it.
--
renderMatches ::
  Cycle            -- ^ Cycle
  -> RestrictedSchedule
  -> Html
renderMatches cycle sched = 
  let scienceTime = getScienceTimeRestricted sched

      lbl = if cycle == allCycles
            then "all proposal cycles"
            else "proposal Cycle " <> toHtml (fromCycle cycle)

  in p ("This page shows Chandra observations from "
        <> lbl
        <> scienceTime
        <> ". "
        -- assume the schedule is all science observations
        <> toHtml (getNumObsRestricted sched)
        <> ". The format is the same as used in the "
        <> (a ! href "/schedule") "schedule view"
        <> ".")


-- | Render the cycle information
--
renderList ::
  [(Cycle, Int)]
  -- ^ The number of proposals in the cycle.
  -> Html
renderList cycles = 
  let toRow (cycle, n) =
        tr $ do
          td (cycleLinkSearch cycle)
          td (toHtml n)

      scycles = sortBy (compare `on` fst) cycles

  in div $ do

    p ("The amount of time varies in a cycle due to changes in "
       <> "Chandra's orbit (some years have more time available) "
       <> "as well as for more mundane reasons, such as Cycle 0 "
       <> "only covering a short period of time as the Chandra X-ray "
       <> "Center gathered experience in the in-orbit performance "
       <> "of the telescope and instruments.")

    standardTable $ do
             thead $ tr $ do
               th "Cycle"
               th "Number of proposals in the cycle"
             tbody (mapM_ toRow scycles)

