{-# LANGUAGE OverloadedStrings #-}

-- | Search on joint missions.

module Views.Search.Mission (indexPage, matchPage)
       where

-- import qualified Prelude as P
import Prelude (Int, ($), compare, fst)

import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad (mapM_)

import Data.Function (on)
import Data.List (sortBy)
import Data.Monoid ((<>))

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (Schedule, JointMission
             , fromMission, fromMissionLong
             , fromMissionLongLink, fromMissionAboutLink)
import Utils (defaultMeta, renderFooter, cssLink
             , getNumObs
             , getScienceTime
             , floatableTable
             )
import Views.Record (CurrentPage(..), mainNavBar)
import Views.Render (standardSchedulePage)

indexPage :: 
  [(JointMission, Int)]
  -> Html
indexPage jms =
  docTypeHtml ! lang "en-US" $
    head (H.title "Chandra observations with other facilities"
          <> defaultMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          <> cssLink "/css/mission.css"
          )
    <>
    body
     (mainNavBar CPExplore
      <> (div ! id "schedule") (renderMissions jms)
      <> renderFooter
     )


matchPage :: 
  JointMission
  -> Schedule
  -> Html
matchPage ms sched =
  let hdrTitle = "Chandra observations: mission " <>
                 H.toHtml (fromMission ms)

      mission = toHtml (fromMissionLong ms)
      pageTitle = "Joint observations with the " <> mission

      mainBlock = renderMatches ms sched
  in standardSchedulePage sched CPExplore hdrTitle pageTitle mainBlock


-- | Render the list of missions
renderMissions ::
  [(JointMission, Int)]
  -> Html
renderMissions jms = 
  let toRow (jm,n) = tr $ do
                    td (fromMissionLongLink jm)
                    (td ! A.title (toValue lbl)) (toHtml n)

      lbl = "Number of observations" :: T.Text
      sjms = sortBy (compare `on` fst) jms

  in do
    p ("The Chandra observatory allows astronomers to propose for time with "
       <> "several other Astronomy facilities, if it significantly improves the  "
       <> "science case for the proposal. Several other observatories have the converse "
       <> "offer, in that they allow astronomers to ask for time on Chandra "
       <> "as well as with their facility. The following table lists these "
       <> "joint facilities, and lets you see what objects were - or will be - "
       <> "observed. Some observations will be simultaneous, in that the object "
       <> "is observed with all the facilities at the same time, but many are "
       <> "not. Note that not all observations or facilities are included in "
       <> "this table as this website only covers a small fraction of the full "
       <> "Chandra schedule. The set of missions for which joint proposals "
       <> "are available changes over time, as new facilities become available "
       <> "and others are decomissioned."
      )
      
    floatableTable $ do
             thead $ tr $ do
               th "Facility"
               th (toHtml lbl)
             tbody (mapM_ toRow sjms)


-- | TODO: combine table rendering with Views.Schedule
--
renderMatches ::
  JointMission
  -> Schedule      -- ^ non-empty list of matches
  -> Html
renderMatches ms sched = 
  let scienceTime = getScienceTime sched
  in -- TODO: improve English here
    p ("This page shows Chandra observations of objects which had "
       <> "joint observations with the "
       <> fromMissionAboutLink ms
       <> scienceTime
       <> ". These observations may be simultaneous, but often "
       <> "they are not. "
       -- assume the schedule is all science observations
       <> toHtml (getNumObs sched)
       <> ". The format is the same as used in the "
       <> (a ! href "/schedule") "schedule view"
       <> ".")
    
