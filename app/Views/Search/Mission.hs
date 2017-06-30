{-# LANGUAGE OverloadedStrings #-}

-- | Search on joint missions.

module Views.Search.Mission (indexPage, matchPage)
       where

-- import qualified Prelude as P
import Prelude (($), Int, Maybe(..), compare, fst)

import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad (mapM_)

import Data.Function (on)
import Data.List (sortBy)
import Data.Monoid ((<>))

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Layout (floatableTable)
import Types (RestrictedSchedule, JointMission
             , fromMission, fromMissionLong
             , fromMissionLongLink, fromMissionAboutLink)
import Utils (getNumObsRestricted
             , getScienceTimeRestricted
             )
import Views.Record (CurrentPage(..))
import Views.Render (standardRestrictedSchedulePage
                     , standardExplorePage)

indexPage :: 
  [(JointMission, Int)]
  -> Html
indexPage jms =
  let hdrTitle = "Chandra observations with other facilities"
      cssPage = Just "/css/mission.css"
      bodyBlock = renderMissions jms
  in standardExplorePage cssPage hdrTitle bodyBlock Nothing

     
matchPage :: 
  JointMission
  -> RestrictedSchedule
  -> Html
matchPage ms sched =
  let hdrTitle = "Chandra observations: mission " <>
                 H.toHtml (fromMission ms)

      mission = toHtml (fromMissionLong ms)
      pageTitle = "Joint observations with the " <> mission

      mainBlock = renderMatches ms sched
  in standardRestrictedSchedulePage sched CPExplore hdrTitle pageTitle mainBlock


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
       <> "not. The set of missions for which joint proposals "
       <> "are available changes over time, as new facilities become available "
       <> "and others are decomissioned or "
       <> (a ! href "https://en.wikipedia.org/wiki/Suzaku_(satellite)")
       "are turned off."
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
  -> RestrictedSchedule
  -> Html
renderMatches ms sched = 
  let scienceTime = getScienceTimeRestricted sched
      
  in -- TODO: improve English here
    p ("This page shows Chandra observations of objects which had "
       <> "joint observations with the "
       <> fromMissionAboutLink ms
       <> scienceTime
       <> ". These observations may be simultaneous, but often "
       <> "they are not. "
       -- assume the schedule is all science observations
       <> toHtml (getNumObsRestricted sched)
       <> ". The format is the same as used in the "
       <> (a ! href "/schedule") "schedule view"
       <> ".")
    
