{-# LANGUAGE OverloadedStrings #-}

-- | Search on joint missions.

module Views.Search.Mission (indexPage, matchPage)
       where

-- import qualified Prelude as P
import Prelude (Int, ($), compare, fst)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad (mapM_)

import Data.Function (on)
import Data.List (sortBy)
import Data.Monoid ((<>), mconcat)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (Schedule(..), JointMission
             , fromMission, fromMissionLong
             , fromMissionLongLink, fromMissionAboutLink)
import Utils (defaultMeta, skymapMeta, renderFooter, cssLink
             , getNumObs)
import Views.Record (CurrentPage(..), mainNavBar)
import Views.Render (makeSchedule)

indexPage :: 
  [(JointMission, Int)]
  -> Html
indexPage jms =
  docTypeHtml ! lang "en-US" $
    head (H.title "Chandra observations with other facilities"
          <> defaultMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    body
     (mainNavBar CPExplore
      <> (div ! id "schedule") (renderMissions jms)
      <> renderFooter
     )


-- TODO: combine with Schedule.schedPage

matchPage :: 
  JointMission
  -> Schedule
  -> Html
matchPage ms sched =
  docTypeHtml ! lang "en-US" $
    head (H.title ("Chandra observations: mission "
                   <> H.toHtml (fromMission ms))
          <> defaultMeta
          <> skymapMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    (body ! onload "createMap(obsinfo);")
     (mainNavBar CPExplore
      <> (div ! id "schedule") (renderMatches ms sched)
      <> renderFooter
     )

-- | Render the list of missions
renderMissions ::
  [(JointMission, Int)]
  -> Html
renderMissions jms = 
  let toRow (jm,n) = tr $ do
                    td (fromMissionLongLink jm)
                    td (toHtml n)

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
      
    table $ do
             thead $ tr $ do
               th "Facility"
               th "Number of observations"
             tbody (mapM_ toRow sjms)


-- | TODO: combine table rendering with Views.Schedule
--
renderMatches ::
  JointMission
  -> Schedule      -- ^ non-empty list of matches
  -> Html
renderMatches ms (Schedule cTime _ done mdoing todo simbad) = 
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo simbad

      mission = toHtml (fromMissionLong ms)
      
  in div ! A.id "scheduleBlock" $ do
    h2 ("Joint observations with the " <> mission)

    svgBlock

    -- TODO: improve English here
    p $ mconcat
        [ "This page shows Chandra observations of objects which had "
        , "joint observations with the "
        , fromMissionAboutLink ms
        , ". These observations may be simultaneous, but often "
        , "they are not. "
          -- assume the schedule is all science observations
        , toHtml (getNumObs done mdoing todo)
        , ". The format is the same as used in the "
        , (a ! href "/schedule") "schedule view"
        , "."
        ]

    tblBlock

