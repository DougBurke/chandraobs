{-# LANGUAGE OverloadedStrings #-}

-- | Search on joint missions.

module Views.Search.Mission (matchPage)
       where

-- import qualified Prelude as P
import Prelude (($))

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>), mconcat)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (Schedule(..), JointMission, fromMission)
import Utils (defaultMeta, skymapMeta, renderFooter, cssLink
             , getNumObs)
import Views.Record (CurrentPage(..), mainNavBar)
import Views.Render (makeSchedule)

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

-- | TODO: combine table rendering with Views.Schedule
--
renderMatches ::
  JointMission
  -> Schedule      -- ^ non-empty list of matches
  -> Html
renderMatches ms (Schedule cTime _ done mdoing todo simbad) = 
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo simbad

      mission = toHtml (fromMission ms)
      
  in div ! A.id "scheduleBlock" $ do
    h2 ("Joint observations with " <> mission)

    svgBlock

    -- TODO: improve English here
    p $ mconcat
        [ "This page shows Chandra observations of objects which had "
        , "joint observations with the "
        , mission
        , " facility. These observations may be simultaneous, but often "
        , "they are not."
          -- assume the schedule is all science observations
        , toHtml (getNumObs done mdoing todo)
        , ". The format is the same as used in the "
        , (a ! href "/schedule") "schedule view"
        , "."
        ]

    tblBlock

