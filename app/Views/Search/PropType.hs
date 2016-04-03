{-# LANGUAGE OverloadedStrings #-}

-- | Information on proposal types - e.g. GO vs GTO vs CAL ...

module Views.Search.PropType (indexPage, matchPage)
       where

-- import qualified Prelude as P
import Prelude (Int, Maybe(Nothing), String, ($), compare, fst)

import qualified Data.Map.Strict as M

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad (mapM_)

import Data.Function (on)
import Data.List (sortBy)
import Data.Monoid ((<>), mconcat)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (Schedule(..), PropType(..), TimeKS
             , toPropTypeLabel
             , normTimeKS
             , showExpTime)
import Utils (defaultMeta, skymapMeta, renderFooter
             , cssLink
             , propTypeLink
             , getScienceTime
             , getNumObs)
import Views.Record (CurrentPage(..), mainNavBar)
import Views.Render (makeSchedule)

indexPage :: 
  M.Map PropType (Int, Int, TimeKS)
  -- Number of proposals, number of targets, total exposure time
  -> Html
indexPage pmap =
  docTypeHtml ! lang "en-US" $
    head (H.title "Chandra observations: proposal types"
          <> defaultMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          <> cssLink "/css/mission.css"
          )
    <>
    body
     (mainNavBar CPExplore
      <> (div ! id "schedule") (renderTypes pmap)
      <> renderFooter
     )


matchPage ::
  PropType
  -> Schedule
  -> Html
matchPage pType sched =
  docTypeHtml ! lang "en-US" $
    head (H.title
          ("Chandra observations: "
           <> toHtml (toPropTypeLabel pType)
           <> " proposals")
          <> defaultMeta
          <> skymapMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          <> cssLink "/css/mission.css"
          )
    <>
    (body ! onload "createMap(obsinfo);")
     (mainNavBar CPExplore
      <> (div ! id "schedule") (renderMatches pType sched)
      <> renderFooter
     )


-- | Text describing the proposal type.
identifyType :: PropType -> Html
identifyType CAL =
  p (strong "Calibration observations"
     <> " are used by the Chandra calibration team "
     <> "at the Chandra X-ray Center to characterise and monitor the "
     <> "behavior of Chandra and its instruments. The observations are "
     <> "vital in ensuring that Astronomers can use the data taken by "
     <> "Chandra to answer their scientific questions. However, the "
     <> "calibration data itself can often also be used for scientific "
     <> "studies as well.")
  
identifyType DDT =
  p (strong "Director's Discretionary Time"
     <> " is used for observing events which "
     <> "could not have been predicted (which means that a Target Of "
     <> "Opportunity proposal could not have been written), or are of "
     <> "such importance to the Scientific community that the proposal "
     <> "goes straight to the Director of the Chandra X-Ray Center, "
     <> "rather than through the standard review process for proposals."
     )

identifyType GO =
  p (strong "Guest Observer"
     <> " proposals form the bulk of the observations performed "
     <> "by Chandra, and are the observations requested by Astronomers "
     <> "from all over the world. These observations are passed through "
     <> "a yearly peer-review process, where panels of experts provide their "
     <> "ranking on the Scientific and technical aspects of each proposal, "
     <> "and only a small fraction get through to be observed by Chandra."
    )

identifyType GTO =
  p (strong "Guaranteed-Time Observations"
     <> " refer to proposals submitted by the "
     <> "instrument teams; that is, the teams that built the various "
     <> "detector and grating systems on Chandra. As part of the process "
     <> "of building an instrument, a fraction of observing time on Chandra "
     <> "was promised to these teams. This fraction has changed over the "
     <> "life time of Chandra, with higher values at the start of the "
     <> "mission.")

identifyType TOO =
  p (strong "Target of Opportunity Observations"
     <> " are for Scientific proposals of "
     <> "time-varying phenomena, such as "
     <> (a ! href "https://en.wikipedia.org/wiki/Gamma-ray_burst")
     "Gamma-Ray Bursts"
     <> " or "
     <> (a ! href "https://en.wikipedia.org/wiki/Flare_star")
     "an X-ray flaring star"
     <> sup "1"
     <> " that can be predicted (in that, a proposal can be written to "
     <> "say that targets will be observed, and the "
     <> preEscapedToHtml ("&ldquo;"::String)
     <> "trigger conditions"
     <> preEscapedToHtml ("&rdquo;"::String)
     <> " that say when the observations can be taken are defined) "
     <> "even if the targets themselves are unknown at the time the "
     <> "proposal is written."
    )


-- | TODO: at the moment the number of targets is actually
--         the number of observations; need to tweak
--         the generating code.
--
renderTypes ::
  M.Map PropType (Int, Int, TimeKS)
  -> Html
renderTypes pmap = 
  let toRow (t, (nprop, _, tot)) = tr $ do
                td (propTypeLink t Nothing)
                td (toHtml nprop)
                -- td (toHtml n2)
                td (toHtml (showExpTime (normTimeKS tot nprop)))

                    
      smap = sortBy (compare `on` fst) (M.toList pmap)

  in do
    p ("Chandra proposals, as well as being split up by "
      <> (a ! href "/search/category/") "category"
      <> ", also have a type, which is used to indicate "
      <> "the origin of the proposal. There are five types:"
      )

    mapM_ identifyType [CAL, DDT, GO, GTO, TOO]

    table $ do
      thead $ tr $ do
        th "Proposal type"
        th "Number of proposals"
        -- th "Number of targets"
        th "Average proposal exposure time"
      tbody (mapM_ toRow smap)


    (p ! class_ "footnote")
      (sup "1"
       <> " Our Sun, "
       <> (a ! href "https://xrt.cfa.harvard.edu/xpow/")
       "which is X-ray variable"
       <> ", is so bright in X-rays "
       <> "that it can not be observed with Chandra because it would "
       <> "destroy the instruments on board the satellite. The scheduling "
       <> "team take a lot of care to ensure that Chandra does not point "
       <> "anywhere near the sun, even during slews between observations."
      )

-- | TODO: combine table rendering with Views.Schedule
--
--   TODO: add in text specific to the proposal type.
--
renderMatches ::
  PropType
  -> Schedule      -- ^ non-empty list of matches
  -> Html
renderMatches ptype (Schedule cTime _ done mdoing todo simbad) = 
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo simbad
      scienceTime = getScienceTime done mdoing todo

      plabel = toHtml (toPropTypeLabel ptype)
      
  in div ! A.id "scheduleBlock" $ do
    h2 plabel

    svgBlock

    -- TODO: improve English here
    p $ mconcat
        [ "This page shows Chandra observations for proposals with "
        , "the proposal type of "
        , plabel
        , scienceTime
        , ". "
          -- assume the schedule is all science observations
        , toHtml (getNumObs done mdoing todo)
        , ". The format is the same as used in the "
        , (a ! href "/schedule") "schedule view"
        , "."
        ]

    tblBlock
