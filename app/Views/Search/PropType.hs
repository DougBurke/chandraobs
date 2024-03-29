{-# LANGUAGE OverloadedStrings #-}

-- | Information on proposal types - e.g. GO vs GTO vs CAL ...

module Views.Search.PropType (indexPage, matchPage)
       where

-- import qualified Prelude as P
import Prelude (Int, Maybe(..), (.), ($), compare, fst)

import qualified Data.Map.Strict as M

import Control.Monad (mapM_)

import Data.Function (on)
import Data.List (sortBy)
import Data.Monoid ((<>))

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import API (propTypeLink)
import Layout (dquote, standardTable)
import Types (RestrictedSchedule, PropType(..), TimeKS
             , toPropTypeLabel
             , normTimeKS
             , showExpTime)
import Utils (HtmlContext(StaticHtml)
             , toLink
             , extLink
             , getScienceTimeRestricted
             , getNumObsRestricted
             )
import Views.Record (CurrentPage(..))
import Views.Render (baseRestrictedSchedulePage,
                     standardExplorePage)

indexPage :: 
  M.Map PropType (Int, Int, TimeKS)
  -- Number of proposals, number of targets, total exposure time
  -> Html
indexPage pmap =
  let hdrTitle = "Chandra observations by proposal type"
      bodyBlock = renderTypes pmap
      mcss = Just "/css/mission.css"
  in standardExplorePage mcss hdrTitle bodyBlock Nothing


matchPage ::
  PropType
  -> RestrictedSchedule
  -> Html
matchPage pType sched =
  let hdrTitle = "Chandra observations: " <> pageTitle <> " proposals"
      jsLoad = "projection.createMap(obsinfo);"
      cssPage = Just "/css/mission.css"

      pageTitle = toHtml (toPropTypeLabel pType)
      mainBlock = renderMatches pType sched
      
  in baseRestrictedSchedulePage sched CPExplore hdrTitle pageTitle
     mainBlock jsLoad cssPage

tstrong :: Html -> Html
tstrong = strong ! class_ "label"

-- | Text describing the proposal type.
identifyType :: PropType -> Html
identifyType CAL =
  p (tstrong "Calibration observations"
     <> " are used by the Chandra calibration team "
     <> "at the Chandra X-ray Center to characterise and monitor the "
     <> "behavior of Chandra and its instruments. The observations are "
     <> "vital in ensuring that Astronomers can use the data taken by "
     <> "Chandra to answer their scientific questions. However, the "
     <> "calibration data itself can often also be used for scientific "
     <> "studies as well.")
  
identifyType CCT =
  p (tstrong "Cool Catalog Target"
     <> " is a set of targets which are spread out across the sky in "
     <> "locations that allow Chandra to 'cool off'. This has become "
     <> "necessary as Chandra begins to show its age, and is no longer "
     <> "able to regulate its temperature as well as it could at launch. "
     <> "The CCT list was recently (2018) created to provide targets "
     <> "that the Chandra planners could select if the 'thermal profile' "
     <> "of the schedule indicates that Chandra would get too warm "
     <> "(that is, it is still cold by our standards, but warmer than "
     <> "the instrument scientists want Chandra to get) and needs to "
     <> "cool down. These observations are relatively short, and "
     <> "there is no guarantee that a particular observation will be "
     <> "done, so are mainly useful for survey-style observational "
     <> "programmes, where data on some objects can be useful, rather "
     <> "than programmes that require oservations of particular objects.")

identifyType DDT =
  p (tstrong "Director's Discretionary Time"
     <> " is used for observing events which "
     <> "could not have been predicted (which means that a Target Of "
     <> "Opportunity proposal could not have been written), or are of "
     <> "such importance to the Scientific community that the proposal "
     <> "goes straight to the Director of the Chandra X-Ray Center, "
     <> "rather than through the standard review process for proposals."
     )

identifyType GO =
  p (tstrong "General Observer"
     <> " proposals form the bulk of the observations performed "
     <> "by Chandra, and are the observations requested by Astronomers "
     <> "from all over the world. These observations are passed through "
     <> "a yearly peer-review process, where panels of experts provide their "
     <> "ranking on the Scientific and technical aspects of each proposal, "
     <> "and only a small fraction get through to be observed by Chandra."
    )

identifyType GTO =
  p (tstrong "Guaranteed-Time Observations"
     <> " refer to proposals submitted by the "
     <> "instrument teams; that is, the teams that built the various "
     <> "detector and grating systems on Chandra. As part of the process "
     <> "of building an instrument, a fraction of observing time on Chandra "
     <> "was promised to these teams. This fraction has changed over the "
     <> "life time of Chandra, with higher values at the start of the "
     <> "mission.")

identifyType TOO =
  p (tstrong "Target of Opportunity Observations"
     <> " are for Scientific proposals of "
     <> "time-varying phenomena, such as "
     <> extLink StaticHtml "https://en.wikipedia.org/wiki/Gamma-ray_burst"
     ("Gamma-Ray Bursts" :: Html)
     <> " or "
     <> extLink StaticHtml "https://en.wikipedia.org/wiki/Flare_star"
     ("an X-ray flaring star" :: Html)
     <> sup "1"
     <> " that can be predicted (in that, a proposal can be written to "
     <> "say that targets will be observed, and the "
     <> dquote "trigger conditions"
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
                td (propTypeLink StaticHtml t Nothing)
                td (toHtml nprop)
                -- td (toHtml n2)
                td (toHtml (showExpTime (normTimeKS tot nprop)))

                    
      smap = sortBy (compare `on` fst) (M.toList pmap)

  in do
    p ("Chandra proposals, as well as being split up by "
      <> toLink StaticHtml "/search/category/" ("category" :: Html)
      <> ", also have a type, which is used to indicate "
      <> "the origin of the proposal. There are six types:"
      )

    let wrapper = (div ! class_ "term") . identifyType 
    mapM_ wrapper [CAL, CCT, DDT, GO, GTO, TOO]

    standardTable $ do
      thead $ tr $ do
        th "Proposal type"
        th "Number of proposals"
        -- th "Number of targets"
        th "Average proposal exposure time"
      tbody (mapM_ toRow smap)


    (p ! class_ "footnote")
      (sup "1"
       <> " Our Sun, "
       <> extLink StaticHtml "https://xrt.cfa.harvard.edu/xpow/"
       ("which is X-ray variable" :: Html)
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
  -> RestrictedSchedule
  -> Html
renderMatches ptype sched = 
  let scienceTime = getScienceTimeRestricted sched

      plabel = toHtml (toPropTypeLabel ptype)
      
  in -- TODO: improve English here
    p ("This page shows Chandra observations for proposals with "
       <> "the proposal type of "
       <> plabel
       <> scienceTime
       <> ". "
       -- assume the schedule is all science observations
       <> toHtml (getNumObsRestricted sched)
       <> ". The format is the same as used in the "
       <> toLink StaticHtml "/schedule" ("schedule view" :: Html)
       <> ".")
