{-# LANGUAGE OverloadedStrings #-}

-- | Search on constraint (related to, but different from, TOOs).

module Views.Search.Constraint (indexPage, matchPage) where

-- import qualified Prelude as P
import Prelude (Maybe(..), ($), compare, fst, mapM_)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Function (on)
import Data.List (sortBy)
import Data.Monoid ((<>))

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (Schedule(..), TimeKS, ConstraintKind(..)
             , showExpTime
             , csToLabel)
import Utils (defaultMeta, skymapMeta, renderFooter, cssLink
             , getScienceTime
             , constraintLinkSearch
             , dquote, standardTable
             )
import Views.Record (CurrentPage(..), mainNavBar)
import Views.Render (makeSchedule)

-- TODO: combine with Schedule.schedPage

indexPage :: 
  [(ConstraintKind, TimeKS)]
  -- ^ Note: an observation can have multiple constraints, so
  --   the times overlap
  -> TimeKS
  -- ^ Time for the "no constraint" category
  -> Html
indexPage cs noneTime =
  docTypeHtml ! lang "en-US" $
    head (H.title "Chandra observations by observational constraint"
          <> defaultMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    body
     (mainNavBar CPExplore
      <> (div ! id "explorebox") 
             (renderConstraints cs noneTime)
      <> renderFooter
     )

-- | Render the results for a single class of constraints.
matchPage :: 
  Maybe ConstraintKind
  -> Schedule
  -> Html
matchPage mcs sched =
  let lbl = case mcs of
        Just cs -> csToLabel cs <> " observations"
        Nothing -> "No constraint"
  in docTypeHtml ! lang "en-US" $
    head (H.title ("Chandra observations: " <> H.toHtml lbl)
          <> defaultMeta
          <> skymapMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    (body ! onload "createMap(obsinfo);")
     (mainNavBar CPExplore
      <> (div ! id "schedule") 
      (renderMatches mcs sched)
      <> renderFooter
     )


proplink :: H.Html
proplink =
  (a ! href "/search/proptype/")
  "Target of Opportunity (TOO) or Director's Discretionary Time (DDT)"

-- | Render the list of contraints.
--
renderConstraints ::
  [(ConstraintKind, TimeKS)]
  -- ^ Note: an observation can have multiple constraints, so
  --   the times overlap
  -> TimeKS
  -- ^ How much time is in the "no constraint" category
  -> Html
renderConstraints cs noneTime= 
  let toRow (ctype, ts) = tr $ do
        td (constraintLinkSearch (Just ctype))
        td (toHtml (showExpTime ts))

      scs = sortBy (compare `on` fst) cs

  in div $ do
    h2 "Observational constraints"
    
    p ("There are a large "
       <> "number of objects which Astronomers want to observe, "
       <> "but for which there is some reason that the object can "
       <> "only be observed at a certain time, or times. This is "
       <> "often due to some property of the source itself - for "
       <> "instance, much can be learned by observing one source "
       <> "eclipse another - but there are also more mundane, "
       <> "technical reasons, such as requiring a particular "
       <> "roll angle" <> sup "1" <> " for the observation "
       <> "or a simultaneous observation with another observatory. "
       <> "The table below lists the time associated with the "
       <> "various constraints that an Astronomer can ask for; these "
       <> "fields are not distinct, in that an observation can require "
       <> "multiple constraints (as can be seen by following the links). "
       <> "Note that this table is distinct from the "
       <> (a ! href "/search/turnaround/") "list of turn-around times"
       <> ", which shows the breakdown of the schedule "
       <> "for observations which are defined outside the normal "
       <> "proposal process, since in those cases it is not known "
       <> "what object is going to go " <> dquote "bang"
       <> " (or do something else equally impressive). "
      )

    p ("As mentioned, the constraints on observations can vary over "
       <> "the lifetime of the Chandra observatory, since changes to "
       <> "the satellite (due to its prolonged exposure to Space) "
       <> "result in an evolving set of rules designed to prolong the "
       <> "telescope's operational life whilst maintaining the "
       <> "Scientific output of the mission. These constraints also "
       <> "contribute to the difficulty in scheduling the observations. "
      )

    standardTable $ do
             thead $ tr $ do
               th "Constraint type"
               th "Exposure time"
             tbody $ do
               mapM_ toRow scs
               tr $ do
                 td (constraintLinkSearch Nothing)
                 td (toHtml (showExpTime noneTime))

    (p ! class_ "footnote")
      (sup "1" <> " The roll angle of the satellite refers to how "
       <> "the satellite is aligned in space (or at least, one of "
       <> "the angles). It determines the orientation of the "
       <> (a ! href "/about/instruments.html") "detector"
       <> " on Chandra, which can be important to make sure that "
       <> "the required data can be collected."
      )


explain :: Maybe ConstraintKind -> H.Html -> H.Html
explain Nothing scienceTime =
  "This is a view of those observations which had no constraint"
  <> scienceTime
  <> ". Surprisingly enough, this can include "
  <> proplink
  <> " observations, because once a target has triggered the "
  <> "observing condition it may just be important to get the "
  <> "observation done as soon as possible."

explain (Just TimeCritical) scienceTime =
  "These observations are listed as time critical"
  <> scienceTime
  <> ". I do not know enough about the details of Chandra's "
  <> "scheduling to understand how this differs from the "
  <> constraintLinkSearch (Just Constrained)
  <> " flag."

explain (Just Monitor) scienceTime =
  "The observations are known as monitoring observations"
  <> scienceTime
  <> ". I believe that every monitoring observation is also "
  <> "listed as "
  <> constraintLinkSearch (Just TimeCritical)
  <> " observation."

explain (Just Constrained) scienceTime =
  "These observations are listed as constrained"
  <> scienceTime
  <> ". I do not know enough about the details of Chandra's "
  <> "scheduling to understand how this differs from the "
  <> constraintLinkSearch (Just TimeCritical)
  <> " flag, except that there's a lot less observations with "
  <> "the "
  <> dquote "Constrained"
  <> " flag."


-- | TODO: combine table rendering with Views.Schedule
--
renderMatches ::
  Maybe ConstraintKind
  -> Schedule          -- ^ non-empty list of matches
  -> Html
renderMatches mcs (Schedule cTime _ done mdoing todo simbad) = 
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo simbad
      scienceTime = getScienceTime done mdoing todo
      
      lbl = case mcs of
        Just cs -> csToLabel cs <> " observations"
        Nothing -> "No constraint"

  in (div ! A.id "scheduleBlock") $ do
    h2 (H.toHtml lbl)

    svgBlock
    p (explain mcs scienceTime)
    tblBlock