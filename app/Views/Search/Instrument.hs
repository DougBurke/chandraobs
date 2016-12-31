{-# LANGUAGE OverloadedStrings #-}

-- | Search on instrument.

module Views.Search.Instrument (indexPage
                               , matchInstPage
                               , matchGratPage
                               , matchIGPage
                               , breakdownPage
                               )
       where

import qualified Prelude as P
import Prelude (($), (*), (/), Int, Maybe(..), Ord, compare, fst, snd, mapM_)

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Aeson ((.=))
import Data.Function (on)
import Data.List (sortBy)
import Data.Monoid ((<>))
import Data.Time (Day)

import Formatting

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import API (instLinkSearch
           , gratLinkSearch
           , igLinkSearch
           , instLinkAbout
           , gratLinkAbout)
import Types (Schedule, TimeKS(..), Instrument, Grating(..)
             , showExpTime, addTimeKS
             , fromInstrument, fromGrating
             )
import Utils (defaultMeta, d3Meta
             , renderFooter, cssLink, jsScript
             , getNumObs
             , getScienceTime
             , standardTable
             , toJSVarObj
             )
import Views.Record (CurrentPage(..), mainNavBar)
import Views.Render (standardSchedulePage
                    , standardExplorePage)

indexPage :: 
  [(Instrument, Int)]
  -> [(Grating, Int)]
  -> [((Instrument, Grating), Int)]
  -> Html
indexPage insts grats igs =
  let bodyBlock = renderTypes insts grats igs
      mid = Just "explorebox"
  in standardExplorePage Nothing "Chandra observations" bodyBlock mid


matchInstPage :: 
  Instrument
  -> Schedule
  -> Html
matchInstPage inst sched =
  let hdrTitle = "Chandra observations with " <> pageTitle
      pageTitle = H.toHtml inst
      mainBlock = renderInstMatches inst sched
  in standardSchedulePage sched CPExplore hdrTitle pageTitle mainBlock


matchGratPage :: 
  Grating
  -> Schedule
  -> Html
matchGratPage grat sched =
  let hdrTitle = "Chandra observations with " <> pageTitle
      pageTitle = H.toHtml grat
      mainBlock = renderGratMatches grat sched
  in standardSchedulePage sched CPExplore hdrTitle pageTitle mainBlock


matchIGPage :: 
  (Instrument, Grating)
  -> Schedule
  -> Html
matchIGPage ig@(inst, grat) sched =
  let hdrTitle = "Chandra observations with " <> pageTitle
      pageTitle = H.toHtml inst <> " and " <> H.toHtml grat
      mainBlock = renderIGMatches ig sched
  in standardSchedulePage sched CPExplore hdrTitle pageTitle mainBlock


renderInstMatches ::
  Instrument       
  -> Schedule      -- ^ non-empty list of matches
  -> Html
renderInstMatches inst sched = 
  let scienceTime = getScienceTime sched
  in p ("This page shows observations of objects that use "
        <> "the "
        <> instLinkAbout inst
        <> " instrument on Chandra"
        <> scienceTime
        <> ". "
        -- assume the schedule is all science observations
        <> toHtml (getNumObs sched)
        <> ". The format is the same as used in the "
        <> (a ! href "/schedule") "schedule view"
        <> ".")


renderGratMatches ::
  Grating       
  -> Schedule      -- ^ non-empty list of matches
  -> Html
renderGratMatches grat sched = 
  let scienceTime = getScienceTime sched
  in p ("This page shows observations of objects that use "
        <> gratLinkAbout grat
        <> " on Chandra"
        <> scienceTime
        <> ". "
        -- assume the schedule is all science observations
        <> toHtml (getNumObs sched)
        <> ". The format is the same as used in the "
        <> (a ! href "/schedule") "schedule view"
        <> ".")


renderIGMatches ::
  (Instrument, Grating)
  -> Schedule      -- ^ non-empty list of matches
  -> Html
renderIGMatches (inst, grat) sched = 
  let scienceTime = getScienceTime sched
  in p ("This page shows observations of objects that use "
        <> instLinkAbout inst
        <> " with "
        <> gratLinkAbout grat
        <> " on Chandra"
        <> scienceTime
        <> ". "
        -- assume the schedule is all science observations
        <> toHtml (getNumObs sched)
        <> ". The format is the same as used in the "
        <> (a ! href "/schedule") "schedule view"
        <> ".")
    

renderTypes ::
  [(Instrument, Int)]
  -> [(Grating, Int)]
  -> [((Instrument, Grating), Int)]
  -> Html
renderTypes insts grats igs = 
  let toRow f (val, n) = tr $ do
        td (f val)
        td (toHtml n)

      sortFst :: Ord a => [(a, b)] -> [(a, b)]
      sortFst = sortBy (compare `on` fst)

      tbl lbl conv xs = standardTable $ do
        thead $ tr $ do
          th lbl
          th "Number of observations"
        tbody (mapM_ (toRow conv) (sortFst xs))
      
  in div 
     (p ("There are several ways to view the configurations: by "
         <> "instrument, grating, or both.")
      <> tbl "Instrument" instLinkSearch insts
      <> tbl "Grating" gratLinkSearch grats
      <> tbl "Instrument & Grating" igLinkSearch igs)


-- Experimental

breakdownPage ::
  M.Map (Instrument, Grating) TimeKS
  -> M.Map Day (M.Map (Instrument, Grating) TimeKS)
  -> Html
breakdownPage total perDay =
  docTypeHtml ! lang "en-US" $
    head (H.title "Chandra observations: a breakdown of exposure times"
          <> defaultMeta
          <> d3Meta
          <> jsScript "/js/breakdown-view.js"
          <> cssLink "/css/breakdown.css"
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    (body ! onload "createBreakdown(seriesinfo);")
     (mainNavBar CPExplore
      <> (div ! id "explorebox") (renderBreakdown total perDay)
      <> renderFooter
     )

renderBreakdown ::
  M.Map (Instrument, Grating) TimeKS
  -> M.Map Day (M.Map (Instrument, Grating) TimeKS)
  -> Html
renderBreakdown total perDay =
  let toRow f (k,t) = tr $ do
        td (f k)
        td (toHtml (showExpTime t))
        td (toHtml (frac t))

      totalTime = P.sum (P.map _toKS (M.elems total))

      -- | Will want to limit the % to a few dp
      frac :: TimeKS -> T.Text
      frac t =
        let v = 100 * _toKS t / totalTime
        in sformat (fixed 2) v

      tbl lbl f xs = standardTable $ do
        thead $ tr $ do
          th lbl
          th "Observing time"
          th "Percentage of time"
        tbody (mapM_ (toRow f) (M.toAscList xs))

      insts = M.mapKeysWith addTimeKS fst total
      grats = M.mapKeysWith addTimeKS snd total
      igs = total

      toKey (inst,grat) =
        fromInstrument inst <> "+" <> fromGrating grat

      toLabel ig@(inst,grat) =
        let val = fromInstrument inst <> case grat of
              NONE -> "" -- maybe say " with no grating"?
              _ -> " + " <> fromGrating grat
        in toKey ig .= val
      lbls = P.map toLabel (M.keys igs)

      toTimes (k, m) =
        let c (key, val) = toKey key .= _toKS val
        in Aeson.object [
          "date" .= k
          , "values" .= Aeson.object (P.map c (M.toAscList m))
          ]
      series = P.map toTimes (M.toAscList perDay)

      -- The total and perDay data are sparse, in that there
      -- are days with no data. Do we want to create a dense
      -- array on output, filling these days in?
      json = Aeson.object [
        "labels" .= Aeson.object lbls
        , "series" .= series
        ]

      calLink = (a ! href "/search/calendar/")
      
  in div $ do
    p ("A " <> em "very" <> " unofficial breakdown of the time spent "
       <> "observing with each instrument configuration. Just to stress, "
       <> "this is " <> strong "not" <> " an official product of the "
       <> "Chandra X-Ray Center, and the data is neither complete or 100% "
       <> "reliable. This should only be taken as a rough estimate of the "
       <> "observation times, and is only for a "
       <> calLink "small fraction"
       <> " of the output of Chandra!"
      )

    p ("The plots show the total number of observing hours "
       <> em "started" <> " in each day, for each detector on Chandra (so this "
       <> "combines both "
       <> (a ! href "/about/instruments.html#grating") "grating"
       <> " and non-grating observations). It does "
       <> strong "not" <> " include non-science observations. "
       <> "Note that if an "
       <> "observation spans one (or more) days then the time is "
       <> "assigned to the start date, which is why it looks like "
       <> "Chandra has managed to squeeze in more than 24 hours "
       <> "of observations in a day! These plots "
       <> em "only" <> " cover the year 2015."
      )

    (div ! id "seriesBlock") ""

    p ("The following tables represent the full data used by this site; "
       <> "that is, the data shown in the "
       <> calLink "calendar view"
       <> ".")

    tbl "Instrument" instLinkSearch insts
    tbl "Grating" gratLinkSearch grats
    tbl "Instrument & Grating" igLinkSearch igs

    toJSVarObj "seriesinfo" json
