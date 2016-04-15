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
import Prelude (($), (*), (/), Ord, Int, compare, fst, snd, mapM_)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Aeson ((.=))
import Data.Function (on)
import Data.Functor (void)
import Data.List (sortBy)
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8')
import Data.Time (Day)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Printf

import Types (Schedule(..), TimeKS(..), Instrument, Grating(..)
             , showExpTime, addTimeKS
             , fromInstrument, fromGrating
             )
import Utils (defaultMeta, d3Meta, skymapMeta
             , renderFooter, cssLink, jsScript
             , instLinkAbout, gratLinkAbout -- , igLinkAbout
             , instLinkSearch, gratLinkSearch, igLinkSearch
             , getNumObs
             , getScienceTime
             , standardTable
             )
import Views.Record (CurrentPage(..), mainNavBar)
import Views.Render (makeSchedule)

indexPage :: 
  [(Instrument, Int)]
  -> [(Grating, Int)]
  -> [((Instrument, Grating), Int)]
  -> Html
indexPage insts grats igs =
  docTypeHtml ! lang "en-US" $
    head (H.title "Chandra observations"
          <> defaultMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    body
     (mainNavBar CPExplore
      <> (div ! id "explorebox") (renderTypes insts grats igs)
      <> renderFooter
     )

-- TODO: combine with Schedule.schedPage

matchInstPage :: 
  Instrument
  -> Schedule
  -> Html
matchInstPage inst sched =
  docTypeHtml ! lang "en-US" $
    head (H.title ("Chandra observations with " <> H.toHtml inst)
          <> defaultMeta
          <> skymapMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    (body ! onload "createMap(obsinfo);")
     (mainNavBar CPExplore
      <> (div ! id "schedule") 
          (renderInstMatches inst sched)
      <> renderFooter
     )

matchGratPage :: 
  Grating
  -> Schedule
  -> Html
matchGratPage grat sched =
  docTypeHtml ! lang "en-US" $
    head (H.title ("Chandra observations with " <> H.toHtml grat)
          <> defaultMeta
          <> skymapMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    (body ! onload "createMap(obsinfo);")
     (mainNavBar CPExplore
      <> (div ! id "schedule") 
          (renderGratMatches grat sched)
      <> renderFooter
     )

matchIGPage :: 
  (Instrument, Grating)
  -> Schedule
  -> Html
matchIGPage ig@(inst, grat) sched =
  docTypeHtml ! lang "en-US" $
    head (H.title ("Chandra observations with "
                   <> H.toHtml inst
                   <> " and "
                   <> H.toHtml grat
                  )
          <> defaultMeta
          <> skymapMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    (body ! onload "createMap(obsinfo);")
     (mainNavBar CPExplore
      <> (div ! id "schedule") 
          (renderIGMatches ig sched)
      <> renderFooter
     )




-- | TODO: combine table rendering with Views.Schedule
--
renderInstMatches ::
  Instrument       
  -> Schedule      -- ^ non-empty list of matches
  -> Html
renderInstMatches inst (Schedule cTime _ done mdoing todo simbad) = 
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo simbad
      scienceTime = getScienceTime done mdoing todo

  in div ! A.id "scheduleBlock" $ do
    h2 (toHtml inst)

    svgBlock

    p ("This page shows observations of objects that use "
       <> "the "
       <> instLinkAbout inst
       <> " instrument on Chandra"
       <> scienceTime
       <> ". "
       -- assume the schedule is all science observations
       <> toHtml (getNumObs done mdoing todo)
       <> ". The format is the same as used in the "
       <> (a ! href "/schedule") "schedule view"
       <> ".")

    tblBlock

renderGratMatches ::
  Grating       
  -> Schedule      -- ^ non-empty list of matches
  -> Html
renderGratMatches grat (Schedule cTime _ done mdoing todo simbad) = 
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo simbad
      scienceTime = getScienceTime done mdoing todo

  in div ! A.id "scheduleBlock" $ do
    h2 (toHtml grat)

    svgBlock

    p ("This page shows observations of objects that use "
       <> gratLinkAbout grat
       <> " on Chandra"
       <> scienceTime
       <> ". "
       -- assume the schedule is all science observations
       <> toHtml (getNumObs done mdoing todo)
       <> ". The format is the same as used in the "
       <> (a ! href "/schedule") "schedule view"
       <> ".")

    tblBlock

renderIGMatches ::
  (Instrument, Grating)
  -> Schedule      -- ^ non-empty list of matches
  -> Html
renderIGMatches (inst, grat) (Schedule cTime _ done mdoing todo simbad) = 
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo simbad
      scienceTime = getScienceTime done mdoing todo

  in div ! A.id "scheduleBlock" $ do
    h2 (toHtml inst <> " and " <> toHtml grat)

    svgBlock

    p ("This page shows observations of objects that use "
       <> instLinkAbout inst
       <> " with "
       <> gratLinkAbout grat
       <> " on Chandra"
       <> scienceTime
       <> ". "
       -- assume the schedule is all science observations
       <> toHtml (getNumObs done mdoing todo)
       <> ". The format is the same as used in the "
       <> (a ! href "/schedule") "schedule view"
       <> ".")
    
    tblBlock

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
      
  in div $ do
    p ("There are several ways to view the configurations: by "
       <> "instrument, grating, or both.")

    tbl "Instrument" instLinkSearch insts
    tbl "Grating" gratLinkSearch grats
    tbl "Instrument & Grating" igLinkSearch igs


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
        in T.pack (printf "%.2f" v)

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

      jsHtml = case decodeUtf8' (LB8.toStrict (Aeson.encode json)) of
        P.Right ans -> toHtml ans
        P.Left _ -> "{}"
      
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

    script ! type_ "text/javascript" $ do
      void "var seriesinfo = "
      jsHtml
      ";"
