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

import qualified Data.Map.Strict as M

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Function (on)
import Data.List (sortBy)
import Data.Monoid ((<>), mconcat)
import Data.Time (Day)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Printf

import Types (Schedule(..), TimeKS(..), Instrument, Grating
             , showExpTime, addTimeKS)
import Utils (defaultMeta, skymapMeta, renderFooter, cssLink
             , instLinkAbout, gratLinkAbout -- , igLinkAbout
             , instLinkSearch, gratLinkSearch, igLinkSearch
             , getNumObs)
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
      <> (div ! id "schedule") 
          (renderTypes insts grats igs)
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

  in div ! A.id "scheduleBlock" $ do
    h2 (toHtml inst)

    svgBlock

    p $ mconcat
        [ "This page shows observations of objects that use "
        , "the "
        , instLinkAbout inst
        , " instrument on Chandra."
          -- assume the schedule is all science observations
        , toHtml (getNumObs done mdoing todo)
        , ". The format is the same as used in the "
        , (a ! href "/schedule") "schedule view"
        , "."
        ]

    tblBlock

renderGratMatches ::
  Grating       
  -> Schedule      -- ^ non-empty list of matches
  -> Html
renderGratMatches grat (Schedule cTime _ done mdoing todo simbad) = 
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo simbad

  in div ! A.id "scheduleBlock" $ do
    h2 (toHtml grat)

    svgBlock

    p $ mconcat
        [ "This page shows observations of objects that use "
        , gratLinkAbout grat
        , " on Chandra."
          -- assume the schedule is all science observations
        , toHtml (getNumObs done mdoing todo)
        , ". The format is the same as used in the "
        , (a ! href "/schedule") "schedule view"
        , "."
        ]

    tblBlock

renderIGMatches ::
  (Instrument, Grating)
  -> Schedule      -- ^ non-empty list of matches
  -> Html
renderIGMatches (inst, grat) (Schedule cTime _ done mdoing todo simbad) = 
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo simbad

  in div ! A.id "scheduleBlock" $ do
    h2 (toHtml inst <> " and " <> toHtml grat)

    svgBlock

    p $ mconcat
        [ "This page shows observations of objects that use "
        , instLinkAbout inst
        , " with "
        , gratLinkAbout grat
        , " on Chandra."
          -- assume the schedule is all science observations
        , toHtml (getNumObs done mdoing todo)
        , ". The format is the same as used in the "
        , (a ! href "/schedule") "schedule view"
        , "."
        ]

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

      tbl lbl conv xs = table $ do
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
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    body
     (mainNavBar CPExplore
      <> renderBreakdown total perDay
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

      totalTime = P.sum (P.map _toS (M.elems total))

      -- | Will want to limit the % to a few dp
      frac :: TimeKS -> P.String
      frac t =
        let v = 100 * _toS t / totalTime
        in printf "%.2f" v

      tbl lbl f xs = table $ do
        thead $ tr $ do
          th lbl
          th "Observing time"
          th "Percentage of time"
        tbody (mapM_ (toRow f) (M.toAscList xs))

      insts = M.mapKeysWith addTimeKS fst total
      grats = M.mapKeysWith addTimeKS snd total
      igs = total
      
  in div $ do
    p ("A " <> em "very" <> " unofficial breakdown of the time spent "
       <> "observing with each instrument configuration. Just to stress, "
       <> "this is " <> strong "not" <> " an official product of the "
       <> "Chandra X-Ray Center, and the data is neither complete or 100% "
       <> "reliable. This should only be taken as a rough estimate of the "
       <> "observation times, and is only for a "
       <> (a ! href "/search/calendar/") "small fraction"
       <> " of the output of Chandra!"
      )

    p ("I may well change to displaying a "
       <> preEscapedToHtml ("&ldquo;"::P.String)
       <> "fancy-schmancy"
       <> preEscapedToHtml ("&rdquo;"::P.String)
       <> " d3 visualization of this data, but wanted to get some numbers "
       <> "out quickly as a check.")

    tbl "Instrument" instLinkSearch insts
    tbl "Grating" gratLinkSearch grats
    tbl "Instrument & Grating" igLinkSearch igs
    
