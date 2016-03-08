{-# LANGUAGE OverloadedStrings #-}

-- | Search on instrument.

module Views.Search.Instrument (indexPage
                               , matchInstPage
                               , matchGratPage
                               , matchIGPage)
       where

-- import qualified Prelude as P
import Prelude (($), Ord, Int, compare, fst, mapM_)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Function (on)
import Data.List (sortBy)
import Data.Monoid ((<>), mconcat)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (Schedule(..), Instrument, Grating)
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
     (mainNavBar CPOther
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
     (mainNavBar CPOther
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
     (mainNavBar CPOther
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
     (mainNavBar CPOther
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
renderInstMatches inst (Schedule cTime _ done mdoing todo) = 
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo

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
renderGratMatches grat (Schedule cTime _ done mdoing todo) = 
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo

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
renderIGMatches (inst, grat) (Schedule cTime _ done mdoing todo) = 
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo

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
