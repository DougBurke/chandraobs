{-# LANGUAGE OverloadedStrings #-}

-- | Search on instrument.

module Views.Search.Instrument (indexPage, matchPage) where

-- import qualified Prelude as P
import Prelude (($), Int, compare, fst, mapM_)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Function (on)
import Data.List (sortBy)
import Data.Monoid ((<>), mconcat)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (Schedule(..), Instrument(..))
import Utils (defaultMeta, skymapMeta, renderFooter, cssLink, instLinkAbout, instLinkSearch)
import Views.Record (CurrentPage(..), mainNavBar)
import Views.Render (makeSchedule)

indexPage :: 
  [(Instrument, Int)]
  -> Html
indexPage insts =
  docTypeHtml ! lang "en-US" $
    head (H.title "Chandra observations" <>
          defaultMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    body
     (mainNavBar CPOther
      <> (div ! id "schedule") 
          (renderTypes insts)
      <> renderFooter
     )

-- TODO: combine with Schedule.schedPage

matchPage :: 
  Instrument
  -> Schedule
  -> Html
matchPage inst sched =
  docTypeHtml ! lang "en-US" $
    head (H.title ("Chandra observations with " <> H.toHtml inst) <>
          defaultMeta
          <> skymapMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    (body ! onload "createMap(obsinfo);")
     (mainNavBar CPOther
      <> (div ! id "schedule") 
          (renderMatches inst sched)
      <> renderFooter
     )

-- | TODO: combine table rendering with Views.Schedule
--
renderMatches ::
  Instrument       
  -> Schedule      -- ^ non-empty list of matches
  -> Html
renderMatches inst (Schedule cTime _ done mdoing todo) = 
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo

  in div ! A.id "scheduleBlock" $ do
    h2 $ toHtml inst

    svgBlock

    p $ mconcat
        [ "This page shows observations of objects that use "
        , "the "
        , instLinkAbout inst
        , " instrument on Chandra "
        , "(since the database only includes a "
        , "small fraction of the mission you will only see a few "
        , "matches). The format is the same as used in the "
        , (a ! href "/schedule") "schedule view"
        , "."
        ]

    tblBlock

renderTypes ::
  [(Instrument, Int)]
  -> Html
renderTypes insts = 
  let toRow (inst,n) = tr $ do
                        td $ instLinkSearch inst
                        td $ toHtml n

      sinsts = sortBy (compare `on` fst) insts
  in div $
     table $ do
       thead $ tr $ do
               th "Instrument"
               th "Number"
       tbody $ mapM_ toRow sinsts
             
