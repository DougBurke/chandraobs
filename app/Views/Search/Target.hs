{-# LANGUAGE OverloadedStrings #-}

-- | Search on target name

module Views.Search.Target (targetPage, noMatchPage) where

-- import qualified Prelude as P
import Prelude (($))

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>))

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (Schedule(..), TargetName)
import Utils (defaultMeta, skymapMeta, renderFooter, cssLink,
              getNumObs)
import Views.Record (CurrentPage(..), mainNavBar)
import Views.Render (makeSchedule)

-- TODO: combine with Schedule.schedPage

targetPage :: 
  TargetName
  -- ^ target name
  -> Schedule
  -- ^ the observations that match this target, organized into a "schedule"
  -> Html
targetPage targetName sched =
  let jsLoad = "createMap(obsinfo);"
  in docTypeHtml ! lang "en-US" $
    head (H.title ("Chandra observations of " <> H.toHtml targetName) <>
          defaultMeta
          <> skymapMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    (body ! onload jsLoad)
     (mainNavBar CPExplore
      <> (div ! id "schedule") 
          (renderMatches targetName sched)
      <> renderFooter
     )

renderMatches ::
  TargetName           -- ^ target name
  -> Schedule      -- ^ non-empty list of matches
  -> Html
renderMatches lbl (Schedule cTime _ done mdoing todo simbad) = 
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo simbad

  in div ! A.id "scheduleBlock" $ do
    h2 (toHtml lbl)

    svgBlock

    p ("This page shows Chandra observations of the target "
       <> toHtml lbl
       <> ". "
       -- TODO: need to mention what is going on here (i.e. what the
       --       search represents)
       <> toHtml (getNumObs done mdoing todo)
       <> ". The format is the same as used in the "
       <> (a ! href "/schedule") "schedule view"
       <> "."
       )

    tblBlock


noMatchPage :: 
  TargetName
  -- ^ target name
  -> Html
noMatchPage targetName =
  docTypeHtml ! lang "en-US" $
    head (H.title ("Chandra observations of " <> H.toHtml targetName) <>
          defaultMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    body
     (mainNavBar CPExplore
      <> (div ! id "schedule")
          (p ("There was no match for " <> toHtml targetName <> "."))
      <> renderFooter
     )

