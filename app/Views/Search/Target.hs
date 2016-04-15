{-# LANGUAGE OverloadedStrings #-}

-- | Search on target name

module Views.Search.Target (targetPage, noMatchPage) where

-- import qualified Prelude as P
import Prelude (($))

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.List (intersperse)
import Data.Monoid ((<>), mconcat)

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
  -> [TargetName]
  -- ^ The "official" name for this source (i.e. the SIMBAD-recognized name);
  --   I think this should always be a singleton (unless QuerySimbad hasn't
  --   been called when it could be empty). It should not have multiple values,
  --   but I haven't convinced myself of this so leave as is for now.
  -> Schedule
  -- ^ the observations that match this target, organized into a "schedule"
  -> Html
targetPage targetName fullNames sched =
  let jsLoad = "createMap(obsinfo);"

      officialName = case fullNames of
        [] -> targetName
        _ -> mconcat (intersperse ", " fullNames)
        
  in docTypeHtml ! lang "en-US" $
    head (H.title ("Chandra observations of " <> H.toHtml officialName) <>
          defaultMeta
          <> skymapMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    (body ! onload jsLoad)
     (mainNavBar CPExplore
      <> (div ! id "schedule") 
          (renderMatches officialName sched)
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

