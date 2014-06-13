{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | The propsoal page

module Views.Proposal (matchPage) where

-- import qualified Prelude as P
import Prelude (($), (==), (++), Either(..), Maybe(..), length, show)

-- import qualified Data.Text as T

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>), mconcat)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (Proposal(..)
              , ScienceObs(..)
              , Schedule(..)
              )
import Utils (defaultMeta, abstractLink, renderFooter, jsScript)
import Views.Record (CurrentPage(..), mainNavBar)
import Views.Render (makeSchedule)

matchPage :: 
  Proposal    -- the proposal
  -> Schedule  -- the observations included in this proposal
  -> Html
matchPage prop@Proposal{..} sched = 
  docTypeHtml ! lang "en-US" $
    head (H.title ("Chandra proposal: " <> toHtml propNum)
          <> defaultMeta 
          <> jsScript "http://code.jquery.com/jquery-1.11.1.min.js"
          <> jsScript "http://d3js.org/d3.v3.min.js"
          <> jsScript "http://d3js.org/d3.geo.projection.v0.min.js"
          <> jsScript "/js/jquery.tablesorter.min.js"
          <> jsScript "/js/table.js"
          <> jsScript "/js/projection.js"
          <> link ! href   "/css/tablesorter.css"
               ! type_  "text/css" 
               ! rel    "stylesheet"
               -- ! A.title  "Default (TableSorter)"
               ! media  "all"
          <> link ! href   "/css/schedule.css"
               ! type_  "text/css" 
               ! rel    "stylesheet"
               -- ! A.title  "Default (TableSorter)"
               ! media  "all"
          <> link ! href   "/css/main.css"
                 ! type_  "text/css" 
                 ! rel    "stylesheet"
                 ! A.title  "Default"
                 ! media  "all"
            )
    <>
    (body ! onload "createMap(obsinfo);")
     (mainNavBar CPOther
      <> (div ! id "schedule") 
          (renderProposal prop sched)
      <> renderFooter
     )

renderProposal :: 
  Proposal
  -> Schedule
  -> Html
renderProposal Proposal{..} (Schedule cTime _ done mdoing todo) =
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo

      catLink = a ! href ("/search/category/" <> toValue propCategory) $ toHtml propCategory

      maybeToList Nothing = []
      maybeToList (Just x) = [x]

      obsList = [x | Right x <- done ++ maybeToList mdoing ++ todo]
      nobs = length obsList
      count = if nobs == 1
              then "consists of one observation"
              else "contains " <> toHtml (show nobs) <> " observations"

      -- TODO: can we create a link just from the proposal number?
      --       probably not, since the links I am using take you to per-ObsId
      --       pages.
      hName = toHtml propName
      abstxt = case obsList of
       (so:_) -> a ! href (abstractLink (soObsId so)) $ hName
       _ ->  hName
  
  in div ! A.id "scheduleBlock" $ do
    h2 $ toHtml propName

    svgBlock

    p $ mconcat
        [ "The proposal, "
        , abstxt
        , ", "
        , count
        , ". It is a cycle "
        , toHtml propCycle
        , " "
        , toHtml propType
        , " observation, and was submitted to the "
        , catLink
        , " category."
        ]

    tblBlock
