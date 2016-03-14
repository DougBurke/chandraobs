{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | The propsoal page

module Views.Proposal (matchPage) where

-- import qualified Prelude as P
import Prelude (($), (==), length, show)

-- import qualified Data.Text as T

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Either (rights)
import Data.Monoid ((<>), mconcat)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (Proposal(..)
              , ScienceObs(..)
              , Schedule(..)
              )
import Utils (defaultMeta, skymapMeta, abstractLink, renderFooter
             , cssLink, categoryLinkSearch, schedToList)
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
          <> skymapMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
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
renderProposal Proposal{..} (Schedule cTime _ done mdoing todo simbad) =
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo simbad

      catLink = categoryLinkSearch propCategory propCategory

      obsList = rights (schedToList done mdoing todo)
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
