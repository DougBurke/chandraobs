{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | List the observations that belong to a proposal.

module Views.Proposal (matchPage) where

import Prelude ((==), length)

import Data.Either (rights)
import Data.Monoid ((<>), mconcat)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import API (abstractLink, categoryLinkSearch)
import Types (Proposal(..)
              , RestrictedSchedule
              , rsoObsId
              )
import Utils (rschedToList, showInt)
import Views.Record (CurrentPage(..))
import Views.Render (standardRestrictedSchedulePage)

matchPage :: 
  Proposal    -- the proposal
  -> RestrictedSchedule  -- the observations included in this proposal
  -> Html
matchPage prop@Proposal{..} sched =
  let hdrTitle = "Chandra proposal: " <> toHtml propNum
      pageTitle = toHtml propName
      mainBlock = renderProposal prop sched
  in standardRestrictedSchedulePage sched CPOther hdrTitle pageTitle mainBlock
     
renderProposal :: 
  Proposal
  -> RestrictedSchedule
  -> Html
renderProposal Proposal{..} sched =
  let catLink = categoryLinkSearch propCategory propCategory

      obsList = rights (rschedToList sched)
      nobs = length obsList
      count = if nobs == 1
              then "consists of one observation"
              else "contains " <> toHtml (showInt nobs) <> " observations"

      -- TODO: can we create a link just from the proposal number?
      --       probably not, since the links I am using take you to per-ObsId
      --       pages.
      hName = toHtml propName
      abstxt = case obsList of
       (so:_) -> let uri = abstractLink (rsoObsId so)
                 in (a ! href uri) hName
       _ ->  hName

  in p (mconcat
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
        ])

