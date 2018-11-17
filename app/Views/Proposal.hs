{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | List the observations that belong to a proposal.

module Views.Proposal (matchPage) where

-- import qualified Text.Blaze.Html5.Attributes as A

import Prelude (Maybe, (==), length, maybe)

import Data.Either (rights)
import Data.Monoid ((<>), mconcat, mempty)

import Text.Blaze.Html5 hiding (map, title, cite)
import Text.Blaze.Html5.Attributes hiding (title, cite)

import API (abstractLink, categoryLinkSearch)
import Types (Proposal(..), ProposalAbstract(..)
             , RestrictedSchedule
             , rsoObsId
             )
import Utils (rschedToList, showInt)
import Views.Record (CurrentPage(..))
import Views.Render (standardRestrictedSchedulePage)

matchPage :: 
  Proposal    -- the proposal
  -> Maybe ProposalAbstract
  -> RestrictedSchedule  -- the observations included in this proposal
  -> Html
matchPage prop@Proposal{..} mAbs sched =
  let hdrTitle = "Chandra proposal: " <> toHtml propNum
      pageTitle = toHtml (maybe propName paTitle mAbs)
      mainBlock = renderProposal pageTitle prop mAbs sched
  in standardRestrictedSchedulePage sched CPOther hdrTitle pageTitle mainBlock
     
renderProposal ::
  Html
  -- ^ The proposal title
  -> Proposal
  -> Maybe ProposalAbstract
  -> RestrictedSchedule
  -> Html
renderProposal title Proposal{..} mAbs sched =
  let catLink = categoryLinkSearch propCategory propCategory

      obsList = rights (rschedToList sched)
      nobs = length obsList
      count = if nobs == 1
              then "consists of one observation"
              else "contains " <> toHtml (showInt nobs) <> " observations"

      -- TODO: can we create a link just from the proposal number?
      --       probably not, since the links I am using take you to per-ObsId
      --       pages.
      abstxt = case obsList of
       (so:_) -> let uri = abstractLink (rsoObsId so)
                 in (a ! href uri) title
       _ ->  title

      {-
      The following is invalid since the proposal number has to be
      zero-padded to 8 characters (on the left). It also doesn't
      buy the user much, since the resulting web page doesn't add
      anything more (other than checking that the text hasn't
      changed).

      getAbstract ProposalAbstract {..} =
        let url = "https://cda.cfa.harvard.edu/srservices/propAbstract.do"
                  <> "?propNum="
                  <> toValue (paNum)
        in p "The proposal abstract is:"
           <> (blockquote ! A.cite url) (p (toHtml paAbstract))
      -}
      
      getAbstract ProposalAbstract {..} =
        p "The proposal abstract is:"
        <> blockquote (p (toHtml paAbstract))

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
     <> maybe mempty getAbstract mAbs

