{-# LANGUAGE OverloadedStrings #-}

-- | Search on subarray setting.

module Views.Search.SubArrays (indexPage) where
-- module Views.Search.SubArrays (indexPage, matchPage) where

import qualified Prelude as P
import Prelude (Maybe(..), Int, ($), (<)
               , otherwise)

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Text.Blaze.Html5 as H

import Control.Arrow (first)
import Control.Monad (forM_)

import Data.Monoid ((<>))

import Text.Blaze.Html5 hiding (map, span, title)
import Text.Blaze.Html5.Attributes hiding (span, start, title, width)

-- import API (constraintLinkSearch)
-- import Layout (dquote, standardTable)
import Types (-- RestrictedSchedule, 
              TimeKS
             , addTimeKS
             , showExpTime)
-- import Utils (getScienceTimeRestricted)
-- import Views.Record (CurrentPage(..))
import Views.Render (-- standardRestrictedSchedulePage,
                    standardExplorePage)

indexPage :: 
  [((Int, Int), TimeKS)]
  -- ^ Start and width of the subarrays.
  -> TimeKS
  -- ^ ACIS exposure time with no sub-array
  -> Html
indexPage cs noSub =
  let hdrTitle = "Chandra observations with a sub array"
      bodyBlock = renderSubArrays cs noSub
      mid = Just "explorebox"
      css = Just "/css/subarrays.css"
  in standardExplorePage css hdrTitle bodyBlock mid

{-
-- | Render the results for a single class of constraints.
matchPage :: 
  Maybe ConstraintKind
  -> RestrictedSchedule
  -> Html
matchPage mcs sched =
  let hdrTitle = "Chandra observations: " <> H.toHtml lbl
      lbl = case mcs of
        Just cs -> csToLabel cs <> " observations"
        Nothing -> "No constraint"

      pageTitle = toHtml lbl
      mainBlock = renderMatches mcs sched
  in standardRestrictedSchedulePage sched CPExplore hdrTitle pageTitle mainBlock

-}


-- | Represent a value of 1 to 1024 by eights. To make things a
--   little simpler, values < 1 or > 1024 are treated as 1 or 1024
--   respectively (and the assumption is that these aren't in the
--   input data).
--
data I8 = I1 | I2 | I3 | I4 | I5 | I6 | I7 | I8
  deriving (P.Enum, P.Eq, P.Ord)

--   deriving (Bounded, Enum, Eq, Ord)

toI8 :: Int -> I8
toI8 x | x < 129   = I1
       | x < 257   = I2
       | x < 385   = I3
       | x < 513   = I4
       | x < 641   = I5
       | x < 769   = I6
       | x < 897   = I7
       | otherwise = I8

fromI8 :: I8 -> T.Text
fromI8 I1 = "1 - 126"
fromI8 I2 = "126 - 256"
fromI8 I3 = "257 - 384"
fromI8 I4 = "385 - 512"
fromI8 I5 = "513 - 640"
fromI8 I6 = "641 - 768"
fromI8 I7 = "769 - 896"
fromI8 I8 = "897 - 1024"

-- | Render the list of sub-arrays.
--
renderSubArrays ::
  [((Int, Int), TimeKS)]
  -- ^ The start and width of the subarrays.
  -> TimeKS
  -- ^ Time with no sub-arrays (ACIS only)
  -> Html
renderSubArrays cs noSub = 
  let i8s (x, y) = (toI8 x, toI8 y)
      xs = P.map (first i8s) cs
      vals = M.fromListWith addTimeKS xs

      cell = (H.span ! class_ "cell") P.. toHtml
      cellE = (H.span ! class_ "cell empty") ("" :: Html)
      cellH = (H.span ! class_ "cell header") P.. toHtml
      
      toCell s8 w8 = 
        case M.lookup (s8, w8) vals of
          Just tks -> cell (showExpTime tks)
          _ -> cellE
        
  in div $ do
    h2 "ACIS sub-arrays"

    p ("This is a quick look at how the total exposure time is "
      <> "split up for those ACIS observations which use a sub-array. "
      <> "That is, only a subset of the 1024 rows in each ACIS chip is "
      <> "used, which means that it takes less time to 'read out' "
      <> "the signal, and so a higher cadence is possible. This is "
      <> "generally used for very-bright X-ray sources, to reduce the "
      <> "degree of 'pile up' that happens (as this is a real pain to "
      <> "try and account for when analysing data).")

    p ("This is an experiment and will be improved soon (e.g. use an "
      <> "image rather than table, summarize the selected row/column, "
      <> "and links; oh so many links)!")

    p ("The total exposure time of ACIS observations with no subarray is "
      <> toHtml (showExpTime noSub) <> ".")
    
    -- have no width = I8 values
    let ids = [I1 .. I8]
    (div ! class_ "cells") $
      do
        forM_ (P.reverse ids) (\w8 -> do
                                  let title = "Number of rows: " <> fromI8 w8
                                  cellH title
                                  forM_ ids (\s8 -> toCell s8 w8)
                              )
        cellE
        forM_ ids (\s8 -> cellH ("Start row: " <> fromI8 s8))
          
    {-
    (p ! class_ "footnote")
      (sup "1" <> " The roll angle of the satellite refers to how "
       <> "the satellite is aligned in space (or at least, one of "
       <> "the angles). It determines the orientation of the "
       <> (a ! href "/about/instruments.html") "detector"
       <> " on Chandra, which can be important to make sure that "
       <> "the required data can be collected."
      )
    -}
    

{-
explain :: Maybe ConstraintKind -> H.Html -> H.Html
explain Nothing scienceTime =
  "This is a view of those observations which had no constraint"
  <> scienceTime
  <> ". Surprisingly enough, this can include "
  <> proplink
  <> " observations, because once a target has triggered the "
  <> "observing condition it may just be important to get the "
  <> "observation done as soon as possible."

explain (Just TimeCritical) scienceTime =
  "These observations are listed as time critical"
  <> scienceTime
  <> ". I do not know enough about the details of Chandra's "
  <> "scheduling to understand how this differs from the "
  <> constraintLinkSearch (Just Constrained)
  <> " flag."

explain (Just Monitor) scienceTime =
  "The observations are known as monitoring observations"
  <> scienceTime
  <> ". I believe that every monitoring observation is also "
  <> "listed as "
  <> constraintLinkSearch (Just TimeCritical)
  <> " observation."

explain (Just Constrained) scienceTime =
  "These observations are listed as constrained"
  <> scienceTime
  <> ". I do not know enough about the details of Chandra's "
  <> "scheduling to understand how this differs from the "
  <> constraintLinkSearch (Just TimeCritical)
  <> " flag, except that there's a lot less observations with "
  <> "the "
  <> dquote "Constrained"
  <> " flag."


-- | TODO: combine table rendering with Views.Schedule
--
renderMatches ::
  Maybe ConstraintKind
  -> RestrictedSchedule
  -> Html
renderMatches mcs sched = 
  let scienceTime = getScienceTimeRestricted sched
  in p (explain mcs scienceTime)

-}
