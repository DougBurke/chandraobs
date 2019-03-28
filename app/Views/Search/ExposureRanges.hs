{-# LANGUAGE OverloadedStrings #-}

-- | Search on exposure range (as a fraction of the full distribution).

module Views.Search.ExposureRanges (indexPage, matchPage) where

import qualified Prelude as P
import Prelude (Int, ($), (.),
               compare, fst, mapM_)

import Data.Function (on)
import Data.List (sortBy)
import Data.Monoid ((<>))

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes (class_)

import API (exposureRangeSearch)
import Layout (standardTable)
import Types (RestrictedSchedule
             , PRange
             , fromPRange
             , TimeKS
             -- , fromTimeKS
             , showExpTime)
import Utils (HtmlContext(StaticHtml)
             , toLink
             , extLink
             , getNumObsRestricted
             , getScienceTimeRestricted)
import Views.Record (CurrentPage(..))
import Views.Render (standardRestrictedSchedulePage
                    , standardExplorePage)

indexPage :: 
  [(PRange, (Int, (TimeKS, TimeKS)))]
  -- ^ breakdown of the exposure pages
  -> Html
indexPage rgs =
  let cssPage = P.Nothing
      bodyBlock = renderList rgs
      mid = P.Just "explorebox"
  in standardExplorePage cssPage
     "Chandra observations by Exposure Percentile" bodyBlock mid

matchPage :: 
  PRange
  -> RestrictedSchedule
  -> Html
matchPage rng sched =
  let rngHtml = toHtml (fromPRange rng <> " %")
      hdrTitle = "Chandra observations: " <> rngHtml
      pageTitle = "Observations with exposures in the range " <> rngHtml

      mainBlock = renderMatches rng sched
      
  in standardRestrictedSchedulePage sched CPExplore hdrTitle pageTitle mainBlock

renderMatches ::
  PRange
  -> RestrictedSchedule
  -> Html
renderMatches rng sched = 
  let scienceTime = getScienceTimeRestricted sched

  in p ("This page shows Chandra observations with "
        <> "exposures in the range "
        <> toHtml (fromPRange rng)
        <> " percent of the "
        <> toLink StaticHtml "/search/exposures/"
         ("exposure-time distribution" :: Html)
        <> scienceTime
        <> ". "
        -- assume the schedule is all science observations
        <> toHtml (getNumObsRestricted sched)
        <> ". The format is the same as used in the "
        <> toLink StaticHtml "/schedule" ("schedule view" :: Html)
        <> ".")


-- | Render the range information
--
renderList ::
  [(PRange, (Int, (TimeKS, TimeKS)))]
  -- This need not be sorted by PRange
  -> Html
renderList rgs = 
  let toRow (rng, (nobs, (tlo, thi))) =
        tr $ do
          td (exposureRangeSearch StaticHtml rng)
          td (toHtml (showExpTime tlo))
          td (toHtml (showExpTime thi))
          td (toHtml nobs)

      srgs = sortBy (compare `on` fst) rgs

      lip = li . p
      
  in div $ do

    p ("When applying for Chandra time, observers tend to pick the "
       <> "shortest length of observation that will let them answer "
       <> "their scientific question, since time on Chandra is hard "
       <> "to get" <> sup "1" <> ". This tends to mean that brighter "
       <> "sources have shorter exposure times, since the important "
       <> "factor is the number of counts (i.e. photons) we measure "
       <> "in an observation, but there are times when this does not "
       <> "hold, such as:")

    ul (
      lip ("timing observations, where we are interested in how "
           <> "the emission from a source varies with time;")
      <>
      lip ("or for very-bright sources an observer may chose to "
          <> "use a grating when observing the source, "
          <> "and this leads to longer exposure times since the "
          <> "grating is not as efficient, and the 'number of counts'"
          <> "requirement is now restricted to a much-smaller range "
          <> "of wavelengths than is used in the 'imaging' case.")
      )

    p ("An additional complication is that as Chandra spends longer "
        <> "in space (2019 is the "
        <> extLink StaticHtml "http://chandra.si.edu/20th/"
        ("twentieth year for Chandra" :: Html)
        <> ") it becomes harder to schedule single long observations"
        <> sup "2"
        <> ", and so individual observations can be smaller than they "
        <> "would have been earlier in the mission.")
        
    p ("This page lets you see how the observations, when broken up "
       <> "by exposure time, vary across the sky. It is a complementary "
       <> "view to the "
       <> toLink StaticHtml "/search/exposures/"
        ("look at the exposure times" :: Html)
       <> " and "
       <> toLink StaticHtml "/search/cycle/"
        ("breakdown by proposal cycle" :: Html)
       <> ".")
     
    standardTable $ do
             thead $ tr $ do
               th "Percentile range"
               th "Lower bound"
               th "Upper bound"
               th "Number of observations"
             tbody (mapM_ toRow srgs)

    (p ! class_ "footnote")
      (sup "1" <> " At each proposal round, there are unfortunately many "
       <> "more proposals made than are accepted.")
      
    (p ! class_ "footnote")
      (sup "2" <> " The aging thermal insulation on the telescope makes "
        <> "it harder to keep the instruments cold enough to work "
        <> "as designed, so the Chandra schedulers have to work hard "
        <> "to design an observation schedule that lets Chandra cool off "
        <> "when needed. An unfortunate consequence of this is that "
        <> "very-long observations now have to be split up into several "
        <> "smaller chunks.")
