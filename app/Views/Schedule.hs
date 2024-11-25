{-# LANGUAGE OverloadedStrings #-}

-- | Display the schedule as a table.
--
module Views.Schedule (schedPage, schedDatePage) where

-- import qualified Prelude as P
import Prelude ((==))

import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H

import Data.Monoid ((<>))
import Data.Maybe (isJust)
import Data.Time (Day, showGregorian)

import Formatting ((%), sformat)
import Formatting.Time (dayName, dayOfMonthS, monthName, year)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (RestrictedSchedule(..))
import Utils (HtmlContext(StaticHtml)
             , extLink
             , getScienceTimeRestricted
             , showInt
             )
import Views.Record (CurrentPage(..))
import Views.Render (standardRestrictedSchedulePage)

schedPage :: 
  RestrictedSchedule
  -> Html
schedPage sched =
  let hdrTitle = "The Chandra schedule"
      (pageTitle, mainBlock) = renderSchedule sched
  in standardRestrictedSchedulePage sched CPSchedule hdrTitle pageTitle mainBlock


schedDatePage ::
  Day
  -> RestrictedSchedule
  -> Html
schedDatePage date sched =
  let hdrTitle = "The Chandra schedule for " <> H.toHtml (showGregorian date)
      (pageTitle, mainBlock) = renderDateSchedule date sched
  in standardRestrictedSchedulePage sched CPSchedule hdrTitle pageTitle
     mainBlock

-- | The previous schedule could be displayed if there is
--   no current schedule, but let's just have a simple display
--   for now.
--
--   TODO:
--     want to make the description of the SVG image be added to
--     the page via JS, so that it is not displayed when there is
--     no JS support. This means breaking things up a bit.
--
renderSchedule :: 
  RestrictedSchedule
  -> (Html, Html)
renderSchedule sched =
  let scienceTime = getScienceTimeRestricted sched
                         
      -- the assumption is that the number of days is > 0
      ndays = rrDays sched
      title = showInt ndays <> "-day Schedule"
      hdays = if ndays == 1
              then "one day"
              else toHtml (showInt ndays <> " days")

      -- TODO: if there is no schedule, what happens to the
      --       SVG and table display which get added automatically?
      missingBlock = 
        p ("There seems to be a problem, in that I do not know what the "
           <> "current observation is!")

      welcome = 
        "This page shows "
        <> hdays
        <> " of the Chandra schedule, either side of today"
        <> scienceTime
        <> "."

      bodyBlock = p (
        welcome
        <> " The size of the circles indicate the exposure time, and "
        <> "the color shows whether the observation has been done, "
        <> "is running now, or is in the future; the same colors "
        <> "are used in the table below. For repeated observations "
        <> "it can be hard to make out what is going on, since the "
        <> "circles overlap! The shaded regions trace "
        <> "the Milky Way galaxy. "
        <> "The points are plotted in the "
        <> extLink StaticHtml "https://en.wikipedia.org/wiki/Equatorial_coordinate_system#Use_in_astronomy"
        ("Equatorial coordinate system" :: T.Text)
        <> ", using the "
        <> extLink StaticHtml "https://en.wikipedia.org/wiki/Aitoff_projection"
        ("Aitoff projection" :: T.Text)
        <> "."
        )
      
  in (toHtml title,
      if isJust (rrDoing sched) then bodyBlock else missingBlock)
  

renderDateSchedule :: 
  Day
  -> RestrictedSchedule
  -> (Html, Html)
renderDateSchedule date sched =
  let scienceTime = getScienceTimeRestricted sched
      nDays = rrDays sched
      
      -- the assumption is that the number of days is > 0
      title = showInt nDays <> "-day Schedule for "
              <> T.pack (showGregorian date)
      hdays = if nDays == 1
              then "one day"
              else toHtml (showInt nDays <> " days")

      dateTxt = sformat (dayName <> ", " % monthName <> " "
                        % dayOfMonthS <> ", " % year) date

      bodyBlock = p (
        "This page shows "
        <> hdays
        <> " of the Chandra schedule either side of "
        <> toHtml dateTxt
        <> scienceTime
        <> ". The format is the same as used in the "
        <> (a ! href "/schedule") "schedule view"
        <> ".")

  in (toHtml title, bodyBlock)

