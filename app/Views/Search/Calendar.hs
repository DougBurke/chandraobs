{-# LANGUAGE OverloadedStrings #-}

-- | Show the "calendar" view.

module Views.Search.Calendar (indexPage) where

import qualified Prelude as P
import Prelude (Maybe(Just), (.), Int, fst)

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Data.Aeson ((.=))
import Data.Monoid ((<>))
import Data.Time (Day , showGregorian)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Utils (d3Meta, jsScript, toJSVarObj)
import Views.Render (extraExplorePage)

indexPage ::
  M.Map Day Int
  -- ^ The count histogram for the number of science observations
  --   per day.
  -> Html
indexPage cts =
  let jsLoad = "createCalendar(calinfo);"
      jsCts = d3Meta <> jsScript "/js/calendar-view.js"
      mJS = Just (jsLoad, jsCts)
      
      mCSS = Just "/css/calendar.css"

      title = "Chandra observations: a calendar view"

      bdy = renderMatches cts
      mIdName = Just "explorebox"
      
  in extraExplorePage mJS mCSS title bdy mIdName
     
renderMatches ::
  M.Map Day Int
  -> Html
renderMatches cts = 
  let svgBlock = 
        (div ! id "calendar") ""
        <> toJSVarObj "calinfo" jsonCts

      fromDay = T.pack . showGregorian
      conv (d, n) = fromDay d .= n

      getDay = fromDay . fst

      -- Should this just be a date that is within the Chandra
      -- schedule, or in the future, or ?
      dummyDate = "2016-01-01"

      (startDate, endDate) = if M.null cts
                             then (dummyDate, dummyDate)
                             else (getDay (M.findMin cts),
                                   getDay (M.findMax cts))

      jsonCts = Aeson.object [
        "startDate" .= startDate
        , "endDate" .= endDate
        , "counts" .= Aeson.object (P.map conv (M.toAscList cts))
        ]

      paraBlock = 
        p ("This page shows the number of Chandra science observations per "
           <> "day. It is " <> em "very"
           <> " experimental and the results should not be taken too "
           <> "seriously as there are a lot of issues regarding "
           <> "getting hold of an accurate schedule. There's also the fact "
           <> "that I only currently include a small fraction of the total "
           <> "schedule, since Chandra was launched "
           <> (a ! href "http://chandra.harvard.edu/about/deployment.html")
           "on July 23, 1999"
           <> " (although observations only started about a month after "
           <> "this). The shaded regions indicate the number of science "
           <> "observations that " <> em "started"
           <>" on that day; normally there are only a handful, but "
           <> "occasionally the count can get quite high, which normally "
           <> "means a set of calibration observations of "
           <> (a ! href "/search/name?target=ArLac") "Ar Lac"
           <> " - for instance, "
           <> (a ! href "/schedule/date/2015-09-26/1")
           "the twenty-one observations on September 26, 2015"
           <> " - but it can sometimes be something different. "
           <> "Selecting a square will bring up a schedule for that day, "
           <> "along with a few days on either side, so you can explore; "
           <> "see if you can find the Venus observations!")

      divCts = 
        h2 "Calendar"
        <> paraBlock
        <> svgBlock
                
  in (div ! id "calendarBlock") divCts

