{-# LANGUAGE OverloadedStrings #-}

-- | Show the "calendar" view.

module Views.Search.Calendar (indexPage) where

import qualified Prelude as P
import Prelude (Maybe(Just), (.), Int, fst)

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as M

import Data.Aeson ((.=))
import Data.Monoid ((<>))
import Data.Time.Calendar (Day, fromGregorian)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import API (jsScript, nameLinkSearch, scheduleOnDate)
import Layout (d3Meta)
import Utils (toJSVarObj, fromDay)
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

      deployLink = 
        (a ! href "http://chandra.harvard.edu/about/deployment.html")
        "on July 23, 1999"

      arLacSearchLink = nameLinkSearch "ArLac" (Just "Ar Lac")

      arLacSchedLink =
        let uri = scheduleOnDate day 1
            day = fromGregorian 2015 9 26
        in (a ! href uri)
           "the twenty-one observations on September 26, 2015"
      
      paraBlocks = 
        p ("This page shows the number of Chandra science observations per "
           <> "day. It is " <> em "very"
           <> " experimental and the results should not be taken too "
           <> "seriously as there are a lot of issues regarding "
           <> "getting hold of an accurate schedule. There's also the fact "
           <> "that the database does not go back all the way back to "
           <> "when Chandra was launched "
           <> deployLink
           <> " (note that observations only started about a month after "
           <> "this). The shaded regions indicate the number of science "
           <> "observations that " <> em "started"
           <>" on that day; normally there are only a handful, but "
           <> "occasionally the count can get quite high, which normally "
           <> "means a set of calibration observations of "
           <> arLacSearchLink
           <> " - for instance, "
           <> arLacSchedLink
           <> " - but it can sometimes be something different. "
           <> "Selecting a square will bring up a schedule for that day, "
           <> "along with a few days on either side, so you can explore; "
           <> "see if you can find the Venus observations!")
        <>
        p ("Periods of missing data could be because Chandra was not "
           <> "observing then - often because it was in its 'safe mode' "
           <> "where it stops observing, makes sure all the instruments "
           <> "are stowed, and ensures it is in an orientation that can "
           <> "contact the "
           <> (a ! href "https://eyes.nasa.gov/dsn/dsn.html")
           "Deep-Space Network"
           <> " - but it could also be due to missing data in the "
           <> "database used by this web site.")
           
      divCts = 
        h2 "Calendar"
        <> paraBlocks
        <> svgBlock
                
  in (div ! id "calendarBlock") divCts

