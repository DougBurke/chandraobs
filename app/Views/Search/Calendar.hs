{-# LANGUAGE OverloadedStrings #-}

-- | Show the "calendar" view.

module Views.Search.Calendar (indexPage) where

import qualified Prelude as P
import Prelude ((.), ($), Int, String, fst)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Aeson ((.=))
import Data.Functor (void)
import Data.Monoid ((<>), mconcat)
import Data.Time (Day , showGregorian)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Utils (defaultMeta, d3Meta, renderFooter
             , jsScript, cssLink)
import Views.Record (CurrentPage(..), mainNavBar)

indexPage ::
  M.Map Day Int
  -- ^ The count histogram for the number of science observations
  --   per day.
  -> Html
indexPage cts =
  let jsLoad = "createCalendar(calinfo);"
  in docTypeHtml ! lang "en-US" $
    head (H.title "Chandra observations: a calendar view"
          <> defaultMeta
          <> d3Meta
          <> jsScript "/js/calendar-view.js"
          <> cssLink "/css/calendar.css"
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    (body ! onload jsLoad)
     (mainNavBar CPExplore
      <> renderMatches cts
      <> renderFooter
     )

renderMatches ::
  M.Map Day Int
  -> Html
renderMatches cts = 
  let svgBlock = do
        div ! id "calendar" $ ""
        script ! type_ "text/javascript" $ do
          void "var calinfo = "
          toHtml (LB8.unpack (Aeson.encode jsonCts))
          ";"

      fromDay = T.pack . showGregorian
      conv (d, n) = fromDay d .= n

      getDay = fromDay . fst

      dummyDate = "2016-01-01"
      (startDate, endDate) = if M.null cts
                             then (dummyDate, dummyDate)
                             else (getDay (M.findMin cts), getDay (M.findMax cts))

      jsonCts = Aeson.object [
        "startDate" .= startDate
        , "endDate" .= endDate
        , "counts" .= Aeson.object (P.map conv (M.toAscList cts))
        ]
                
  in div ! A.id "calendarBlock" $ do
    h2 $ toHtml ("Calendar"::String)

    p $ mconcat
        [ "This page shows the number of Chandra science observations per "
        , "day. It is " <> em "very" <> " experimental and the results should "
        , "not be taken too seriously as there are a lot of issues regarding "
        , "getting hold of an accurate schedule. There's also the fact that "
        , "I only currently include a small fraction of the total schedule! "
        , "The shaded regions indicate the number of science observations that "
        , "started on that day; normally there are only a handful, but occasionally "
        , "the count can get quite high, which normally means a set of "
        , "calibration observations of Ar Lac - for instance, "
        , (a ! href "/schedule/date/2015-09-26/1")
          "the twenty-one observations on September 26, 2015"
        , " - but it can sometimes be something different. "
        , "Selecting a day will bring up a schedule for that day, along with "
        , "a few days on either side, so you can explore; see if you can find "
        , "the Venus observations!"
        ]

    svgBlock

