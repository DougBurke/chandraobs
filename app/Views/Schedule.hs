{-# LANGUAGE OverloadedStrings #-}

-- | Display the schedule as a table.

module Views.Schedule (schedPage) where

-- import qualified Prelude as P
import Prelude ((.), ($), (==), Integer, Maybe(..), Show, String, map, mapM_, return, show, take, truncate)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad (void)

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mconcat, mempty)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Database (Schedule(..))
import PersistentTypes
import Types (Grating(..))
import Utils (defaultMeta, obsURI, showExp, showTimeDeltaFwd, showTimeDeltaBwd, projectMollweide)
import Views.Record (CurrentPage(..), mainNavBar)

jsScript :: AttributeValue -> Html
jsScript uri = script ! src uri $ ""

schedPage :: 
  Schedule
  -> Html
schedPage sched =
  docTypeHtml ! lang "en-US" $
    head (H.title "The Chandra schedule" 
          <> defaultMeta 
          <> jsScript "http://code.jquery.com/jquery-1.11.1.min.js"
          <> jsScript "http://d3js.org/d3.v3.min.js"
          <> jsScript "/js/jquery.tablesorter.min.js"
          <> jsScript "/js/table.js"
          <> jsScript "/js/projection.js"
          <> link ! href   "/css/tablesorter.css"
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
    (body ! onload "projectionMollweide(obsinfo);")
     (mainNavBar CPSchedule
      <> (div ! id "schedule") 
         (renderSchedule sched)
     )

linkToRecord :: Record -> Html
linkToRecord r = 
  let uri = obsURI r
  in a ! href uri $ toHtml $ recordTarget r

-- | The previous schedule could be displayed if there is
--   no current schedule, but let's just have a simple display
--   for now.
--
renderSchedule :: 
  Schedule
  -> Html
renderSchedule (Schedule _ _ _ Nothing _) =
  div ! A.id "schedule" $ 
    p "There seems to be a problem, in that I do not know what the current observation is!"

renderSchedule (Schedule cTime ndays done (Just doing) todo) =
  let conv :: Show a => a -> Html
      conv = toHtml . show

      instVal r = fromMaybe "n/a" $ do
        inst <- recordInstrument r
        grat <- recordGrating r
        return $ toHtml inst <> if grat == NONE then mempty else " with " <> toHtml grat

      -- convert UTCTime to an integer
      aTime :: Record -> AttributeValue
      aTime = toValue . (truncate :: POSIXTime -> Integer) . utcTimeToPOSIXSeconds . recordStartTime
        
      toRow :: (UTCTime -> String) -> Record -> Html
      toRow ct r = tr $ do
         td $ linkToRecord r
         td ! dataAttribute "sortvalue" (toValue (recordTime r)) $ showExp r
         td ! dataAttribute "sortvalue" (aTime r) $ toHtml $ ct $ recordStartTime r
         td $ instVal r

      cRow r = toRow (showTimeDeltaBwd cTime) r ! A.id "current" -- TODO: check Fwd/Bwd
      pRow r = toRow (`showTimeDeltaBwd` cTime) r ! A.class_ "prev"
      nRow r = toRow (showTimeDeltaFwd cTime) r ! A.class_ "next"

      getLongLat r = (recordRa r, recordDec r)

      -- gah - manual conversion to JSON
      dataRow :: String -> Record -> Html
      dataRow s r =
        let (x, y) = projectMollweide . getLongLat $ r
        in mconcat [" { x: ", toHtml x, ", y: ", toHtml y,
                    ", texp: ", toHtml (recordTime r),
                    ", id: '", toHtml (recordObsname r), "'",
                    ", label: ", conv (recordTarget r),
                    ", status: ", conv s
                   , " }, "
                   ]
      
  in div ! A.id "scheduleBlock" $ do
    p $ mconcat 
        [ "This page shows ", conv ndays
        , " days of the Chandra schedule around today. "
        , "The current time is: "
        , conv cTime
        , "."
        ]

    table ! A.id "scheduledObs" ! class_ "tablesorter" $ do
      thead $ tr $ do
        th "Target"
        th "Exposure time"
        th "Start"
        th "Instrument"
      tbody $ do
        mapM_ pRow done
        cRow doing
        mapM_ nRow todo

    -- Set up the coordinates
    script ! type_ "text/javascript" $ do
      void $ "var obsinfo = ["
      mapM_ (dataRow "done") done
      dataRow "doing" doing
      mapM_ (dataRow "todo") todo
      " ];"
      
    div ! id "map" $ ""
