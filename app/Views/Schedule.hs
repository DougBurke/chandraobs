{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Display the schedule as a table.
--
module Views.Schedule (schedPage, schedDatePage) where

-- import qualified Prelude as P
import Prelude (Maybe(..), ($), (==), show)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>), mconcat)

#if defined(MIN_VERSION_time) && MIN_VERSION_time(1,5,0)
import Data.Time (Day, defaultTimeLocale, formatTime, showGregorian)
#else
import Data.Time (Day, formatTime, showGregorian)
import System.Locale (defaultTimeLocale)
#endif

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (Schedule(..))
import Utils (defaultMeta, skymapMeta, cssLink, renderFooter
             , getScienceTime)
import Views.Record (CurrentPage(..), mainNavBar)
import Views.Render (makeSchedule)

schedPage :: 
  Schedule
  -> Html
schedPage sched =
  docTypeHtml ! lang "en-US" $
    head (H.title "The Chandra schedule" 
          <> defaultMeta
          <> skymapMeta
          <> cssLink "/css/schedule.css"
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    (body ! onload "createMap(obsinfo);")
     (mainNavBar CPSchedule
      <> (div ! id "schedule") 
         (renderSchedule sched)
      <> renderFooter
     )

schedDatePage ::
  Day
  -> Schedule
  -> Html
schedDatePage date sched =
  docTypeHtml ! lang "en-US" $
    head (H.title ("The Chandra schedule for " <> H.toHtml (showGregorian date))
          <> defaultMeta
          <> skymapMeta
          <> cssLink "/css/schedule.css"
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    (body ! onload "createMap(obsinfo);")
     (mainNavBar CPSchedule
      <> (div ! id "schedule") 
         (renderDateSchedule date sched)
      <> renderFooter
     )

-- | The previous schedule could be displayed if there is
--   no current schedule, but let's just have a simple display
--   for now.
--
renderSchedule :: 
  Schedule
  -> Html
renderSchedule (Schedule _ _ _ Nothing _ _) =
  div ! A.id "schedule" $ 
    p ("There seems to be a problem, in that I do not know what the "
       <> "current observation is!")

renderSchedule (Schedule cTime ndays done mdoing todo simbad) =
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo simbad
      scienceTime = getScienceTime done mdoing todo
                         
      -- the assumption is that ndays > 0
      title = show ndays <> "-day Schedule"
      hdays = if ndays == 1
              then "one day"
              else toHtml (show ndays) <> " days"
      
  in div ! A.id "scheduleBlock" $ do
    h2 (toHtml title)

    svgBlock

    p $ mconcat 
        [ "This page shows ", hdays
        , " of the Chandra schedule, either side of today"
        , scienceTime
        , ". The size of the circles indicate the exposure time, and "
        , "the color shows whether the observation has been done, "
        , "is running now, or is in the future; the same colors "
        , "are used in the table below. For repeated observations "
        , "it can be hard to make out what is going on, since the "
        , "circles overlap! The shaded regions trace "
        , "the Milky Way galaxy. "
        , "The points are plotted in the "
        , a ! href "http://en.wikipedia.org/wiki/Equatorial_coordinate_system#Use_in_astronomy" $ "Equatorial coordinate system"
        , ", using the "
        , a ! href "http://en.wikipedia.org/wiki/Aitoff_projection" $ "Aitoff projection"
        , ". See "
        , a ! href "http://burro.astr.cwru.edu/" $ "Chris Mihos'"
        , " page on "
        , a ! href "http://burro.cwru.edu/Academics/Astr306/Coords/coords.html" $ "Astronomical coordinate systems"
        , " for more informaion."
        ]

    tblBlock

  

renderDateSchedule :: 
  Day
  -> Schedule
  -> Html
renderDateSchedule date (Schedule cTime ndays done mdoing todo simbad) =
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo simbad
      scienceTime = getScienceTime done mdoing todo

      -- the assumption is that ndays > 0
      title = show ndays <> "-day Schedule for " <> showGregorian date
      hdays = if ndays == 1
              then "one day"
              else toHtml (show ndays) <> " days"

      dateStr = formatTime defaultTimeLocale "%A, %B %e, %Y" date 
      
  in div ! A.id "scheduleBlock" $ do
    h2 (toHtml title)

    svgBlock

    p $ mconcat 
        [ "This page shows ", hdays
        , " of the Chandra schedule either side of "
        , toHtml dateStr
        , scienceTime
        , ". The format is the same as used in the "
        , (a ! href "/schedule") "schedule view"
        , "."
        ]

    tblBlock

