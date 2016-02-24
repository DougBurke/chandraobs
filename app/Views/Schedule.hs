{-# LANGUAGE OverloadedStrings #-}

-- | Display the schedule as a table.

module Views.Schedule (schedPage) where

-- import qualified Prelude as P
import Prelude (($), Maybe(..))

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>), mconcat)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (ChandraTime(..), Schedule(..))
import Utils (defaultMeta, skymapMeta, cssLink, renderFooter)
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

renderSchedule (Schedule cTime ndays done mdoing todo) =
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo

  in div ! A.id "scheduleBlock" $ do
    p  $ "The current time is: " <> toHtml (ChandraTime cTime) <> "."

    svgBlock

    p $ mconcat 
        [ "This page shows ", toHtml ndays
        , " days of the Chandra schedule, centered on today. "
        , "The size of the circles indicate the exposure time, and "
        , "the color shows whether the observation has been done, "
        , "is running now, or is in the future; the same colors "
        , "are used in the table below. For repeated observations "
        , "it can be hard to make out what is going on, since the "
        , "circles overlap! "
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

