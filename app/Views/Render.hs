{-# LANGUAGE OverloadedStrings #-}

-- | A collection of routines.

module Views.Render (makeSchedule) where

-- import qualified Prelude as P
import Prelude ((.), ($), (==), (-), Integer, Either(..), Maybe(..), Show, String, mapM_, maybe, return, show, truncate)

-- import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad (void)

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mconcat, mempty)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (ScienceObs(..), ObsIdVal(..), Grating(..), ChandraTime(..), RA(..), Dec(..), TimeKS(..), Constraint(..))
import Types (Record, recordObsId, recordTarget, recordStartTime, recordTime, recordInstrument, recordGrating, recordRa, recordDec, showExp, showRA, showDec)
import Utils (obsURIString,
              showTimeDeltaFwd, showTimeDeltaBwd,
              linkToRecord)

-- | Convert the obsname of a record to an identifier
--   used in the HTML to identify riw/object.
idLabel :: Record -> String
idLabel = ("i" <>) . show . fromObsId . recordObsId

-- | Create a graph and table representing the
--   available observations.
--
makeSchedule ::
  UTCTime          -- current time
  -> [Record]      -- observations in the past
  -> Maybe Record  -- the current observation (if there is one)
  -> [Record]      -- observations in the future
  -> (Html, Html)  -- ("graph", table)
makeSchedule cTime done mdoing todo =
  let instVal r = fromMaybe "n/a" $ do
        inst <- recordInstrument r
        grat <- recordGrating r
        return $ toHtml inst <> if grat == NONE then mempty else " with " <> toHtml grat

      -- convert UTCTime to an integer
      aTime :: Record -> AttributeValue
      aTime = toValue . (truncate :: POSIXTime -> Integer) . utcTimeToPOSIXSeconds . _toUTCTime . recordStartTime

      hover r = let lbl = toValue $ idLabel r
                in tr ! id (toValue lbl)
                      ! onmouseover ("selectObs('" <> lbl <> "');")
                      ! onmouseout  ("deselectObs('" <> lbl <> "');")
      
      showJoint (Left _) = "n/a"
      showJoint (Right so) = maybe "n/a" toHtml (soJointWith so)

      showConstraint _ (Left _) = "n/a"
      showConstraint f (Right so) = case f so of
         NoConstraint -> "no"
         Preferred    -> "preferred"
         Required     -> "yes"

      toRow :: (ChandraTime -> String) -> Record -> Html
      toRow ct r = hover r $ do
         td $ linkToRecord r
         td $ showJoint r
         td $ showConstraint soTimeCritical r
         td $ showConstraint soMonitor r
         td $ showConstraint soConstrained r
         td ! dataAttribute "sortvalue" (toValue (recordTime r)) $ showExp r
         td ! dataAttribute "sortvalue" (aTime r)
            ! class_ "starttime" 
              $ toHtml $ ct $ recordStartTime r
         td $ instVal r
         let ra = recordRa r
             dec = recordDec r
         td ! dataAttribute "sortvalue" (toValue ra)  $ toHtml $ showRA ra
         td ! dataAttribute "sortvalue" (toValue dec) $ toHtml $ showDec dec

      cRow r = toRow (showTimeDeltaBwd (ChandraTime cTime) . _toUTCTime) r ! A.class_ "current"
      pRow r = toRow (`showTimeDeltaBwd` cTime) r ! A.class_ "prev"
      nRow r = toRow (showTimeDeltaFwd cTime) r ! A.class_ "next"

      getLongLat r = (recordRa r, recordDec r)

      conv :: Show a => a -> Html
      conv = toHtml . show

      tblBlock = table ! A.id "scheduledObs" ! class_ "tablesorter" $ do
                    thead $ tr $ do
                      th "Target"
                      th "Joint with"
                      th "Time critical"
                      th "Monitor"
                      th "Constrained"
                      th "Exposure time"
                      th "Start"
                      th "Instrument"
                      th "Right Ascension"
                      th "Declination"
                    tbody $ do
                      mapM_ pRow done
                      maybe mempty cRow mdoing
                      mapM_ nRow todo

      -- gah - manual conversion to JSON
      dataRow :: String -> Record -> Html
      dataRow s r =
        let (x, y) = getLongLat r
        -- we want to store the raw values, so extract numeric values from
        -- RA/Dec/time/... where appropriate    
        in mconcat [" { longitude: ", toHtml (180 - _unRA x),
                    ", latitude: ", toHtml (_unDec y),
                    ", texp: ", toHtml (_toS (recordTime r)),
                    ", idname: '", toHtml (idLabel r), "'",
                    ", label: ", conv (recordTarget r),
                    ", urifrag: ", conv (obsURIString (recordObsId r)),
                    ", status: ", conv s
                   , " }, "
                   ]
  
      svgBlock = do
        div ! id "map" $ ""
        script ! type_ "text/javascript" $ do
                   void "var obsinfo = ["
                   mapM_ (dataRow "done") done
                   maybe mempty (dataRow "doing") mdoing
                   mapM_ (dataRow "todo") todo
                   " ];"

  in (svgBlock, tblBlock)

