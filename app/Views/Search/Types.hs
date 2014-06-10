{-# LANGUAGE OverloadedStrings #-}

-- | Search on SIMBAD object type.

module Views.Search.Types (matchPage) where

-- import qualified Prelude as P
import Prelude (($), (++), Bool(..), Maybe(..), String, const, either, length, show)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>), mconcat, mempty)
import Data.Time (UTCTime)

import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (SimbadInfo, ScienceObs, Proposal, ObsInfo(..))
import Utils (defaultMeta, renderFooter)
import Views.Record (CurrentPage(..)
                    , mainNavBar, obsNavBar)

matchPage :: 
  String          -- SIMBAD object type (full version)
  -> [ScienceObs] -- non-empty list of observations that match this object type
  -> Html
matchPage objType matches =
  docTypeHtml ! lang "en-US" $
    head (H.title ("Chandra observations of " <> H.toHtml objType) <>
          defaultMeta <>
           link ! href   "/css/main.css"
                ! type_  "text/css" 
                ! rel    "stylesheet"
                ! A.title  "Default"
                ! media  "all"
          )
    <>
    body
     (mainNavBar CPOther
      <> (div ! id "mainBar") 
          (renderMatches objType matches)
      <> renderFooter
     )

-- | TODO: combine table rendering with Views.Schedule
--
--   TODO: will want a map of the sky here too
renderMatches ::
  String      -- ^ SIMBAD object type (full)
  -> [ScienceObs]  -- ^ non-empty list of matches
  -> Html
renderMatches objType matches = 
  p $ "There are " <> toHtml (length matches) <> " observations of " <> toHtml objType <> " in the database."

{-

  let single = case matches of
                [_] -> True
                _ -> False

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
      showJoint (Right so) = fromMaybe "n/a" $ toHtml `fmap` (soJointWith so)

      toRow :: (ChandraTime -> String) -> Record -> Html
      toRow ct r = hover r $ do
         td $ linkToRecord r
         td $ showJoint r
         td ! dataAttribute "sortvalue" (toValue (recordTime r)) $ showExp r
         td ! dataAttribute "sortvalue" (aTime r)
            ! class_ "starttime" 
              $ toHtml $ ct $ recordStartTime r
         td $ instVal r
         let ra = recordRa r
             dec = recordDec r
         td ! dataAttribute "sortvalue" (toValue ra)  $ toHtml $ showRA ra
         td ! dataAttribute "sortvalue" (toValue dec) $ toHtml $ showDec dec

      -- TODO: for current row, check whether should be using DeltaBwd/DeltaFwd
      cRow r = toRow (showTimeDeltaBwd (ChandraTime cTime) . _toUTCTime) r ! A.class_ "current"
      pRow r = toRow (`showTimeDeltaBwd` cTime) r ! A.class_ "prev"
      nRow r = toRow (showTimeDeltaFwd cTime) r ! A.class_ "next"

      getLongLat r = (recordRa r, recordDec r)

      conv :: Show a => a -> Html
      conv = toHtml . show

  in p $ if single
         then "Chandra observation of " <> H.toHtml objType
         else "Chandra observations of " <> H.toHtml objType
     <> table ! A.id "typeObs" ! class_ "tablesorter' $ do
          thead $ tr $ do
            th "Target"
            th "Joint with"
            th "Exposure time"
            th "Start"
            th "Instrument"
            th "Right Ascension"
            th "Declination"
          tbody $ mapM_ (toRow . Right) matches

-}
