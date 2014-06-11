{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Search on SIMBAD object type.

module Views.Search.Types (matchPage) where

-- import qualified Prelude as P
import Prelude (($), (==), String, length)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>)) -- , mconcat, mempty)
-- import Data.Time (UTCTime)

import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (ScienceObs(..), SimbadType(..), SimbadTypeInfo)
import Utils (defaultMeta, renderFooter)
import Views.Record (CurrentPage(..)
                    , mainNavBar)

matchPage :: 
  SimbadTypeInfo
  -> [ScienceObs] -- non-empty list of observations that match this object type
  -> Html
matchPage (shortType, longType) matches =
  docTypeHtml ! lang "en-US" $
    head (H.title ("Chandra observations of " <> H.toHtml longType) <>
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
          (renderMatches shortType longType matches)
      <> renderFooter
     )

-- | TODO: combine table rendering with Views.Schedule
--
--   TODO: will want a map of the sky here too
renderMatches ::
  SimbadType
  -> String        -- ^ long version of SimbadType
  -> [ScienceObs]  -- ^ non-empty list of matches
  -> Html
renderMatches shortType longType matches = do
  h2 $ toHtml longType
  let nmatch = length matches
  p $ if nmatch == 1
      then "There is one observation in the database."
      else "There are " <> toHtml nmatch <> " observations in the database."
  p $ "This will eventually include a display similar to the "
      <> (a ! href "/schedule/" $ "schedule page")
      <> "."

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
