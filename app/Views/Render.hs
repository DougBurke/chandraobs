{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | A collection of routines.

module Views.Render ( makeSchedule
                     ) where

-- import qualified Prelude as P
import Prelude ((.), ($), (==), (-), (+)
               , Int, Integer, Either(..), Maybe(..)
               , map, mapM_, maybe, return, show, truncate)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Aeson ((.=))
import Data.Bits (shiftL)
import Data.Functor (void)
import Data.List (foldl', intersperse)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>), mconcat, mempty)
import Data.Text.Encoding (decodeUtf8')
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (ScienceObs(..), ObsIdVal(..), Grating(..), ChandraTime(..)
             , RA(..), Dec(..), TimeKS(..), Constraint(..), ConShort(..)
             , SimbadInfo(..), Record, TargetName)
import Types (recordObsId, recordTarget, recordStartTime, recordTime
             , recordInstrument, recordGrating, recordRa, recordDec
             , showExp, toMission)
import Utils (obsURIString
             , showTimeDeltaFwd, showTimeDeltaBwd
             , linkToRecord
             , instLinkSearch
             , gratLinkSearch
             , typeDLinkSearch
             , basicTypeLinkSearch
             , constellationLinkSearch
             , jointLinkSearch
             , cleanJointName
             )

-- | Convert the obsname of a record to an identifier
--   used in the HTML to identify riw/object.
idLabel :: Record -> T.Text
idLabel = ("i" <>) . T.pack . show . fromObsId . recordObsId

-- | Take the joint-with field and create an entry for
--   it.
--
--   There should be no un-supported missions, but just
--   in case these are reported directly, with no link.
--
--   TODO: do I need to add in a sortvalue for the column?
--
makeJointLinks :: T.Text -> Html
makeJointLinks jw =
  let toks = T.splitOn "+" (cleanJointName jw)
      conv m = maybe (toHtml m) jointLinkSearch
               (toMission m)
      ms = map conv toks
  in mconcat (intersperse "," ms)

-- | Create a graph and table representing the
--   available observations.
--
--   The column search javascript code must be updated
--   if any columns are added or removed. This is in
--   static/js/table.js
--
makeSchedule ::
  UTCTime          -- current time
  -> [Record]      -- observations in the past
  -> Maybe Record  -- the current observation (if there is one)
  -> [Record]      -- observations in the future
  -> M.Map TargetName SimbadInfo
  -- Target information (may be empty)
  -> (Html, Html)  -- ("graph", table)
makeSchedule cTime done mdoing todo simbad =
  let instVal r = fromMaybe "n/a" $ do
        inst <- recordInstrument r
        grat <- recordGrating r
        return (instLinkSearch inst
                <> if grat == NONE
                   then mempty
                   else " with " <> gratLinkSearch grat)

      -- TODO: add in a sortvalue for this column so that "n/a" and "" can be
      --       moved before or after the other types. As I have now decided
      --       to treat "unidentified" sources as an actual group, this
      --       needs a link, which removes the need to handle "n/a",
      --       but "" is still there. Perhaps that can be changed to
      --       "n/a" now.
      linkToSimbad (Left _) = ""
      linkToSimbad (Right so) = case M.lookup (soTarget so) simbad of
        Just si -> typeDLinkSearch (smiType3 si) (smiType si)
        Nothing -> basicTypeLinkSearch Nothing

      -- convert UTCTime to an integer; this does not have to
      -- special case unscheduled records where the time is set
      -- to futureTime, as that is a value intentionally set
      -- to "the future".
      --
      aTime :: Record -> AttributeValue
      aTime = toValue . (truncate :: POSIXTime -> Integer) . utcTimeToPOSIXSeconds . _toUTCTime . recordStartTime

      hover r = let lbl = toValue $ idLabel r
                in tr ! id (toValue lbl)
                      ! onmouseover ("selectObs('" <> lbl <> "');")
                      ! onmouseout  ("deselectObs('" <> lbl <> "');")
      
      showJoint (Left _) = "n/a"
      showJoint (Right so) = maybe "n/a" makeJointLinks (soJointWith so)
                             

      -- TODO: Are there any soConstrained fields with a Preferred constraint?
      --       If so, the output of this could be improved
      showConstraint :: (T.Text, Constraint) -> Maybe T.Text
      showConstraint (_, NoConstraint) = Nothing
      showConstraint (l, Preferred)    = Just (l <> " preferred")
      showConstraint (l, Required)     = Just l

      showTOO (Left _) = "n/a"
      showTOO (Right ScienceObs{..}) = toHtml $ fromMaybe "n/a" soTOO

      -- for now just the short form
      showConstellation (Left _)   = "n/a"
      showConstellation (Right ScienceObs{..}) =
            constellationLinkSearch soConstellation (fromConShort soConstellation)

      score NoConstraint = 0
      score Preferred    = 1
      score Required     = 2

      -- "constraints" are a combination of soTimeCritical, soMonitor, soConstrained
      --
      -- for the score, we have 2 bits of info for each constraint as there are
      -- three fields in a constraint, so need to combine them by shifting by
      -- 2, picking an order
      constraintScore :: Either a ScienceObs -> Int
      constraintScore (Left _) = 0
      constraintScore (Right ScienceObs{..}) = 
          let f o n = n + shiftL o 2
          in foldl' f 0 $ map score [soTimeCritical, soMonitor, soConstrained]

      constraintText :: Either a ScienceObs -> Html
      constraintText (Left _) = "n/a"
      constraintText (Right ScienceObs{..}) = 
          let cons = [("time critical", soTimeCritical),
                      ("monitor", soMonitor),
                      ("constrained", soConstrained)]
          in case mapMaybe showConstraint cons of
               [] -> "n/a"
               xs -> toHtml (mconcat (intersperse ", " xs))

      toRow :: (ChandraTime -> T.Text) -> Record -> Html
      toRow ct r = hover r $ do
         td (linkToRecord r)
         td (linkToSimbad r)
         (td ! dataAttribute "sortvalue" (toValue (recordTime r))) (showExp r)
         (td ! dataAttribute "sortvalue" (aTime r)
             ! class_ "starttime") (toHtml (ct (recordStartTime r)))
         td (instVal r)

         td (showJoint r)
         td (showTOO r)
         {-
         td $ showConstraint soTimeCritical r
         td $ showConstraint soMonitor r
         td $ showConstraint soConstrained r
         -}
         (td ! dataAttribute "sortValue" (toValue (constraintScore r)))
           (constraintText r)

         let ra = recordRa r
             dec = recordDec r
         (td ! dataAttribute "sortvalue" (toValue ra))  (toHtml ra)
         (td ! dataAttribute "sortvalue" (toValue dec)) (toHtml dec)
         td (showConstellation r)

      cRow r = toRow (showTimeDeltaBwd (ChandraTime cTime) . _toUTCTime) r ! A.class_ "current"
      pRow r = toRow (`showTimeDeltaBwd` cTime) r ! A.class_ "prev"
      nRow r = toRow (showTimeDeltaFwd cTime) r ! A.class_ "next"

      getLongLat r = (recordRa r, recordDec r)

      -- TODO: change order
      tblBlock = table ! A.id "scheduledObs" ! class_ "tablesorter" $ do
                    thead $ tr $ do
                      th "Target"
                      th "Object type"
                      th "Exposure time"
                      th "Start"
                      th "Instrument"
                      th "Joint with"
                      th "TOO period"
                      {-
                      th "Time critical"
                      th "Monitor"
                      th "Constrained"
                      -}
                      th "Constraints"
                      th "Right Ascension"
                      th "Declination"
                      th "Constellation"
                    tbody $ do
                      mapM_ pRow done
                      maybe mempty cRow mdoing
                      mapM_ nRow todo

      dataRow :: T.Text -> Record -> Aeson.Value
      dataRow s r =
        let (x, y) = getLongLat r
            -- repeats the linkToSimbad logic; but this time assumes that
            -- it is safe to include non-science obs as they won't match
            sinfo = case M.lookup (recordTarget r) simbad of
              Just si -> [ "simbadType" .= smiType si ]
              Nothing -> []
              
        -- we want to store the raw values, so extract numeric values from
        -- RA/Dec/time/... where appropriate
        -- could also include object type if available
        in Aeson.object
           ([ "longitude" .= (180 - _unRA x)
            , "latitude" .= _unDec y
            , "texp" .= _toKS (recordTime r)
            , "idname" .= idLabel r
            , "label" .= recordTarget r
            , "urifrag" .= obsURIString (recordObsId r)
            , "status" .= s
            ] <> sinfo)
           
      obsInfo =
        map (dataRow "done") done
        <> maybe [] ((:[]) . (dataRow "doing")) mdoing
        <> map (dataRow "todo") todo

      jsHtml = case decodeUtf8' (LB8.toStrict (Aeson.encode obsInfo)) of
        Right ans -> toHtml ans
        Left _ -> "[]"

      svgBlock = do
        (div ! id "map") ""
        script ! type_ "text/javascript" $ do
                   void "var obsinfo = "
                   jsHtml
                   ";"

  in (svgBlock, tblBlock)

