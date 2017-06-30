{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | A collection of routines.

module Views.Render (standardSchedulePage
                    , extraSchedulePage
                    , baseSchedulePage
                    , standardRestrictedSchedulePage
                    , standardExplorePage
                    , extraExplorePage
                    ) where

-- import qualified Prelude as P
import Prelude ((.), ($), (==), (-), (+)
               , Int, Integer, Either(..), Maybe(..)
               , fmap, map, mapM_, maybe, not, return, snd, truncate
               , zip)

import Prelude ((<$>), mconcat, mempty)

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Aeson ((.=))
import Data.Bits (shiftL)
import Data.Char (isSpace)
import Data.List (foldl', intersperse)
import Data.Maybe (fromMaybe, mapMaybe)

import Data.Monoid ((<>))

import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import API (obsURIString
            , linkToRecord
            , linkToRestrictedRecord
            , instLinkSearch
            , gratLinkSearch
            , tooLinkSearch
            , typeDLinkSearch
            , basicTypeLinkSearch
            , constellationLinkSearch
            , jointLinkSearch
            , constraintLinkSearch
            , cssLink)
import Layout (defaultMeta, skymapMeta, renderFooter)
import Types (ScienceObs(..), ObsIdVal(..), Grating(..), ChandraTime(..)
             , NonScienceObs(nsTarget)
             , RA(..), Dec(..), TimeKS(..), Constraint(..), ConShort(..)
             , SimbadInfo(..), Record, TOORequest(..)
             , ConstraintKind(..)
             , Schedule(..)
             , RestrictedSchedule(..), RestrictedRecord
             , RestrictedSO
             -- , RestrictedNS
             , TargetName(..)
             , recordObsId, recordTarget, recordStartTime, recordTime
             , recordInstrument, recordGrating, recordRa, recordDec
             , showExp, showExpRestricted

             , rrecordObsId
             , rrecordInstrument
             , rrecordGrating
             , rrecordStartTime
             , rrecordTime
             , rrecordTarget
             , rrecordRA
             , rrecordDec
               
             , rsoTarget
             , rsoJointWith
             , rsoTOO
             , rsoConstraints
             , rsoConstellation

             , rnsTarget
               
             , toMission)
import Utils (showTimeDeltaFwd, showTimeDeltaBwd
             , cleanJointName
             , showInt
             , toJSVarArr
             )

import Views.Record (CurrentPage(..), mainNavBar)

-- | Convert the obsname of a record to an identifier
--   used in the HTML to identify riw/object.
idLabel :: Record -> T.Text
idLabel = ("i" <>) . showInt . fromObsId . recordObsId

ridLabel :: RestrictedRecord -> T.Text
ridLabel = ("i" <>) . showInt . fromObsId . rrecordObsId

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

-- Can we just send in the information we need to create
-- the schedule, to reduce the DB queries. If it's too much,
-- so that we need most of a Record to do this, then it's
-- plan D.
--

makeScheduleRestricted ::
  RestrictedSchedule
  -> (Html, Html)  -- ("graph", table)
makeScheduleRestricted (RestrictedSchedule cTime _ done mdoing todo simbad) =
  let instVal r = fromMaybe "n/a" $ do
        inst <- rrecordInstrument r
        grat <- rrecordGrating r
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
      linkToSimbad (Right so) = case M.lookup (rsoTarget so) simbad of
        Just si -> typeDLinkSearch (smiType3 si) (smiType si)
        Nothing -> basicTypeLinkSearch Nothing

      -- convert UTCTime to an integer. If there's no time then replace
      -- with 0.
      --
      getT :: ChandraTime -> Integer
      getT = (truncate :: POSIXTime -> Integer) .
             utcTimeToPOSIXSeconds . _toUTCTime
               
      aTime :: RestrictedRecord -> AttributeValue
      aTime r = toValue (case rrecordStartTime r of
                          Just t -> getT t
                          Nothing -> 0)

      hover r = let lbl = toValue (ridLabel r)
                in tr ! id (toValue lbl)
                      ! onmouseover ("projection.selectObs('" <> lbl <> "');")
                      ! onmouseout  ("projection.deselectObs('" <> lbl <> "');")
      
      showJoint (Left _) = "n/a"
      showJoint (Right so) = maybe "n/a" makeJointLinks (rsoJointWith so)
                             
      -- TODO: Are there any soConstrained fields with a Preferred constraint?
      --       If so, the output of this could be improved
      showConstraint :: (ConstraintKind, Constraint) -> Maybe Html
      showConstraint (_, NoConstraint) = Nothing
      showConstraint (l, Preferred)    = Just (constraintLinkSearch (Just l)
                                               <> " preferred")
      showConstraint (l, Required)     = Just (constraintLinkSearch (Just l))

      showTOO (Left _) = "n/a"
      -- showTOO (Right ScienceObs{..}) = maybe "n/a" tooLinkSearch soTOO
      showTOO (Right so) = tooLinkSearch (trType `fmap` (rsoTOO so))

      -- for now just the short form
      showConstellation (Left _)   = "n/a"
      showConstellation (Right so) =
        let con = rsoConstellation so
        in constellationLinkSearch con (fromConShort con)

      score NoConstraint = 0
      score Preferred    = 1
      score Required     = 2

      -- "constraints" are a combination of soTimeCritical, soMonitor,
      -- and soConstrained
      --
      -- for the score, we have 2 bits of info for each constraint as there are
      -- three fields in a constraint, so need to combine them by shifting by
      -- 2, picking an order
      constraintScore :: Either a RestrictedSO -> Int
      constraintScore (Left _) = 0
      constraintScore (Right so) = 
          let f o n = n + shiftL o 2
          in foldl' f 0 $ map score (rsoConstraints so)

      constraintText :: Either a RestrictedSO -> Html
      constraintText (Left _) = "n/a"
      constraintText (Right so) = 
          let cons = zip [TimeCritical, Monitor, Constrained]
                     (rsoConstraints so)
          in case mapMaybe showConstraint cons of
               [] -> "n/a"
               xs -> toHtml (mconcat (intersperse ", " xs))

      sortRow sortVal row =
        row ! dataAttribute "sortvalue" (toValue sortVal)
        
      -- The sortvalue for this column is the SIMBAD name for
      -- the source, if it exists.
      --
      -- Add a prefix of "_" to calibration observations to separate
      -- out.
      makeTargetRow r =
        let row = td (linkToRestrictedRecord r)
            sname = case r of
              Left ns -> "_" <> fromTargetName (rnsTarget ns)
              Right so ->
                let tname = rsoTarget so
                in case M.lookup tname simbad of
                  Just si -> fromTargetName (smiName si)
                  Nothing -> fromTargetName tname

            -- Spaces should not be a problem, but remove them just in case.
            cname = T.filter (not . isSpace) sname
        in sortRow cname row

      toRow :: (Maybe ChandraTime -> T.Text) -> RestrictedRecord -> Html
      toRow ct r = hover r $ do
         makeTargetRow r
         td (linkToSimbad r)
         sortRow (rrecordTime r) (td (showExpRestricted r))
         sortRow (aTime r) ((td ! class_ "starttime")
                            (toHtml (ct (rrecordStartTime r))))
         td (instVal r)

         td (showJoint r)
         td (showTOO r)
         sortRow (constraintScore r) (td (constraintText r))

         let ra = rrecordRA r
             dec = rrecordDec r
         sortRow ra (td (toHtml ra))
         sortRow dec (td (toHtml dec))
         td (showConstellation r)

      tNow = ChandraTime cTime
      curTime Nothing = "observation is not scheduled"
      curTime (Just t) = showTimeDeltaBwd (Just tNow) (_toUTCTime t)
      
      cRow r = toRow curTime r ! A.class_ "current"
      pRow r = toRow (`showTimeDeltaBwd` cTime) r ! A.class_ "prev"
      nRow r = toRow (showTimeDeltaFwd cTime) r ! A.class_ "next"

      -- TODO: change order
      tblBlock = table ! A.id "scheduledObs" ! class_ "tablesorter" $ do
                    thead $ tr $ do
                      th "Target"
                      th "Object type"
                      th "Exposure time"
                      th "Start"
                      th "Instrument"
                      th "Joint with"
                      th "Turnaround"
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

      dataRow :: T.Text -> RestrictedRecord -> Aeson.Value
      dataRow s r =
        let -- repeats the linkToSimbad logic; but this time assumes that
            -- it is safe to include non-science obs as they won't match
            sinfo = case M.lookup (rrecordTarget r) simbad of
              Just si -> [ "simbadType" .= smiType si ]
              Nothing -> []

        -- we want to store the raw values, so extract numeric values from
        -- RA/Dec/time/... where appropriate
        -- could also include object type if available
        in Aeson.object
           ([ "longitude" .= (180 - _unRA (rrecordRA r))
            , "latitude" .= _unDec (rrecordDec r)
            , "texp" .= _toKS (rrecordTime r)
            , "idname" .= ridLabel r
            , "label" .= rrecordTarget r
            , "urifrag" .= obsURIString (rrecordObsId r)
            , "status" .= s
            ] <> sinfo)
           
      obsInfo =
        map (dataRow "done") done
        <> maybe [] ((:[]) . (dataRow "doing")) mdoing
        <> map (dataRow "todo") todo

      svgBlock = do
        (div ! id "map") ""
        toJSVarArr "obsinfo" obsInfo

  in (svgBlock, tblBlock)



-- This is the original code
makeSchedule ::
  Schedule
  -> (Html, Html)  -- ("graph", table)
makeSchedule (Schedule cTime _ done mdoing todo simbad) =
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

      -- convert UTCTime to an integer. If there's no time then replace
      -- with 0.
      --
      getT :: ChandraTime -> Integer
      getT = (truncate :: POSIXTime -> Integer) .
             utcTimeToPOSIXSeconds . _toUTCTime
               
      aTime :: Record -> AttributeValue
      aTime r = toValue (case recordStartTime r of
                          Just t -> getT t
                          Nothing -> 0)

      hover r = let lbl = toValue $ idLabel r
                in tr ! id (toValue lbl)
                      ! onmouseover ("projection.selectObs('" <> lbl <> "');")
                      ! onmouseout  ("projection.deselectObs('" <> lbl <> "');")
      
      showJoint (Left _) = "n/a"
      showJoint (Right so) = maybe "n/a" makeJointLinks (soJointWith so)
                             

      -- TODO: Are there any soConstrained fields with a Preferred constraint?
      --       If so, the output of this could be improved
      showConstraint :: (ConstraintKind, Constraint) -> Maybe Html
      showConstraint (_, NoConstraint) = Nothing
      showConstraint (l, Preferred)    = Just (constraintLinkSearch (Just l)
                                               <> " preferred")
      showConstraint (l, Required)     = Just (constraintLinkSearch (Just l))

      showTOO (Left _) = "n/a"
      -- showTOO (Right ScienceObs{..}) = maybe "n/a" tooLinkSearch soTOO
      showTOO (Right ScienceObs{..}) = tooLinkSearch (trType `fmap` soTOO)

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
          let cons = [(TimeCritical, soTimeCritical),
                      (Monitor, soMonitor),
                      (Constrained, soConstrained)]
          in case mapMaybe showConstraint cons of
               [] -> "n/a"
               xs -> toHtml (mconcat (intersperse ", " xs))

      sortRow sortVal row =
        row ! dataAttribute "sortvalue" (toValue sortVal)
        
      -- The sortvalue for this column is the SIMBAD name for
      -- the source, if it exists.
      --
      -- Add a prefix of "_" to calibration observations to separate
      -- out.
      makeTargetRow r =
        let row = td (linkToRecord r)
            sname = case r of
              Left ns -> "_" <> fromTargetName (nsTarget ns)
              Right so -> case M.lookup (soTarget so) simbad of
                Just si -> fromTargetName (smiName si)
                Nothing -> fromTargetName (soTarget so)

            -- Spaces should not be a problem, but remove them just in case.
            cname = T.filter (not . isSpace) sname
        in sortRow cname row

      toRow :: (Maybe ChandraTime -> T.Text) -> Record -> Html
      toRow ct r = hover r $ do
         makeTargetRow r
         td (linkToSimbad r)
         sortRow (recordTime r) (td (showExp r))
         sortRow (aTime r) ((td ! class_ "starttime")
                            (toHtml (ct (recordStartTime r))))
         td (instVal r)

         td (showJoint r)
         td (showTOO r)
         sortRow (constraintScore r) (td (constraintText r))

         let ra = recordRa r
             dec = recordDec r
         sortRow ra (td (toHtml ra))
         sortRow dec (td (toHtml dec))
         td (showConstellation r)

      tNow = ChandraTime cTime
      curTime Nothing = "observation is not scheduled"
      curTime (Just t) = showTimeDeltaBwd (Just tNow) (_toUTCTime t)
      
      cRow r = toRow curTime r ! A.class_ "current"
      pRow r = toRow (`showTimeDeltaBwd` cTime) r ! A.class_ "prev"
      nRow r = toRow (showTimeDeltaFwd cTime) r ! A.class_ "next"

      -- TODO: change order
      tblBlock = table ! A.id "scheduledObs" ! class_ "tablesorter" $ do
                    thead $ tr $ do
                      th "Target"
                      th "Object type"
                      th "Exposure time"
                      th "Start"
                      th "Instrument"
                      th "Joint with"
                      th "Turnaround"
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
        let -- repeats the linkToSimbad logic; but this time assumes that
            -- it is safe to include non-science obs as they won't match
            sinfo = case M.lookup (recordTarget r) simbad of
              Just si -> [ "simbadType" .= smiType si ]
              Nothing -> []

        -- we want to store the raw values, so extract numeric values from
        -- RA/Dec/time/... where appropriate
        -- could also include object type if available
        in Aeson.object
           ([ "longitude" .= (180 - _unRA (recordRa r))
            , "latitude" .= _unDec (recordDec r)
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

      svgBlock = do
        (div ! id "map") ""
        toJSVarArr "obsinfo" obsInfo

  in (svgBlock, tblBlock)


-- | Pages which include a schedule block.
--
standardSchedulePage ::
  Schedule
  -> CurrentPage
  -- ^ location for the nav bar
  -> Html
  -- ^ title (header)
  -> Html
  -- ^ title of the page (displayed as H2 above the SVG display)
  -> Html
  -- ^ explanation block; displayed below the SVG display
  -> Html
standardSchedulePage sched navLoc hdrTitle pageTitle explain =
  let jsLoad = "projection.createMap(obsinfo);"
  in extraSchedulePage sched navLoc hdrTitle pageTitle explain jsLoad

standardRestrictedSchedulePage ::
  RestrictedSchedule
  -> CurrentPage
  -- ^ location for the nav bar
  -> Html
  -- ^ title (header)
  -> Html
  -- ^ title of the page (displayed as H2 above the SVG display)
  -> Html
  -- ^ explanation block; displayed below the SVG display
  -> Html
standardRestrictedSchedulePage sched navLoc hdrTitle pageTitle explain =
  let jsLoad = "projection.createMap(obsinfo);"
  in extraRestrictedSchedulePage sched navLoc hdrTitle pageTitle explain jsLoad

-- | Pages which include a schedule block.
--
extraSchedulePage ::
  Schedule
  -> CurrentPage
  -- ^ location for the nav bar
  -> Html
  -- ^ title (header)
  -> Html
  -- ^ title of the page (displayed as H2 above the SVG display)
  -> Html
  -- ^ explanation block; displayed below the SVG display
  -> AttributeValue
  -- ^ onload javascript
  -> Html
extraSchedulePage sched navLoc hdrTitle pageTitle explain jsLoad =
  baseSchedulePage sched navLoc hdrTitle pageTitle explain jsLoad Nothing

extraRestrictedSchedulePage ::
  RestrictedSchedule
  -> CurrentPage
  -- ^ location for the nav bar
  -> Html
  -- ^ title (header)
  -> Html
  -- ^ title of the page (displayed as H2 above the SVG display)
  -> Html
  -- ^ explanation block; displayed below the SVG display
  -> AttributeValue
  -- ^ onload javascript
  -> Html
extraRestrictedSchedulePage sched navLoc hdrTitle pageTitle explain jsLoad =
  baseRestrictedSchedulePage sched navLoc hdrTitle pageTitle explain jsLoad Nothing

-- | Pages which include a schedule block.
--
baseSchedulePage ::
  Schedule
  -> CurrentPage
  -- ^ location for the nav bar
  -> Html
  -- ^ title (header)
  -> Html
  -- ^ title of the page (displayed as H2 above the SVG display)
  -> Html
  -- ^ explanation block; displayed below the SVG display
  -> AttributeValue
  -- ^ onload javascript
  -> Maybe AttributeValue
  -- ^ optional extra CSS page
  -> Html
baseSchedulePage sched navLoc hdrTitle pageTitle explain jsLoad mcss =
  let hdr = (H.title hdrTitle
             <> defaultMeta
             <> skymapMeta

             -- TODO: should /css/schedule.css be made optional
             <> cssLink "/css/schedule.css"

             <> fromMaybe mempty (cssLink <$> mcss)

             <> (cssLink "/css/main.css" ! A.title "Default")
            )

      (svgBlock, tblBlock) = makeSchedule sched
      
      -- TODO: clean up CSS for div.schedule div.scheduleBlock
      bodyBlock = div ! A.id "scheduleBlock" $ do
        h2 pageTitle
        svgBlock
        explain
        tblBlock

  in docTypeHtml ! lang "en-US" $
    head hdr
    <>
    (body ! onload jsLoad)
      (mainNavBar navLoc
       <> (div ! id "schedule") bodyBlock
       <> renderFooter
      )


baseRestrictedSchedulePage ::
  RestrictedSchedule
  -> CurrentPage
  -- ^ location for the nav bar
  -> Html
  -- ^ title (header)
  -> Html
  -- ^ title of the page (displayed as H2 above the SVG display)
  -> Html
  -- ^ explanation block; displayed below the SVG display
  -> AttributeValue
  -- ^ onload javascript
  -> Maybe AttributeValue
  -- ^ optional extra CSS page
  -> Html
baseRestrictedSchedulePage sched navLoc hdrTitle pageTitle explain jsLoad mcss =
  let hdr = (H.title hdrTitle
             <> defaultMeta
             <> skymapMeta

             -- TODO: should /css/schedule.css be made optional
             <> cssLink "/css/schedule.css"

             <> fromMaybe mempty (cssLink <$> mcss)

             <> (cssLink "/css/main.css" ! A.title "Default")
            )

      (svgBlock, tblBlock) = makeScheduleRestricted sched
      
      -- TODO: clean up CSS for div.schedule div.scheduleBlock
      bodyBlock = div ! A.id "scheduleBlock" $ do
        h2 pageTitle
        svgBlock
        explain
        tblBlock

  in docTypeHtml ! lang "en-US" $
    head hdr
    <>
    (body ! onload jsLoad)
      (mainNavBar navLoc
       <> (div ! id "schedule") bodyBlock
       <> renderFooter
      )


-- | Default page for the explore menu.
--
standardExplorePage ::
  Maybe AttributeValue
  -- ^ CSS file location (set last if given)
  -> Html
  -- ^ title (header)
  -> Html
  -- ^ main text of the page; appears within the div created by the
  --   following argument.
  -> Maybe AttributeValue
  -- ^ If Nothing, place contents within div#schedule, otherwise
  --   use div#<contents>
  -> Html
standardExplorePage = extraExplorePage Nothing

-- | Page for the explore menu with optional JS assets
--   (onload event and JS to set up).
--
extraExplorePage ::
  Maybe (AttributeValue, Html)
  -- ^ The onload value and any code/stylesheets to load in the
  --   page header.
  -> Maybe AttributeValue
  -- ^ CSS file location (set last if given)
  -> Html
  -- ^ title (header)
  -> Html
  -- ^ main text of the page; appears within the div created by the
  --   following argument.
  -> Maybe AttributeValue
  -- ^ If Nothing, place contents within div#schedule, otherwise
  --   use div#<contents>
  -> Html
extraExplorePage mJS mCSSPage hdrTitle bodyBlock mIdName =
  let hdr = (H.title hdrTitle
             <> defaultMeta
             <> fromMaybe mempty (snd <$> mJS)
             <> (cssLink "/css/main.css" ! A.title "Default")
             <> fromMaybe mempty (cssLink <$> mCSSPage)
            )

      idVal = fromMaybe "schedule" mIdName

      bdy = mainNavBar CPExplore
            <> (div ! id idVal) bodyBlock
            <> renderFooter

      bodyCon = case mJS of
        Just (jsLoad, _) -> body ! onload jsLoad
        _ -> body

  in (docTypeHtml ! lang "en-US") (head hdr <> bodyCon bdy)

