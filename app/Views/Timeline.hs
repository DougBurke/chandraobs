{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Timeline/Schedule visualization.

module Views.Timeline ( relatedView
                      , scheduleView
                      ) where

import qualified Data.Map.Strict as M
-- import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Graphics.Vega.VegaLite as VL

import Data.Aeson (object, toJSONList)
import Data.Aeson.Types (Value, Pair, (.=))
import Data.Either (rights)
-- import Data.Function ((&))
import Data.Maybe (fromMaybe, isJust, mapMaybe, maybeToList)
-- import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (UTCTime(..))

import Formatting ((%), sformat)
import Formatting.Time (dateDash, hms)

import Sorted ( SortedList
              , fromSL
              )
import Types ( RestrictedSchedule(..)
             , ScienceObs(..)
             , Grating(NONE)
             , ChandraTime
             , TimeKS
             , TargetName
             , SimbadInfo
             , SIMCategory
             , smiType
             , fromObsId
             , fromTargetName
             , fromInstrument
             , fromGrating
             , fromChandraTime
             , endCTime
             , showExpTime
             , rsoTarget
             , rsoObsId
             , rsoStartTime
             , rsoExposureTime
             , rsoInstrument
             , rsoGrating
             , rsoConstellation
             , getConstellationNameStr
             )


-- We want to make sure we always use the same format
-- since I have seen differences in engineering (with
-- milliseconds) and science (nearest second). This is
-- now less relevant, as we don't show engineering data,
-- but keep in place.
--
showTime :: ChandraTime -> T.Text
showTime = showUTCTime . fromChandraTime

showUTCTime :: UTCTime -> T.Text
showUTCTime =
   let tfmt = dateDash % "T" <> hms % "Z"
   in sformat tfmt

addTimes ::
  Maybe ChandraTime
  -> TimeKS
  -> [Pair]
addTimes mstart tks = case mstart of
     Just t0 -> let t1 = endCTime t0 tks
                in [ "start" .= showTime t0
                   , "end" .= showTime t1
                   ]
     _ -> []


-- It would be nice to be able to restrict the SIMBAD classification
-- (e.g. merge related concepts) so that we could be left with of order
-- 5 classifications.
--
getObjectType :: M.Map TargetName SimbadInfo -> TargetName -> SIMCategory
getObjectType simMap target =
  let simInfo = M.lookup target simMap
  in maybe "Unknown" smiType simInfo


{-

Explore visualization of the schedule

I started off including the engineering data, but as we essentially
lose this for observed data the display is confusing, so I have
removed these objects.

-}

standardTooltips :: VL.BuildEncodingSpecs
standardTooltips =
      VL.tooltips [ [ VL.TName "target"
                    , VL.TmType VL.Ordinal
                    , VL.TTitle "Target" ]
                  , [ VL.TName "object-type"
                    , VL.TmType VL.Nominal
                    , VL.TTitle "Classification" ]
                  , [ VL.TName "exposure"
                    , VL.TmType VL.Nominal
                    , VL.TTitle "Exposure time" ]
                  , [ VL.TName "instrument"
                    , VL.TmType VL.Nominal
                    , VL.TTitle "Instrument" ]
                  , [ VL.TName "obsid"
                    , VL.TmType VL.Ordinal
                    , VL.TTitle "Observation Id" ]
                  , [ VL.TName "constellation"
                    , VL.TmType VL.Ordinal
                    , VL.TTitle "Constellation"
                    ]
                  ]


scheduleView :: RestrictedSchedule -> Value
scheduleView RestrictedSchedule {..} =
  let titleText = "Chandra Schedule"

      -- An example date is: 2024-03-23T00:36:21Z".  See
      -- https://d3js.org/d3-time-format#isoParse and note the use of
      -- showTime to make sure we use this format
      --
      dateFmt = VL.FoUtc "%Y-%m-%dT%H:%M:%SZ"
      sched = VL.dataFromJson jsData [ VL.Parse [ ("start", dateFmt)
                                                , ("end", dateFmt)
                                                ]
                                     ]

      jsData = toJSONList (map rrToRow rows2)
      rows = rrDone <> maybeToList rrDoing <> rrToDo
      rows2 = rights rows
      rrToRow = object . rsoToJSON

      getCon = getConstellationNameStr . rsoConstellation

      rsoToJSON rso =
        let target = rsoTarget rso

            iname = fromInstrument (rsoInstrument rso)
            gname = case rsoGrating rso of
              NONE -> ""
              g -> "/" <> fromGrating g

            exptime = rsoExposureTime rso

            instrument = iname <> gname
            obsid = fromObsId (rsoObsId rso)
            base = [ "target" .= fromTargetName target
                   , "exposure" .= showExpTime exptime
                   , "obsid" .= obsid
                   , "instrument" .= instrument
                   , "object-type" .= getObjectType rrSimbad target
                   , "constellation" .= getCon rso
                   ]

        in base
           <> addTimes (rsoStartTime rso) exptime

      pick = VL.SelectionName "pick"

      enc = VL.encoding
            . VL.position VL.X [ VL.PName "start"
                               , VL.PmType VL.Temporal
                               , VL.PAxis [ VL.AxFormatAsTemporal
                                            -- I think this is actually drawn
                                            -- in local units...
                                            -- , VL.AxTitle "UTC"
                                          , VL.AxNoTitle
                                          ]
                               , VL.PScale [ VL.SDomainOpt
                                             (VL.DSelection "brush") ]
                               ]
            . VL.position VL.X2 [ VL.PName "end" ]
            . VL.position VL.Y [ VL.PName "instrument"
                               , VL.PmType VL.Nominal
                               , VL.PAxis [ VL.AxTitle "Instrument" ]
                               ]
            . VL.color [ VL.MSelectionCondition pick
                         [ VL.MName "object-type"
                         , VL.MmType VL.Nominal
                         , VL.MLegend [ VL.LTitle "SIMBAD classification"
                                      , VL.LTitleAnchor VL.AMiddle
                                      -- , VL.LOrient VL.LOBottom
                                      , VL.LPadding 10
                                      , VL.LStrokeColor "gray"
                                      ]
                         ]
                         [ VL.MString "gray" ]
                       ]
            . standardTooltips
            . VL.opacity [ VL.MSelectionCondition pick
                           [ VL.MNumber 0.7 ]
                           [ VL.MNumber 0.3 ]
                         ]

      -- What interactivity do we want?
      --
      selOpts = [ VL.BindLegend (VL.BLFieldEvent "object-type" "mouseover")
                , VL.On "mouseover"
                ]

      {-
         This is a nice idea but it's hard to clear the widget
         when the selection is removed.

      -- Annoying to reprocess the data to get this
      allCats = map (getObjectType . rsoTarget) rows2
      cats = (Set.toList . Set.fromList) allCats
      
      selOpts = [ VL.Fields [ "object-type" ]
                , VL.Bind [ VL.ISelect "object-type" [ VL.InOptions cats
                                                     , VL.InName "Select a classification: "
                                                     ]
                          ]
                , VL.Clear "click"
                ]
      -}

      {-
         We could include constellation names, but the issue is what
         to attatch the selection to?

      allConNames = map getCon rows2
      conNames = (Set.toList . Set.fromList) allConNames

            . VL.select "foo" VL.Single
              [ VL.Fields [ "constellation" ]
              , VL.Bind [ VL.ISelect "constellation"
                          [ VL.InOptions conNames ]
                        ]
              ]

      -}
      
      sel = VL.selection
            . VL.select "pick" VL.Single selOpts
            . VL.select "brush" VL.Interval [ VL.Encodings [ VL.ChX ]
                                            -- , VL.BindScales
                                            ]

      noEnc = VL.encoding
              . VL.position VL.X [ VL.PName "start"
                                 , VL.PmType VL.Temporal
                                 , VL.PAxis [ VL.AxFormatAsTemporal
                                            , VL.AxNoTitle
                                            ]
                                 , VL.PScale [ VL.SDomainOpt
                                               (VL.DSelection "brush") ]
                                 -- , VL.PAxis axOpts  -- err, does not seem to work
                                 ]
              . VL.position VL.X2 [ VL.PName "end" ]
              . VL.color [ VL.MName "object-type"
                         , VL.MmType VL.Nominal
                         ]
              . VL.opacity [ VL.MNumber 0.7 ]
              . standardTooltips

      noInst = [ VL.mark VL.Bar [ ]
               , noEnc []
               , VL.height 40
               , VL.width widthVal
               ]

      tline = [ VL.mark VL.Bar [ VL.MTooltip VL.TTEncoding ]
              , enc []
              , sel []
              , VL.height 350
              , VL.width widthVal
              ]

      widthVal = 600

      -- How does the system decide which layer to use to control
      -- the axis properties? Is it the last one (seems to be here)
      -- or something more complex?
      --
      {-
      (yearNum, monthOfYear, dayOfMonth) = toGregorian (utctDay rrTime)
      nowDate = [ VL.DTYear (fromInteger yearNum)
                , VL.DTMonthNum monthOfYear
                , VL.DTDate dayOfMonth
                ]
      -- ymd = VL.Utc VL.YearMonthDate
      ymd = VL.TU VL.YearMonthDate
      axOpts = [ VL.AxDataCondition
                 (VL.FEqual "value" (VL.DateTime nowDate)
                  & VL.FilterOpTrans (VL.MTimeUnit ymd))
                 (VL.CAxGridDash [] [2, 2])
               ]
      -}

      nowEnc = VL.encoding
               . VL.position VL.X [ VL.PName "time"
                                  , VL.PmType VL.Temporal
                                  -- , VL.PAxis axOpts
                                  ]
               . VL.tooltip [ VL.TString "Current time" ]

      curTime = toJSONList [ object [ "time" .= showUTCTime rrTime ] ]
      now = [ VL.dataFromJson curTime [ VL.Parse [ ("time", dateFmt) ] ]
            , nowEnc []
            , VL.mark VL.Rule [ VL.MColor "black"
                              , VL.MOpacity 0.5
                              , VL.MSize 4
                              ]
            ]

      -- Adding the now layer to the top plot stops it being
      -- dislayed in the bottom layer! Why?
      --
      -- top = [VL.layer (map VL.asSpec [noInst, now])]
      top = noInst

      bottom = [VL.layer (map VL.asSpec [tline, now])]

  in (VL.fromVL . VL.toVegaLite)
     [ VL.title titleText [ VL.TFontSize 18 ]
     , sched
     , VL.vConcat (map VL.asSpec [top, bottom])
     ]




-- Could send in proposal title
-- We do not care that the data is sorted
--
relatedView ::
  UTCTime
  -> T.Text
  -> SortedList a ScienceObs
  -> M.Map TargetName SimbadInfo
  -> Value
relatedView currentTime propTitle sl simMap =
  let -- An example date is: 2024-03-23T00:36:21Z".  See
      -- https://d3js.org/d3-time-format#isoParse and note the use of
      -- showTime to make sure we use this format
      --
      dateFmt = VL.FoUtc "%Y-%m-%dT%H:%M:%SZ"
      sched = VL.dataFromJson jsData [ VL.Parse [ ("start", dateFmt)
                                                , ("end", dateFmt)
                                                ]
                                     ]

      jsData = toJSONList (map (object . sobsToRow) rows)
      rows = fromSL sl

      sobsToRow ScienceObs{..} =
        let target = soTarget

            iname = fromInstrument soInstrument
            gname = case soGrating of
              NONE -> ""
              _ -> "/" <> fromGrating soGrating

            exptime = fromMaybe soApprovedTime soObservedTime

            instrument = iname <> gname
            obsid = fromObsId soObsId

            isPublic = maybe False (<= currentTime) soPublicRelease

            base = [ "target" .= fromTargetName target
                   , "exposure" .= showExpTime exptime
                   , "obsid" .= obsid
                   , "instrument" .= instrument
                   , "processed" .= isJust soObservedTime
                   , "public" .= isPublic
                   , "object-type" .= getObjectType simMap target
                   , "constellation" .= getConstellationNameStr soConstellation
                   ]

        in base
           <> addTimes soStartTime exptime

      pick = VL.SelectionName "pick"
      selOpts = [ VL.On "mouseover" ]
      sel = VL.selection
            . VL.select "pick" VL.Single selOpts
            {-

              This is nice, but what we'd like to change the X axis
              doing this, and how do we "get out" of it?

            -}
            . VL.select "brush" VL.Interval [ VL.Encodings [ VL.ChX ]
                                            , VL.BindScales
                                            -- clear event does not seem to work
                                            -- , VL.Clear "click[event.shiftkey]"
                                            ]

      -- We assume the time range is large enough that we do not
      -- care about using start vs (start + end) / 2.
      --
      enc = VL.encoding
            . VL.position VL.X [ VL.PName "start"
                               , VL.PmType VL.Temporal
                               , VL.PAxis [ VL.AxFormatAsTemporal
                                          -- I think this is actually drawn
                                          -- in local units...
                                          -- , VL.AxTitle "UTC"
                                          , VL.AxNoTitle
                                          ]
                               ]
            . VL.position VL.Y [ VL.PName "object-type"
                               , VL.PmType VL.Nominal
                               , VL.PAxis [ VL.AxTitle "Classification" ]
                               ]
            . standardTooltips
            . VL.color [ VL.MName "public"
                       , VL.MmType VL.Nominal
                       , VL.MLegend [ VL.LTitle "Data is public"
                                    , VL.LTitleAnchor VL.AMiddle
                                      -- , VL.LOrient VL.LOBottom
                                    , VL.LPadding 10
                                    , VL.LStrokeColor "gray"
                                    , VL.LLabelExpr "datum.value ? 'yes' : 'no'"
                                    ]
                       ]
            . VL.size [ VL.MSelectionCondition pick
                        [ VL.MNumber 200 ]
                        [ VL.MNumber 100 ]
                      ]
            . VL.opacity [ VL.MSelectionCondition pick
                           [ VL.MNumber 0.7 ]
                           [ VL.MNumber 0.3 ]
                         ]

      related = [ VL.mark VL.Circle [ VL.MTooltip VL.TTEncoding ]
                , sched
                , sel []
                , enc []
                ]

      nowEnc = VL.encoding
               . VL.position VL.X [ VL.PName "time"
                                  , VL.PmType VL.Temporal
                                  ]
               . VL.tooltip [ VL.TString "Current time" ]

      curTime = toJSONList [ object [ "time" .= showUTCTime currentTime ] ]
      now = [ VL.dataFromJson curTime [ VL.Parse [ ("time", dateFmt) ] ]
            , nowEnc []
            , VL.mark VL.Rule [ VL.MColor "black"
                              , VL.MOpacity 0.5
                              , VL.MSize 4
                              ]
            ]

      -- The assumption is that we have a start time for at least one
      -- observation, which could be guaranteed if we specified the
      -- sort order but the types would not know this.
      --
      allTimes = mapMaybe soStartTime rows
      firstTime = fromChandraTime (head allTimes)  -- partial
      lastTime = fromChandraTime (last allTimes)   -- partial

      mnow = if currentTime >= firstTime && currentTime <= lastTime
             then Just now else Nothing

      specs = [ related ] <> maybeToList mnow

  in (VL.fromVL . VL.toVegaLite)
     [ VL.title propTitle [ VL.TFontSize 18 ]
     , VL.width 800
     , VL.height 200
     , VL.layer (map VL.asSpec specs)
     ]
