{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Timeline/Schedule visualization.

module Views.Timeline ( scheduleView
                      ) where

import qualified Data.Map.Strict as M
-- import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Graphics.Vega.VegaLite as VL

import Data.Aeson (Value, (.=), object, toJSONList)
import Data.Either (rights)
import Data.Maybe (maybeToList)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (UTCTime(..))

import Formatting ((%), sformat)
import Formatting.Time (dateDash, hms)

import Types ( RestrictedSchedule(..)
             , ChandraTime
             , Grating(NONE)
             , smiType
             , fromObsId
             , fromTargetName
             , fromInstrument
             , fromGrating
             , fromChandraTime
             , endCTime
             , rsoTarget
             , rsoObsId
             , rsoStartTime
             , rsoExposureTime
             , rsoInstrument
             , rsoGrating
             )
import Utils (showInt)


{-

Explore visualization of the schedule

I started off including the engineering data, but as we essentially
lose this for observed data the display is confusing, so I have
removed these objects.

-}

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

      -- It would be nice to be able to restrict the SIMBAD classification
      -- (e.g. merge related concepts) so that we could be left with of order
      -- 5 classifications.
      --
      getObjectType target =
        let simInfo = M.lookup target rrSimbad
        in maybe "Unknown" smiType simInfo

      rsoToJSON rso =
        let target = rsoTarget rso

            iname = fromInstrument (rsoInstrument rso)
            gname = case rsoGrating rso of
              NONE -> ""
              g -> "/" <> fromGrating g

            instrument = iname <> gname
            obsid = fromObsId (rsoObsId rso)
            label = fromTargetName target
                    <>" (" <> showInt obsid <> ")"
            
            base = [ "label" .= label
                   , "instrument" .= instrument
                   , "object-type" .= getObjectType target
                   ]

        in base
           <> addTimes (rsoStartTime rso) (rsoExposureTime rso)

      addTimes mstart tks =
        let -- We want to make sure we always use the same format
            -- since I have seen differences in engineering (with
            -- milliseconds) and science (nearest second). This is
            -- now less relevant, as we don't show engineering data,
            -- but keep in place.
            --
            showTime :: ChandraTime -> T.Text
            showTime t =
              let utc = fromChandraTime t
                  tfmt = dateDash % "T" <> hms % "Z"
              in sformat tfmt utc

        in case mstart of
             Just t0 -> let t1 = endCTime t0 tks
                        in [ "start" .= showTime t0
                           , "end" .= showTime t1
                           ]
             _ -> []

      pick = VL.SelectionName "pick"
      enc = VL.encoding
            . VL.position VL.X [ VL.PName "start"
                               , VL.PmType VL.Temporal
                               , VL.PAxis [ VL.AxTitle "UTC" ]
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
                                      , VL.LOrient VL.LOBottom
                                      , VL.LPadding 10
                                      , VL.LStrokeColor "gray"
                                      ]
                         ]
                         [ VL.MString "gray" ]
                       ]
            . VL.tooltip [ VL.TName "label"
                         , VL.TmType VL.Ordinal
                         ]
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
      
      sel = VL.selection
            . VL.select "pick" VL.Single selOpts
      
      tline = [ VL.mark VL.Bar [ VL.MTooltip VL.TTEncoding ]
              , enc []
              , sel []
              ]

      -- use the search time as "now"
      (year, monthOfYear, dayOfMonth) = toGregorian (utctDay rrTime)
      dayTime = fst (properFraction (utctDayTime rrTime))
      (hours, hrem) = dayTime `divMod` 3600
      (mins, secs) = hrem `divMod` 60

      -- ugh, this should be wrapped up somehow
      curTime = [ VL.DTYear (fromIntegral year)
                , VL.DTMonthNum monthOfYear
                , VL.DTDate dayOfMonth
                , VL.DTHours hours
                , VL.DTMinutes mins
                , VL.DTSeconds secs
                ]

      nowEnc = VL.encoding
               . VL.position VL.X [ VL.PDatum (VL.DateTime curTime) ]
               . VL.size [ VL.MNumber 4 ]
               -- The opacity is behaving strangely (if >~ 0.05 it seems
               -- to not really be opaque)
               . VL.opacity [ VL.MNumber 0.04 ]

      -- It looks like MOpacity and MSize do not change the mark, hence
      -- the use of encoding statements.
      --
      now = [ VL.mark VL.Rule [ VL.MColor "black" ]
            , nowEnc []
            ]

      specs = [tline, now]

  in (VL.fromVL . VL.toVegaLite)
     [ VL.title titleText [ VL.TFontSize 18 ]
     , VL.width 800
     , VL.height 400
     , sched
     , VL.layer (map VL.asSpec specs)
     ]
