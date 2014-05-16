{-# LANGUAGE OverloadedStrings #-}

-- | The WWT page.
-- 
module Views.WWT (wwtPage) where

import qualified Prelude as P
import Prelude (($))

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mconcat)

import Text.Blaze.Html5 hiding (style, title)
import Text.Blaze.Html5.Attributes hiding (span, title, name)

import Types (Instrument(..))
import PersistentTypes
import Utils (defaultMeta, obsURI)

{-

complete WWT page?

code on using WWT/HTML5

http://www.worldwidetelescope.org/docs/Samples/displaycode.htm?codeExample=WWTWebClientPolyHtml5.html

-}

-- | Create a WWT view of the observation
wwtPage :: 
  P.Bool -- ^ True if this is the current observation
  -> Record
  -> Html
wwtPage f rs =
  let ra = recordRa rs
      dec = recordDec rs
      roll = recordRoll rs
      name = recordTarget rs
      inst = fromMaybe ACISI (recordInstrument rs)
      iName = case inst of
                ACISI -> "ACIS-I"
                ACISS -> "ACIS-S"
                HRCI  -> "HRC-I"
                HRCS  -> "HRC-S"

      initialize = "initialize(" <> toValue ra <> "," 
                                 <> toValue dec <> "," 
                                 <> toValue roll <> ",\"" 
                                 <> toValue inst <> "\",\"" 
                                 <> toValue name <> "\")" 

      host = (div ! id "WorldWideTelescopeControlHost") 
               $ (div ! id "WWTCanvas" 
                    ! style "width: 700px; height: 700px; border-style: none; border-width: 0px") ""

      zoomSource = 
        button ! id "jump"
               ! A.name "jump"
               ! value "jump"
               ! type_ "button"
               ! onclick "resetLocation();"
           $ "Jump to Source"

      fov = 
        span "View Instrument outline" <>
        input ! id "fov" 
              ! type_ "checkbox"
              ! checked "checked"
              ! onclick "toggleFOV();"

      crossHair = 
        span "View cross hair" <>
        input ! id "crosshairs" 
              ! type_ "checkbox"
              ! checked "checked"
              ! onclick "toggleCrosshairs();"

      constellation = 
        span "Show constellations" <>
        input ! id "constellations" 
              ! type_ "checkbox"
              ! checked "checked"
              ! onclick "toggleConstellations();"

      boundaries = 
        span "Show constellation boundaries" <>
        input ! id "boundaries" 
              ! type_ "checkbox"
              ! checked "checked"
              ! onclick "toggleBoundaries();"

      userInput = mconcat [zoomSource, fov, crossHair, constellation, boundaries]

      obsLink = let cts = "Observation details."
                in if f
                   then a ! href "/" $ cts
                   else a ! href (obsURI rs) $ cts

  in docTypeHtml $
    head 
     (H.title "View in the World Wide Telescope" <>
      defaultMeta <>
      (script ! src "http://www.worldwidetelescope.org/scripts/wwtsdk.aspx") "" <>
      (script ! src "/js/wwt.js") ""
     )
    <>
    (body ! onload initialize)
     (mconcat 
        [ p ("Observation: " <> toHtml name <> ". " <> obsLink)
        , p ("The instrument outline approximates that of the " <> iName <> ".")
        , (div ! style "float: left;") userInput
        , host
        ])


