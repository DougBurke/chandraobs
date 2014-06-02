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
import Types (Record, recordTarget, recordInstrument, recordRa, recordDec, recordRoll)
import Utils (defaultMeta, obsURI)
import Views.Record (CurrentPage(..), mainNavBar)

{-

complete WWT page?

code on using WWT/HTML5

http://www.worldwidetelescope.org/docs/Samples/displaycode.htm?codeExample=WWTWebClientPolyHtml5.html

-}

-- | Create a WWT view of the observation.
--
--   Thought about a next/prev link to take you through the
--   WWT views, but this is complicated, since we do not
--   provide WWT links for cal/non-science observations.
--
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

      -- apparently width/height need to be given inline not via CSS
      host = (div ! id "WorldWideTelescopeControlHost") 
               $ (div ! style "width: 700px; height: 700px;"
                      ! id "WWTCanvas") ""

      zoomSource = 
        button ! id "jump"
               ! A.name "jump"
               ! value "jump"
               ! type_ "button"
               ! onclick "resetLocation();"
           $ "Jump to Source"

      -- can we use label rather than span here?
      cBox :: P.String -> AttributeValue -> AttributeValue -> Html
      cBox lbl idVal oClick =
        H.label $ H.toHtml (lbl <> " ") <>
                  input ! id idVal
                        ! type_ "checkbox"
                        ! checked "checked"
                        ! onclick oClick

      -- fov = cBox "View Instrument outline" "fov" "toggleFOV();"
      crossHair = cBox "Show cross hair" "crosshairs" "toggleCrosshairs();"
      constellation = cBox "Show constellations" "constellations" "toggleConstellations();"
      boundaries = cBox "Show constellation boundaries" "boundaries" "toggleBoundaries();"

      userInput = mconcat [ zoomSource
                          -- , fov
                          , crossHair, constellation, boundaries]

      obsLink = if f
                then a ! href "/" $ "observation page"
                else a ! href (obsURI rs) $ "observation page"

      targetName = H.toHtml $ recordTarget rs
      titleVal =
          H.toHtml ("The World Wide Telescope view of " :: P.String)
          <> targetName

  in docTypeHtml ! lang "en-US" $
    head 
     (H.title titleVal
      <> defaultMeta
      <> (script ! src "http://www.worldwidetelescope.org/scripts/wwtsdk.aspx") ""
      <> (script ! src "/js/wwt.js") ""
      <> link ! href   "/css/main.css"
              ! type_  "text/css" 
              ! rel    "stylesheet"
              ! A.title  "Default"
              ! media  "all"

     )
    <>
    (body ! onload initialize)
     (mconcat 
        [ mainNavBar CPOther
        , p ("The instrument outline approximates that of the " 
             <> iName <> " observation of " 
             <> targetName <> ". Return to the "
             <> obsLink <> "."
            )
        , (div ! id "wwtControls") userInput
        , host
        ])


