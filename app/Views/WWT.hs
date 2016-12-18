{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | The WWT page.
--
--   TODO: should there be navigation links to the next/previous observation
--         via WWT? difficult since we don't use this for cal obs
--
module Views.WWT (wwtPage) where

import qualified Prelude as P
import Prelude (($))

import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>), mconcat)

import Text.Blaze.Html5 hiding (style, title)
import Text.Blaze.Html5.Attributes hiding (span, title, name)

import Types (ScienceObs(..), Instrument(..), TargetName, ObsIdVal)
import Utils (defaultMeta, jsScript, cssLink, obsURI, renderFooter)
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
  -> ScienceObs
  -> Html
wwtPage isCurrent ScienceObs{..} =
  let initialize = "initialize(" <> toValue soRA <> "," 
                                 <> toValue soDec <> "," 
                                 <> toValue soRoll <> ",\"" 
                                 <> toValue soInstrument <> "\",\"" 
                                 <> toValue soTarget <> "\")" 

      titleVal = "The World Wide Telescope view of "
                 <> H.toHtml soTarget

  in docTypeHtml ! lang "en-US" $
    head 
     (H.title titleVal
      <> defaultMeta
      <> jsScript "http://www.worldwidetelescope.org/scripts/wwtsdk.aspx"
      <> jsScript "/js/wwt.js"
      <> (cssLink "/css/main.css" ! A.title  "Default")
      <> cssLink "/css/wwt.css"
     )
    <>
    (body ! onload initialize)
     (mconcat 
        [ mainNavBar CPOther
        , noJSPara
        , warningDiv
        , outlinePara isCurrent soTarget soInstrument soObsId
        , userInputDiv
        , hostDiv
        , renderFooter
        ])


noJSPara :: Html
noJSPara =
  (p ! class_ "nojavascript")
  ("This page requires JavaScript and it appears that it is " <>
   "not available.")

warningDiv :: Html
warningDiv =
  (div ! class_ "nowwt")
  ("It appears that the javascript needed to display the " <>
   "World Wide Telescope has not loaded.")

outlinePara ::
  P.Bool
  -- ^ True if this is the current observation
  -> TargetName
  -> Instrument
  -> ObsIdVal
  -> Html
outlinePara isCurrent targetName instrument obsid =
  let cts = "The instrument outline approximates that of the " 
            <> iName <> " observation of " 
            <> H.toHtml targetName <> ". Return to the "
            <> obsLink <> "."

      iName = case instrument of
                ACISI -> "ACIS-I"
                ACISS -> "ACIS-S"
                HRCI  -> "HRC-I"
                HRCS  -> "HRC-S"

      obsLink =
        let uri = if isCurrent then "/" else obsURI obsid
        in (a ! href uri) "observation page"

  in (p ! class_ "wwt" ) cts

-- apparently width/height need to be given inline not via CSS
hostDiv :: Html
hostDiv =
  let innerDiv =
        (div ! style "width: 700px; height: 700px;" ! id "WWTCanvas") ""
  in (div ! id "WorldWideTelescopeControlHost") innerDiv

userInputDiv :: Html
userInputDiv =
  let zoomSource = 
        button ! id "jump"
               ! A.name "jump"
               ! value "jump"
               ! type_ "button"
               ! onclick "resetLocation();"
           $ "Jump to Source"

      inputBox idVal oClick = input ! id idVal
                                    ! type_ "checkbox"
                                    ! checked "checked"
                                    ! onclick oClick
        
      -- can we use label rather than span here?
      cBox :: T.Text -> AttributeValue -> AttributeValue -> Html
      cBox lbl idVal oClick =
        H.label (H.toHtml (lbl <> " ") <> inputBox idVal oClick)

      -- fov = cBox "View Instrument outline" "fov" "toggleFOV();"
      crossHair = cBox "Show cross hair" "crosshairs" "toggleCrosshairs();"
      constellation = cBox "Show constellations" "constellations" "toggleConstellations();"
      boundaries = cBox "Show constellation boundaries" "boundaries" "toggleBoundaries();"

      userInput = mconcat [ zoomSource
                          -- , fov
                          , crossHair, constellation, boundaries]

  in (div ! id "wwtControls") userInput

