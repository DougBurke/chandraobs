{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Layout HTML.

module Layout (
  defaultMeta
  , skymapMeta
  , d3Meta
  , jqueryMeta
  , renderLinks
  , renderFooter
  , getFact
  , dquote
  , standardTable
  , floatableTable
  , noJSPara
    
  , addClass

  )
       where

import qualified Data.Text as T

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Char (intToDigit)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Monoid ((<>))
import Data.Time (UTCTime)

import System.Random (Random(..), getStdRandom)

import Text.Blaze.Html5 (AttributeValue, Html
                        , a
                        , img
                        , meta
                        , preEscapedToHtml
                        , table, tbody, td, tr
                        , textValue, toHtml, toValue)
import Text.Blaze.Html5.Attributes (alt
                                   , checked, class_, content
                                   , height, href, httpEquiv
                                   , onclick, rel
                                   , src, type_, value, width)
import Text.Blaze.Internal (Attributable)

import API (categoryLinkSearch
           , instLinkSearch
           , nameLinkSearch
           , obsIdLink
           , obsURI
           , propTypeLink
           , tooLinkSearch
           , typeDLinkSearch
           , seqLink
           , cssLink, jsScript
           )
import Types (ChipStatus(..)
             , Constraint(..)
             , ConLong(..)
             , ConShort(..)
             , Grating(..)
             , Instrument(..)
             , ObsIdStatus(Discarded)
             , Proposal(..)
             , ScienceObs(..)
             , SimbadInfo(..)
             , TimeKS(..)
             , TOORequest(..)
             , RA(..), Dec(..) -- only needed for WWT experiments
             , fromMissionLongLink
             , getConstellationName
             , getJointObs
             , toMission
             , toPropType
             , fromObsId
             )
import Utils (cleanJointName
             , isChandraImageViewable
             , publicImageURL
             , showInt)


-- | This includes a favicon link.
defaultMeta :: Html
defaultMeta = meta H.! httpEquiv "Content-Type"
                   H.! content "text/html; charset=UTF-8"
              <> icoLink

icoLink :: Html
icoLink = H.link H.! rel "icon"
                 H.! type_ "image/gif"
                 H.! href "/img/cxc.gif"

-- | Load the JS and CSS needed to display the sky map and table info.
--
skymapMeta :: Html
skymapMeta =
  d3Meta
  <> jsScript "/js/jquery.tablesorter.min.js"
  <> jsScript "/js/table.js"
  <> jsScript "/js/projection.js"
  <> cssLink "/css/tablesorter.css"
  <> cssLink "/css/schedule.css"

-- | Load D3 and JQuery.
d3Meta :: Html
d3Meta =
  jqueryMeta
  <> jsScript "https://d3js.org/d3.v3.min.js"
  <> jsScript "https://d3js.org/d3.geo.projection.v0.min.js"

-- | Load JQuery.
--
--   It is likely that this should not be used, as you probably want
--   skymapMeta or d3Meta instead.
--
--   We also load in the "base" module here, as it is a convenient,
--   if not semantically-sensible, place to put it.
--
jqueryMeta :: Html
jqueryMeta =
  jsScript "https://code.jquery.com/jquery-1.11.1.min.js"
  <> jsScript "/js/base.js"
  

-- | The standard footer; needs to match up with the html files in static/.
renderFooter :: Html
renderFooter =
  H.p H.! A.id "banner" $
    mconcat [
      "The 'What is Chandra doing now?' web site is developed by "
      , "the Science Data Systems group of the "
      , (a H.! href "http://chandra.si.edu/") "Chandra X-ray Center"
      , ". The information on this site is intended for "
      , "educational purposes and should not be relied on for "
      , "scientific use."
    ]

-- | Return a "random" Chandra fact, which is a string of
--   text (i.e. not div or p container).
--
getFact :: IO Html
getFact = do
  n <- getStdRandom (randomR (0, length facts - 1))
  return (facts !! n)

facts :: [Html]
facts = [
  "Chandra flies 200 times higher than Hubble - more than 1/3 of the way to the moon!"
  , "Chandra can observe X-rays from clouds of gas so vast that it takes light five million years to go from one side to the other!"
  , "During maneuvers from one target to the next, Chandra slews more slowly than the minute hand on a clock."
  , "At 45 feet long, Chandra is the largest satellite the shuttle has ever launched."
  , "If Colorado were as smooth as Chandra's mirrors, Pikes Peak would be less than one inch tall!"
  , "Chandra's resolving power is equivalent to the ability to read a stop sign at a distance of twelve miles."
  , "The electrical power required to operate the Chandra spacecraft and instruments is 2 kilowatts, about the same power as a hair dryer."
  , "The light from some of the quasars observed by Chandra will have been traveling through space for ten billion years."
  , "STS-93, the space mission that deployed Chandra, was the first NASA shuttle mission commanded by a woman."
  , "Chandra can observe X-rays from particles up to the last second before they fall into a black hole!!!"
  ]


-- | This is used so that the WWT widget can know where to look.
--
setupLocation :: ScienceObs -> Html
setupLocation ScienceObs{..} =
  let -- TODO: what is the best way of specifying the location?
      jsSettings = mconcat [
        -- note; do not want RA/Dec HTML conversion here; treat as
        -- plain numbers!
        "var scienceObs = {ra: ", toHtml (_unRA soRA),
        ", dec: ", toHtml (_unDec soDec),
        ", roll: ", toHtml soRoll,
        ", instrument: '", toHtml soInstrument, "'",
        ", name: '", toHtml soTarget, "'};"
        ]

  in H.script jsSettings


-- | Display the DSS/RASS/PSPC/WWT images and the observational
--   details (so the name is a slight mis-nomer).
--
-- The DSS/RASS/PSPC links are autogenerated using the following
-- scheme:
--
-- http://asc.harvard.edu/targets/<sequence>/<sequence>.<obsid>.soe.dss.gif
-- http://asc.harvard.edu/targets/<sequence>/<sequence>.<obsid>.soe.rass.gif
-- http://asc.harvard.edu/targets/<sequence>/<sequence>.<obsid>.soe.pspc.gif
--
-- Constaints on the obsid are five-digit 0-encoded.
--
-- Early observations use a different scheme, and do not include the
-- RASS. Unfortunately I do not know when the changeover (is there an
-- easy way to tell; perhaps may have to do a special lookup table)
--
-- http://cxc.harvard.edu/targets/<sequence>/<sequence>.mpvis.dss.gif
-- http://cxc.harvard.edu/targets/<sequence>/<sequence>.mpvis.pspc.gif
--
-- The public image is taken from the Chandra archive, and follows
-- the rules encoded in Utils.publicImageURL. Actually, this is currently
-- disabled as it is causing too much activity for the CDA.
--
-- We also display the observational details - in \"raw\" form - as a text box
-- as part of this section.
--
-- This is modified somewhat for discarded observations (not sure the best
-- way to do this at this time).
--
renderLinks ::
  UTCTime -- the current time
  -> Maybe Proposal
  -> Maybe SimbadInfo
  -- ^ assumed to be for the observation
  -> ScienceObs
  -> Html
renderLinks tNow mprop msimbad so@ScienceObs{..} =
  let optSel :: Html -> T.Text -> Bool -> Html
      optSel txt idlbl cf = 
        let idName = toValue (idlbl <> "button")
            jsClick = "imageSwitch.switchOption('" <> toValue idlbl <> "')"
            
            base = H.input H.! type_ "radio"
                           H.! A.name  "opttype"
                           H.! value (toValue idlbl)
                           H.! A.id idName
                           H.! onclick jsClick
                               
        in (if cf then base H.! checked "checked" else base)
           <> (H.label H.! A.for idName) txt

      showChandraImage = isChandraImageViewable soPublicRelease soDataMode
                         soInstrument tNow

      -- There is a little bit of historical baggage here, since the
      -- views were originally labelled by the Astronomer-friendly
      -- terms, but are now moving to a more readable (hopefully)
      -- label.
      --
      buttons =  [ "View: " ] ++
                 [ optSel "Chandra" "Chandra" True | showChandraImage ] ++
                 [ optSel "Optical" "DSS" (not showChandraImage)
                 , optSel "X-ray (RASS)" "RASS" False
                 , optSel "X-ray (PSPC)" "PSPC" False
                 , optSel "Details" "Details" False
                 , optSel "Interactive" "WWT" False
                 ]

      form = addClass "radiobuttons" H.div (mconcat buttons)

      -- The ObsId is 0-padded (on the left) to ensure 5 digits
      --
      nzeros :: Int
      nzeros = 4 - floor (logBase (10::Double)
                          (fromIntegral (fromObsId soObsId)))
               
      obsid5 = toValue (take nzeros "00000")
               <> toValue soObsId
      urlHead = "http://asc.harvard.edu/targets/"
                <> toValue soSequence
                <> "/"
                <> toValue soSequence
                <> "."
                <> obsid5
                <> ".soe."

      boxSize = toValue (680::Int)

      link :: T.Text -> AttributeValue -> Bool -> Html
      link lbl frag af = 
        let uri = urlHead <> frag <> ".gif"
        in img H.! src    uri
               H.! alt    (textValue ("The instrument field-of-view on top of the " <> lbl <> " image of the source."))
               H.! width  boxSize
               H.! height boxSize
               H.! A.id   (textValue lbl)
               H.! class_ (if af then "active" else "inactive")

      defaultLinks =
        link "DSS" "dss" (not showChandraImage) <>
        link "PSPC" "pspc" False <>
        link "RASS" "rass" False <>
        renderObsIdDetails mprop msimbad so <>
        renderWWT so

      -- The Chandra images are not guaranteed to be square, so
      -- forcing a size on them here leads to a stretched
      -- display for the rectangular images. This is not
      -- ideal.
      --
      chandraLink =
        let uri = publicImageURL soObsId soInstrument
        in img H.! src    (textValue uri)
               H.! alt    (textValue "The Chandra image of this observation.")
               -- H.! width  boxSize
               -- H.! height boxSize
               H.! A.id   (textValue "Chandra")
               H.! class_ "active"

      firstLink = if showChandraImage then chandraLink else mempty
      allLinks = firstLink <> defaultLinks

      showLinks = addClass "links" H.div allLinks

  in setupLocation so <> form <> showLinks


-- | Display detailed information about a science observation,
--   for those that just need to know the details.
--
--   Could try and link to the roll/pitch/slew GIF - e.g.
--   http://cxc.harvard.edu/targets/601090/601090.rollvis.gif
--   but it is not obvious how valid it is (since the above
--   does not cover May 2014 when that target was scheduled),
--   and it is rather meaningless to anyone but an expert.
--
--   I expect the proposal to always be available, but just
--   in case it isn't.
--
--   Note: for discarded observations, a "header" is added
--   to point this out, but the details are still included
--   "for fun".
--
--   Note: this does not need to be sent in the HTML context
--   (i.e. static or dynamic) since the obsid link here is
--   to the CDA site, not a page on our site).
--
renderObsIdDetails ::
  Maybe Proposal
  -> Maybe SimbadInfo
  -> ScienceObs
  -> Html
renderObsIdDetails mprop msimbad so@ScienceObs{..} =
  let name = soTarget
      inst = soInstrument
      grat = soGrating
      instInfo = instLinkSearch inst <>
                 if grat == NONE
                 then mempty
                 else ", " <> toHtml grat

      left = addClass "key" td
      right = addClass "value" td

      keyVal k v = tr (left k <> " " <> right v)

      oLink = a H.! href (obsIdLink soObsId) $ toHtml soObsId
      sLink = a H.! href (seqLink soObsId)   $ toHtml soSequence
      pLink = a H.! href ("/proposal/" <> H.toValue soProposal) $ toHtml soProposal

       -- rely on the ToMarkup instance of TimeKS
      expLink = case soObservedTime of
        Just t -> keyVal "Exposure (observed):" (toHtml t <> " ks")
        _ -> keyVal "Exposure (approved):" (toHtml soApprovedTime <> " ks")

      -- NOTE: can this be cleared up now that I have a better
      -- understanding of the jointwith and exposure-time fields?
      --
      missToLink mission = maybe (toHtml mission)
                           fromMissionLongLink (toMission mission)
                           
      toJ (l,v) = keyVal "Joint with:"
                  (missToLink l <> " for " <> toHtml (_toKS v) <> " ks")
                  
      jvs = getJointObs so

      jointElems = 
        if isJust soJointWith && null jvs
        then keyVal "Awarded Chandra time by:"
             (missToLink (cleanJointName (fromJust soJointWith)))
        else mconcat (map toJ jvs)

      cToL NoConstraint = "None" -- not used
      cToL Preferred    = "Preferred"
      cToL Required     = "Yes"
      clbls = ["Time critical:", "Monitor:", "Constrained:"]
      cvals = [soTimeCritical, soMonitor, soConstrained]
      constraintElems =
        let f (k,v) = keyVal k (cToL v)
        in mconcat (map f (filter ((/= NoConstraint) . snd) (zip clbls cvals)))

      conLink con = 
        let uri = H.toValue ("/search/constellation/" <> fromConShort con)
        in case getConstellationName con of
          Just ln -> Just ((a H.! href uri) (toHtml (fromConLong ln)))
          _ -> Nothing
        
      too = case soTOO of
        Just t  -> keyVal "Turnaround time:" (tooLinkSearch (Just (trType t)))
        Nothing -> mempty

      -- the "chip" display depends on whether this has been archived or
      -- not (and if it's ACIS or HRC), since we display different things.
      chipDetails = case soDetector of
        Just dm -> keyVal "Chips:" (toHtml dm)
        _ -> fromMaybe mempty $ keyVal "Chips: " . toHtml <$> detector

      -- for now just convert to ACIS-??? treating optional as on
      detector = 
        if soInstrument `elem` [HRCI, HRCS]
        then Nothing
        else Just $ let sts = [ soACISI0, soACISI1, soACISI2, soACISI3,
                                soACISS0, soACISS1, soACISS2, soACISS3,
                                soACISS4, soACISS5
                              ]
                        ks = filter ((/= ChipOff) . snd) $ zip [0..] sts
                    in "ACIS-" ++ map (intToDigit . fst) ks

      -- assume they are both set or unset
      subArray = do
        start <- soSubArrayStart
        nrow <- soSubArraySize
        return $ keyVal "Sub Array:" $ toHtml ("Start: " <> showInt start <> " Rows: " <> showInt nrow)

      -- bundle several items into a single line
      propInfo Proposal {..} =
        let p0 = toHtml propType
            plink n = propTypeLink n Nothing
            ptype = maybe p0 plink (toPropType propType)
        in 
          keyVal "Proposal:"
          ("Cycle " <> toHtml propCycle <> ", "
           <> ptype <> ", "
           <> categoryLinkSearch propCategory propCategory
          )

      simbadInfo SimbadInfo {..} =
        keyVal "SIMBAD Type:" (typeDLinkSearch smiType3 smiType)
      
      discardRows = [ tr (addClass "note" td
                          "Note: the observation was discarded")
                    | soStatus == Discarded ]

      -- I do not think we need more precision for the roll than an integer
      roll :: Int
      roll = round soRoll
      degSymbol = "\176" -- aka \u00b0, the degree symbol

      tblRows =
        mconcat discardRows
        <>
        mconcat
          [ keyVal "Target:" (nameLinkSearch name Nothing)
          , maybe mempty simbadInfo msimbad
          , keyVal "Observation Details:" oLink
          , keyVal "Sequence Summary:" sLink
          , keyVal "Proposal Id:" pLink
          , maybe mempty propInfo mprop
          , too
          , keyVal "Instrument:" instInfo
          , chipDetails
          , fromMaybe mempty subArray
          , fromMaybe mempty (keyVal "Data Mode:" . toHtml <$> soDataMode)
          -- rely on the ToMarkup instance of ChandraTime
          , fromMaybe mempty (keyVal "Date:" . toHtml <$> soStartTime)
          , expLink
          -- rely on the ToMarkup instance of RA
          , keyVal "Right Ascension:" (toHtml soRA)
          -- rely on the ToMarkup instance of Dec
          , keyVal "Declination:" (toHtml soDec)
          , keyVal "Roll:" (toHtml roll <> degSymbol)
          , fromMaybe mempty
            (keyVal "Constellation:" <$> conLink soConstellation)
          , jointElems
          , constraintElems
          ]

      -- ignore the thead element
      tbl = table (tbody tblRows)

  in (addClass "inactive" H.div H.! A.id "Details") tbl


-- | Create the div to contain the WWT view.
--
--   *VERY EXPERIMENTAL*
--
renderWWT ::
  ScienceObs
  -> Html
renderWWT ScienceObs{..} =
  let hostDiv = let innerDiv =
                      (H.div
                       -- Pick the size to match the DSS/RASS/PSPC size
                       H.! A.style "width: 680px; height: 680px;"
                       H.! A.id "WWTCanvas") ""
                in (H.div H.! A.id "WorldWideTelescopeControlHost") innerDiv

      noJS = noJSPara
        ("This page requires JavaScript and it appears that it is " <>
         "not available.")

      -- NOTE: this hard-codes the URL of the site, which is not ideal
      httpHref = "http://chandraobservatory.herokuapp.com" <>
                 obsURI soObsId

      warningDiv =
        (H.p H.! class_ "nowwt")
        ("It appears that the javascript needed to display the " <>
         "World Wide Telescope has not loaded. If you are using the " <>
         "https version of the site, please try visiting the " <>
         (a H.! href httpHref) "http version of this page " <>
         "to see if that works.")

      cts = noJS <> warningDiv <> hostDiv
      
  in (addClass "inactive" H.div H.! A.id "WWT") cts


noJSPara :: Html -> Html
noJSPara = (H.p H.! class_ "nojavascript")


ldquo, rdquo :: Html
ldquo = preEscapedToHtml ("&ldquo;" :: T.Text)
rdquo = preEscapedToHtml ("&rdquo;" :: T.Text)

dquote :: Html -> Html
dquote txt = ldquo <> txt <> rdquo

standardTable, floatableTable :: Html -> Html
standardTable = addClass "standard" table
floatableTable = addClass "floatable" table

-- | Try to make the code a little-bit easier to read
addClass :: Attributable a => AttributeValue -> a -> a
addClass cls base = base H.! class_ cls

