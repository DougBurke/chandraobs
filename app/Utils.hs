{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | general routines

module Utils (
     defaultMeta
     , skymapMeta
     , d3Meta
     , jqueryMeta
     , fromBlaze
     , obsURI
     , obsURIString
     , standardResponse
     , showTimeDeltaFwd
     , showTimeDeltaBwd
     , detailsLink, abstractLink
     , getTimes
     , renderLinks
     , getFact
     , linkToRecord
     , linkToRecordA
     , renderFooter
     , jsScript
     , cssLink
     , instLinkSearch
     , gratLinkSearch
     , igLinkSearch
     , instLinkAbout
     , gratLinkAbout
     -- , igLinkAbout
     , constellationLinkSearch
     , typeLinkURI
     , typeDLinkURI
     , typeLinkSearch
     , typeDLinkSearch
     , basicTypeLinkSearch
     , categoryLinkSearch
     , propTypeLink
     , nameLinkSearch
     , jointLinkSearch
     , cleanJointName
     , schedToList
     , getNumObs
     , getScienceExposure
     , getScienceTime
     , dquote
     ) where

import qualified Data.ByteString as B
import qualified Data.Text as T

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Blaze.ByteString.Builder (toByteString)

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Control.Applicative ((<$>))
#endif

import Data.Char (intToDigit)
import Data.Either (rights)
import Data.List (foldl')
import Data.Maybe (fromJust, fromMaybe, isJust)

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Data.Monoid ((<>), mconcat, mempty)
#else
import Data.Monoid ((<>))
#endif

import Data.Text.Encoding (encodeUtf8)

import Network.HTTP.Types.URI (encodePath
                              , encodePathSegments
                              , simpleQueryToQuery)

import System.Random (Random(..), getStdRandom)

import Text.Blaze.Html.Renderer.Text

import Web.Scotty

#if defined(MIN_VERSION_time) && MIN_VERSION_time(1,5,0)
import Data.Time (UTCTime, NominalDiffTime, addUTCTime, defaultTimeLocale, diffUTCTime, formatTime)
#else
import Data.Time (UTCTime, NominalDiffTime, addUTCTime, diffUTCTime, formatTime)
import System.Locale (defaultTimeLocale)
#endif

import Types (ScienceObs(..), ObsIdVal(..)
             , Instrument, Grating(..)
             , ChandraTime(..), TimeKS(..)
             , Constraint(..), ConLong(..), ConShort(..)
             , SimbadType(..), SimbadInfo(..)
             , Instrument(..)
             , ChipStatus(..)
             , Proposal(..)
             , PropType, PropCategory
             , Record
             , JointMission
             , TargetName
             , recordObsId, recordTarget, recordStartTime
             , recordTime, futureTime
             , getJointObs, getConstellationName
             , simbadTypeToDesc
             , fromMission, toMission, fromMissionLongLink
             , addTimeKS, zeroKS, isZeroKS, showExpTime
             , fromPropType, toPropType, toPropTypeLabel
             )

-- | Convert a record into the URI fragment that represents the
--   page for the record.`<
obsURI :: ObsIdVal -> H.AttributeValue
obsURI = H.toValue . obsURIString

obsURIString :: ObsIdVal -> String
obsURIString (ObsIdVal oi) = "/obsid/" <> show oi

fromBlaze :: H.Html -> ActionM ()
fromBlaze = html . renderHtml

-- A placeholder in case we want to set up any 
-- response settings.
standardResponse :: ActionM ()
standardResponse = return ()

jsScript :: H.AttributeValue -> H.Html
jsScript uri = (H.script H.! A.src uri) ""

cssLink :: H.AttributeValue -> H.Html
cssLink uri =
  H.link H.! A.href   uri
         H.! A.type_  "text/css"
         H.! A.rel    "stylesheet"
         H.! A.media  "all"

defaultMeta :: H.Html
defaultMeta = H.meta H.! A.httpEquiv "Content-Type"
                     H.! A.content "text/html; charset=UTF-8"

-- | Load the JS and CSS needed to display the sky map and table info.
--
skymapMeta :: H.Html
skymapMeta =
  d3Meta
  <> jsScript "/js/jquery.tablesorter.min.js"
  <> jsScript "/js/table.js"
  <> jsScript "/js/projection.js"
  <> cssLink "/css/tablesorter.css"
  <> cssLink "/css/schedule.css"

-- | Load D3 and JQuery.
d3Meta :: H.Html
d3Meta =
  jqueryMeta
  <> jsScript "https://d3js.org/d3.v3.min.js"
  <> jsScript "https://d3js.org/d3.geo.projection.v0.min.js"

-- | Load JQuery.
--
--   It is likely that this should not be used, as you probably want
--   skymapMeta or d3Meta instead.
jqueryMeta :: H.Html
jqueryMeta = jsScript "https://code.jquery.com/jquery-1.11.1.min.js"

plural :: Int -> String
plural i = if i > 1 then "s" else ""

getTimeElems ::
  UTCTime     -- time 1
  -> UTCTime  -- time 2, >= time 1
  -> (NominalDiffTime, Int, Int, Int, NominalDiffTime, NominalDiffTime, NominalDiffTime)
getTimeElems t1 t2 = 
  let delta = diffUTCTime t2 t1
      m = delta / 60
      h = delta / 3600
      d = delta / (24 * 3600)
      nm = round m -- ceiling or round
      nh = round h
      nd = round d

  in (delta, nd, nh, nm, d, h, m)

-- | Report a day that's more than about a week in the
--   future or past.
showTime :: UTCTime -> String
showTime = formatTime defaultTimeLocale "%A, %B %e, %Y"

-- | Come up with a string representing the time difference. It
--   is probably not general enough, since it adds in a
--   prefix (here, "in"), in most cases. So, this is to be
--   used for time differences in the future.
--
--   Unlike the other "show time difference" routines, this
--   one special cases a value where time2 == futureTime,
--   which means that it is unscheduled.
--
showTimeDeltaFwd ::
  UTCTime     -- time 1
  -> ChandraTime  -- time 2, >= time 1
  -> String   -- time1 relative to time2
showTimeDeltaFwd t1 c2@(ChandraTime t2) = 
  let (delta, nd, nh, nm, d, h, m) = getTimeElems t1 t2

      mins = "in " <> show nm <> " minute" <> plural nm
      hours = "in " <> show nh <> " hour" <> plural nh
      days = "in " <> show nd <> " day" <> plural nd
      other = showTime t2

  in if c2 == futureTime
     then "observation is not scheduled"
     else if delta < 60
          then "now"
          else if m < 60
               then mins
               else if h < 24
                    then hours
                    else if d < 7
                         then days
                         else "on " <> other

-- | Come up with a string representing the time difference.  This is
--   to be used for time differences in the future; see also
--   showTimeDeltaBwd.
showTimeDeltaBwd ::
  ChandraTime     -- time 1
  -> UTCTime  -- time 2, >= time 1
  -> String   -- time1 relative to time2
showTimeDeltaBwd (ChandraTime t1) t2 = 
  let (delta, nd, nh, nm, d, h, m) = getTimeElems t1 t2

      mins = show nm <> " minute" <> plural nm <> " ago"
      hours = show nh <> " hour" <> plural nh <> " ago"
      days = show nd <> " day" <> plural nd <> " ago"
      other = showTime t1

  in if delta < 60
     then "now"
     else if m < 60
            then mins
            else if h < 24
                 then hours
                 else if d < 7
                      then days
                      else "on " <> other


-- TODO:
-- Ideally we would link to something a bit more readable than the
-- archive page (ie its use of frames), and also present the link
-- in a more-friendly manner than as a link from the obsid or
-- sequence number.
--
obsIdLink :: ObsIdVal -> H.AttributeValue
obsIdLink ObsIdVal{..} =
  H.toValue $ "http://cda.cfa.harvard.edu/chaser/startViewer.do?menuItem=details&obsid=" ++ show fromObsId

seqLink :: ObsIdVal -> H.AttributeValue
seqLink ObsIdVal{..} =
  H.toValue ("http://cda.cfa.harvard.edu/chaser/startViewer.do?menuItem=sequenceSummary&obsid=" <> show fromObsId)

detailsLink, abstractLink :: ObsIdVal -> H.AttributeValue
detailsLink = obsIdLink
abstractLink ObsIdVal{..} = 
  H.toValue ("http://cda.cfa.harvard.edu/chaser/startViewer.do?menuItem=propAbstract&obsid=" <> show fromObsId)

jointLink :: JointMission -> H.AttributeValue
jointLink jm = H.toValue ("/search/joint/" <> fromMission jm)

-- | Create a link to the mission.
jointLinkSearch :: JointMission -> H.Html
jointLinkSearch jm =
  (H.a H.! A.href (jointLink jm)) (H.toHtml (fromMission jm))


-- | Remove the CXO- prefix from the "joint with" field,
--   since I have seen two cases of "CXO-HST". Presumably this
--   is for time awarded by the other facility (e.g. HST).
--
cleanJointName :: T.Text -> T.Text
cleanJointName j = if "CXO-" `T.isPrefixOf` j then T.drop 4 j else j 

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
renderObsIdDetails ::
  Maybe Proposal
  -> Maybe SimbadInfo
  -> ScienceObs
  -> H.Html
renderObsIdDetails mprop msimbad so@ScienceObs{..} =
  let name = soTarget
      inst = soInstrument
      grat = soGrating
      instInfo = instLinkSearch inst <>
                 if grat == NONE
                 then mempty
                 else ", " <> H.toHtml grat

      left = (H.td H.! A.class_ "key")
      right = (H.td H.! A.class_ "value")

      keyVal k v = H.tr (left k <> " " <> right v)

      oLink = H.a H.! A.href (obsIdLink soObsId) $ H.toHtml soObsId
      sLink = H.a H.! A.href (seqLink soObsId)   $ H.toHtml soSequence
      pLink = H.a H.! A.href ("/proposal/" <> H.toValue soProposal) $ H.toHtml soProposal

       -- rely on the ToMarkup instance of TimeKS
      expLink = case soObservedTime of
        Just t -> keyVal "Exposure (observed):" (H.toHtml t <> " ks")
        _ -> keyVal "Exposure (approved):" (H.toHtml soApprovedTime <> " ks")

      -- NOTE: can this be cleared up now that I have a better
      -- understanding of the jointwith and exposure-time fields?
      --
      missToLink mission = maybe (H.toHtml mission)
                           fromMissionLongLink (toMission mission)
                           
      toJ (l,v) = keyVal "Joint with:"
                  (missToLink l <> " for " <> H.toHtml (_toKS v) <> " ks")
                  
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
          Just ln -> Just ((H.a H.! A.href uri) (H.toHtml (fromConLong ln)))
          _ -> Nothing
        
      too = maybe mempty (keyVal "TOO:" . H.toHtml) soTOO

      -- the "chip" display depends on whether this has been archived or
      -- not (and if it's ACIS or HRC), since we display different things.
      chipDetails = case soDetector of
        Just dm -> keyVal "Chips:" (H.toHtml dm)
        _ -> fromMaybe mempty $ keyVal "Chips: " . H.toHtml <$> detector

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
        return $ keyVal "Sub Array:" $ H.toHtml ("Start: " ++ show start ++ " Rows: " ++ show nrow)

      -- bundle several items into a single line
      propInfo Proposal {..} =
        let p0 = H.toHtml propType
            plink n = propTypeLink n Nothing
            ptype = maybe p0 plink (toPropType propType)
        in 
          keyVal "Proposal:"
          ("Cycle " <> H.toHtml propCycle <> ", "
           <> ptype <> ", "
           <> categoryLinkSearch propCategory propCategory
          )

      simbadInfo SimbadInfo {..} =
        keyVal "SIMBAD Type:" (typeDLinkSearch smiType3 smiType)

      discardRows = if soStatus == "discarded"
                    then [ H.tr ((H.td H.! A.class_ "note")
                                 "Note: the observation was discarded") ]
                    else []

      tblRows =
        mconcat discardRows
        <>
        mconcat
          [ keyVal "Target:" (nameLinkSearch name)
          , maybe mempty simbadInfo msimbad
          , keyVal "Observation Details:" oLink
          , keyVal "Sequence Summary:" sLink
          , keyVal "Proposal Id:" pLink
          , maybe mempty propInfo mprop
          , too
          , keyVal "Instrument:" instInfo
          , chipDetails
          , fromMaybe mempty subArray
          , fromMaybe mempty (keyVal "Data Mode:" . H.toHtml <$> soDataMode)
          -- rely on the ToMarkup instance of ChandraTime
          , keyVal "Date:" (H.toHtml soStartTime)
          , expLink
          -- rely on the ToMarkup instance of RA
          , keyVal "Right Ascension:" (H.toHtml soRA)
          -- rely on the ToMarkup instance of Dec
          , keyVal "Declination:" (H.toHtml soDec)
          , keyVal "Roll:" (H.toHtml soRoll <> "\176") -- this should be \u00b0, the degree symbol
          , fromMaybe mempty $ keyVal "Constellation:" <$> conLink soConstellation
          , jointElems
          , constraintElems
          ]

      -- ignore the thead element
      tbl = H.table (H.tbody tblRows)

  in (H.div H.! A.class_ "inactive" H.! A.id "Details") 
      tbl

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
-- We also display the observational details - in \"raw\" form - as a text box
-- as part of this section.
--
-- This is modified somewhat for discarded observations (not sure the best way to
-- do this at this time).
--
renderLinks :: 
  Bool -- True if current obs
  -> Maybe Proposal
  -> Maybe SimbadInfo
  -- ^ assumed to be for the observation
  -> ScienceObs
  -> H.Html
renderLinks f mprop msimbad so@ScienceObs{..} =
  let optSel :: String -> Bool -> H.Html
      optSel lbl cf = 
        let idName = H.toValue (lbl++"button")
            base = H.input H.! A.type_ "radio"
                           H.! A.name  "opttype"
                           H.! A.value (H.toValue lbl)
                           H.! A.id idName
                           H.! A.onclick
                               ("switchOption('" <> H.toValue lbl <> "')")
        in (if cf then base H.! A.checked "checked" else base)
           <> (H.label H.! A.for idName) (H.toHtml lbl)

      wwtLink = if f
                then (H.a H.! A.href "/wwt.html") "WWT"
                else (H.a H.! A.href (H.toValue (obsURI soObsId) <> "/wwt")) "WWT"

      form = H.div H.! A.class_ "radiobuttons" $
              mconcat [ "View: "
                      , optSel "DSS" True
                      , optSel "RASS" False
                      , optSel "PSPC" False
                      , optSel "Details" False
                      , " or in "
                      , wwtLink
                      ]

      urlHead = mconcat [ "http://asc.harvard.edu/targets/"
                        , H.toValue soSequence, "/"
                        , H.toValue soSequence, "."
                        , H.toValue soObsId, ".soe."
                        ]

      link :: String -> H.AttributeValue -> Bool -> H.Html
      link lbl frag af = 
        let uri = urlHead <> frag <> ".gif"
        in H.img H.! A.src    uri
                 H.! A.alt    (H.toValue ("The instrument field-of-view on top of the " <> lbl <> " image of the source."))
                 H.! A.width  (H.toValue (680::Int))
                 H.! A.height (H.toValue (680::Int))
                 H.! A.id     (H.toValue lbl)
                 H.! A.class_ (if af then "active" else "inactive")

  in form <>
    (H.div H.! A.class_ "links")
     (link "DSS" "dss" True <>
      link "PSPC" "pspc" False <>
      link "RASS" "rass" False <>
      renderObsIdDetails mprop msimbad so)
 
getTimes ::
  Record
  -> (ChandraTime, ChandraTime) -- start and end times
getTimes rs =
  let sTime = _toUTCTime (recordStartTime rs)
      expTime = fromInteger . ceiling $ 1000 * _toKS (recordTime rs)
      eTime = addUTCTime expTime sTime
  in (ChandraTime sTime, ChandraTime eTime)

-- | Return a "random" Chandra fact. The HTML is inserted into
--   a div with class of "fact".
--
getFact :: IO H.Html
getFact = do
  n <- getStdRandom (randomR (0, length facts - 1))
  return $ facts !! n

facts :: [H.Html]
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

linkToRecord :: Record -> H.Html
linkToRecord = linkToRecordA recordTarget

linkToRecordA ::
  H.ToMarkup a
  => (Record -> a)
  -> Record
  -> H.Html
linkToRecordA f r = 
  let uri = obsURI (recordObsId r)
  in (H.a H.! A.href uri) (H.toHtml (f r))

-- | The standard footer; needs to match up with the html files in static/.
renderFooter :: H.Html
renderFooter =
  H.p H.! A.id "banner" $
    mconcat [
      "The 'What is Chandra doing now?' web site is written "
      , "by "
      , (H.a H.! A.href "http://twitter.com/doug_burke") "@doug_burke"
      , ", comes with no warranty (in other words, I make no "
      , "guarantee that the information presented is correct, although "
      , "I try my best to make sure it is), and is not "
      , "an official product of the "
      , (H.a H.! A.href "http://chandra.si.edu/") "Chandra X-ray Center"
      , "."
    ]

-- Note that there is a slight difference in the instrument and grating
-- search links: for instrument the "nice" conversion is used to create
-- the last element of the path (e.g. ACIS-I), rather than an internal
-- form (ACISI); this is not true for gratings since the "nice" form
-- looks like "Low Energy Transmission Grating" rather than "LETG".
--

-- | Add in a link to the instrument search page.
instLinkSearch :: Instrument -> H.Html
instLinkSearch inst = 
  let iLink = "/search/instrument/" <> H.toValue inst
  in (H.a H.! A.href iLink) (H.toHtml inst)

-- | Add in a link to the grating search page.
gratLinkSearch :: Grating -> H.Html
gratLinkSearch grat = 
  let gLink = "/search/grating/" <> H.toValue (show grat)
  in (H.a H.! A.href gLink) (H.toHtml grat)

-- | Add in a link to the combined instrument+grating search page.
igLinkSearch :: (Instrument, Grating) -> H.Html
igLinkSearch (inst, grat) = 
  let linkVal = "/search/instgrat/" <> H.toValue frag
      frag = show inst ++ "-" ++ show grat
      lbl = H.toHtml inst <> " with " <> H.toHtml grat
  in (H.a H.! A.href linkVal) lbl

-- | Add in a link to a "what is this" page for the
--   instrument.
instLinkAbout :: Instrument -> H.Html
instLinkAbout inst = 
  let iLink = "/about/instruments.html#" <> H.toValue inst
  in (H.a H.! A.href iLink) (H.toHtml inst)

-- | Add in a link to a "what is this" page for the
--   grating.
gratLinkAbout :: Grating -> H.Html
gratLinkAbout NONE = "no grating"
gratLinkAbout grat = 
  let linkVal = "/about/instruments.html#" <> H.toValue (show grat)
  in (H.a H.! A.href linkVal) ("the " <> H.toHtml grat)

{-
-- | Link to the general instruments page
igLinkAbout :: H.Html
igLinkAbout = (H.a H.! A.href "/about/instruments.html") "Chandra instruments"
-}

-- | Add in a link to the constellation search page.
constellationLinkSearch ::
  H.ToMarkup a
  => ConShort
  -> a
  -- ^ The link text
  -> H.Html
constellationLinkSearch con lbl = 
  let iLink = "/search/constellation/" <> H.toValue (fromConShort con)
  in (H.a H.! A.href iLink) (H.toHtml lbl)

typeLinkURI :: SimbadType -> B.ByteString
typeLinkURI st =
  let uri = ["search", "type", fromSimbadType st]
  in toByteString (encodePathSegments uri)

typeDLinkURI :: SimbadType -> B.ByteString
typeDLinkURI st =
  let uri = ["search", "dtype", fromSimbadType st]
  in toByteString (encodePathSegments uri)
  
-- | Add in a link to the object-type search page.
--
--  Note that we take care to encode the path because of special
--  characters in the Simbad type (in particular, '?').
--
typeLinkSearch ::
  H.ToMarkup a
  => SimbadType
  -> a
  -> H.Html
typeLinkSearch st lbl = 
  let iLink = H.unsafeByteStringValue (typeLinkURI st)
  in (H.a H.! A.href iLink) (H.toHtml lbl)

typeDLinkSearch ::
  H.ToMarkup a
  => SimbadType
  -> a
  -> H.Html
typeDLinkSearch st lbl = 
  let iLink = H.unsafeByteStringValue (typeDLinkURI st)
  in (H.a H.! A.href iLink) (H.toHtml lbl)

-- | Should this be a wrapper around typeLinkSearch or
--   typeDLinkSearch?
basicTypeLinkSearch :: Maybe SimbadType -> H.Html
basicTypeLinkSearch Nothing =
  (H.a H.! A.href "/search/type/unidentified") "Unidentified"
basicTypeLinkSearch (Just s) =
  let txt = fromMaybe "unknown SIMBAD type" (simbadTypeToDesc s)
  in typeLinkSearch s txt

-- | Add in a link to the obervation category search page.
categoryLinkSearch ::
  H.ToMarkup a
  => PropCategory
  -> a
  -> H.Html
categoryLinkSearch cat lbl = 
  let iLink = "/search/category/" <> H.toValue cat
  in (H.a H.! A.href iLink) (H.toHtml lbl)

-- | Link to a proposal type.
propTypeLink :: PropType -> Maybe T.Text -> H.Html
propTypeLink propType mlbl =
  let lbl = fromMaybe (toPropTypeLabel propType) mlbl
      pLink = "/search/proptype/" <> H.toValue (fromPropType propType)
  in (H.a H.! A.href pLink) (H.toHtml lbl)

-- | Add a link to the name-search for an object.
nameLinkSearch :: TargetName -> H.Html
nameLinkSearch name =
  let uriBS = toByteString (encodePath ["search", "name"] qry)
      uri = H.unsafeByteStringValue uriBS
      -- be explicit that query has a value
      qry = simpleQueryToQuery [("target", encodeUtf8 name)]
  in (H.a H.! A.href uri) (H.toHtml name)
    
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- | Convert the schedule contents to a list (although the
--   routine is generic in the list input.
--
schedToList :: [a] -> Maybe a -> [a] -> [a]
schedToList done mdoing todo = done ++ maybeToList mdoing ++ todo

getNumObs :: [a] -> Maybe a -> [a] -> String
getNumObs done mdoing todo =
  let xs = schedToList done mdoing todo
      nobs = length xs
      obslen = case nobs of
        0 -> "are no observations"
        1 -> "is one observation"
        _ -> "are " ++ show nobs ++ " observations"
  in "There " ++ obslen ++ ", but this is a small fraction of the Chandra mission"


-- | Return the total obervation time for the science observations in the
--   schedule.
getScienceExposure ::
  [Record]
  -> Maybe Record
  -> [Record]
  -> TimeKS
getScienceExposure done mdoing todo =
  let sobs = rights (schedToList done mdoing todo)
      getTime so = fromMaybe (soApprovedTime so) (soObservedTime so)
  in foldl' addTimeKS zeroKS (map getTime sobs)


getScienceTime ::
  [Record]
  -> Maybe Record
  -> [Record]
  -> H.Html
getScienceTime done mdoing todo =
  let etime = getScienceExposure done mdoing todo
  in if isZeroKS etime
     then mempty
     else ", and the total science exposure time for these observations is "
          <> H.toHtml (showExpTime etime)

ldquo, rdquo :: H.Html
ldquo = H.preEscapedToHtml ("&ldquo;" :: T.Text)
rdquo = H.preEscapedToHtml ("&rdquo;" :: T.Text)

dquote :: H.Html -> H.Html
dquote txt = ldquo <> txt <> rdquo
