{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Create URIs for various access points.
--
module API (scheduleOnDate

           , obsURI
           , obsURIString

           , linkToRecord
           , linkToRestrictedRecord
           , linkToRecordA
             
           , abstractLink
           , detailsLink
           , obsIdLink
           , seqLink

           , propTypeLink
           , proposalLink
             
           , tooLinkSearch
           , tooLinkSearchLong
             
           , instLinkSearch
           , gratLinkSearch
           , igLinkSearch
           , instLinkAbout
           , gratLinkAbout
             -- , igLinkAbout

           , constellationLinkSearch
           , cycleLinkSearch
             
           , typeLinkURI
           , typeDLinkURI
           , typeLinkSearch
           , typeDLinkSearch
           , basicTypeLinkSearch
           , categoryLinkSearch
           , nameLinkSearch
           , jointLinkSearch
           , constraintLinkSearch
             
           , fromMissionAboutLink
           , fromMissionLongLink

           , jsScript
           , cssLink

           )
       where

import qualified Data.ByteString as B
import qualified Data.Text as T

import qualified Text.Blaze.Html5 as H

import Text.Blaze.Html5 (AttributeValue, Html
                        , link, script
                        , textValue, toHtml)
import Text.Blaze.Html5.Attributes (href, media, rel, src, type_)

import Blaze.ByteString.Builder (toByteString)

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Time.Calendar (Day)

import Network.HTTP.Types.URI (encodePathSegments
                               , queryTextToQuery
                               , renderQuery)

import Types (ConShort(..)
             , ConstraintKind(..)
             , Cycle(..)
             , Instrument(..)
             , JointMission
             , Grating(..)
               
             , ObsIdVal
             , fromObsId
               
             , PropCategory
             , Proposal(..)
             , PropType
             , Record
             , RestrictedRecord
               
             , SimbadType
             , fromSimbadType
               
             , TargetName
             , fromTargetName
               
             , TOORequestTime
             , recordTarget
             , recordObsId

             , rrecordTarget
             , rrecordObsId
               
             , fromMission
             , getMissionInfo
             
             , fromPropType
             , toPropTypeLabel
             , rtToLabel
             , simbadTypeToDesc
             , csToLabel, csToLC
               )
import Utils (HtmlContext(StaticHtml), toLink, fromDay, showInt)

-- | Convert a record into the URI fragment that represents the
--   page for the record.
obsURI :: ObsIdVal -> AttributeValue
obsURI = textValue . obsURIString

obsURIString :: ObsIdVal -> T.Text
obsURIString oi = "/obsid/" <> showInt (fromObsId oi)

-- | Show the schedule around the given date.
--
scheduleOnDate ::
  Day
  -- ^ The center of the schedule
  -> Int
  -- ^ The number of days before and after the day to
  --   include. It is expected that this is 0 or greater.
  -> AttributeValue
  -- ^ The path to use.
scheduleOnDate d n =
  let day = textValue (fromDay d)
      ndays = textValue (showInt n)
  in "/schedule/date/" <> day <> "/" <> ndays


linkToRecordA ::
  H.ToMarkup a
  => (Record -> a)
  -> Record
  -> Html
linkToRecordA f r = 
  let uri = obsURI (recordObsId r)
  in toLink StaticHtml uri (toHtml (f r))

linkToRecord :: Record -> Html
linkToRecord = linkToRecordA recordTarget

linkToRestrictedRecordA ::
  H.ToMarkup a
  => (RestrictedRecord -> a)
  -> RestrictedRecord
  -> Html
linkToRestrictedRecordA f r = 
  let uri = obsURI (rrecordObsId r)
  in toLink StaticHtml uri (toHtml (f r))

linkToRestrictedRecord :: RestrictedRecord -> Html
linkToRestrictedRecord = linkToRestrictedRecordA rrecordTarget


data ViewerOption =
  ViewDetails | ViewAbstract | ViewSequence
  deriving Eq

vToValue :: ViewerOption -> T.Text
vToValue ViewDetails = "details"
vToValue ViewAbstract = "propAbstract"
vToValue ViewSequence = "sequenceSummary"

-- TODO:
-- Ideally we would link to something a bit more readable than the
-- archive page (ie its use of frames), and also present the link
-- in a more-friendly manner than as a link from the obsid or
-- sequence number.
--
viewerURI :: ViewerOption -> ObsIdVal -> AttributeValue
viewerURI opt oid =
  let base = "https://cda.cfa.harvard.edu/chaser/startViewer.do?menuItem="
      optVal = vToValue opt
      oidstr = showInt (fromObsId oid)
  in textValue (base <> optVal <> "&obsid=" <> oidstr)

obsIdLink :: ObsIdVal -> AttributeValue
obsIdLink = viewerURI ViewDetails

detailsLink, abstractLink :: ObsIdVal -> AttributeValue
detailsLink = obsIdLink
abstractLink = viewerURI ViewAbstract

seqLink :: ObsIdVal -> AttributeValue
seqLink = viewerURI ViewSequence
     
-- | Link to a proposal type.
propTypeLink :: HtmlContext -> PropType -> Maybe T.Text -> Html
propTypeLink ctx propType mlbl =
  let lbl = fromMaybe (toPropTypeLabel propType) mlbl
      pLink = "/search/proptype/" <> H.toValue (fromPropType propType)
  in toLink ctx pLink (toHtml lbl)


proposalLink :: HtmlContext -> Proposal -> Maybe T.Text -> Html
proposalLink ctx Proposal{..} mlbl =
  let lbl = fromMaybe propName mlbl
      uri = "/proposal/" <> H.toValue propNum
  in toLink ctx uri (toHtml lbl)

     
-- | Link to the TOO category. See also `tooLinkSearchLong`.
--
tooLinkSearch :: HtmlContext -> Maybe TOORequestTime -> Html
tooLinkSearch ctx too =
  let lbl = maybe "None" rtToLabel too
  in tooLinkSearchLong ctx too lbl

-- | Link to the TOO category. See also `tooLinkSearch`.
--
tooLinkSearchLong :: HtmlContext -> Maybe TOORequestTime -> T.Text -> Html
tooLinkSearchLong ctx too lbl =
  let ttype = maybe "none" (T.toLower . rtToLabel) too
      uri = "/search/turnaround/" <> ttype
      uriVal = textValue uri
  in toLink ctx uriVal (toHtml lbl)


-- Note that there is a slight difference in the instrument and grating
-- search links: for instrument the "nice" conversion is used to create
-- the last element of the path (e.g. ACIS-I), rather than an internal
-- form (ACISI); this is not true for gratings since the "nice" form
-- looks like "Low Energy Transmission Grating" rather than "LETG".
--

-- | Add in a link to the instrument search page.
instLinkSearch :: HtmlContext -> Instrument -> Html
instLinkSearch ctx inst = 
  let iLink = "/search/instrument/" <> H.toValue inst
  in toLink ctx iLink (toHtml inst)

-- | Add in a link to the grating search page.
gratLinkSearch :: HtmlContext -> Grating -> Html
gratLinkSearch ctx grat = 
  let gLink = "/search/grating/" <> H.toValue (show grat)
  in toLink ctx gLink (toHtml grat)

-- | Add in a link to the combined instrument+grating search page.
igLinkSearch :: (Instrument, Grating) -> Html
igLinkSearch (inst, grat) = 
  let linkVal = "/search/instgrat/" <> H.toValue frag
      frag = show inst ++ "-" ++ show grat
      lbl = toHtml inst <> " with " <> toHtml grat
  in toLink StaticHtml linkVal lbl

-- | Add in a link to a "what is this" page for the
--   instrument.
instLinkAbout :: Instrument -> Html
instLinkAbout inst = 
  let iLink = "/about/instruments.html#" <> H.toValue inst
  in toLink StaticHtml iLink (toHtml inst)

-- | Add in a link to a "what is this" page for the
--   grating.
gratLinkAbout :: Grating -> Html
gratLinkAbout NONE = "no grating"
gratLinkAbout grat = 
  let linkVal = "/about/instruments.html#" <> H.toValue (show grat)
  in toLink StaticHtml linkVal ("the " <> toHtml grat)

{-
-- | Link to the general instruments page
igLinkAbout :: Html
igLinkAbout = (a H.! href "/about/instruments.html") "Chandra instruments"
-}

-- | Add in a link to the constellation search page.
constellationLinkSearch ::
  H.ToMarkup a
  => HtmlContext
  -> ConShort
  -> a
  -- ^ The link text
  -> Html
constellationLinkSearch ctx con lbl = 
  let iLink = "/search/constellation/" <> H.toValue (fromConShort con)
  in toLink ctx iLink (toHtml lbl)

-- | Link to the given search (apart from for the "All" cycles,
--   which we currently don't support in the schedule view since
--   it has too many observations).
--
cycleLinkSearch ::
  HtmlContext
  -> Cycle
  -> Html
cycleLinkSearch ctx cyc =
  let iLink = "/search/cycle/" <> H.toValue lbl
      lbl = fromCycle cyc
  in if lbl == "all"
     then "All cycles"
     else toLink ctx iLink (toHtml ("Cycle " <> lbl))


data TypeOption = TypeLink | TypeDLink deriving Eq

tToValue :: TypeOption -> T.Text
tToValue TypeLink = "type"
tToValue TypeDLink = "dtype"
  
toTypeLinkURI :: TypeOption -> SimbadType -> B.ByteString
toTypeLinkURI t st =
  -- as the SIMBAD type includes items like "Y*?", ensure the
  -- URI is encoded
  let uri = ["search", tToValue t, fromSimbadType st]
  in toByteString (encodePathSegments uri)
     
typeLinkURI :: SimbadType -> B.ByteString
typeLinkURI = toTypeLinkURI TypeLink

typeDLinkURI :: SimbadType -> B.ByteString
typeDLinkURI = toTypeLinkURI TypeDLink
  
-- | Add in a link to the object-type search page.
--
--  Note that we take care to encode the path because of special
--  characters in the Simbad type (in particular, '?').
--
typeLinkSearch ::
  H.ToMarkup a
  => HtmlContext
  -> SimbadType
  -> a
  -> Html
typeLinkSearch ctx st lbl = 
  let iLink = H.unsafeByteStringValue (typeLinkURI st)
  in toLink ctx iLink (toHtml lbl)

typeDLinkSearch ::
  H.ToMarkup a
  => HtmlContext
  -> SimbadType
  -> a
  -> Html
typeDLinkSearch ctx st lbl = 
  let iLink = H.unsafeByteStringValue (typeDLinkURI st)
  in toLink ctx iLink (toHtml lbl)

-- | Should this be a wrapper around typeLinkSearch or
--   typeDLinkSearch?
basicTypeLinkSearch :: Maybe SimbadType -> Html
basicTypeLinkSearch Nothing =
  toLink StaticHtml "/search/type/unidentified" "Unidentified"
basicTypeLinkSearch (Just s) =
  let txt = fromMaybe "unknown SIMBAD type" (simbadTypeToDesc s)
  in typeLinkSearch StaticHtml s txt

-- | Add in a link to the obervation category search page.
categoryLinkSearch ::
  H.ToMarkup a
  => HtmlContext
  -> PropCategory
  -> a
  -> Html
categoryLinkSearch ctx cat lbl = 
  let iLink = "/search/category/" <> H.toValue cat
  in toLink ctx iLink (toHtml lbl)

-- | Search for the given target. This is an exact (case insensitive)
--   search.
--
targetSearch :: TargetName -> AttributeValue
targetSearch tn =
  -- since the target name could contain any character, ensure this
  -- is encoded
  let qry = queryTextToQuery [("target", Just (fromTargetName tn))]
      uriBS = "/search/name" <> renderQuery True qry
  in H.unsafeByteStringValue uriBS
     
-- | Add a link to the name-search for an object.
nameLinkSearch ::
  HtmlContext
  -> TargetName
  -> Maybe T.Text
  -- ^ The text to use for the link; if Nothing then use the
  --   target name.
  -> Html
nameLinkSearch ctx tgt mlbl =
  let lbl = fromMaybe (fromTargetName tgt) mlbl
  in toLink ctx (targetSearch tgt) (toHtml lbl)
    
jointLink :: JointMission -> AttributeValue
jointLink jm = H.toValue ("/search/joint/" <> fromMission jm)

-- | Create a link to the mission.
jointLinkSearch :: JointMission -> Html
jointLinkSearch jm =
  toLink StaticHtml (jointLink jm) (toHtml (fromMission jm))

-- | Link to the constraint search.
constraintLinkSearch :: Maybe ConstraintKind -> Html
constraintLinkSearch (Just cs) =
  let uri = textValue ("/search/constraints/" <> csToLC cs)
  in toLink StaticHtml uri (toHtml (csToLabel cs))
constraintLinkSearch Nothing =
  let uri = textValue "/search/constraints/none"
  in toLink StaticHtml uri "None"


-- | Link to the search page.
--
fromMissionLongLink :: HtmlContext -> JointMission -> Maybe H.Html
fromMissionLongLink ctx m =
  case getMissionInfo m of
    Just (shortName, longName, _) ->
      let url = H.toValue ("/search/joint/" <> shortName)
      in Just (toLink ctx url (toHtml longName))
    Nothing -> Nothing
    

-- | Link to a page about the mission.
fromMissionAboutLink :: JointMission -> Maybe H.Html
fromMissionAboutLink m =
  case getMissionInfo m of
    Just (_, longName, url) -> Just (toLink StaticHtml url (toHtml longName))
    Nothing -> Nothing


jsScript :: AttributeValue -> Html
jsScript uri = (script H.! src uri) ""

cssLink :: AttributeValue -> Html
cssLink uri =
  link H.! href   uri
       H.! type_  "text/css"
       H.! rel    "stylesheet"
       H.! media  "all"

