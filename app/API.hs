{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Create URIs for various access points.
--
module API (targetSearch
           , scheduleOnDate

           , obsURI
           , obsURIString

           , linkToRecord
           , linkToRecordA
             
           , abstractLink
           , detailsLink
           , obsIdLink
           , seqLink

           , propTypeLink
             
           , tooLinkSearch
           , tooLinkSearchLong
             
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
           , nameLinkSearch
           , jointLinkSearch
           , constraintLinkSearch
             
           , jsScript
           , cssLink

           )
       where

import qualified Data.ByteString as B
import qualified Data.Text as T

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Blaze.Html5 (AttributeValue, Html, textValue, toHtml)

import Blaze.ByteString.Builder (toByteString)

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Calendar (Day)

import Network.HTTP.Types.URI (encodePath
                               , encodePathSegments
                               , simpleQueryToQuery)

import Types (ConShort(..)
             , ConstraintKind(..)
             , Instrument(..)
             , JointMission
             , Grating(..)
             , ObsIdVal(..)
             , PropCategory
             , PropType
             , Record
             , SimbadType(..)
             , TargetName
             , TOORequestTime
             , recordTarget, recordObsId
             , fromMission
             , fromPropType
             , toPropTypeLabel
             , rtToLabel
             , simbadTypeToDesc
             , csToLabel, csToLC
               )
import Utils (fromDay, showInt)

-- | Convert a record into the URI fragment that represents the
--   page for the record.
obsURI :: ObsIdVal -> AttributeValue
obsURI = textValue . obsURIString

obsURIString :: ObsIdVal -> T.Text
obsURIString (ObsIdVal oi) = "/obsid/" <> showInt oi

-- | Search for the given target. This is an exact (case insensitive)
--   search.
--
targetSearch :: TargetName -> AttributeValue
targetSearch name =
  "/search/name?target=" <> textValue name

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


linkToRecord :: Record -> Html
linkToRecord = linkToRecordA recordTarget

linkToRecordA ::
  H.ToMarkup a
  => (Record -> a)
  -> Record
  -> Html
linkToRecordA f r = 
  let uri = obsURI (recordObsId r)
  in (H.a H.! A.href uri) (toHtml (f r))


-- TODO:
-- Ideally we would link to something a bit more readable than the
-- archive page (ie its use of frames), and also present the link
-- in a more-friendly manner than as a link from the obsid or
-- sequence number.
--
viewerURI :: T.Text -> ObsIdVal -> AttributeValue
viewerURI opts ObsIdVal{..} =
  let base = "http://cda.cfa.harvard.edu/chaser/startViewer.do?menuItem="
  in textValue (base <> opts <> "&obsid=" <> showInt fromObsId)

obsIdLink :: ObsIdVal -> AttributeValue
obsIdLink = viewerURI "details"

detailsLink, abstractLink :: ObsIdVal -> AttributeValue
detailsLink = obsIdLink
abstractLink = viewerURI "propAbstract"

seqLink :: ObsIdVal -> AttributeValue
seqLink = viewerURI "sequenceSummary"
     
-- | Link to a proposal type.
propTypeLink :: PropType -> Maybe T.Text -> Html
propTypeLink propType mlbl =
  let lbl = fromMaybe (toPropTypeLabel propType) mlbl
      pLink = "/search/proptype/" <> H.toValue (fromPropType propType)
  in (H.a H.! A.href pLink) (toHtml lbl)

-- | Link to the TOO category. See also `tooLinkSearchLong`.
--
--   It is assumed that the input argument is valid.
tooLinkSearch :: Maybe TOORequestTime -> Html
tooLinkSearch too =
  let lbl = maybe "None" rtToLabel too
  in tooLinkSearchLong too lbl

-- | Link to the TOO category. See also `tooLinkSearch`.
--
--   It is assumed that the input argument is valid.
tooLinkSearchLong :: Maybe TOORequestTime -> T.Text -> Html
tooLinkSearchLong too lbl =
  let ttype = maybe "none" (T.toLower . rtToLabel) too
      uri = encodePathSegments ["search", "turnaround", ttype]
      uriVal = H.unsafeByteStringValue (toByteString uri)
  in (H.a H.! A.href uriVal) (toHtml lbl)


-- Note that there is a slight difference in the instrument and grating
-- search links: for instrument the "nice" conversion is used to create
-- the last element of the path (e.g. ACIS-I), rather than an internal
-- form (ACISI); this is not true for gratings since the "nice" form
-- looks like "Low Energy Transmission Grating" rather than "LETG".
--

-- | Add in a link to the instrument search page.
instLinkSearch :: Instrument -> Html
instLinkSearch inst = 
  let iLink = "/search/instrument/" <> H.toValue inst
  in (H.a H.! A.href iLink) (toHtml inst)

-- | Add in a link to the grating search page.
gratLinkSearch :: Grating -> Html
gratLinkSearch grat = 
  let gLink = "/search/grating/" <> H.toValue (show grat)
  in (H.a H.! A.href gLink) (toHtml grat)

-- | Add in a link to the combined instrument+grating search page.
igLinkSearch :: (Instrument, Grating) -> Html
igLinkSearch (inst, grat) = 
  let linkVal = "/search/instgrat/" <> H.toValue frag
      frag = show inst ++ "-" ++ show grat
      lbl = toHtml inst <> " with " <> toHtml grat
  in (H.a H.! A.href linkVal) lbl

-- | Add in a link to a "what is this" page for the
--   instrument.
instLinkAbout :: Instrument -> Html
instLinkAbout inst = 
  let iLink = "/about/instruments.html#" <> H.toValue inst
  in (H.a H.! A.href iLink) (toHtml inst)

-- | Add in a link to a "what is this" page for the
--   grating.
gratLinkAbout :: Grating -> Html
gratLinkAbout NONE = "no grating"
gratLinkAbout grat = 
  let linkVal = "/about/instruments.html#" <> H.toValue (show grat)
  in (H.a H.! A.href linkVal) ("the " <> toHtml grat)

{-
-- | Link to the general instruments page
igLinkAbout :: Html
igLinkAbout = (H.a H.! A.href "/about/instruments.html") "Chandra instruments"
-}

-- | Add in a link to the constellation search page.
constellationLinkSearch ::
  H.ToMarkup a
  => ConShort
  -> a
  -- ^ The link text
  -> Html
constellationLinkSearch con lbl = 
  let iLink = "/search/constellation/" <> H.toValue (fromConShort con)
  in (H.a H.! A.href iLink) (toHtml lbl)

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
  -> Html
typeLinkSearch st lbl = 
  let iLink = H.unsafeByteStringValue (typeLinkURI st)
  in (H.a H.! A.href iLink) (toHtml lbl)

typeDLinkSearch ::
  H.ToMarkup a
  => SimbadType
  -> a
  -> Html
typeDLinkSearch st lbl = 
  let iLink = H.unsafeByteStringValue (typeDLinkURI st)
  in (H.a H.! A.href iLink) (toHtml lbl)

-- | Should this be a wrapper around typeLinkSearch or
--   typeDLinkSearch?
basicTypeLinkSearch :: Maybe SimbadType -> Html
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
  -> Html
categoryLinkSearch cat lbl = 
  let iLink = "/search/category/" <> H.toValue cat
  in (H.a H.! A.href iLink) (toHtml lbl)

-- | Add a link to the name-search for an object.
nameLinkSearch :: TargetName -> Html
nameLinkSearch name =
  let uriBS = toByteString (encodePath ["search", "name"] qry)
      uri = H.unsafeByteStringValue uriBS
      -- be explicit that query has a value
      qry = simpleQueryToQuery [("target", encodeUtf8 name)]
  in (H.a H.! A.href uri) (toHtml name)
    
jointLink :: JointMission -> AttributeValue
jointLink jm = H.toValue ("/search/joint/" <> fromMission jm)

-- | Create a link to the mission.
jointLinkSearch :: JointMission -> Html
jointLinkSearch jm =
  (H.a H.! A.href (jointLink jm)) (toHtml (fromMission jm))

-- | Link to the constraint search.
constraintLinkSearch :: Maybe ConstraintKind -> Html
constraintLinkSearch (Just cs) =
  let uri = H.toValue ("/search/constraints/" <> csToLC cs)
  in (H.a H.! A.href uri) (toHtml (csToLabel cs))
constraintLinkSearch Nothing =
  let uri = H.toValue ("/search/constraints/none" :: T.Text)
  in (H.a H.! A.href uri) "None"


jsScript :: AttributeValue -> Html
jsScript uri = (H.script H.! A.src uri) ""

cssLink :: AttributeValue -> Html
cssLink uri =
  H.link H.! A.href   uri
         H.! A.type_  "text/css"
         H.! A.rel    "stylesheet"
         H.! A.media  "all"

