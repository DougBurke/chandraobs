{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | general routines

module Utils (
     defaultMeta
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
     ) where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Applicative ((<$>))

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mconcat, mempty)
import Data.Time (UTCTime, NominalDiffTime, addUTCTime, diffUTCTime, formatTime)

import System.Locale (defaultTimeLocale)
import System.Random (Random(..), getStdRandom)

import Text.Blaze.Html.Renderer.Text

import Web.Scotty

import Types (ScienceObs(..), ObsIdVal(..), Grating(..), ChandraTime(..), TimeKS(..), Constraint(..), ConLong(..), ConShort(..))
import Types (Record, recordObsId, recordTarget, recordStartTime, recordTime)
import Types (getJointObs, getConstellationName)

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

defaultMeta :: H.Html
defaultMeta = H.meta H.! A.httpEquiv "Content-Type"
                     H.! A.content "text/html; charset=UTF-8"

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
showTimeDeltaFwd ::
  UTCTime     -- time 1
  -> ChandraTime  -- time 2, >= time 1
  -> String   -- time1 relative to time2
showTimeDeltaFwd t1 (ChandraTime t2) = 
  let (delta, nd, nh, nm, d, h, m) = getTimeElems t1 t2

      mins = "in " <> show nm <> " minute" <> plural nm
      hours = "in " <> show nh <> " hour" <> plural nh
      days = "in " <> show nd <> " day" <> plural nd
      other = showTime t2

  in if delta < 60
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
  H.toValue $ "http://cda.cfa.harvard.edu/chaser/startViewer.do?menuItem=sequenceSummary&obsid=" ++ show fromObsId

detailsLink, abstractLink :: ObsIdVal -> H.AttributeValue
detailsLink = obsIdLink
abstractLink ObsIdVal{..} = 
  H.toValue $ "http://cda.cfa.harvard.edu/chaser/startViewer.do?menuItem=propAbstract&obsid=" ++ show fromObsId

-- | Display detailed information about a science observation,
--   for those that just need to know the details.
--
--   Could try and link to the roll/pitch/slew GIF - e.g.
--   http://cxc.harvard.edu/targets/601090/601090.rollvis.gif
--   but it is not obvious how valid it is (since the above
--   does not cover May 2014 when that target was scheduled),
--   and it is rather meaningless to anyone but an expert.
--
renderObsIdDetails :: ScienceObs -> H.Html
renderObsIdDetails so@ScienceObs{..} =
  let name = soTarget
      inst = soInstrument
      grat = soGrating
      instInfo = H.toHtml inst <>
                 if grat == NONE
                 then mempty
                 else ", " <> H.toHtml grat

      left = (H.span H.! A.class_ "key")
      right = (H.span H.! A.class_ "value")

      keyVal k v = left k <> " " <> right v <> H.br

      oLink = H.a H.! A.href (obsIdLink soObsId) $ H.toHtml soObsId
      sLink = H.a H.! A.href (seqLink soObsId)   $ H.toHtml soSequence
      pLink = H.a H.! A.href ("/proposal/" <> H.toValue soProposal) $ H.toHtml soProposal

       -- rely on the ToMarkup instance of TimeKS
      expLink = case soObservedTime of
        Just t -> keyVal "Exposure (observed):" (H.toHtml t <> " ks")
        _ -> keyVal "Exposure (approved):" (H.toHtml soApprovedTime <> " ks")

      toJ (l,v) = keyVal "Joint with:" (l <> " for " <> H.toHtml (_toS v) <> " ks")
      jointElems = mconcat $ map toJ $ getJointObs so

      cToL NoConstraint = "None" -- not used
      cToL Preferred    = "Preferred"
      cToL Required     = "Yes"
      clbls = ["Time critical:", "Monitor:", "Constrained:"]
      cvals = [soTimeCritical, soMonitor, soConstrained]
      constraintElems =
        let f (k,v) = keyVal k (cToL v)
        in mconcat $ map f $ filter ((/= NoConstraint) . snd) $ zip clbls cvals

      conLink con = 
        let uri = H.toValue $ "/search/constellation/" ++ fromConShort con
        in case getConstellationName con of
          Just ln -> return $ (H.a H.! A.href uri) $ H.toHtml $ fromConLong ln
          _ -> Nothing
        
      too = maybe mempty (\t -> keyVal "TOO:" (H.toHtml t)) soTOO

  in (H.div H.! A.class_ "inactive" H.! A.id "Details") 
      (mconcat
       [ keyVal "Observation Details:" oLink
       , keyVal "Sequence Summary:" sLink
       , keyVal "Proposal Id:" pLink
       , too
       , keyVal "Target:" (H.toHtml name)
       , keyVal "Instrument:" instInfo
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
       )

-- Display the DSS/RASS/PSPC links. I assume that they
-- can be auto-generated from the sequence and obsid
-- values, that is:
--
-- http://asc.harvard.edu/targets/<sequence>/<sequence>.<obsid>.soe.dss.gif
-- http://asc.harvard.edu/targets/<sequence>/<sequence>.<obsid>.soe.rass.gif
-- http://asc.harvard.edu/targets/<sequence>/<sequence>.<obsid>.soe.pspc.gif
--
-- We also display the observational details - in \"raw\" form - as a text box
-- as part of this section.
--
renderLinks :: 
  Bool -- True if current obs
  -> ScienceObs
  -> H.Html
renderLinks f so@ScienceObs{..} = 
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
      renderObsIdDetails so) 
 
getTimes ::
  Record
  -> (ChandraTime, ChandraTime) -- start and end times
getTimes rs =
  let sTime = _toUTCTime $ recordStartTime rs
      expTime = fromInteger . ceiling $ 1000 * _toS (recordTime rs)
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

linkToRecordA :: (Record -> String) -> Record -> H.Html
linkToRecordA f r = 
  let uri = obsURI $ recordObsId r
  in H.a H.! A.href uri $ H.toHtml $ f r

-- | The standard footer; needs to match up with static/*.html.
renderFooter :: H.Html
renderFooter =
  H.p H.! A.id "banner" $
    mconcat [
      "The 'What is Chandra doing now?' web site is written "
      , "by "
      , H.a H.! A.href "http://twitter.com/doug_burke" $ "@doug_burke"
      , ", comes with no warranty (in other words, I make no "
      , "guarantee that the information presented is correct, although "
      , "I try my best to make sure it is), and is not "
      , "an official product of the "
      , H.a H.! A.href "http://chandra.si.edu/" $ "Chandra X-ray Center"
      , "."
    ]

jsScript :: H.AttributeValue -> H.Html
jsScript uri = H.script H.! A.src uri $ ""

