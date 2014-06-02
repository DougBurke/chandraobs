{-# LANGUAGE OverloadedStrings #-}

-- | general routines

module Utils (
     defaultMeta
     , fromBlaze
     , obsURI
     , obsURIString
     , standardResponse
     , showExpTime
     , showExp
     , showRA
     , showDec
     , showTimeDeltaFwd
     , showTimeDeltaBwd
     , detailsLink, abstractLink
     , getTimes
     , renderLinks
     , getFact
     , linkToRecord
     , linkToRecordA
     , safeObsId
     ) where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mconcat, mempty)
import Data.Time (UTCTime, NominalDiffTime, addUTCTime, diffUTCTime, formatTime)

import System.Locale (defaultTimeLocale)
import System.Random (Random(..), getStdRandom)

import Text.Blaze.Html.Renderer.Text
import Text.Printf

import Web.Scotty

import Types (ObsName(..), ObsIdVal(..), Sequence(..), Grating(..), RA(..), Dec(..), ChandraTime(..), TimeKS(..))
import Types (Record, recordSequence, recordObsname, recordTarget, recordStartTime, recordTime, recordInstrument, recordGrating, recordRa, recordDec, recordRoll, recordPitch, recordSlew)

-- | Convert a record into the URI fragment that represents the
--   page for the record.`<
obsURI :: Record -> H.AttributeValue
obsURI = H.toValue . obsURIString

obsURIString :: Record -> String
obsURIString rs = 
  case recordObsname rs of
    ObsId (ObsIdVal i) -> "/obsid/" <> show i
    SpecialObs s -> "/obs/" <> s

fromBlaze :: H.Html -> ActionM ()
fromBlaze = html . renderHtml

-- A placeholder in case we want to set up any 
-- response settings.
standardResponse :: ActionM ()
standardResponse = return ()

defaultMeta :: H.Html
defaultMeta = H.meta H.! A.httpEquiv "Content-Type"
                     H.! A.content "text/html; charset=UTF-8"

-- | Convert a more "friendly" exposure time value.
--
--   As the minimum time appears to be 0.1 ks we do not
--   have to deal with sub minute values, but include
--   just in case. Assume that max is ~ 100ks, which is
--   ~ 28 hours, so need to deal with days.
--
--   The rounding may be a bit surprising, since
--   1 day + 1 minute will get reported as
--   "1 day 1 hour".
--
showExpTime :: TimeKS -> String
showExpTime (TimeKS tks) = 
  let s = tks * 1000
      m = s / 60
      h = m / 60

  in if s < 3600
     then showUnits s 60 "minute" "second"
     else if h < 24
          then showUnits m 60 "hour" "minute"
          else showUnits h 24 "day" "hour"

-- | Make a nice readable value; ie
--   "x unit1 y unit2"
--
showUnits :: 
  Double      -- value in units of unit2
  -> Int      -- scale value
  -> String   -- unit1: singular unit (scale * unit2)
  -> String   -- unit2: unit 
  -> String
showUnits v s u1 u2 = 
  let v1 = ceiling v :: Int -- round up
      (a, b) = v1 `divMod` s

      units 0 _ = ""
      units 1 u = "1 " ++ u
      units x u = show x ++ " " ++ u ++ "s"

      astr = units a u1
      bstr = units b u2

      sep = if null astr || null bstr then "" else " and "

  in astr ++ sep ++ bstr

plural :: Int -> String
plural i = if i > 1 then "s" else ""

getTimeElems ::
  UTCTime     -- time 1
  -> UTCTime  -- time 2, >= time 1
  -> (NominalDiffTime, Int, Int, Int, NominalDiffTime, NominalDiffTime, NominalDiffTime, String)
getTimeElems t1 t2 = 
  let delta = diffUTCTime t2 t1
      m = delta / 60
      h = delta / 3600
      d = delta / (24 * 3600)
      nm = round m -- ceiling or round
      nh = round h
      nd = round d

      other = formatTime defaultTimeLocale "%A, %B %e, %Y" t2

  in (delta, nd, nh, nm, d, h, m, other)

-- | Come up with a string representing the time difference. It
--   is probably not general enough, since it adds in a
--   prefix (here, "in"), in most cases. So, this is to be
--   used for time differences in the future.
showTimeDeltaFwd ::
  UTCTime     -- time 1
  -> ChandraTime  -- time 2, >= time 1
  -> String   -- time1 relative to time2
showTimeDeltaFwd t1 (ChandraTime t2) = 
  let (delta, nd, nh, nm, d, h, m, other) = getTimeElems t1 t2

      mins = "in " <> show nm <> " minute" <> plural nm
      hours = "in " <> show nh <> " hour" <> plural nh
      days = "in " <> show nd <> " day" <> plural nd

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
  let (delta, nd, nh, nm, d, h, m, other) = getTimeElems t1 t2

      mins = show nm <> " minute" <> plural nm <> " ago"
      hours = show nh <> " hour" <> plural nh <> " ago"
      days = show nd <> " day" <> plural nd <> " ago"

  in if delta < 60
     then "now"
     else if m < 60
            then mins
            else if h < 24
                 then hours
                 else if d < 7
                      then days
                      else "on " <> other


showExp :: Record -> H.Html
showExp = H.toHtml . showExpTime . recordTime

-- TODO:
-- Ideally we would link to something a bit more readable than the
-- archive page (ie its use of frames), and also present the link
-- in a more-friendly manner than as a link from the obsid or
-- sequence number.
--
obsIdLink :: ObsIdVal -> H.AttributeValue
obsIdLink obsId =
  H.toValue $ "http://cda.cfa.harvard.edu/chaser/startViewer.do?menuItem=details&obsid=" ++ show (fromObsId obsId)

seqLink :: ObsIdVal -> H.AttributeValue
seqLink obsId =
  H.toValue $ "http://cda.cfa.harvard.edu/chaser/startViewer.do?menuItem=sequenceSummary&obsid=" ++ show (fromObsId obsId)

detailsLink, abstractLink :: ObsIdVal -> H.AttributeValue
detailsLink = obsIdLink
abstractLink obsId = 
  H.toValue $ "http://cda.cfa.harvard.edu/chaser/startViewer.do?menuItem=propAbstract&obsid=" ++ show (fromObsId obsId)

-- | Display detailed information about a science observation,
--   for those that just need to know the details.
--
--   Could try and link to the roll/pitch/slew GIF - e.g.
--   http://cxc.harvard.edu/targets/601090/601090.rollvis.gif
--   but it is not obvious how valid it is (since the above
--   does not cover May 2014 when that target was scheduled),
--   and it is rather meaningless to anyone but an expert.
--
renderObsIdDetails :: 
  Record     -- it is assumed this is for an ObsId and not SpecialObs
  -> H.Html
renderObsIdDetails rs =
  let name = recordTarget rs
      instInfo = fromMaybe "" $ do
        inst <- recordInstrument rs
        grat <- recordGrating rs
        return $ H.toHtml inst <>
                 if grat == NONE
                 then mempty
                 else ", " <> H.toHtml grat

      left = (H.span H.! A.class_ "key")
      right = (H.span H.! A.class_ "value")

      keyVal k v = left k <> " " <> right v <> H.br

      {-
      showDetails =
         H.div H.! A.class_ "showdetails"
               H.! A.id "showdetails"
               H.! A.onclick "showDetails()"
           $ "Show details ..."
      -}

      -- These are not total; they require the record to
      -- be a science observation.
      Just seqNum = recordSequence rs
      ObsId obsId = recordObsname rs

      oLink = H.a H.! A.href (obsIdLink obsId) $ H.toHtml obsId
      sLink = H.a H.! A.href (seqLink obsId)   $ H.toHtml seqNum

  in -- showDetails <>
     (H.div H.! A.class_ "inactive" H.! A.id "Details") 
      (keyVal "Observation Details:" oLink
       <>
       keyVal "Sequence Summary:" sLink
       <>
       keyVal "Target:" (H.toHtml name)
       <>
       keyVal "Instrument:" instInfo
       <>
       keyVal "Date:" (H.toHtml (show (recordStartTime rs)))
       <>
       keyVal "Exposure:" (H.toHtml (show (recordTime rs) ++ " ks"))
       <>
       keyVal "Right Ascension:" (H.toHtml (showRA (recordRa rs)))
       <>
       keyVal "Declination:" (H.toHtml (showDec (recordDec rs)))
       <>
       keyVal "Roll:" (H.toHtml (recordRoll rs))
       <>
       keyVal "Pitch:" (H.toHtml (recordPitch rs))
       <>
       keyVal "Slew:" (H.toHtml (recordSlew rs))
       )

showRA :: RA -> String
showRA (RA ra) = 
  let rah = ra / 15.0
      h, m :: Int
      r1, r2 :: Double
      (h, r1) = properFraction rah
      ram = r1 * 60
      (m, r2) = properFraction ram
      s = r2 * 60
  in printf "%dh %dm %.1fs" h m s

showDec :: Dec -> String
showDec (Dec dec) = 
  let dabs = abs dec
      d, m :: Int
      r1, r2 :: Double
      (d, r1) = properFraction dabs
      dm = r1 * 60
      (m, r2) = properFraction dm
      s = r2 * 60
      c = if dec < 0 then '-' else '+'
  in printf "%c%dd %d' %.1f\"" c d m s

safeObsId :: ObsName -> Maybe ObsIdVal
safeObsId (ObsId i) = Just i
safeObsId _         = Nothing

-- Display the DSS/RASS/PSPC links. I assume that they
-- can be auto-generated from the sequence and obsid
-- values, that is:
--
-- http://asc.harvard.edu/targets/<sequence>/<sequence>.<obsid>.soe.dss.gif
-- http://asc.harvard.edu/targets/<sequence>/<sequence>.<obsid>.soe.rass.gif
-- http://asc.harvard.edu/targets/<sequence>/<sequence>.<obsid>.soe.pspc.gif
--
-- We now also display the observational details as a text box
-- as part of this section. This is an experiment.
--
renderLinks :: 
  Bool -- True if current obs
  -> Record 
  -> H.Html
renderLinks f rs = 
  let mrec = do
        seqNum <- recordSequence rs
        obsId <- safeObsId $ recordObsname rs
        return (seqNum, obsId)

      optSel :: String -> Bool -> H.Html
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
                else (H.a H.! A.href (H.toValue (obsURI rs) <> "/wwt")) "WWT"

      form = H.div H.! A.class_ "radiobuttons" $
              mconcat [ "View: "
                      , optSel "DSS" True
                      , optSel "RASS" False
                      , optSel "PSPC" False
                      , optSel "Details" False
                      , " or in "
                      , wwtLink
                      ]

  in case mrec of
    Just (Sequence seqNum, ObsIdVal obsId) ->
       let urlHead = mconcat [ "http://asc.harvard.edu/targets/"
                             , show seqNum, "/", show seqNum, "."
                             , show obsId, ".soe."
                             ]

           link :: String -> String -> Bool -> H.Html
           link lbl frag af = 
             let uri = urlHead <> frag <> ".gif"
             in H.img H.! A.src    (H.toValue uri)
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
           renderObsIdDetails rs) 

    Nothing -> mempty

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
  let uri = obsURI r
  in H.a H.! A.href uri $ H.toHtml $ f r

