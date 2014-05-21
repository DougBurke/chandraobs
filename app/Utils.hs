{-# LANGUAGE OverloadedStrings #-}

-- | general routines

module Utils (
     ObsInfo(..)
     , ObsStatus(..)
     , defaultMeta
     , fromBlaze
     , navLinks
     , obsURI
     , standardResponse
     , showExpTime
     , showExp
     , showTimeDeltaFwd
     , showTimeDeltaBwd
     , detailsLink, abstractLink
     , getObsStatus, getTimes
     , renderLinks
     , demo
     ) where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mconcat, mempty)
import Data.Time (UTCTime, addUTCTime, diffUTCTime, formatTime)

import System.Locale (defaultTimeLocale)

import Text.Blaze.Html.Renderer.Text
import Text.Printf

import Web.Scotty

import PersistentTypes
import Types (ObsName(..), Grating(..))

-- | I just want a simple way of passing around 
--   useful information about an observation.
data ObsInfo = ObsInfo {
  oiCurrentObs :: Record
  , oiPrevObs  :: Maybe Record
  , oiNextObs  :: Maybe Record
  }

-- | Convert a record into the URI fragment that represents the
--   page for the record.`<
obsURI :: Record -> H.AttributeValue
obsURI rs = 
  case recordObsname rs of
    ObsId i -> "/obsid/" <> H.toValue i
    SpecialObs s -> "/obs/" <> H.toValue s

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
showExpTime :: Double -> String
showExpTime tks = 
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

-- | Come up with a string representing the time difference. It
--   is probably not general enough, since it adds in a
--   prefix (here, "in"), in most cases. So, this is to be
--   used for time differences in the future.
showTimeDeltaFwd ::
  UTCTime     -- time 1
  -> UTCTime  -- time 2, >= time 1
  -> String   -- time1 relative to time2
showTimeDeltaFwd t1 t2 = 
  let delta = diffUTCTime t2 t1
      m = delta / 60
      h = delta / 3600
      d = delta / (24 * 3600)
      nm = round m -- ceiling or round
      nh = round h
      nd = round d

      mins = "in " <> show nm <> " minute" <> plural nm
      hours = "in " <> show nh <> " hour" <> plural nh
      days = "in " <> show nd <> " day" <> plural nd

      other = formatTime defaultTimeLocale "%A, %B %Y" t2

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
  UTCTime     -- time 1
  -> UTCTime  -- time 2, >= time 1
  -> String   -- time1 relative to time2
showTimeDeltaBwd t1 t2 = 
  let delta = diffUTCTime t2 t1
      m = delta / 60
      h = delta / 3600
      d = delta / (24 * 3600)
      nm = round m -- ceiling or round
      nh = round h
      nd = round d

      mins = show nm <> " minute" <> plural nm <> " ago"
      hours = show nh <> " hour" <> plural nh <> " ago"
      days = show nd <> " day" <> plural nd <> " ago"

      other = formatTime defaultTimeLocale "%A, %B %Y" t2

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
obsIdLink, seqLink :: Int -> H.AttributeValue
obsIdLink obsId =
  H.toValue $ "http://cda.cfa.harvard.edu/chaser/startViewer.do?menuItem=details&obsid=" ++ show obsId

seqLink obsId =
  H.toValue $ "http://cda.cfa.harvard.edu/chaser/startViewer.do?menuItem=sequenceSummary&obsid=" ++ show obsId

detailsLink, abstractLink :: Int -> H.AttributeValue
detailsLink = obsIdLink
abstractLink obsId = 
  H.toValue $ "http://cda.cfa.harvard.edu/chaser/startViewer.do?menuItem=propAbstract&obsid=" ++ show obsId

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

showRA :: Double -> String
showRA ra = 
  let rah = ra / 15.0
      h, m :: Int
      r1, r2 :: Double
      (h, r1) = properFraction rah
      ram = r1 * 60
      (m, r2) = properFraction ram
      s = r2 * 60
  in printf "%dh %dm %.1fs" h m s

showDec :: Double -> String
showDec dec = 
  let dabs = abs dec
      d, m :: Int
      r1, r2 :: Double
      (d, r1) = properFraction dabs
      dm = r1 * 60
      (m, r2) = properFraction dm
      s = r2 * 60
      c = if dec < 0 then '-' else '+'
  in printf "%c%dd %d' %.1f\"" c d m s

-- | Create the forward/backward links for observation pages.
--
navLinks :: 
  Bool -- True if this is the current observation
  -> ObsInfo 
  -> H.Html
navLinks f (ObsInfo _ mPrevObs mNextObs) =
  let prevLink = case mPrevObs of
        Just prevObs -> H.a H.! A.href (H.toValue (obsURI prevObs))
                            H.! A.class_ "prev"
                            $ "Previous observation"
        _ -> mempty

      nextLink = case mNextObs of
        Just nextObs -> H.a H.! A.href (H.toValue (obsURI nextObs))
                            H.! A.class_ "next"
                            $ "Next observation"
        _ -> mempty

      thisLink = if f
                 then H.span H.! A.class_ "current"
                        $ "Current observation"
                 else H.a H.! A.href "/index.html"
                          H.! A.class_ "current"
                          $ "Current observation"

  in H.p H.! A.class_ "navlinks"
      $ mconcat [ prevLink, " ", thisLink, " ", nextLink ]

safeObsId :: ObsName -> Maybe Int
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
    Nothing -> ""
    Just (seqNum, obsId) ->
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


getTimes ::
  Record
  -> (UTCTime, UTCTime) -- start and end times
getTimes rs =
  let sTime = recordStartTime rs
      expTime = fromInteger . ceiling $ 1000 * recordTime rs
      eTime = addUTCTime expTime sTime
  in (sTime, eTime)

data ObsStatus = Done | Doing | Todo deriving Eq

getObsStatus :: 
  (UTCTime, UTCTime) -- observation start and end times
  -> UTCTime        -- current time
  -> ObsStatus
getObsStatus (sTime,eTime) cTime = 
  if cTime < sTime
  then Todo
  else if cTime <= eTime
       then Doing
       else Done

demo :: H.Html
demo = H.div H.! A.class_ "watermark" $ "Demo"

