{-# LANGUAGE OverloadedStrings #-}

-- | general routines

module Utils (
     ObsInfo(..)
     , defaultMeta
     , fromBlaze
     , navLinks
     , obsURI
     , renderRecord
     , standardResponse
     ) where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Maybe (isJust)
import Data.Monoid ((<>), mconcat, mempty)

import Text.Blaze.Html.Renderer.Text
import Text.Printf

import Web.Scotty

import PersistentTypes
import Types (ObsName(..))

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

-- The flag is true if this is the current observation
renderRecord :: Bool -> Record -> H.Html
renderRecord f rs = 
  let cts = if isJust (recordSequence rs)
            then renderObsId f rs
            else renderSpecial rs
  in (H.div H.! A.class_ "observation") cts

-- The flag is true if this is the current observation
renderObsId :: Bool -> Record -> H.Html
renderObsId f rs = 
  H.p ("Target: " <> H.toHtml (recordTarget rs))
  <>
  H.p ("ObsId: " <> H.toHtml (recordObsname rs))
  <>
  renderLocation rs
  <>
  renderLinks f rs

renderSpecial :: Record -> H.Html
renderSpecial rs = 
  H.p ("Target: " <> H.toHtml (recordTarget rs))
  <>
  H.p ("Name: " <> H.toHtml (recordObsname rs))
  <>
  renderLocation rs

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

-- Display the location fields
renderLocation :: Record -> H.Html
renderLocation rs = 
  let roll = recordRoll rs
      pitch = recordPitch rs
      slew = recordSlew rs
      ra = recordRa rs
      dec = recordDec rs
  in (H.div H.! A.class_ "location") 
      (H.span ("RA: " <> H.toHtml (showRA ra)) <>
       H.span ("Dec: " <> H.toHtml (showDec dec)) <>
       H.span ("Roll: " <> H.toHtml roll) <>
       H.span ("Pitch: " <> H.toHtml pitch) <>
       H.span ("Slew: " <> H.toHtml slew))


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
renderLinks :: 
  Bool -- True if current obs
  -> Record 
  -> H.Html
renderLinks f rs = 
  let mrec = do
        seqNum <- recordSequence rs
        obsId <- safeObsId $ recordObsname rs
        return (seqNum, obsId)

      imgSel :: String -> Bool -> H.Html
      imgSel lbl cf = 
        let idName = H.toValue (lbl++"button")
            base = H.input H.! A.type_ "radio"
                           H.! A.name  "imgtype"
                           H.! A.value (H.toValue lbl)
                           H.! A.id idName
                           H.! A.onclick
                               ("switchImage('" <> H.toValue lbl <> "')")
        in (if cf then base H.! A.checked "checked" else base)
           <> (H.label H.! A.for idName) (H.toHtml lbl)

      wwtLink = if f
                then (H.a H.! A.href "/wwt.html") "WWT"
                else (H.a H.! A.href (H.toValue (obsURI rs) <> "/wwt")) "WWT"

      form = H.div H.! A.class_ "radiobuttons" $
              mconcat [ "View: "
                      , imgSel "DSS" True
                      , imgSel "RASS" False
                      , imgSel "PSPC" False
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
                      H.! A.alt    (H.toValue lbl)
                      H.! A.width  (H.toValue (680::Int))
                      H.! A.height (H.toValue (680::Int))
                      H.! A.id     (H.toValue lbl)
                      H.! A.class_ (if af then "active" else "inactive")

       in form <>
         (H.div H.! A.class_ "links")
          (link "DSS" "dss" True <>
           link "PSPC" "pspc" False <>
           link "RASS" "rass" False) 

