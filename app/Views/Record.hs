{-# LANGUAGE OverloadedStrings #-}

-- | The record page.

module Views.Record (recordPage, renderStuff) where

import qualified Prelude as P
import Prelude (($), (==), return, show)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Applicative ((<$>))

import Data.Maybe (fromMaybe, isJust)
import Data.Monoid ((<>), mempty)
import Data.Time (UTCTime)

import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (title)

import PersistentTypes
import Types (ObsName(..), Instrument, Grating(..))
import Utils ( ObsInfo(..), ObsStatus(..)
             , abstractLink, defaultMeta
             , obsURI, renderLinks, showExp
             , getObsStatus, getTimes, demo)

-- The specific page for this observation. At present I have not
-- worked out how this interacts with the top-level page; i.e.
-- the current observation (i.e. should the current observation
-- be flagged as such when using this view?)
--
recordPage :: UTCTime -> ObsInfo -> Html
recordPage cTime oi@(ObsInfo currentObs _ _) =
  let initialize = "initialize()"

      obsName = recordObsname currentObs

  in docTypeHtml $
    head (H.title ("Chandra observation: " <> toHtml obsName) <>
            defaultMeta <>
            (script ! src "/js/main.js") "" <>
            link ! href   "/css/main.css"
                 ! type_  "text/css" 
                 ! rel    "stylesheet"
                 ! A.title  "Default"
                 ! media  "all"
            )
    <>
    (body ! onload initialize)
     (demo <> renderStuff cTime oi <> renderLinks P.False currentObs) 

-- | A redesign of the page.
--
--   Would like to use the flexbox model to layout items -
--   e.g.
--   http://css-tricks.com/flexbox-bar-navigation/
--   but it may not be present on all web browsers.
--
renderStuff :: 
  UTCTime     -- Current time
  -> ObsInfo 
  -> Html
renderStuff cTime oi = 
  let rs = oiCurrentObs oi
      prevObs = oiPrevObs oi
      nextObs = oiNextObs oi

      -- does blaze not have role?
      -- navBar = nav ! role "navigation" $ ul $
      obsBar = nav ! class_ "obslinks" $ ul $
                 fromMaybe mempty (navPrev <$> prevObs) <>
                 fromMaybe mempty (navNext <$> nextObs)

      navBar = nav ! class_ "main" $ ul $
                 li (a ! href "/index.html" $ "What is Chandra doing?") <>
                 li (a ! href "/about/instruments.html" $ "Chandra Instruments") <>
                 li (a ! href "/about/index.html" $ "About")

      obs = div ! class_ "observation" $
              obsBar <> if isJust (recordSequence rs)
                        then targetInfo cTime rs
                        else otherInfo cTime rs

  in navBar <> obs

-- | TODO: should it link to "/index.html" if the previous observation is current? 
navPrev :: Record -> Html
navPrev rs = 
    li ! class_ "prevLink"
     $ a ! href (toValue (obsURI rs))
         $ "Previous observation"

-- | TODO: should it link to "/index.html" if the next observation is current? 
navNext :: Record -> Html
navNext rs = 
    li ! class_ "nextLink"
     $ a ! href (toValue (obsURI rs))
       $ "Next observation"

-- | Add in a link to a "what is this" page for the
--   instrument.
instLink :: Instrument -> Html
instLink inst = 
  let iLink = "/about/instruments.html#" <> toValue inst
  in a ! href iLink $ toHtml inst

-- | Display information for a \"science\" observation.
targetInfo :: 
  UTCTime    -- current time
  -> Record  -- this is assumed to be for an ObsId, not SpecialObs
             -- ie it will crash if it is not sent one.
  -> Html
targetInfo cTime rs = 
  let targetStr = recordTarget rs

      -- assume this pattern match can not fail
      ObsId obsId = recordObsname rs

      (sTime, eTime) = getTimes rs
      obsStatus = getObsStatus (sTime, eTime) cTime 
      obsVal = case obsStatus of
                 Todo  -> "will be observed by"
                 Doing -> "is being observed by"
                 Done  -> "was observed by"

      endVal = toHtml $ case obsStatus of
                 Todo  -> ", and will end at " 
                 Doing -> ", and will end at "
                 Done  -> ", ending at "
               <> show eTime
                 
      -- TODO Add in a nice display of UTCTime

      lenVal = showExp rs

      tName = toHtml targetStr
      instInfo = fromMaybe mempty $ do
        inst <- recordInstrument rs
        grat <- recordGrating rs
        return $ " " <> obsVal <> " " <> instLink inst <>
                   if grat == NONE
                   then mempty
                   else " and the " <> toHtml grat

      targetP = p ! class_ "targetInfo"
                   $ tName <> instInfo <> " for " <> lenVal <>
                     endVal <> "."

      abstractVal = toHtml targetStr <> case obsStatus of
                      Todo  -> " will be observed"
                      Doing -> " is being observed"
                      Done  -> " was observed"
      abstract = p ! class_ "obsdetails"
                     $ "Find out why " <> 
                        (a ! href (abstractLink obsId)
                           $ toHtml abstractVal)
                        <> "."

  in targetP <> abstract

-- | Display information for a \"non-science\" observation.
otherInfo :: 
  UTCTime    -- current time
  -> Record  -- this is assumed to be for an ObsId, not SpecialObs
             -- ie it will crash if it is not sent one.
  -> Html
otherInfo cTime rs = 
  let targetStr = recordTarget rs

      (sTime, eTime) = getTimes rs
      obsStatus = getObsStatus (sTime, eTime) cTime 
      obsVal = case obsStatus of
                 Todo  -> "will be run"
                 Doing -> "is being run"
                 Done  -> "was run"

      endVal = toHtml $ case obsStatus of
                 Todo  -> ", and will end at " 
                 Doing -> ", and will end at "
                 Done  -> ", ending at "
               <> show eTime
                 
      -- TODO Add in a nice display of UTCTime

      lenVal = showExp rs

      tName = toHtml targetStr

      targetP = p ! class_ "targetInfo"
                   $ "The observation " <> tName <> " " <>
                     obsVal <> " for " <> lenVal <> endVal <> "."

  in targetP

-- TODO: now include some observation details - link to page and the ra/dec/... set up

{- 
renderSpecial2 :: Record -> Html
renderSpecial2 rs =
  navLeft oi <>
   
  p ("Target: " <> toHtml (recordTarget rs))
  <>
  p ("Name: " <> toHtml (recordObsname rs))
  <>
  p ("Date: " <> toHtml (show (recordStartTime rs)))
  <>
  p ("Length: " <> showExp rs)
  <>
  renderLocation rs

-}
