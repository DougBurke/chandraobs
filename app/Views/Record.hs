{-# LANGUAGE OverloadedStrings #-}

-- | The record page.

module Views.Record (recordPage, renderStuff) where

import qualified Prelude as P
import Prelude (($), (==), return)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Applicative ((<$>))

import Data.Maybe (fromMaybe, isJust)
import Data.Monoid ((<>), mconcat, mempty)
import Data.Time (UTCTime)

import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (title)

import PersistentTypes
import Types (ObsName(..), Instrument, Grating(..))
import Utils ( ObsInfo(..), ObsStatus(..)
             , abstractLink, defaultMeta
             , obsURI, renderLinks, showExp
             , showTimeDeltaFwd
             , showTimeDeltaBwd
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

      -- since using float: right need to do all but the first in right-to-left
      -- order
      navBar = nav ! class_ "main" $ ul $
                 (li ! class_ "chosen") (a ! href "/index.html" $ "What is Chandra doing?") <>
                 li (a ! href "/about/index.html" $ "About") <>
                 li (a ! href "/about/instruments.html" $ "Chandra Instruments") 

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
      abstractVal = toHtml targetStr <> case obsStatus of
                      Todo  -> " will be observed"
                      Doing -> " is being observed"
                      Done  -> " was observed"
      abstract = p ! class_ "obsdetails"
                     $ "Find out why " <> 
                        (a ! href (abstractLink obsId)
                           $ toHtml abstractVal)
                        <> "."

  in statusPara (P.True, sTime, eTime, rs) cTime obsStatus <> abstract

-- | Display information for a \"non-science\" observation.
otherInfo :: 
  UTCTime    -- current time
  -> Record  -- this is assumed to be for an ObsId, not SpecialObs
             -- ie it will crash if it is not sent one.
  -> Html
otherInfo cTime rs = 
  let (sTime, eTime) = getTimes rs
      obsStatus = getObsStatus (sTime, eTime) cTime 
  in statusPara (P.False, sTime, eTime, rs) cTime obsStatus

-- | Create the paragraph describing the observing status -
--   i.e. if it has been, will be, or is being, observed.
--
statusPara :: 
  (P.Bool, UTCTime, UTCTime, Record) 
  -- ^ True if science obs/False if not,
  --   start time, end time, record
  -> UTCTime    -- ^ current time
  -> ObsStatus     -- ^ status of observation
  -> Html
statusPara (science, sTime, eTime, rs) cTime obsStatus = 
  let cts Todo = 
        mconcat [ targetName
                , " will be observed ", instInfo
                , " for ", lenVal, ". It will start "
                , toHtml (showTimeDeltaFwd cTime eTime)
                , "."
                ]
      cts Doing = 
        mconcat [ targetName
                , " is being observed ", instInfo
                , " for ", lenVal
                , ". The observation started "
                , toHtml (showTimeDeltaBwd sTime cTime)
                , " and ends "
                , toHtml (showTimeDeltaFwd cTime eTime)
                , "."
                ]
                  
      cts Done = 
        mconcat [ targetName
                , " was observed ", instInfo
                , " for ", lenVal, ", and ended "
                , toHtml (showTimeDeltaBwd eTime cTime)
                , "."
                ]

      instInfo = fromMaybe mempty $ do
        inst <- recordInstrument rs
        grat <- recordGrating rs
        return $ "by " <> instLink inst <>
                   if grat == NONE
                   then mempty
                   else " and the " <> toHtml grat

      prefix = if science then "" else "The calibration observation "
      targetName = toHtml $ prefix <> recordTarget rs
                 
      -- TODO Add in a nice display of UTCTime

      lenVal = showExp rs

  in p ! class_ "targetInfo" $ cts obsStatus

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
