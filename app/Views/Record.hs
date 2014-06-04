{-# LANGUAGE OverloadedStrings #-}

-- | The record page.

module Views.Record (CurrentPage(..)
                     , recordPage
                     , renderStuff
                     , renderTwitter
                     , mainNavBar
                     , obsNavBar
                     ) where

import qualified Prelude as P
import Prelude ((.), ($), (==), (&&), (++), Eq, Bool(..), Maybe(..), fst, null, return, show, snd)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Applicative ((<$>))

import Data.Function (on)
import Data.List (groupBy, intersperse)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Monoid ((<>), mconcat, mempty)
import Data.Time (UTCTime)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (ObsName(..), ObsIdVal(..),
              Instrument, Grating(..),
              ObsInfo(..), ObsStatus(..),
              ChandraTime(..),
              getObsStatus)
import Types (Record, recordSequence, recordObsname, recordTarget, recordInstrument, recordGrating, showExp)
import Utils ( 
             abstractLink, defaultMeta
             , obsURI, renderLinks
             , showTimeDeltaFwd
             , showTimeDeltaBwd
             , getTimes
             , safeObsId
             )

-- The specific page for this observation. At present I have not
-- worked out how this interacts with the top-level page; i.e.
-- the current observation (i.e. should the current observation
-- be flagged as such when using this view?)
--
recordPage :: 
  UTCTime  -- the current time
  -> Maybe Record -- the currently running observation
  -> ObsInfo  -- the observation being displayed
  -> [Record] -- related observations (same sequence number)
  -> Html
recordPage cTime mObs oi@(ObsInfo thisObs _ _) matches =
  let initialize = "initialize()"

      obsName = recordObsname thisObs

  in docTypeHtml ! lang "en-US" $
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
     (mainNavBar CPOther
      <> obsNavBar mObs oi
      <> (div ! id "mainBar") 
         (renderStuff cTime thisObs matches
          <> renderLinks False thisObs)
      <> (div ! id "otherBar") renderTwitter)

-- | A redesign of the page.
--
--   Would like to use the flexbox model to layout items -
--   e.g.
--   http://css-tricks.com/flexbox-bar-navigation/
--   but it may not be present on all web browsers.
--
renderStuff :: 
  UTCTime           -- Current time
  -> Record
  -> [Record]       -- observations with the same sequence number
  -> Html
renderStuff cTime rs matches = 
  div ! id "observation" $
    if isJust (recordSequence rs)
    then targetInfo cTime rs matches
    else otherInfo cTime rs

-- | What is the page being viewed?
--
--   The CPOther category is for WWT/specific obsid/special obs
--   pages, since separating out these is currently un-needed.
--
data CurrentPage = 
  CPIndex | CPSchedule | CPAbout | CPInstruments | CPView | CPOther
  deriving Eq

-- | Display the main navigation bar.
mainNavBar :: CurrentPage -> Html
mainNavBar cp = 
  let mkA ix u t pg = 
        let aa = if pg == cp then a ! class_ "chosen" else a
        in aa ! id ix ! href u $ t

      indexA = mkA "home"  "/index.html"             "What is Chandra doing now?"
      schedA = mkA "sched" "/schedule/index.html"    "Schedule"
      aboutA = mkA "about" "/about/index.html"       "About"
      instA  = mkA "insts" "/about/instruments.html" "Instruments"
      viewA  = mkA "views" "/about/views.html"       "Views"

      -- since using float: right need to do all but the first in
      -- right-to-left order

  in nav ! customAttribute "role" "navigation" $ ul $
       li (indexA CPIndex)
       <> li (schedA CPSchedule)
       <> li (aboutA CPAbout)
       <> li (instA CPInstruments)
       <> li (viewA CPView)

-- | Display the observation navigation bar
obsNavBar :: 
  Maybe Record   -- the current observation
  -> ObsInfo 
  -> Html
obsNavBar mObs oi = 
  let prevObs = oiPrevObs oi
      nextObs = oiNextObs oi

      pFlag = isJust prevObs && prevObs == mObs
      nFlag = isJust nextObs && nextObs == mObs

  in nav ! id "obslinks" $ ul $
        fromMaybe mempty (navPrev pFlag <$> prevObs) <>
        fromMaybe mempty (navNext nFlag <$> nextObs)

navPrev :: 
  Bool   -- True if the previous link is the currently-executed observation
  -> Record 
  -> Html
navPrev f rs =
    let uri = if f then "/index.html" else toValue (obsURI rs)
    in li ! class_ "prevLink"
       $ a ! href uri
           $ "Previous observation"

navNext ::
  Bool   -- True if the previous link is the currently-executed observation
  -> Record 
  -> Html
navNext f rs = 
    let uri = if f then "/index.html" else toValue (obsURI rs)
    in li ! class_ "nextLink"
       $ a ! href uri
         $ "Next observation"

-- | Add in a link to a "what is this" page for the
--   instrument.
instLink :: Instrument -> Html
instLink inst = 
  let iLink = "/about/instruments.html#" <> toValue inst
  in a ! href iLink $ toHtml inst

-- | Given a list of observations from a proposal, group them by target name.
--
groupProposal ::
  [Record]  -- these are expected to be science observations
  -> Html
groupProposal matches =
  let obs = mapMaybe getObs matches
      getObs r = do
        let n = recordTarget r
        o <- safeObsId (recordObsname r)
        return (n, o)
        
      grps = groupBy ((==) `on` fst) obs

      -- special case knowledge of URI mapping, should be abstracted out
      toURI (ObsIdVal o) = toValue $ "/obsid/" ++ show o
      toLink o = a ! href (toURI o) $ toHtml o

      tgtLinks [] = mempty -- should not happen
      tgtLinks xs@(x:_) = mconcat $ [toHtml (fst x), " ("] ++ intersperse ", " (P.map (toLink . snd) xs) ++ [")"]

  in mconcat $ intersperse "; " $ P.map tgtLinks grps
     
-- | Display information for a \"science\" observation.
targetInfo :: 
  UTCTime    -- current time
  -> Record  -- this is assumed to be for an ObsId, not SpecialObs
             -- ie it will crash if it is not sent one.
  -> [Record] -- observations with the same sequence number
  -> Html
targetInfo cTime rs matches = 
  let targetStr = recordTarget rs

      -- assume this pattern match can not fail
      ObsId obsId = recordObsname rs

      (sTime, eTime) = getTimes rs
      obsStatus = getObsStatus (sTime, eTime) cTime 
      abstractVal = toHtml targetStr <> case obsStatus of
                      Todo  -> " will be observed"
                      Doing -> " is being observed"
                      Done  -> " was observed"
      abstract = p $ "Find out why "
                      <> (a ! href (abstractLink obsId)
                           $ toHtml abstractVal)
                      <> ". Use SIMBAD to find out about "
                      <> (a ! href simbadLink $ toHtml targetStr)
                      <> " (this is not guaranteed to find the "
                      <> "correct source since it relies on an "
                      <> "identifiable string being used as the "
                      <> "observation target name, which isn't always "
                      <> "the case)."
                      <> otherMatches

      otherMatches = 
        if null matches
        then mempty
        else mconcat [" See related observations: ", groupProposal matches, "."]

      -- Does blaze quote/protect URLs? It appears not,
      -- or perhaps I just didn't look correctly.
      -- TODO: I do need to protect + characters since
      --   PSR J2307+2225 ends up having the + disappear
      simbadLink = 
        toValue $
          "http://simbad.harvard.edu/simbad/sim-id?Ident=" <> 
          targetStr <> 
          "&NbIdent=1&Radius=2&Radius.unit=arcmin&submit=submit+id"

  in statusPara (True, sTime, eTime, rs) cTime obsStatus 
     <> abstract
     -- <> renderObsIdDetails rs

-- | Display information for a \"non-science\" observation.
otherInfo :: 
  UTCTime    -- current time
  -> Record  -- this is assumed to be for an ObsId, not SpecialObs
             -- ie it will crash if it is not sent one.
  -> Html
otherInfo cTime rs = 
  let (sTime, eTime) = getTimes rs
      obsStatus = getObsStatus (sTime, eTime) cTime 
  in statusPara (False, sTime, eTime, rs) cTime obsStatus

-- | Create the paragraph describing the observing status -
--   i.e. if it has been, will be, or is being, observed.
--
statusPara :: 
  (Bool, ChandraTime, ChandraTime, Record) 
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
                , toHtml (showTimeDeltaFwd cTime sTime)
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

  in p $ cts obsStatus

renderTwitter :: Html
renderTwitter = 
  (div ! class_ "tweetstream") (
    a ! class_ "twitter-timeline"
      ! href "https://twitter.com/chandraxray" 
      ! dataAttribute "widget-id" "469095554312450049" $ "Tweets by @chandraxray"   )
  <>
  script "!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document,'script','twitter-wjs');"

