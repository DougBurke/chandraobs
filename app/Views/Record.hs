{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | The record page.

module Views.Record (CurrentPage(..)
                     , recordPage
                     , renderStuff
                     , renderTwitter
                     , mainNavBar
                     , obsNavBar
                     ) where

import qualified Prelude as P
import Prelude ((.), (-), ($), (==), (/=), (&&), (++), Eq, Bool(..), Either(..), Maybe(..), String, const, either, elem, filter, fst, length, map, maybe, null, otherwise, snd, splitAt, uncurry, zip)

import qualified Data.Text as T

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Blaze.ByteString.Builder (toByteString)

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))

import Data.Char (toLower)
import Data.Function (on)
import Data.List (groupBy, intersperse)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Monoid ((<>), mconcat, mempty)
import Data.Time (UTCTime)

import Network.HTTP.Types.URI (encodePathSegments)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (ScienceObs(..), NonScienceObs(..), 
              SimbadInfo(..),
              Proposal(..),
              SimbadType(..),
              Grating(..),
              ObsInfo(..), ObsStatus(..),
              ChandraTime(..), Constraint(..),
              ConLong(..), ConShort(..),
              getObsStatus, getJointObs, toSIMBADLink,
              getConstellationName)
import Types (Record, recordObsId, showExpTime)
import Utils ( 
             abstractLink, defaultMeta
             , obsURI, renderLinks
             , showTimeDeltaFwd
             , showTimeDeltaBwd
             , getTimes
             , renderFooter
             , instLinkSearch
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
  -> (Maybe SimbadInfo, (Maybe Proposal, [ScienceObs]))  -- other observations in the proposal
  -> Html
recordPage cTime mObs oi@(ObsInfo thisObs _ _) dbInfo =
  let initialize = "initialize()"
      obsId = recordObsId thisObs

      imgLinks = either (const mempty) (renderLinks False) thisObs

  in docTypeHtml ! lang "en-US" $
    head (H.title ("Chandra observation: " <> toHtml obsId) <>
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
         (renderStuff cTime thisObs dbInfo
          <> imgLinks)
      <> (div ! id "otherBar") renderTwitter)
      <> renderFooter

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
  -> (Maybe SimbadInfo, (Maybe Proposal, [ScienceObs]))  -- other observations in the proposal
  -> Html
renderStuff cTime rs dbInfo = 
  div ! id "observation" $
    case rs of
      Left ns -> otherInfo cTime ns
      Right so -> targetInfo cTime so dbInfo

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
obsNavBar mObs ObsInfo{..} = 
  let prevObs = oiPrevObs
      nextObs = oiNextObs

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
    let uri = if f then "/index.html" else toValue (obsURI (recordObsId rs))
    in li ! class_ "prevLink"
       $ a ! href uri
           $ "Previous observation"

navNext ::
  Bool   -- True if the previous link is the currently-executed observation
  -> Record 
  -> Html
navNext f rs = 
    let uri = if f then "/index.html" else toValue (obsURI (recordObsId rs))
    in li ! class_ "nextLink"
       $ a ! href uri
         $ "Next observation"

-- | Given a list of observations from a proposal, group them by target name.
--
groupProposal :: [ScienceObs] -> Html
groupProposal matches =
  let obs = P.map (soTarget &&& soObsId) matches
      grps = groupBy ((==) `on` fst) obs

      toLink o = a ! href (obsURI o) $ toHtml o

      tgtLinks [] = mempty -- should not happen
      tgtLinks xs@(x:_) = mconcat $ [toHtml (fst x), " ("] ++ intersperse ", " (P.map (toLink . snd) xs) ++ [")"]

  in mconcat $ intersperse "; " $ P.map tgtLinks grps

-- | Some types do not map well into the sentence structure,
--   so manually convert those that do not. At present it only
--   changes the "Region defined in the sky" type.
--
--   It also includes 'a ...' or 'an ...'.
cleanupSIMBADType :: String -> String
cleanupSIMBADType [] = []
cleanupSIMBADType "Region defined in the sky" = "an area of the sky" 
cleanupSIMBADType s@(c:_) | toLower c `elem` "aeiou" = "an " ++ s
                          | otherwise                = "a " ++ s
     
-- | Display information for a \"science\" observation.
--
targetInfo :: 
  UTCTime    -- current time
  -> ScienceObs
  -> (Maybe SimbadInfo, (Maybe Proposal, [ScienceObs]))  -- other observations in the proposal
  -> Html
targetInfo cTime so@ScienceObs{..} (msimbad, (mproposal, matches)) = 
  let (sTime, eTime) = getTimes (Right so)
      obsStatus = getObsStatus (sTime, eTime) cTime 
      targetName = toHtml soTarget
      lenVal = toHtml $ showExpTime $ fromMaybe soApprovedTime soObservedTime

      otherName = case msimbad of
        Just SimbadInfo{..} -> case siName of
          Just sName -> if siSimilar then mempty else (" - also called " <> toHtml sName <> " -")
          _ -> mempty
        _ -> mempty


      conLink = "/search/constellation/" <> toValue (fromConShort soConstellation)
      
      -- constellation info; it should always succeed but just in case we
      -- ignore missing cases
      constellationTxt = case getConstellationName soConstellation of
        Just con -> let conStr = fromConLong con
                    in "The target" <> otherName <> " is located in the constellation "
                       <> (a ! href conLink) (toHtml conStr)
                       <> if hasSimbad then " and " else mempty
        _ -> "The target "

      hasSimbad = case msimbad of
        Just SimbadInfo{..} -> isJust siName
        _ -> False

      -- TODO: check case and spaces
      simbadTxt SimbadInfo{..} = 
        case (siName, siType, siType3) of
          (Just sname, Just stype, Just stype3) ->
            let slink = H.toValue $ toSIMBADLink sname

                typeLink = H.unsafeByteStringValue $ toByteString $ encodePathSegments
                                 ["search", "type", T.pack (fromSimbadType stype3)]
                typeStr = toHtml $ cleanupSIMBADType stype

            in mconcat [
                  " is "
                  , a ! href typeLink $ typeStr
                  , ". More information on the target can be found at "
                  , a ! href slink $ "SIMBAD"
                  ]

          _ -> mempty

      abstxt = case obsStatus of
                 Todo -> "will be observed"
                 Doing -> "is being observed"
                 Done -> "was observed"

      endSentence [] = "." -- should not happen
      endSentence s = if P.last s == '.' then mempty else "."

      reason = case mproposal of
        Just Proposal{..} -> ", and is part of the proposal " <>
                             (a ! href ("/proposal/" <> toValue propNum) $ toHtml propName)
                             <> endSentence propName
        _ -> ". See why it " <>
             (a ! href (abstractLink soObsId) $ abstxt)
             <> "."

      instInfo = mconcat [
                  "by ", instLinkSearch soInstrument,
                   if soGrating == NONE
                   then mempty
                   else " and the " <> toHtml soGrating
                   ]

      -- For now ignore the "turnaround" time value, since it's
      -- not obvious how useful it is for the public. It might be
      -- nice to add some text saying that it is/was a "fast"/"quick" 
      -- request, but leave that for later.
      --
      -- TOOO: link to a description of what a TOO is.
      --
      tooTxt :: ObsStatus -> String -> Html
      tooTxt Done _ = 
        p "This was a TOO (target of opportunity) observation."
      tooTxt _ _ = 
        p "This is a TOO (target of opportunity) observation."

      tooPara = fromMaybe mempty $ tooTxt obsStatus <$> soTOO

      cts Todo = 
        mconcat [ "The target - "
                , targetName
                , " - will be observed ", instInfo
                , " for ", lenVal, ". "
                , "It will start "
                , toHtml (showTimeDeltaFwd cTime sTime)
                , reason
                ]
      cts Doing = 
        mconcat [ "The target - "
                , targetName
                , " - is being observed ", instInfo
                , " for ", lenVal, ". "
                , "The observation started "
                , toHtml (showTimeDeltaBwd sTime cTime)
                , " and ends "
                , toHtml (showTimeDeltaFwd cTime eTime)
                , reason
                ]
      cts Done = 
        mconcat [ "The target - "
                , targetName
                , " - was observed ", instInfo
                , " for ", lenVal, ", ended "
                , toHtml (showTimeDeltaBwd eTime cTime)
                , reason
                ]

      otherMatches = 
        if null matches
        then mempty
        else mconcat [" See related observations: ", groupProposal matches, "."]

      sciencePara = p $ cts obsStatus
                        <> constellationTxt
                        <> maybe mempty simbadTxt msimbad
                        <> "."
                        <> otherMatches

      addList [] = []
      addList [x] = [x]
      addList (x1:x2:[]) = [x1, " and ", x2]
      addList xs = let (ls, [r1, r2]) = splitAt (length xs - 2) xs
                   in intersperse ", " ls ++ [r1, ", and", r2]
 
      -- Too many options (can all three fields contain all three
      -- constraint values? to easily create a nice piece of prose, so
      -- for now just go with the ugly suffix "(preferred)" with the
      -- possibility of improving this at a later date.
      --
      cToL v Preferred = v <> " (preferred)"
      cToL v _         = v
      clbls = ["time critical", "monitoring", "constrained"]
      cvals = [soTimeCritical, soMonitor, soConstrained]
      copts = map (uncurry cToL) $ filter ((/= NoConstraint) . snd) $ zip clbls cvals
      constrainedObs = 
        if null copts
        then mempty
        else let vrb = if obsStatus == Done then "was" else "is"
             in mconcat
               [ "This ", vrb, " a "
               , mconcat $ addList copts
               , " observation." ]

      -- TODO: integrate with the rest of the text
      verb = case obsStatus of
               Todo  -> "will be"
               Doing -> "is"
               Done  -> "was"

      toJ (l, tks) = l <> " (for " <> toHtml (showExpTime tks) <> ")"

      jointObs = case soJointWith of
        Just _ -> mconcat
                   [ "This ", verb, " a joint observation with "
                   , mconcat $ addList $ map toJ $ getJointObs so
                   , ". However, if does not necessarily mean that the "
                   , "observations were done at the same time! "
                   ]
        Nothing -> mempty

      -- if there are constriants and a joint observation then the
      -- paragraph does not read well.
      constraintsPara = 
        let c = jointObs <> constrainedObs
        in if isNothing soJointWith && null copts  then mempty else p c

  in sciencePara 
     <> constraintsPara
     <> tooPara

-- | Display information for a \"non-science\" observation.
otherInfo :: 
  UTCTime    -- current time
  -> NonScienceObs
  -> Html
otherInfo cTime ns = 
  let (sTime, eTime) = getTimes (Left ns)
      obsStatus = getObsStatus (sTime, eTime) cTime 
  in nonSciencePara (sTime, eTime, cTime) ns obsStatus

-- | Create the paragraph describing the observing status -
--   i.e. if it has been, will be, or is being, observed.
--
nonSciencePara ::
  (ChandraTime, ChandraTime, UTCTime)
  -- ^ start time, end time, current time
  -> NonScienceObs
  -> ObsStatus     -- ^ status of observation
  -> Html
nonSciencePara (sTime, eTime, cTime) NonScienceObs{..} obsStatus = 
  let cts Todo = 
        mconcat [ "The calibration observation - "
                , targetName
                , " - will run for "
                , lenVal, ", and will start "
                , toHtml (showTimeDeltaFwd cTime sTime)
                , "."
                ]
      cts Doing = 
        mconcat [ "The calibration observation - "
                , targetName
                , " - is running for "
                , lenVal
                , ". The observation started "
                , toHtml (showTimeDeltaBwd sTime cTime)
                , " and ends "
                , toHtml (showTimeDeltaFwd cTime eTime)
                , "."
                ]
      cts Done = 
        mconcat [ "The calibration observation - "
                , targetName
                , " - was run for "
                , lenVal
                , " and finished "
                , toHtml (showTimeDeltaBwd eTime cTime)
                , "."
                ]

      targetName = toHtml nsTarget
      lenVal = toHtml $ showExpTime nsTime

  in p $ cts obsStatus

renderTwitter :: Html
renderTwitter = 
  (div ! class_ "tweetstream") (
    a ! class_ "twitter-timeline"
      ! href "https://twitter.com/chandraxray" 
      ! dataAttribute "widget-id" "469095554312450049" $ "Tweets by @chandraxray"   )
  <>
  script "!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document,'script','twitter-wjs');"

