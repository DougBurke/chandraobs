{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | The record page.

module Views.Record (CurrentPage(..)
                     , recordPage
                     , renderStuff
                     , obsNavBar
                     , singleColBody
                     , withTwitterBody

                     , renderRelatedObs

                     , noObsIdParas
                     ) where

import qualified Prelude as P
import Prelude ((.), (-), ($), (>), (==), (&&), (>>=)
               , Eq, Either(..), Maybe(..), String
               , const, either, elem, fst, length, map
               , maybe, null, otherwise, snd, splitAt)

import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))

import Data.Char (toLower)
import Data.Function (on)
import Data.List (groupBy, intersperse, sortOn)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Monoid ((<>), mconcat, mempty)
import Data.Time (UTCTime)

import Text.Blaze.Html5 hiding (map, title, u)
import Text.Blaze.Html5.Attributes hiding (title)

import API (abstractLink, instLinkSearch, gratLinkSearch
           , typeLinkSearch
           , nameLinkSearch
           , constellationLinkSearch
           , proposalLink
           , propTypeLink
           , fromMissionLongLink
           , obsURI
           , skyLink
           , jsScript, cssLink)
import Layout (defaultMeta
              -- , jqueryMeta
              , renderLinks
              , renderFooter)
import Sorted (StartTimeOrder
              , SortedList
              , fromSL, lengthSL)
       
import Types (Record, ScienceObs(..), NonScienceObs(..)
             , SimbadInfo(..)
             , Proposal(..)
             , Grating(..)
             , ObsInfo(..), ObsStatus(..)
             , ChandraTime, Constraint(..)
             , ConLong(..)
             , ObsIdStatus(Discarded)
             , PropType(TOO)
             , SimbadLoc(SimbadCfA)
             , TargetName, SIMCategory
             , zeroKS
             , getObsStatus
             , toSIMBADLink
             , getJointObs
             , getConstellationName
             , similarName
             , toMission
             , recordObsId, showExpTime
             )
import Utils (HtmlContext(..)
             , toLink
             , extLink
             , showTimeDeltaFwd
             , showTimeDeltaBwd
             , getTimes
             )

wwtLoc :: AttributeValue
wwtLoc = "https://web.wwtassets.org/engine/7/wwtsdk.js"

-- The specific page for this observation. At present I have not
-- worked out how this interacts with the top-level page; i.e.
-- the current observation (i.e. should the current observation
-- be flagged as such when using this view?)
--
recordPage :: 
  UTCTime  -- the current time
  -> Maybe Record -- the currently running observation
  -> ObsInfo  -- the observation being displayed
  -> (Maybe SimbadInfo, (Maybe Proposal, SortedList StartTimeOrder ScienceObs))  -- other observations in the proposal
  -> Html
recordPage cTime mObs oi@(ObsInfo thisObs _ _) dbInfo =
  let initialize = "main.initialize()"
      obsId = recordObsId thisObs

      (msimbad, (mprop, _)) = dbInfo
      imgLinks = either (const mempty)
                 (renderLinks cTime mprop msimbad) thisObs

      -- only need WWT JS for science observations
      wwtJS = either (const mempty)
              (const (jsScript wwtLoc <>
                      jsScript "/js/wwt.js"))
              thisObs
              
  in docTypeHtml ! lang "en-US" $
    head (H.title ("Chandra observation: " <> toHtml obsId) <>
            defaultMeta <>
            -- jqueryMeta <>  -- used by WWT; OR IS IT???
            jsScript "/js/base.js" <>
            jsScript "/js/image-switch.js" <>
            jsScript "/js/main.js" <>
            wwtJS <>
            (cssLink "/css/main.css" ! A.title  "Default")
            )
    <>
    (body ! onload initialize)
     (withTwitterBody CPOther
       ((div ! id "mainBar") 
         (obsNavBar StaticHtml mObs oi
         <> renderStuff StaticHtml cTime thisObs dbInfo
         <> imgLinks)))

-- | A redesign of the page.
--
--   Would like to use the flexbox model to layout items -
--   e.g.
--   http://css-tricks.com/flexbox-bar-navigation/
--   but it may not be present on all web browsers.
--
renderStuff ::
  HtmlContext
  -> UTCTime
  -- ^ Current time
  -> Record
  -> (Maybe SimbadInfo, (Maybe Proposal, SortedList StartTimeOrder ScienceObs))
  -- ^ other observations in the proposal
  -> Html
renderStuff ctx cTime rs dbInfo = 
  (div ! id "observation")
    (case rs of
      Left ns -> otherInfo cTime ns
      Right so -> targetInfo ctx cTime so dbInfo)

{-

I want something that I can match to a UI call - e.g.
something like

   /api/html/getObservationInfo/{:obsid}

the question is, is this the API we want/need (sending
in the obsid), or perhaps we already have some/all of
the information we need so we can avoid some DB calls.

-- | Return the HTML for the observation div, i.e. the information
--   for an obsid (either science or non-science).
--
getObservationHTML ::
  Maybe ObsId
  -- ^ The ObsId to show (if Nothing then use the current observation)
  -> (ObsId -> ActionM (Maybe Obsinfo))
  -- ^ Action to query the database about the observation
  -> ActionM (Maybe Record)
  -- ^ The current observation
  -> (ObsInfo -> ActionM
      (Maybe SimbadInfo,
       (Maybe Proposal, SortedList StartTimeOrder ScienceObs)))
  -- ^ Return related information.
  -> ActionM (Either Html Html)
  -- ^ A div, with id of "observation" and contents for the observation.
  --   The left response is if there is an error recovering the info
  --   (e.g. the observation is unknown or there was a database error).
getObservationHTML mobs getObsInfo getCurrentObs getRelatedInfo = do
  let odiv = (div ! class_ "observation")

actually, the logic of the following is wrong, since it has mobs wrong;
if it's nothing then it doesn't mean that we should error out.

                     not sure how this is going to be used, is the problem.
  
  case mobs of
    Just obs -> do
      cTime <- liftIO getCurrentTime
      mCurrent <- getCurrentObs
      dbInfo <- getRelatedInfo obs
      return (Right (renderStuff cTime rs dbInfo))

    Nothing -> do
      fact <-liftIO getFact
      return (Left ((div ! id "observation") cts))


-}


noObsIdParas :: Html -> Html
noObsIdParas fact =
  (p ! class_ "error")
  ("The observation is unknown, but I can tell you " <>
   "this fun Chandra fact:")
  <> (p ! class_ "fact") fact
  

-- | What is the page being viewed?
--
--   The CPOther category is for WWT/specific obsid/special obs
--   pages, since separating out these is currently un-needed.
--
data CurrentPage = 
  CPIndex | CPSchedule | CPExplore | CPAbout | CPInstruments | CPView | CPOther
  deriving Eq

-- | Create a "single-column" page body layout.
--
singleColBody ::
  CurrentPage
  -> Html  -- ^ The body contents (will be wrapped in a main tag)
  -> Html
singleColBody cp bdy =
  mainNavBar cp
  <> main bdy
  <> renderFooter

-- | Show Twitter to the right of the "main" body.
--
withTwitterBody ::
  CurrentPage
  -> Html  -- ^ The body contents (will be wrapped in a main tag)
  -> Html
withTwitterBody cp bdy =
  mainNavBar cp
  <> (main bdy <> twitterDiv)
  <> renderFooter


-- | Display the main navigation bar.
mainNavBar :: CurrentPage -> Html
mainNavBar cp = 
  let mkA ix u t pg = 
        let aa = if pg == cp then a ! class_ "chosen" else a
        in aa ! id ix ! href u $ t

      indexA = mkA "home"  "/index.html"             "What is Chandra doing now?"
      schedA = mkA "sched" "/schedule/index.html"    "Schedule"
      explA  = mkA "expl"  "/search/index.html"      "Explore"
      aboutA = mkA "about" "/about/index.html"       "About"
      instA  = mkA "insts" "/about/instruments.html" "Instruments"
      viewA  = mkA "views" "/about/views.html"       "Views"

      -- The first a objects are float left, the remaining are float right,
      -- and so need to be in reverse order. Check static/css/main.css
      -- 'nav ul li:nth-child(-n+a)' to find out what a is.

      lis = li (indexA CPIndex)
            <> li (schedA CPSchedule)
            <> li (explA CPExplore)
            -- remaining elements are in reverse display order,
            -- looking left-to-right, as they are floated.
            <> li (aboutA CPAbout)
            <> li (instA CPInstruments)
            <> li (viewA CPView)

      awayPara = mempty

      {-
       - This could be picked up from the database (i.e. have a table
       - with message and time-range to display, but leave that thought
       - for now
      awayPara = (p ! A.style "clear: both; text-align: center;")
                 ("I am on vacation so the site is not being updated. "
                  <> "Please come back on August 6th.")

      awayPara = (p ! A.style "clear: both; text-align: center;")
                 ("Chandra is currently being "
                  <> (a ! href "https://www.nasa.gov/feature/chandra-operations-resume-after-cause-of-safe-mode-identified")
                  "checked out after going into a safe mode"
                  <> " recently. Updates to the schedule are expected soon!.")

      centeredP = (p ! A.style "clear: both; text-align: center;")
      awayPara = centeredP
                 ("We are currently moving hardware around, which means that "
                  <> "updates to the scheduling and processing information "
                  <> "will be delayed for the first week in December.")
      -}

  in header ((nav ! customAttribute "role" "navigation")
             (ul lis <> awayPara))

-- | Display the observation navigation bar.
--
--   If there are no next or preceeding observation - which should
--   mean that the observation is discarded or unscheduled -
--   then add nothing.
--
obsNavBar ::
  HtmlContext
  -> Maybe Record   -- the current observation
  -> ObsInfo 
  -> Html
obsNavBar ctx mObs ObsInfo{..} = 
  let prevObs = oiPrevObs
      nextObs = oiNextObs

      mkNavLink ty rs =
        let lbl1 = (H.span ! class_ "directionLabel")
                   (toHtml (ty <> " observation:"))
            lbl2 =
              let obsName = case rs of
                    Left NonScienceObs{..} -> "Calibration ("
                                              <> toHtml nsObsId <> ")"
                    Right ScienceObs{..} -> toHtml soTarget
              in (H.span ! class_ "obsLabel") obsName

            alink = case ctx of
              StaticHtml -> a ! href (getStaticUri rs)
              DynamicHtml -> a ! href (getDynamicUri rs)

        in alink (lbl1 <> " " <> lbl2)

      getStaticUri o = if Just o == mObs
                       then "/index.html"
                       else toValue (obsURI (recordObsId o))

      getDynamicUri _ = "#"

      entry ::
        T.Text
        -- ^ Previous or Next (not worth creating a type for just yet) 
        -> AttributeValue  -- class
        -> Record -- observation being pointed to
        -> Html
      entry ty cls rs =
        let ll = li ! class_ cls
                    ! dataAttribute "obsid" obsid
            obsid = toValue (recordObsId rs)
        in ll (mkNavLink ty rs)

      navPrev = entry "Previous" "prevLink"
      navNext = entry "Next"     "nextLink"

      barContents =
        maybe mempty navPrev prevObs <>
        maybe mempty navNext nextObs
        
      bar = (nav ! id "obslinks") (ul barContents)

  in if isJust prevObs P.|| isJust nextObs then bar else mempty

-- | Given a list of observations from a proposal, group them by target name.
--
--   There is a special case if they all have the same name as the supplied
--   target name.
groupProposal ::
  HtmlContext
  -> TargetName
  -> SortedList StartTimeOrder ScienceObs
  -> Html
groupProposal ctx tName matches =
  let obs = P.map (soTarget &&& soObsId) (fromSL matches)
      sobs = sortOn fst obs
      grps = groupBy ((==) `on` fst) sobs

      mklink o =
        let uri = case ctx of
              StaticHtml -> obsURI o
              DynamicHtml -> "#"
            lbl = toHtml o
        in (a ! href uri
              ! class_ "obsidlink"
              ! dataAttribute "obsid" (toValue o))
           lbl

      addCommas xs = mconcat (intersperse ", " (P.map (mklink . snd) xs))
                     
      tgtLinks [] = mempty -- should not happen
      tgtLinks xs@(x:_) = toHtml (fst x) <> " (" <> addCommas xs <> ")"
      
      out = mconcat (intersperse "; " (P.map tgtLinks grps))

  in case grps of
    [xs@(x:_)] | fst x == tName -> addCommas xs
               | otherwise      -> out
    _ -> out

-- | Some types do not map well into the sentence structure,
--   so manually convert those that do not. At present it only
--   changes the "Region defined in the sky" type.
--
--   It also includes 'a ...' or 'an ...'.
cleanupSIMBADType :: SIMCategory -> SIMCategory
cleanupSIMBADType s | s == "Region defined in the sky" = "an area of the sky"
                    | otherwise =
                      case T.uncons s of
                        Nothing -> s
                        Just (c, _) | toLower c `elem` vowels -> "an " <> s
                        _ -> "a " <> s

-- Giving an explicit type is needed in GHC 7.10     
vowels :: String
vowels = "aeiou"

-- | Display information for a \"science\" observation.
--
-- TODO: send HtmlContext to details display
--
-- Note: the information content now depends on the HtmlContext; in particular
--       the "related observatios" are not included with a Dynamic context.
--
targetInfo ::
  HtmlContext
  -> UTCTime    -- current time
  -> ScienceObs
  -> (Maybe SimbadInfo, (Maybe Proposal, SortedList StartTimeOrder ScienceObs))
  -- other observations in the proposal
  -> Html
targetInfo ctx cTime so@ScienceObs{..} (msimbad, (mproposal, matches)) = 
  let mTimes = getTimes (Right so)
      obsStatus = getObsStatus mTimes cTime 
      targetName = nameLinkSearch ctx soTarget Nothing
      lenVal = toHtml (showExpTime (fromMaybe soApprovedTime soObservedTime))

      -- The search using the alternative name could well return different
      -- items (until the search is normalised so that it handles this),
      -- although from my initial tests I don't see any (could do a search to
      -- find a case), so leave off for now
      otherName = case msimbad of
        Just sm -> if similarName sm soTarget
                   then mempty 
                   else " - also called "
                        <> toHtml (smiName sm)
                        -- <> nameLinkSearch (smiName sm) Nothing
                        <> " -"
        _ -> mempty


      -- constellation info; it should always succeed but just in case we
      -- ignore missing cases
      constellationTxt = case getConstellationName soConstellation of
        Just con -> let conStr = fromConLong con
                    in "The target" <> otherName
                       <> " is located in the constellation "
                       <> constellationLinkSearch ctx soConstellation conStr
                       <> if hasSimbad then " and " else mempty
        _ -> "The target "

      -- Note when a subarray is in use and it's NOT a grating observation
      subArrayTxt = case (soGrating, soSubArrayStart, soSubArraySize) of
        (NONE, Just _, Just nrows) -> 
          let frac = 1024 `P.div` nrows
              term 2 = "only half of the chip "
              term 4 = "only one-quarter of the chip "
              term 8 = "only one-eigth of the chip "
              term _ = "a custom sub array "
          in if frac == 1
             then mempty -- this should not happen
             else "The source is so bright in X-rays that "
                  <> term frac
                  <> verb <> " used for the observation. "
        _ -> mempty

      hasSimbad = isJust msimbad

      -- TODO: check case and spaces
      simbadTxt SimbadInfo{..} = 
        let slink = H.toValue (toSIMBADLink sloc smiName)
            sloc = SimbadCfA  -- TODO: allow configurable, either by the app, or
                              --       by the user

        in " is "
           <> typeLinkSearch ctx smiType3 (cleanupSIMBADType smiType)
           <> ". "
           <> subArrayTxt
           <> "More information on the target can be found at "
           <> extLink ctx slink ("SIMBAD" :: Html)
           <> ". "

      abstxt = case obsStatus of
                 Unscheduled -> "is planned to be observed"
                 Todo -> "will be observed"
                 Doing -> "is being observed"
                 Done -> "was observed"

      endSentence s | T.null s = "." -- should not happen
                    | otherwise = let lchar = T.last s
                                  in if lchar `elem` endChars then mempty else "."

      -- Need to specify a type in GHC 7.10
      endChars :: String
      endChars = ".?"

      reason = case mproposal of
        Just prop ->
          let proplink = proposalLink ctx prop Nothing
                <> endSentence (propName prop)
          in (if obsStatus == Unscheduled
              then "It is part of the proposal "
              else ", and is part of the proposal ") <> proplink
                             
        _ -> ". See why it " <>
             (a ! href (abstractLink soObsId) $ abstxt)
             <> "."

      instInfo = mconcat [
                  "by ", instLinkSearch ctx soInstrument,
                   if soGrating == NONE
                   then mempty
                   else " and the "
                        <> gratLinkSearch ctx soGrating
                   ]

      -- For now ignore the "turnaround" time value, since it's
      -- not obvious how useful it is for the public. It might be
      -- nice to add some text saying that it is/was a "fast"/"quick" 
      -- request, but leave that for later.
      --
      tooTxt :: ObsStatus -> a -> Html
      tooTxt Done _ = 
        p ("This was an observation from a " <> tooLink <> " proposal.")
      tooTxt _ _ = 
        p ("This is an observation from a " <> tooLink <> " proposal.")

      tooLink = propTypeLink ctx TOO (Just "TOO (target of opportunity)")

      tooPara = maybe mempty (tooTxt obsStatus) soTOO

      sTime = fst <$> mTimes
      eTime = snd <$> mTimes
      
      cts Unscheduled =
        "The target - " <> targetName
        <> " - will be observed " <> instInfo
        <> " for " <> lenVal <> ", but there is currently "
        <> "no scheduled date for the observation "
        <> "(it is likely that it was scheduled but for some "
        <> "reason it was not observed and so has been "
        <> "removed from the schedule). "
        <> reason
      cts Todo =
        "The target - " <> targetName
        <> " - will be observed " <> instInfo
        <>  " for " <> lenVal <> ". It will start "
        <> toHtml (showTimeDeltaFwd cTime sTime)
        <> reason
      cts Doing =
        "The target - " <> targetName
        <> " - is being observed " <> instInfo
        <> " for " <> lenVal <> ". The observation started "
        <> toHtml (showTimeDeltaBwd sTime cTime)
        <> " and ends "
        <> toHtml (showTimeDeltaFwd cTime eTime)
        <> reason
      cts Done = 
        "The target - " <> targetName
        <> " - was observed " <> instInfo
        <> " for " <> lenVal <> ", ended "
        <> toHtml (showTimeDeltaBwd eTime cTime)
        <> reason

      otherMatches = case ctx of
        DynamicHtml -> mempty
        StaticHtml -> relatedObservationsText ctx soTarget matches

      sciencePara = p (cts obsStatus
                       <> " "
                       <> constellationTxt
                       <> maybe (". " <> subArrayTxt) simbadTxt msimbad
                       <> otherMatches)

      addList [] = ""
      addList [x] = x
      addList [x1, x2] = x1 <> " and " <> x2
      addList xs = let (ls, [lelem]) = splitAt (length xs - 1) xs
                   in mconcat (intersperse ", " ls)
                      <> ", and " <> lelem  -- Oxford comma FTW
 
      -- Too many options (can all three fields contain all three
      -- constraint values? to easily create a nice piece of prose, so
      -- for now just go with the ugly suffix "(preferred)" with the
      -- possibility of improving this at a later date.
      --
      conLink (NoConstraint, _ , _) = Nothing
      conLink (c, lbl, uriFrag) =
        let fullLbl = lbl <> suffix
            suffix = if c == Preferred then " (preferred)" else ("" :: Html)
            ans = case ctx of
                   StaticHtml -> toLink StaticHtml ("/search/" <> uriFrag) fullLbl
                   DynamicHtml -> skyLink uriFrag fullLbl
        in Just ans

      czs = [ (soTimeCritical, "time-critical", "constraints/timecritical")
            , (soMonitor, "monitoring", "constraints/monitor")
            , (soConstrained, "constrained", "constraints/constrained")
            ]
      copts = mapMaybe conLink czs
      constrainedObs = 
        if null copts
        then mempty
        else let vrb = if obsStatus == Done then "was" else "is"
             in mconcat
               [ "This ", vrb, " a "
               , addList copts
               , " observation." ]

      -- TODO: integrate with the rest of the text
      (verb, verb2) = case obsStatus of
        Unscheduled -> ("will be", "will all be") -- not sure 100% correct
        Todo  -> ("will be", "will all be")
        Doing -> ("is", "will be")
        Done  -> ("was", "were")

      -- Support missions with no links, but that should not
      -- happen (only needed because I did not encode the mission
      -- invariant in the database, but this gives flexibility in
      -- case new missions are added).
      --
      missToLink mission = fromMaybe (toHtml mission)
        (toMission mission >>= fromMissionLongLink ctx)

      toJ (l, tks) = missToLink l
                     <> " (for " <> toHtml (showExpTime tks)
                     <> ")"

      -- Differentiate between this being a Chandra proposal or
      -- a proposal from another mission.
      --
      jointObs = case soJointWith of
        Just jName ->
          if "CXO-" `T.isPrefixOf` jName
          then otherMission (T.drop 4 jName) <> " "
          else jointMission <> " "
        Nothing -> mempty

      otherMission mission =
        let missInfo = missToLink mission
        in "This " <> verb <> " a " <> missInfo <> " proposal "
           <> "that was also awarded Chandra time. It does not "
           <> "mean that observations " <> verb2
           <> " done at the same time!"

      jointMission = 
        let ms = getJointObs so
            jvals = addList (map toJ ms)
        in "This " <> verb <> " a joint observation with "
           <> jvals <> ". However, it does not necessarily mean "
           <> "that the observations " <> verb2
           <> " done at the same time!"

      -- if there are constriants and a joint observation then the
      -- paragraph does not read well.
      constraintsPara = 
        let c = jointObs <> constrainedObs
        in if isNothing soJointWith && null copts  then mempty else p c

  in if soStatus == Discarded
     then discardedPara
     else sciencePara <> constraintsPara <> tooPara


-- | Report the related observation, or observations.
--
--   The text is *not* placed within a container (e.g. a p
--   element).
--
relatedObservationsText ::
  HtmlContext
  -> TargetName
  -> SortedList StartTimeOrder ScienceObs
  -> Html
relatedObservationsText ctx targetName matches =
  let nmatches = lengthSL matches
      suffix = if nmatches == 1 then "" else "s"

  in if nmatches == 0
     then mempty
     else " See related observation"  -- note leading space
          <> suffix
          <> ": "
          <> groupProposal ctx targetName matches
          <> "."

-- | Return a paragraph representing the related observations.
--
renderRelatedObs ::
  TargetName
  -> SortedList StartTimeOrder ScienceObs
  -> Maybe Html
renderRelatedObs targetName matches =
  if lengthSL matches == 0
  then Nothing
  else Just (p (relatedObservationsText DynamicHtml targetName matches))

-- | How best to indicate a discarded observation? Should there be some
--   mention of the ObsId and target?
--
--   Really the next/previous links are invalid here, but this would break
--   things (unless we exclude these entities when returning results),
--   I do not want to do this at this time.
--
discardedPara :: H.Html
discardedPara = p "This observation was discarded."


-- | Display information for a \"non-science\" observation.
otherInfo :: 
  UTCTime    -- current time
  -> NonScienceObs
  -> Html
otherInfo cTime ns = 
  let mTimes = getTimes (Left ns)
      obsStatus = getObsStatus mTimes cTime 
  in nonSciencePara (mTimes, cTime) ns obsStatus

-- | Create the paragraph describing the observing status -
--   i.e. if it has been, will be, or is being, observed.
--
nonSciencePara ::
  (Maybe (ChandraTime, ChandraTime), UTCTime)
  -- ^ start time, end time, current time
  -> NonScienceObs
  -> ObsStatus     -- ^ status of observation
  -> Html
nonSciencePara (mTimes, cTime) NonScienceObs{..} obsStatus =
  -- should not have Unscheduled observations here, but support just in case
  let showLen Unscheduled =
        if nsTime > nullTime
        then " - will run for " <> lenVal <> "."
        else " - has no scheduled observation date."
      showLen Todo =
        if nsTime > nullTime
        then mconcat [ " - will run for "
                , lenVal
                , ", starting "
                ]
        else " - will start "
      showLen Doing = 
        if nsTime > nullTime
        then mconcat [ " - is running for "
                , lenVal
                ]
        else " - is running now"
      showLen Done = 
        if nsTime > nullTime
        then mconcat [ " - was run for "
                , lenVal
                , " and finished "
                ]
        else " - finished "

      sTime = fst <$> mTimes
      eTime = snd <$> mTimes
      
      cts Unscheduled = 
        mconcat [ "The calibration observation - "
                , targetName
                , showLen Unscheduled
                ]
      cts Todo = 
        mconcat [ "The calibration observation - "
                , targetName
                , showLen Todo
                , toHtml (showTimeDeltaFwd cTime sTime)
                , "."
                ]
      cts Doing = 
        mconcat [ "The calibration observation - "
                , targetName
                , showLen Doing
                , ". The observation started "
                , toHtml (showTimeDeltaBwd sTime cTime)
                , " and ends "
                , toHtml (showTimeDeltaFwd cTime eTime)
                , "."
                ]
      cts Done = 
        mconcat [ "The calibration observation - "
                , targetName
                , showLen Done
                , toHtml (showTimeDeltaBwd eTime cTime)
                , "."
                ]

      nullTime = zeroKS
      targetName = toHtml nsTarget
      lenVal = toHtml $ showExpTime nsTime

  in p (cts obsStatus)

twitterDiv :: Html
twitterDiv = aside ((div ! id "otherBar") renderTwitter)

renderTwitter :: Html
renderTwitter = 
  (div ! class_ "tweetstream") (
    a ! class_ "twitter-timeline"
      ! href "https://twitter.com/chandraxray?ref_src=twsrc%5Etfw"
      ! dataAttribute "width" "520"
      ! dataAttribute "height" "650"
      ! dataAttribute "theme" "light" $ "Tweets by chandraxray"
  ) <>
  (script ! async "true"
          ! src "https://platform.twitter.com/widgets.js"
          ! charset "utf-8" $ "")

{-
Different widget given by

    a ! class_ "twitter-follow-button"
      ! href "https://twitter.com/chandraxray?ref_src=twsrc%5Etfw"
      ! dataAttribute "show-count" "false" $ "Follow @chandraxray"

No idea what this looks like
-}
