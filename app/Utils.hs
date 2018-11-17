{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | general routines

module Utils (
     HtmlContext(..)
     , fromBlaze
     , standardResponse
     , showTimeDeltaFwd
     , showTimeDeltaBwd
     , getTimes
     , cleanJointName
     -- , schedToList
     , rschedToList
     , getNumObs, getNumObsRestricted
     , getScienceExposure
     , getScienceTime, getScienceTimeRestricted

     , isChandraImageViewable
     , publicImageURL
       
     , toJSVarArr
     , toJSVarObj
       
     -- , makeCodeLink

     , makeETag
     , timeToRFC1123

     , fromDay
       
       -- useful in conversion of String-handling code
       -- to use Text instead
     , showInt
     ) where

import qualified Data.Aeson as Aeson

import qualified Data.ByteString.Lazy.Char8 as LB8

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import qualified Formatting as F
import qualified Formatting.Time as FT

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Either (rights)
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Maybe (fromMaybe)

import Data.Monoid ((<>))

import Data.Text.Encoding (decodeUtf8')
import Data.Time (Day, NominalDiffTime, UTCTime
                 , addUTCTime, diffUTCTime, showGregorian)

import Formatting ((%.))

import Text.Blaze.Html.Renderer.Text

import Web.Scotty

import Git (CommitId, fromCommitId)
import Types (ScienceObs(..)
             , ChandraTime, toChandraTime, fromChandraTime
             , Instrument(..)
             , ObsIdVal(..)
             , TimeKS, fromTimeKS
             , Record
             , Schedule(..)
             , RestrictedSchedule(..), RestrictedRecord
             , recordStartTime
             , recordTime
             , rsoExposureTime
             , addTimeKS, zeroKS, isZeroKS, showExpTime
             )

-- | Should HTML be generated for the static or dynamic version
--   of a page.
--
data HtmlContext = StaticHtml | DynamicHtml deriving Eq

-- | Convert Blaze's HTML to a HTML page.
fromBlaze :: H.Html -> ActionM ()
fromBlaze = html . renderHtml

-- A placeholder in case we want to set up any 
-- response settings.
standardResponse :: ActionM ()
standardResponse = return ()

plural :: Int -> T.Text
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
showTime :: UTCTime -> T.Text
showTime =
  -- this is the equivalent of
  --    formatTime defaultTimeLocale "%A, %B %e, %Y"
  let tfmt = FT.dayName <> ", " F.% FT.monthName <> " "
             F.% FT.dayOfMonthS <> ", " F.% FT.year
  in F.sformat tfmt
     
-- This could be more generic, but trying to identify where
-- specific conversions are needed for Text.
--
-- I think we could get away with just 'Int -> T.Text'.
--
showInt :: (Integral a, F.Buildable a) => a -> T.Text
showInt = F.sformat F.int


-- It is expected that a and b are both Int, but keep it
-- polymorphic for now.
getTimeLabels ::
  (a -> T.Text -> b)
  -> a
  -> a
  -> a
  -> UTCTime
  -> (b, b, b, T.Text)
getTimeLabels f nm nh nd t =
  (f nm "minute", f nh "hour", f nd "day", showTime t)


-- | Come up with a string representing the time difference. It
--   is probably not general enough, since it adds in a
--   prefix (here, "in"), in most cases. So, this is to be
--   used for time differences in the future.
--
showTimeDeltaFwd ::
  UTCTime     -- time 1
  -> Maybe ChandraTime  -- time 2, >= time 1
  -> T.Text   -- time1 relative to time2
showTimeDeltaFwd _ Nothing = "observation is not scheduled"
showTimeDeltaFwd t1 (Just ct2) = 
  let t2 = fromChandraTime ct2
      (delta, nd, nh, nm, d, h, m) = getTimeElems t1 t2

      -- TODO: look at F.plural
      toTxt n lbl =
        F.sformat ("in " F.% F.int F.% " " F.% F.stext F.% F.stext)
        n lbl (plural n)
      
      (mins, hours, days, other) = getTimeLabels toTxt nm nh nd t2

      {-
      mins = toTxt nm "minute"
      hours = toTxt nh "hour"
      days = toTxt nd "day"
      other = showTime t2
      -}
      
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
--   showTimeDeltaFwd.
--
showTimeDeltaBwd ::
  Maybe ChandraTime     -- time 1
  -> UTCTime  -- time 2, >= time 1
  -> T.Text   -- time1 relative to time2
showTimeDeltaBwd Nothing _ = "observation is not scheduled"  
showTimeDeltaBwd (Just ct1) t2 = 
  let t1 = fromChandraTime ct1
      (delta, nd, nh, nm, d, h, m) = getTimeElems t1 t2

      -- TODO: look at F.plural
      toTxt n lbl =
        F.sformat (F.int F.% " " F.% F.stext F.% F.stext F.% " ago")
        n lbl (plural n)

      (mins, hours, days, other) = getTimeLabels toTxt nm nh nd t1

      {-
      mins = toTxt nm "minute"
      hours = toTxt nh "hour"
      days = toTxt nd "day"
      other = showTime t1
      -}
      
  in if delta < 60
     then "now"
     else if m < 60
            then mins
            else if h < 24
                 then hours
                 else if d < 7
                      then days
                      else "on " <> other



-- | Remove the CXO- prefix from the "joint with" field,
--   since I have seen two cases of "CXO-HST". Presumably this
--   is for time awarded by the other facility (e.g. HST).
--
cleanJointName :: T.Text -> T.Text
cleanJointName j = if "CXO-" `T.isPrefixOf` j then T.drop 4 j else j 


getTimes ::
  Record
  -> Maybe (ChandraTime, ChandraTime) -- start and end times
getTimes rs =
  case recordStartTime rs of
    Just t -> let sTime = fromChandraTime t
                  ks = 1000 * fromTimeKS (recordTime rs)
                  expTime = (fromInteger . ceiling) ks 
                  eTime = addUTCTime expTime sTime
              in Just (toChandraTime sTime, toChandraTime eTime)
    Nothing -> Nothing


-- | Convert the schedule contents to a list.
--
schedToList :: Schedule -> [Record]
schedToList Schedule{..} = scDone ++ toList scDoing ++ scToDo

rschedToList :: RestrictedSchedule -> [RestrictedRecord]
rschedToList RestrictedSchedule{..} = rrDone ++ toList rrDoing ++ rrToDo

getNumObsHelper :: [a] -> T.Text
getNumObsHelper xs =
  let nobs = length xs
      obslen = case nobs of
        0 -> "are no observations"
        1 -> "is one observation"
        _ -> "are " <> showInt nobs <> " observations"
  in "There " <> obslen

getNumObs :: Schedule -> T.Text
getNumObs = getNumObsHelper . schedToList

getNumObsRestricted :: RestrictedSchedule -> T.Text
getNumObsRestricted = getNumObsHelper . rschedToList


-- | Return the total obervation time for the science observations in the
--   schedule.
getScienceExposure :: Schedule -> TimeKS
getScienceExposure sched =
  let sobs = rights (schedToList sched)
      getTime so = fromMaybe (soApprovedTime so) (soObservedTime so)
  in foldl' addTimeKS zeroKS (map getTime sobs)

getScienceExposureRestricted :: RestrictedSchedule -> TimeKS
getScienceExposureRestricted sched =
  let sobs = rights (rschedToList sched)
  in foldl' addTimeKS zeroKS (map rsoExposureTime sobs)


getScienceTimeHelper :: TimeKS -> H.Html
getScienceTimeHelper etime =
  if isZeroKS etime
  then mempty
  else ", and the total science exposure time for these observations is "
       <> H.toHtml (showExpTime etime)


getScienceTime :: Schedule -> H.Html
getScienceTime = getScienceTimeHelper . getScienceExposure


getScienceTimeRestricted :: RestrictedSchedule -> H.Html
getScienceTimeRestricted = getScienceTimeHelper . getScienceExposureRestricted


-- | This handles both wheter the image is publically available as well
--   as "is this worth showing to users" (at present CC-mode data is not
--   shown).
--
--   TEMPORARILY DISABLED.
--
isChandraImageViewable ::
  Maybe UTCTime    -- ^ soPublicRelease field
  -> Maybe T.Text  -- ^ soDataMode field
  -> Instrument    -- ^ soInstrument field
  -> UTCTime       -- ^ current time
  -> Bool          -- ^ is the image "viewable"
isChandraImageViewable _ _ _ _ = False
{-
isChandraImageViewable relDate dataMode inst tNow =
  let -- Isn't this the logic of the Ord typeclass for Maybe?
      isPublic = case relDate of
        Just pDate -> pDate < tNow
        Nothing -> False

      -- If it is HRC the soDataMode will be Nothing. This is okay.
      -- I am not sure if we can have ACIS data with soDataMode equal to
      -- Nothing, so add a check here, just in case.
      --
      notCC = case dataMode of
        Just mode -> not ("CC" `T.isPrefixOf` mode)
        Nothing -> inst `elem` [HRCI, HRCS]

  in isPublic && notCC
-}

-- TODO: how to find the correct version number (i.e.
-- 'N00x' value)? One option would be to provide an
-- endpoint (e.g. /api/image/:obsid) which would
-- do the navigation, but leave that for the (possible)
-- future.
--
publicImageURL :: ObsIdVal -> Instrument -> T.Text
publicImageURL obsid inst =
  let obsidTxt = F.sformat (F.left 5 '0' %. F.int) (fromObsId obsid)
      instTxt = case inst of
        ACISI -> "acis"
        ACISS -> "acis"
        HRCI -> "hrc"
        HRCS -> "hrc"

  in "https://cda.cfa.harvard.edu/chaser/viewerImage.do?obsid="
     <> obsidTxt <> "&filename=" <> instTxt
     <> "f" <> obsidTxt
     <> "N001_full_img2.jpg&filetype=loresimg_jpg"



{- At the moment this is only used in static/about/index.html, but
   the link there is added via the configure script, so this routine
   is not needed.

-- | Create a link to the code version used for this build.
makeCodeLink :: H.Html -> H.Html
makeCodeLink =
  let uri = "https://bitbucket.org/doug_burke/chandraobs/commits/"
            <> gitCommitId
  in (H.a H.! A.href (H.toValue uri))
-}

-- | What is the etag for a resource?
--
--   This is intended for resources that depend on the database.
--   It returns a strong validation, since the implication is that
--   the resource should be byte identical if the ETAGs match.
--   It includes the start and end quotes (in part so that if
--   I change my mind and go to a weak validator it can be used
--   in the same manner).
--
makeETag ::
  CommitId      -- ^ commit identifier
  -> T.Text     -- ^ the resource path (local, not absolute)
  -> UTCTime    -- ^ the last-modified date of the database
  -> LT.Text
makeETag cid path lastMod =
  let cval = fromCommitId cid
      -- Only use a small subset of the commit id as it should
      -- be unique enough for our needs.
      --
      -- I doubt the use of %Q is needed, but support it just in case.
      --
      -- The ETag is meant to be opaque; one way to do this is to
      -- use base 64 encoding, but for our purposes this seems overkill;
      -- if some entity wants to try and tweak the ETag settings then
      -- they can.
      --
      -- The path isn't really needed, but include some version of it
      -- for "fun".
      --
      time = F.sformat (FT.epoch <> FT.pico) lastMod
      pathTxt = showInt (T.length path)
      dquot = "\""
      txt = dquot <> T.take 8 cval <> pathTxt <> time <> dquot
  in LT.fromStrict txt

-- | Perhaps should use HTTPDate here, but unsure about the
-- conversion from UTCTime to HTTPDate. The format for
-- the last-modified date appears to be RFC1123, so use
--   "Sun, 15 May 2016 13:11:48 GMT"
-- (assume that the RFC822 format is enough, too lazy to look to
-- see what the additions in RFC1123 are)
--
-- Would it be worth proposing rfc822DateFormat to formatting
-- (and have I exactly captured this RFC here)?
--
timeToRFC1123 :: UTCTime -> LT.Text
timeToRFC1123 =
  let tfmt = FT.dayNameShort <> ", "
             F.% FT.dayOfMonth <> " "
             F.% FT.monthNameShort <> " "
             F.% FT.year <> " "
             F.% FT.hms <> " "
             F.% FT.tzName
  in F.format tfmt

-- This was
--  -- let rfc822DateFormat = "%a, %_d %b %Y %H:%M:%S %Z"
--  let rfc822DateFormat = "%a, %d %b %Y %H:%M:%S %Z"
--  in LT.pack . formatTime defaultTimeLocale rfc822DateFormat
--
-- note ghc 7.8 does not export rfc822DateFormat

-- | Create a javascript block that creates a variable with
--   the given label set to the value.
--
toJSVar ::
  Aeson.ToJSON a
  => H.Html
  -- ^ The default value to use when the conversion of the input
  --   value fails. It is expected to be @"[]"@ or @"{}"@.
  -> T.Text
  -- ^ The name of the JS variable to create.
  -> a
  -- ^ The value to convert to JSON; if it can not be decodeded
  --   because of a UTF-8 conversion error then the default value
  --   will be used instead.
  -> H.Html
toJSVar defVal lbl js =

  -- TODO: do we need all this conversion?
  --   a) need to add toEncoding of our datatypes
  --      (although note that at present the data sent to this
  --       routine is created on the fly)
  --   b) can we just H.toHtml the output of Aeson.encode?
  --   c) if we can, how do we handle encoding errors
  --  
  let jsHtml = case decodeUtf8' (LB8.toStrict (Aeson.encode js)) of
        Right ans -> H.toHtml ans
        Left _ -> defVal

  in (H.script H.! A.type_ "text/javascript")
     ("var " <> H.toHtml lbl <> " = " <> jsHtml <> ";")

-- | Create a javascript block that creates a variable with
--   the given label set to the value.
--
--   There is *no* check that the input value maps to an
--   object.
--
toJSVarObj ::
  Aeson.ToJSON a
  => T.Text
  -- ^ The name of the JS variable to create.
  -> a
  -- ^ The value to convert to JSON; if it can not be decodeded
  --   because of a UTF-8 conversion error then an empty object
  --   will be used instead.
  -> H.Html
toJSVarObj = toJSVar "{}"

-- | Create a javascript block that creates a variable with
--   the given label set to the value.
--
--
--   There is *no* check that the input value maps to an
--   array.
--
toJSVarArr ::
  Aeson.ToJSON a
  => T.Text
  -- ^ The name of the JS variable to create.
  -> a
  -- ^ The value to convert to JSON; if it can not be decodeded
  --   because of a UTF-8 conversion error then an empty array
  --   will be used instead.
  -> H.Html
toJSVarArr = toJSVar "[]"

-- | Convert to a text representation (a text version of
--   @showGregorian@).
--
fromDay :: Day -> T.Text
fromDay = T.pack . showGregorian
