{-# LANGUAGE FlexibleContexts #-}
{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
{-# Language TupleSections #-}

--
-- Usage:
--    ./getschedule <n>
--
--    <n> defaults to 2
--
-- Aim:
--
-- Add n short-term schedule entries to the database. The idea
-- is to query the CXC server, find out the list of short-term
-- schedules that are available, and then process the n most-recent
-- schedules, adding them to the database. The aim is to add
-- each schedule as a single transaction, so it is either all
-- added or not (ideally on a per schedule page basis but for now
-- it is all pages).
--
-- This is not intended to "update" the database, to reflect changes
-- in previously-recorded ObsIds. That will need a to-be-written
-- tool (e.g. updateschedule ?).
--
-- It also does *not* query Simbad for any information on the
-- observations. It does not seem worth the effort to couple
-- the information requests given that CXC and Simbad need not
-- both be working at the same time.
--
-- There is a conflict between "adding the archival records" and
-- "add new records", since the decision of what obsids you may
-- or may not care about are not necessarily the same (since the
-- traversal is forward or backward in time). This code is
-- not written to address this; it probably should be. For now
-- I am focussing on the "adding new records".
--

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Parsec.Error as PE
import qualified Text.Parsec.Pos as PP

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, void, when)
import Control.Monad.IO.Class (liftIO)

import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid ((<>))
import Data.Sequence ((|>), (><))
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime, getCurrentTime)

import Database.Groundhog (PersistBackend
                          , (&&.)
                          , (==.)
                          , count
                          , delete
                          , project
                          , select
                          , selectAll)
import Database.Groundhog.Postgresql (SqlDb, Conn, in_, insert_)

import Network.HTTP.Conduit
import Numeric.Natural

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.HTML.TagSoup (Tag(..)
                         , (~==)
                         , innerText
                         , isTagOpenName
                         , parseTags
                         , partitions)
import Text.Parsec ((<?>), parse, spaces, try)
import Text.Read (readMaybe)

import Database (runDb, getInvalidObsIds, updateLastModified
                 , insertProposal)
import OCAT (OCAT, isScienceObsE, noDataInOCAT
            , queryOCAT, ocatToScience, ocatToNonScience
            , addProposal
            , showInt)
import Parser (parseSTS
              , parseReadable
              , parseRAStr
              , parseDecStr
              , parseTime
              , handleTime)

import Types (ChandraTime(..), ScheduleItem(..))
import Types (ShortTermTag(..), ShortTermSchedule(..), toShortTermTag)
import Types (NonScienceObs(..), ScienceObs(..), ObsIdVal(..))
import Types (InvalidObsId(..), Field(..))
import Types (ObsIdStatus(..), TargetName(..))
import Types (Proposal(propNum))

-- The assumption is that all pages are accessible from 
-- baseLoc <> startPage and that they are in reverse
-- temporal order.
--
baseLoc, startPage :: String
baseLoc = "http://cxc.harvard.edu/target_lists/stscheds/"
startPage = "oldscheds.html"

-- | TODO: Add in identifier to the request for tracking by CDA.
--
--   There is *no* parsing of the result
getPage ::
  String  -- ^ page name (under baseLoc)
  -> IO T.Text
getPage name =
  decodeUtf8 . L.toStrict <$> simpleHttp (baseLoc <> name)

-- This is from a manual check - June 26 2017 - of the contents.
-- It may include pages which have been hidden/not included on
-- purpose (e.g. discarded). We should be able to live with this.
--
unlistedTags :: [ShortTermTag]
unlistedTags =
  map toShortTermTag
  ["122107", "APR0708A", "APR0802C", "APR1408A", "APR2103A", "APR2103B",
   "APR2202C", "APR2808B", "APR2911B", "APR3001", "APR3003A", "AUG0408B",
   "AUG1108A", "AUG1608A", "AUG1808D", "AUG2106C", "AUG2408A", "AUG2508B",
   "AUG2602C", "AUG2806",
   "DEC0108A", "DEC0301", "DEC0599",
   "DEC0808A", "DEC1299", "DEC1508C", "DEC1808A", "DEC2107A", "DEC2203B",
   "DEC2208B", "DEC2407B", "DEC2908B", "FEB0408B",
   -- "FEB1102",  this has errors and is superceeded by FEB1102D
   "FEB1108B",
   "FEB1808B", "FEB2508A", "JAN0603C", "JAN0708A", "JAN1201", "JAN1408C",
   "JAN2108B", "JAN2808A", "JAN3000", "JUL0708B", "JUL1408A", "JUL1600",
   "JUL2002C", "JUL2108A", "JUL2604B", "JUL2803C", "JUL2806A", "JUL2808A",
   "JUN1608A", "JUN2308A", "JUN3008C", "MAR0303C", "MAR1008A", "MAR1708A",
   "MAR2403BA", "MAR2408A", "MAR3108A", "MAY0508A", "MAY1203C", "MAY1208B",
   "MAY1608B", "MAY2404C", "MAY2608A", "MAY2702C", "NOV0308B", "NOV1008B",
   "NOV1102B", "NOV1708A", "NOV2408C", "NOV2601", "NOV2700", "OCT0408A",
   "OCT0702C", "OCT0708B", "OCT1308A", "OCT1910A", "OCT2008A", "OCT2102",
   "OCT2108A", "OCT2503", "OCT2603A", "OCT2708B", "SEP0108B", "SEP0808A",
   "SEP080A", "SEP1508B", "SEP2208B", "SEP2908B"]

-- The URL name is reverse-engineered from the tag; as the tag
-- was extracted from the file name we really should just use the
-- file name itself, to avoid case issues.
--
hardcodedScheduleList :: Seq.Seq (ShortTermTag, T.Text)
hardcodedScheduleList =
  let toKV t = (t, "stsched" <> fromShortTermTag t <> ".html")
  in Seq.fromList (map toKV unlistedTags)


-- | Find out the full list of schedule pages. The order is intended
--   to be newest first, but the last ones are from the unlisted
--   (hardcoded) set.
--
--   This is not a total function, since it will error out
--   on problems (e.g. decoding errors).
--
downloadScheduleList ::
  IO (Seq.Seq (ShortTermTag, T.Text))
  -- ^ The "names" and URLs for each schedule.
downloadScheduleList = do
  txt <- getPage startPage
  let tags = parseTags txt

      ctxt = " BEGIN LIST " :: T.Text
      ps = head (partitions (TagComment ctxt ~==) tags)

      pages = extractPages ps
      allPages = pages >< hardcodedScheduleList
      
  return allPages

-- | Download the given schedule. The return value is the text
--   representation of the content (so no tags have been stripped).
--
--   There is *no* parsing of the response.
--
downloadSchedulePage ::
  (ShortTermTag, T.Text)
  -> IO T.Text
downloadSchedulePage = getPage . T.unpack . snd
  
type TagList = [Tag T.Text]

-- | Given a list of tags, extract all the page links and ids
--   from it.
--
--   No error checking; that is, if the format of the page
--   changes then the results are not guaranteed.
--
extractPages :: TagList -> Seq.Seq (ShortTermTag, T.Text)
extractPages = go Seq.empty
  where
    go s [] = s
    go s xs = case getLink xs of
      (Just ans, ys) -> go (s |> ans) ys
      (Nothing, ys) -> go s ys


getLink :: TagList -> (Maybe (ShortTermTag, T.Text), TagList)
getLink (TagOpen "A" attrs : TagText txt : TagClose "A" : xs)
  = ((toShortTermTag txt, ) <$> lookup "HREF" attrs, xs)
getLink (_ : xs) = (Nothing, xs)
getLink [] = (Nothing, [])

-- | Find the wanted items.
--
wanted ::
  (Eq c, Ord c)
  => Natural
  -- ^ The maximum number of items to return
  -> (a -> c)
  -- ^ The projection function to identify the element for
  --   comparison
  -> [a]
  -- ^ The items that we know about.
  -> (b -> c)
  -> Seq.Seq b
  -- ^ The items to be filtered.
  -> Seq.Seq b
  -- ^ The requested items.
wanted n proj1 known proj2 =
  let keys = toList (fmap proj1 known)
      seen = Set.fromList keys
      notSeen x = proj2 x `Set.notMember` seen

  in Seq.take (fromIntegral n) . Seq.filter notSeen

{-
Formats for the short-term schedule:

*)

http://cxc.harvard.edu/target_lists/stscheds/stschedMAY2311A.html

<H4 ALIGN=CENTER>MAY2311A</H4>

<pre id="schedule">

Seq #     ObsID Constr.    Target              Start        Time   SI   Grat    RA       Dec    Roll   Pitch   Slew
----------------------------------------------------------------------------------------------------------------------------
 ----     GG_28       CAL-ER (55418) 2011:142:10:00:07.996   0.0   --    --  212.0000 -33.0000 291.50 153.11 155.85
...

*)

http://cxc.harvard.edu/target_lists/stscheds/stschedAPR1311A.html

<H4 ALIGN=CENTER>APR1311A</H4>

<TABLE BORDER=1 CELLSPACING=2 CELLPADDING=2>
<TR><TH>Seq. No.</TH>
<TH>ObsID</TH>
<TH>Target</TH>
<TH>Start Time</TH>
<TH>Duration</TH>
<TH>Inst.</TH>
<TH>Grat.</TH>
...

*)

http://cxc.harvard.edu/target_lists/stscheds/stschedDEC2704B.html

<H4 ALIGN=CENTER>DEC2704B</H4>

<TABLE BORDER=1 CELLSPACING=2 CELLPADDING=2>
<TR><TH>Seq. No.</TH>
<TH>ObsID</TH>
<TH>Target</TH>
<TH>Start Time</TH>
<TH>Duration</TH>
<TH>Inst.</TH>
<TH>Grat.</TH>
<TH>RA</TH>
<TH>Dec</TH>
<TH>Roll</TH>
<TH>Pitch</TH>
<TH>PI</TH>
</TR>

... but at some point we get

<TR>
<TD><A HREF="/cgi-gen/mp/target.cgi?<font"><font</A></TD>
<TD>color="#FF0000">(nosq)</font></TD>
<TD><a</TD>
<TD>NONE</TD>
<TD>68.3752</TD>
<TD>24.3648</TD>
<TD>275.39</TD>
<TD> 9:29:50.40</TD>
<TD> 29:24:0.00</TD>
<TD>dss</TD>
<TD>pspc</TD>
<TD></TD>
</TR>

this row is wrong, and it's not obvious how to "fix" it. There are several
like it on that page.

*)

 # Error parsing NOV2502B
 "from-file" (line 1, column 1):
 Table header does not match: ["Seq. No.","ObsID","Target","Start Time","Duration","Inst.","Grat.","RA","Dec","Roll","PI"]
 # For now, halting execution

Missing pitch column.

*)

 # Error parsing NOV2501
 "from-file" (line 1, column 1):
 Expected 11 columns, found 0 in:
 [TagOpen "TR" [],TagText "\n"]
 ->[]

*)

 # Error parsing JUN0401
 "from-file" (line 1, column 1):
 "<manual parsing>" (line 1, column 6):
 unexpected end of input
 expecting white space
 Unable to parse as readable: T_E52
 cols: ["NULL","T_E52","\160","2001:155:14:18:20.300","9.2","--","--","10:36:42.00","-27:31:39.00","248.41","NULL"]

Argh; CAL-ER is missing...

*)

 # Error parsing MAR0501
 "from-file" (line 1, column 1):
 "<manual parsing>" (line 1, column 4):
 unexpected "."
 expecting digit, white space or ":"
 cols: ["900061","02421","\160HDF-N","2001:063:17:10:11.015","62.4","ACIS-I","NONE","189.2017","62.2370","143.50","BRANDT    "]

Yay; RA and Dec are back to being in decimal ...

*)

 # Error parsing NOV1800
 "from-file" (line 1, column 1):
 "<manual parsing>" (line 1, column 6):
 unexpected end of input
 expecting white space
 Unable to parse as readable: TE77B
 cols: ["NULL","TE77B","","2000:323:15:45:00.000","5.0","--","--","13.5600","-65.2500","321.99","NULL"]

*)

 # Error parsing MAY0700
 "from-file" (line 1, column 1):
 Table header does not match: ["Seq. No.","ObsID","Target","Start Time","Duration","Inst.","Grat.","RA","Dec","Roll","PI","DSS Image","PSPC Image","RASS Image"]

*)

 # Error parsing FEB1102
 "from-file" (line 1, column 1):
 "<manual parsing>" (line 2, column 1):
 unexpected end of input
 expecting space or obsid
 cols: ["200153","\n","\160\&02545 00 0 M 1-16","2002:042:14:39:14.260","50.0","ACIS-S","NONE","7:37:13.54","-9:38:42.72","307.33"," KASTNER   "]

Hmm, this line looks like it is corrupted. Fortunately it appears that
this must be from a discarded/unused schedule as the obsid is already
in the database, so I am going to just manually ignore it. Indeed,
there's a FEB1102D page, so easiest is just to remove this schedule from
the hardcoded list.

-}

-- | Hack up a parse error.
parseError :: String -> PE.ParseError
parseError emsg =
  PE.newErrorMessage (PE.Message emsg) (PP.initialPos "from-file")


-- | Convert a schedule to a list of records.
--
--   The input text is the raw HTML of the page. There is
--   limited format support (i.e. it is hoped that the page
--   format is fixed, at least for the time range covered
--   here).
--
--
--   The parse error is not guaranteed to contain a useful
--   position.
--
toSchedule :: T.Text -> Either PE.ParseError [ScheduleItem]
toSchedule txt =
  let isHeader t = not (isTagOpenName "pre" t || isTagOpenName "TABLE" t)

  in case dropWhile isHeader (parseTags txt) of
    (TagOpen "pre" _:rest) -> parseScheduleFromText rest
    (TagOpen "TABLE" _:rest) -> parseScheduleFromHTML rest
    _ -> Left (parseError "Unable to find the start of the data")


-- | The "new" format.
--
--   Why do I have removeHeader after converting back to a string,
--   rather than removing it from the tags?
--
parseScheduleFromText ::
  [Tag T.Text] ->
  Either PE.ParseError [ScheduleItem]
parseScheduleFromText tags =
  case (removeHeader . T.unpack . innerText) tags of
    Just txt -> parseSTS txt
    Nothing -> Left (parseError "Unable to strip out the header")

-- | The "old" format.
--
-- Split up into rows and then parse each row. It is possible
-- to return the empty list, if the schedule happens to consist
-- entirely of engineering observations.
--
parseScheduleFromHTML ::
  [Tag T.Text] ->
  Either PE.ParseError [ScheduleItem]
parseScheduleFromHTML tags =
  let findRow = isTagOpenName "TR"
  in case partitions findRow tags of
    (hdr:rest) -> do
      colOption <- validateHeaderRow hdr
      mrows <- mapM (processRow colOption) rest
      Right (catMaybes mrows)
                     
    _ -> Left (parseError "expected multiple table rows")
         

-- | Extract the row data, extracting only those text elements
--   within the given tag name.
--
getRowText :: T.Text -> [Tag T.Text] -> [T.Text]
getRowText tagname tags =
  let -- Is there a better way than this? Could try and use
      -- Text.HTML.TagSoup.Tree but it is not stable.
      --
      go ::
        [Tag T.Text]
        -- ^ The tags to process
        -> Maybe (Seq.Seq T.Text)
        -- ^ The contents of the current open tag; if Nothing then
        --   the tag is not open
        -> Seq.Seq T.Text
        -- ^ The current store
        -> [T.Text]
        -- ^ The results
      go [] Nothing store = toList store
      go [] (Just r) store = toList (store |> flatten r)

      -- Note: not sure I can be guaranteed that I will always see a
      --       close tag, so need to deal with that possibility.
      --
      go (TagOpen t _ : xs) Nothing store
        | t == tagname = go xs (Just Seq.empty) store
        | otherwise    = go xs Nothing store
      go (TagOpen t _ : xs) rr@(Just r) store
        | t == tagname = go xs (Just Seq.empty) (store |> flatten r)
        | otherwise    = go xs rr store
                                       
      -- go (TagClose _:xs) Nothing store = go xs Nothing store
      go (TagClose t:xs) rr@(Just r) store
        | t == tagname = go xs Nothing (store |> flatten r)
        | otherwise    = go xs rr store
                                       
      -- go (TagText _ : xs) Nothing store = go xs Nothing store
      go (TagText t : xs) (Just r) store =
        let nr = r |> t
        in go xs (Just nr) store
      
      go (_:xs) r store = go xs r store

      flatten = T.intercalate " " . toList
      
  in go tags Nothing Seq.empty

data NumCols = Col11 | Col12 | Col13 | Col14
  deriving Eq

-- | Check the order of the rows has not changed.
--
--   Could parse them so we return a map for further processing,
--   but leave that for later additions, if needed.
--
--   Ugly code, but it works.
--
validateHeaderRow :: [Tag T.Text] -> Either PE.ParseError NumCols
validateHeaderRow tags =
  let tnames = getRowText "TH" tags
      tnamesL = map T.toLower tnames
  in if tnamesL == ["seq. no.", "obsid", "target", "start time",
                    "duration", "inst.", "grat.", "ra", "dec",
                    "roll", "pitch", "pi"]
     then Right Col12
     else if tnamesL == ["seq. no.", "obsid", "target", "start time",
                    "duration", "inst.", "grat.", "ra", "dec",
                    "roll", "pi"]
          then Right Col11
          else if tnamesL == ["seq. no.", "obsid", "target", "start time",
                              "duration", "inst.", "grat.", "ra", "dec",
                              "roll", "pi", "dss image", "pspc image",
                              "rass image"]
               then Right Col14
               else if tnamesL == ["seq. no.", "obsid", "target", "start time",
                                   "duration", "inst.", "grat.", "ra", "dec",
                                   "roll", "pi", "dss image", "pspc image"]
                    then Right Col13
                    else Left (parseError
                               ("Table header does not match: " <> show tnames))


-- Unfortunately the old records do not provide an ObsId value
-- engineering/non-science observations, so we have to
-- skip these.
--
-- We now also skip rows where the obsid element begins with
-- 'color="#...' since this is a problem in the DEC2704B
-- schedule, and maybe other times. I don't want to skip
-- arbitrary errors in case there are other problem rows.
--
-- Similarly, if the first cell is
-- "REMAINING SCHEDULE POSTPONED DUE TO BRIGHT STAR HOLD"
-- then the row is skipped (e.g.
-- http://cxc.harvard.edu/target_lists/stscheds/stschedJAN2102.html)
-- We could stop processing this table, but for now do not
-- bother with that complexity (since no current way to
-- send a message back to the parser saying "stop".
--
-- APR1403A has a row "Radiation interrupt. SC107 trip @ 107:2036 UT"
--
-- empty rows are skipped (e.g. NOV2501)
--
processRow ::
  NumCols
  -> [Tag T.Text]
  -> Either PE.ParseError (Maybe ScheduleItem)
processRow colOpt tags =
  let cols = getRowText "TD" tags
      ncols = length cols

      expCols = case colOpt of
        Col11 -> 11
        Col12 -> 12
        Col13 -> 13
        Col14 -> 14
        
      -- Fortunately the first 10 columns are the same (at least for the
      -- columns we use).
      --
      [seqStr, obsidStr, targetStr, startStr, durStr, _, _,
       raStr, decStr, rollStr] = take 10 cols

      lexeme p = spaces >> p >>= \a -> spaces >> return a
      qparse p = parse (lexeme p) "<manual parsing>" . T.unpack

      getsi = do
       obsid <- qparse (parseReadable <?> "obsid") obsidStr
       start <- qparse (parseTime <?> "time") startStr
       texp <- qparse (parseReadable <?> "duration") durStr
       ra <- qparse (try (parseRAStr <?> "RA sexagesimal")
                     <|>
                     (parseReadable <?> "RA (decimal)")) raStr
       dec <- qparse (try (parseDecStr <?> "Dec sexagesimal")
                      <|>
                      (parseReadable <?> "Dec (decimal)")) decStr
       roll <- qparse (parseReadable <?> "Roll") rollStr

       let (tks, t1, _) = handleTime start texp
       
       Right ScheduleItem { siObsId = ObsIdVal obsid
                          , siScienceObs = True
                          , siStart = t1
                          , siDuration = tks
                          , siRA = ra
                          , siDec = dec
                          , siRoll = roll
                          }

      -- at least one record is "\160CAL-ER", one is just "\160",
      -- and one is "". It looks like sequence==NULL is a good check
      -- in these conditions.
      --
      isEngineering = "CAL-ER" `T.isSuffixOf` targetStr
                      || seqStr == "NULL"

      -- Since could have multiple reaons for skipping, just match on
      -- the header, which is unique enough.
      --
      skipRow r = ("REMAINING SCHEDULE POSTPONED DUE TO "
                   `T.isPrefixOf` r) ||
                  ("Radiation interrupt. " `T.isPrefixOf` r)

      -- brightStarHold = "REMAINING SCHEDULE POSTPONED DUE TO BRIGHT STAR HOLD"
      -- radiationTrip = "REMAINING SCHEDULE POSTPONED DUE TO RADIATION TRIP"

  in if ncols == expCols
     then if isEngineering || ("color=\"#" `T.isPrefixOf` obsidStr)
          then Right Nothing
          else case getsi of
            Right si -> Right (Just si)
            Left pe -> Left (parseError (show pe <> "\ncols: " <> show cols))
       
     else case cols of
       (col:_) | skipRow col -> Right Nothing
       _ -> if ncols == 0
            then Right Nothing
            else Left (parseError
                       ("Expected " <> show expCols <>
                        " columns, found " <> show ncols <> " in:\n"
                        <> show tags <> "\n->" <> show cols))

--   The parser does not quite match the on-file format,
--   in that the "header" line (e.g. AUG2916A) is assumed
--   to start with a comment token (#) to match the format
--   used in HackData. The simplest change is to just skip
--   the first non new-line characters. Actually, also need
--   to skip the other header lines (strip out 
--
removeHeader :: String -> Maybe String
removeHeader txt =
  let ls = lines txt
      -- make sure have more consecutive '-' than expect
      -- for the sequence column (although those start with
      -- a space), and then we want to drop this header
      -- line

      noHeaders = dropWhile (\l -> not ("--------" `isPrefixOf` l)) ls

  in case noHeaders of
    (_:nls) -> Just (unlines nls)
    [] -> Nothing


-- | Record that this short-term schedule page has been processed.
--
addShortTermTag ::
  PersistBackend m
  => UTCTime
  -> ShortTermTag
  -> Maybe T.Text
  -- ^ The error text from decoding the page
  -> m ()
addShortTermTag time tag mErr =
  let store = ShortTermSchedule {
        stsTag = tag
        , stsChecked = time
        , stsErrorOnParse = mErr }
      
  in insert_ store >> updateLastModified time


-- | Add an invalid obsid, i.e. one for which there's no data
--   in the OCAT.
--
addInvalidObsId ::
  PersistBackend m
  => UTCTime
  -> ObsIdVal
  -> T.Text
  -> m Bool
  -- Always returns False
addInvalidObsId t obsid errMsg =
  liftIO (putStrLn ("Error with " <> show (fromObsId obsid) <> ":"
                    <> T.unpack errMsg))
  >> insert_ InvalidObsId { ioObsId = obsid
                          , ioChecked = t
                          , ioMessage = errMsg }
  >> return False

       
-- | Add an ObsId. If the data can not be parsed it ends up
--   being added as an invalid obsid instead.
--
addObsId ::
  PersistBackend m
  => UTCTime
  -> ObsIdVal
  -- The ObsId of the OCAT response
  -> ScheduleItem
  -- The corresponding short-term schedule item; this is used to fill in
  -- the NonScienceObs field if the OCAT response is lacking
  -> OCAT
  -> m Bool
  -- Returns False on error
addObsId t obsid sts ocat =
  case isScienceObsE ocat of
    Left emsg -> addInvalidObsId t obsid emsg
    Right f -> if f
               then addScienceObs t obsid ocat
               else addNonScienceObs t obsid sts ocat

       
-- | Add a science observation.
--
addScienceObs ::
  PersistBackend m
  => UTCTime
  -> ObsIdVal
  -> OCAT
  -> m Bool
  -- ^ Returns False if there was an error.
addScienceObs t obsid ocat = do
  ansE <- liftIO (ocatToScience ocat)
  case ansE of
    Left emsg -> addInvalidObsId t obsid emsg
    Right (prop, sObs) -> do
      void (insertProposal prop)  -- may already exist
      insertScience sObs
      let pnum = propNum prop
      nprop <- count (PaNumField ==. pnum)
      if nprop == 0
        then addProposal pnum
        else return True
                 

-- | In most cases the obsid should not be known about, but there
--   is some window that allows an existing obsid to be queried
--   here. This happened with obsid 6432 which is marked as
--   discarded. So why is it being queried again?
--

-- insertScience = insert_

insertScience ::
  PersistBackend m
  => ScienceObs
  -> m ()
insertScience obs = do
  let obsId = soObsId obs
      match = SoObsIdField ==. obsId

      otxt = showInt (fromObsId obsId)
      put = liftIO . T.putStrLn
      putE = liftIO . T.hPutStrLn stderr
      
  ms <- select match
  case ms of
    [] -> insert_ obs
    
    [old] -> if old == obs
             then put ("Already know about ObsId: " <> otxt)
             else do
               put ("Replacing ObsId: " <> otxt)
               put (" Old: " <> T.pack (show old))
               put (" New: " <> T.pack (show obs))
               delete match
               insert_ obs
               
    _ -> putE ("SHOULD NOT BE POSSIBLE: multiple ObsIds found for " <> otxt)
         >> putE "EXITING AS A PRECAUTION..."
         >> liftIO exitFailure


-- | Add a non-science observation.
--
--   This is only for "old" non-science observations; i.e. those
--   which have happened but the OCAT either has an answer or
--   partial answer. It is not expected to be used for upcoming
--   observations, since in these case OCAT has no response.
--
--   TODO: of ocatToNonScience fails, do we try and build a
--         response from the input ScheduleItem, or assume that
--         this is an invalid observation?
--
--   NOTE:
--     this assumes that obsid is not already in the database.
--
addNonScienceObs ::
  PersistBackend m
  => UTCTime
  -> ObsIdVal
  -> ScheduleItem
  -> OCAT
  -> m Bool
  -- ^ Returns False if there was an error.
addNonScienceObs t obsid ScheduleItem{..} ocat = 
  case ocatToNonScience ocat of
    Left emsg -> addInvalidObsId t obsid emsg

    Right nsObs -> do
      -- what values can we take from the short-term schedule?
      -- for now just take the start time, but it is **UNCLEAR**
      -- what really should be going on here.
      --
      case nsStartTime nsObs of
        Nothing -> insert_ (nsObs {nsStartTime = Just siStart})
        _ -> insert_ nsObs

      return True
           

  
-- | This is expected to be an "unobserved" non-science observation,
--   i.e. one for which we do not expect an OCAT response.
--
addNonScienceObsManual ::
  PersistBackend m
  => ScheduleItem
  -> m Bool
  -- ^ Returns False if there was an error.
addNonScienceObsManual ScheduleItem{..} = 
  let nsObs = NonScienceObs
              { nsStatus = Unobserved
              , nsObsId = siObsId
              , nsTarget = TN name
              , nsStartTime = Just siStart
              , nsTime = siDuration
              , nsRa = siRA
              , nsDec = siDec
              , nsRoll = siRoll
              }

      name = "CAL-ER (" <> showInt (fromObsId siObsId) <> ")"
              
  in insert_ nsObs
     >> return True

-- | This queries the archive for these records, adds them to the
--   database, and updates the knowledge of what schedules have
--   been processed and if there was any error.
--
--   TODO: should this query SimBad as well?
--
addToSchedule ::
  (PersistBackend m, SqlDb (Conn m))
  => UTCTime
  -> ShortTermTag
  -> [ScheduleItem]
  -> m (Int, Int)
  -- ^ The number of failed and successful requests.
addToSchedule time tag xs = do
  let put = liftIO . T.putStrLn
  put ("# Success reading " <> fromShortTermTag tag)

  wantedItems <- findUnknownObs xs

  let norig = length xs
      nwant = length wantedItems
      
      sorig = showInt norig

      -- we want to process "new" non-science observations differently
      -- to the rest, since OCAT does not appear to contain any
      -- data until they have happened (and then not for all non-science
      -- observations in the short-term schedule). 
      --
      -- So, filter out non-science observations later than now
      -- (this is an approximation).
      --
      isTodoNS si@ScheduleItem{..} =
        if not siScienceObs && _toUTCTime siStart >= time
        then Left si
        else Right si
      (todoNonScience, rest) = partitionEithers (map isTodoNS wantedItems)

      ntodo = length todoNonScience
      nrest = length rest
      stodo = showInt ntodo
      srest = showInt nrest

      addTodo =
        when (ntodo > 0) $ do
          put ("# todo processing " <> stodo <> " of " <> sorig <> " records")
          forM_ todoNonScience addNonScienceObsManual

      -- assume that if addRest is called that it will have something to do
      addRest = do
        put ("# std  processing " <> srest <> " of " <> sorig <> " records")
        let stsMap = M.fromList (map (\si -> (siObsId si, si)) rest)
  
        ocatsE <- liftIO (queryOCAT (M.keys stsMap))

        case ocatsE of
          Right ocats -> do
            okay <- forM (zip [1..] ocats) $ \(i, (obsid, mocat)) ->
              let hdr = "# [" <> showInt i <> "/" <> srest <> "] " <>
                        showInt (fromObsId obsid) <> " "

                  -- we are guaranteed that this partial function is safe
                  -- (until I rewrite this code ...)
                  sts = fromJust (M.lookup obsid stsMap)
                  
              in case mocat of
                Just ocat -> put (hdr <> "found")
                             >> addObsId time obsid sts ocat

                -- This could record a non-science obs as invalid, but
                -- not clear what the best approach is here
                Nothing -> put (hdr <> "NO DATA")
                           >> addInvalidObsId time obsid noDataInOCAT

            let ngood = length (filter id okay)
                nbad = length (filter not okay)
                
            when (nbad > 0) $
               put ("## There were " <> showInt nbad <> " problems")
            
            addShortTermTag time tag Nothing
            return (nbad, ngood)

          Left emsg -> addShortTermTag time tag (Just emsg)
                       >> return (nrest, 0)

      nothingWanted = addShortTermTag time tag Nothing
  
  -- do we have any records to parse?
  if nwant > 0
    then addTodo
         >> addRest
         >>= \(nb, ng) -> return (nb, ntodo + ng)
                          
    else nothingWanted >>
         return (0, 0)

    
-- | Return those items that need querying from OCAT.
--
--   It looks like it might be useful to preserve the knowledge whether
--   an ObsId is a science or non-science observation downstream, which
--   is why the return is [ScheduleItem] and not [ObsIdVal].
--
findUnknownObs ::
  (PersistBackend m, SqlDb (Conn m))
  => [ScheduleItem]
  -- ^ The set of observations we are interested in. It is assumed that
  --   this is a "small" number (value intentionally not defined).
  -> m [ScheduleItem]
  -- ^ The observations to query
findUnknownObs todo = do

  -- remove any "invalid" obsids
  invalids <- fmap ioObsId <$> getInvalidObsIds
  let invalidSet = Set.fromList invalids
  
      todo1 = filter (\ScheduleItem{..} -> siObsId `Set.notMember` invalidSet)
              todo

      getInfo ScheduleItem{..} =
        (if siScienceObs then Right else Left) siObsId
                                  
      (ns, sc) = partitionEithers (map getInfo todo1)

  -- The logic for whether we want to re-query an obsid is unclear:
  --
  -- for science observations, it depends if adding a "new" schedule
  -- or a "historical" one, since the decision to skip an obsid
  -- depends on this AND THIS CODE DOES NOT YET IMPLEMENT THIS
  -- DISTINCTION
  --
  -- for engineering/non-science observations, it is tricky because
  -- it is not obvious when we can query OCAT, or "trust" the
  -- results.
  --
  -- Note that some observations can be marked as "observed" and never
  -- "promoted" to archived.
  --
  let scField = SoObsIdField `in_` sc &&.
                SoStatusField `in_` [Archived, Observed]
      nsField = NsObsIdField `in_` ns
      
  scObs <- project SoObsIdField scField
  nsObs <- project NsObsIdField nsField

  -- remove the obsids we do not want to query; use todo1 since this
  -- has already filtered out any unknown obsids
  --
  let toRemove = Set.union (Set.fromList scObs) (Set.fromList nsObs)
      out = filter (\ScheduleItem{..} -> siObsId `Set.notMember` toRemove)
            todo1

  return out


updateScheduleFromPage ::
  (PersistBackend m, SqlDb (Conn m))
  => UTCTime
  -- ^ Time to add to the database to indicate the page was
  --   processed (approximate)
  -> Seq.Seq (ShortTermTag, T.Text)
  -- ^ The latest version of the short-term schedule (in
  --   descending order, so the first entry is the latest)
  -> m (Int, Int)
  -- ^ The number of failed and successful ObsId downloads.
updateScheduleFromPage tNow schedList = do
  
  schedInfo <- map snd <$> selectAll
  let todo = toList (wanted 1 stsTag schedInfo fst schedList)
      printErr = liftIO . T.hPutStrLn stderr

  case todo of
    [] -> return (0, 0)
    
    [tagInfo] -> do
      let tag = fst tagInfo
      liftIO (T.putStrLn ("###   page : " <> fromShortTermTag tag))
      res <- liftIO (downloadSchedulePage tagInfo)
      case toSchedule res of
        Right sts -> addToSchedule tNow tag sts

        Left pe -> do
          let emsg = T.pack (show pe)
              
          printErr ("# Error parsing " <> fromShortTermTag tag)
          printErr emsg
          -- addShortTermTag tNow tag (Just emsg)
          -- return (1, 0)
          -- halt rather than mark-as-bad as I don't want to accidentally
          -- mark pages we just can't parse yet as bad; this actually
          -- may be a better solution that continuing.
          printErr "# For now, halting execution"
          liftIO exitFailure

    _ -> do
      -- this is a programmer error
      printErr ("ERROR: expected 1, found " <>
                showInt (length todo) <> " pages")
      liftIO exitFailure


-- | Extract any new schedule information and add it to the
--   system.
--
--   If there's any problem downloading or parsing information
--   then the routine will error out.
--
run ::
  Natural  -- ^ The maximum number of schedule pages to process.
  -> IO ()
run nmax = do
  schedList <- downloadScheduleList
  when (Seq.null schedList)
    (hPutStrLn stderr "No pages read in!" >> exitFailure)

  putStrLn ("# Number of schedule pages: " ++ show (Seq.length schedList))
  -- forM_ schedList $ \(ShortTermTag {..}, slug) ->
  --   T.putStrLn (" tag=" <> fromShortTermTag <> " [" <> slug <> "]")

  -- Process each page as a db operation
  --
  let ntot = fromIntegral nmax :: Int
      
  res <- forM [1..ntot] $ \i -> do
    putStrLn ("## page " ++ show i <> " of " <> show ntot)

    tNow <- getCurrentTime
    runDb (updateScheduleFromPage tNow schedList)

  let (nbads, ngoods) = unzip res
      nbad = sum nbads
      ngood = sum ngoods
      
  putStrLn ""
  putStrLn ("Downloaded " ++ show ngood ++ " ObsIds")

  when (nbad > 0) (putStrLn ("There were " ++ show nbad ++
                             " error cases") >> exitFailure)
  

usage :: IO ()
usage = do
  pName <- T.pack <$> getProgName
  T.hPutStrLn stderr ("Usage: " <> pName <> " [n]")
  T.hPutStrLn stderr "\n       n defaults to 2"
  exitFailure
  
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> run 2
    [ns] -> case readMaybe ns of
      Just n-> run n
      _ -> usage
      
    _ -> usage
  
