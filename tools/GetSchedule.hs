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

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Parsec.Error as PE
import qualified Text.Parsec.Pos as PP

import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (liftIO)

import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime, getCurrentTime)

import Database.Groundhog (PersistBackend
                            -- , (&&.)
                            --, (==.), (/=.), (||.), (>=.)
                          , project
                          , selectAll)
import Database.Groundhog.Postgresql (SqlDb, Conn, in_, insert_)

import Formatting (int, sformat)

import Network.HTTP.Conduit
import Numeric.Natural

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.HTML.TagSoup (Tag(..), (~==), innerText, parseTags, partitions)
import Text.Read (readMaybe)

import Database (runDb, getInvalidObsIds, updateLastModified
                 , insertProposal)
import OCAT (OCAT, isScienceObsE, noDataInOCAT
            , queryOCAT, ocatToScience, ocatToNonScience)
import Parser (parseSTS)

import Types (ChandraTime(..), ScheduleItem(..))
import Types (ShortTermTag(..), ShortTermSchedule(..), toShortTermTag)
import Types (NonScienceObs(..), ObsIdVal(..))
import Types (InvalidObsId(..), Field(..))
import Types (ObsIdStatus(..), TargetName(..))

-- The assumption is that all pages are accessible from 
-- baseLoc <> startPage and that they are in reverse
-- temporal order.
--
baseLoc, startPage :: String
baseLoc = "http://cxc.harvard.edu/target_lists/stscheds/"
startPage = "oldscheds.html"

getPage ::
  String  -- ^ page name (under baseLoc)
  -> IO T.Text
getPage name =
  decodeUtf8 . L.toStrict <$> simpleHttp (baseLoc <> name)
  
-- | Find out the full list of schedule pages, in reverse
--   temporal order (so first is newest).
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

  return (extractPages ps)

-- | Download the given schedule. The return value is the text
--   representation of the content (so no tags have been stripped).
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
      (Just ans, ys) -> go (s Seq.|> ans) ys
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


-- | Convert a schedule to a list of records.
--
--   The input text is the raw HTML of the page. There is
--   limited format support (i.e. it is hoped that the page
--   format is fixed, at least for the time range covered
--   here).
--
--   Argh:
--     http://cxc.harvard.edu/target_lists/stscheds/stschedAPR1311A.html
--   uses a HTML table; later versions used the simpler structure
--   that the code handles.
--
--   The parse error is not guaranteed to contain a useful
--   position.
--
toSchedule :: T.Text -> Either PE.ParseError [ScheduleItem]
toSchedule txt =
  let clean = removeHeader . T.unpack . innerText . parseTags

      emsg = PE.Message "Unable to remove the header"
      epos = PP.initialPos "from-file"
      pe = PE.newErrorMessage emsg epos
  in case clean txt of
    Just t -> parseSTS t
    Nothing -> Left pe


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


showInt :: Int -> T.Text
showInt = sformat int


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
  insert_ InvalidObsId { ioObsId = obsid
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
    Right (prop, sObs) -> insertProposal prop  -- may already exist
                          >> insert_ sObs  -- should not exist
                          >> return True
                 
     
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
    Left emsg -> insert_ InvalidObsId { ioObsId = obsid
                                      , ioChecked = t
                                      , ioMessage = emsg }
                 >> return False

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
      
  -- We are only really interested in this observation if it
  -- is not known about. The assumption is that we are just populating
  -- the database, so if we've already seen an ObsId then we don't
  -- need to re-query for it.
  --
  -- It's up to updateschedule to validate these fields (e.g.
  -- to check if they have changed since they were queried).
  --
  {-
  The original attempt before I had my epiphany to move all the
  complexity to updateschedule

  let scField = (SoObsIdField `in_` sc) &&.
                (SoStatusField `in_` [Archived, Observed, Discarded])
                
                {-
                (NOT ((SoStatusField ==. Observed) &&.
                      (SoPublicReleaseField >=. Just tNow)))
                -}
                
      -- what status values can non-science obs have?
      nsField = (NsObsIdField `in_` ns) &&.
                (NsStatusField `in_` [Archived, Observed, Discarded])
  -}

  let scField = SoObsIdField `in_` sc
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

  case todo of
    [] -> return (0, 0)
    
    [tagInfo] -> do
      let tag = fst tagInfo
      res <- liftIO (downloadSchedulePage tagInfo)
      case toSchedule res of
        Right sts -> addToSchedule tNow tag sts

        Left pe -> do
          liftIO (T.putStrLn ("# Error parsing " <> fromShortTermTag tag))
          let emsg = T.pack (show pe)
          liftIO (T.putStrLn emsg)
          addShortTermTag tNow tag (Just emsg)
          return (1, 0)

    _ ->
      -- this is a programmer error
      liftIO (T.hPutStrLn stderr ("ERROR: expected 1, found " <>
                                  showInt (length todo) <> " pages")
              >> exitFailure)


-- | Extract any new schedule information and add it to the
--   system.
--
--   If there's any problem downloading or parsing information
--   then the routine will error out.
--
runIt ::
  Natural  -- ^ The maximum number of schedule pages to process.
  -> IO ()
runIt nmax = do
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
    [] -> runIt 2
    [ns] -> case readMaybe ns of
      Just n-> runIt n
      _ -> usage
      
    _ -> usage
  
