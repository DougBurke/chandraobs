{-# Language OverloadedStrings #-}
{-# Language TupleSections #-}

--
-- Download new short-term schedule files from the CXC server.
-- Other than checking the files can be parsed, it does *not*
-- add them to the database. Actually, it probably should,
-- so that it can act in a "what's new" mode.
--
-- The assumption is that once a page has been downloaded then
-- it is not going to be updated on the server; that is, we do
-- not have to worry about a downloaded page becoming invalid.
--
-- The data files are written to
--    store/
--
-- Issues
--  - to be written
--  - need to update InitDB to read in these files rather than
--    using the data in HackData
--  - not sure how to handle downloading "old" files.
--

import qualified Data.ByteString.Lazy as L
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T

import Control.Monad (forM_, when)

import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.List (isPrefixOf)
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8)

import Network.HTTP.Conduit

-- listDirectory is only in directory >= 1.2.5.0
-- import System.Directory (createDirectoryIfMissing, listDirectory)
import System.Directory (createDirectoryIfMissing, getDirectoryContents)
import System.Exit (exitFailure)
import System.IO (hPrint, hPutStrLn, stderr)

import Text.HTML.TagSoup (Tag(..), (~==), innerText, parseTags, partitions)
import Text.Parsec.Error (ParseError)

import Parser (parseSTS)
import Types (ChandraTime, NonScienceObs, ScheduleItem(..))

-- | Each schedule is refered to by a date and letter - e.g.
--   AUG2216B.
--
newtype STSId = STSId T.Text
              deriving Eq
                         
type STS = (ScheduleItem, Maybe NonScienceObs)

-- The assumption is that all pages are accessible from 
-- baseLoc <> startPage and that they are in reverse
-- temporal order.
--
baseLoc, startPage :: String
baseLoc = "http://cxc.harvard.edu/target_lists/stscheds/"
startPage = "oldscheds.html"

storeDir :: FilePath
storeDir = "store"

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
  IO (Seq.Seq (STSId, T.Text)) -- ^ The "names" and URLs for each schedule.
downloadScheduleList = do
  txt <- getPage startPage
  let tags = parseTags txt

      ctxt = " BEGIN LIST " :: T.Text
      ps = head (partitions (TagComment ctxt ~==) tags)

  return (extractPages ps)

-- | Download the given schedule. The return value is the text
--   representation of the content (so no tags have been stripped).
--
downloadSchedule ::
  (STSId, T.Text)
  -> IO T.Text
downloadSchedule = getPage . T.unpack . snd
  
type TagList = [Tag T.Text]

-- | Given a list of tags, extract all the page links and ids
--   from it.
--
--   No error checking; that is, if the format of the page
--   changes then the results are not guaranteed.
--
extractPages :: TagList -> Seq.Seq (STSId, T.Text)
extractPages = go Seq.empty
  where
    go s [] = s
    go s xs = case getLink xs of
      (Just ans, ys) -> go (s Seq.|> ans) ys
      (Nothing, ys) -> go s ys


-- | This strips out leading and trailing white space, which
--   appears to be unnescessary but left in, just in case.
--
toSTSId :: T.Text -> STSId
toSTSId = STSId . T.toUpper . T.strip

fromSTSId :: STSId -> T.Text
fromSTSId (STSId x) = x

getLink :: TagList -> (Maybe (STSId, T.Text), TagList)
getLink (TagOpen "A" attrs : TagText txt : TagClose "A" : xs)
  = ((toSTSId txt, ) <$> lookup "HREF" attrs, xs)
getLink (_ : xs) = (Nothing, xs)
getLink [] = (Nothing, [])

-- | What pages have already been downloaded. This creates the
--   storage directory if needed.
--
--   There's no definite form for the names, since most are of
--   the form 06AUG16A but older ones change the order - e.g.
--   APR2002A and older ones drop the letter (FEB0402).
--
--   There's even an incomplete month in JUL210A; this looks
--   like it could be slightly problematic as it goes
--   JUL0510B, JUL0210A, JUL210A, JUN2810A, but JUL0210A
--   is different to JUL210A.
--   
getDownloaded :: IO (Set.Set T.Text)
getDownloaded = do
  createDirectoryIfMissing False storeDir
  files <- getDirectoryContents storeDir
  let out = filter (`notElem` [".", ".."]) files
  return (Set.fromList (map T.pack out))

-- | Find the new pages to download.
--
--   This /only/ looks for new files, and assumes that the
--   input sequence is in descending "temporal" order,
--   even if the file names do not match.
--
findNew ::
  Seq.Seq (STSId, T.Text)
  -> Set.Set T.Text
  -> Seq.Seq (STSId, T.Text)
findNew scheds got = go Seq.empty (Seq.viewl scheds)
  where
    go out Seq.EmptyL = out
    go out (x Seq.:< xs) = if fromSTSId (fst x) `Set.member` got
                           then out
                           else go (out Seq.|> x) (Seq.viewl xs)


-- | Convert a schedule to a list of records.
--
--   The input text is the raw HTML of the page. There is
--   limited format support (i.e. it is hoped that the page
--   format is fixed, at least for the time range covered
--   here).
--
toSchedule :: T.Text -> Either ParseError [STS]
toSchedule = parseSTS . removeHeader . T.unpack . innerText . parseTags

--   The parser does not quite match the on-file format,
--   in that the "header" line (e.g. AUG2916A) is assumed
--   to start with a comment token (#) to match the format
--   used in HackData. The simplest change is to just skip
--   the first non new-line characters. Actually, also need
--   to skip the other header lines (strip out 
--
removeHeader :: String -> String
removeHeader txt =
  let ls = lines txt
      -- make sure have more consecutive '-' than expect
      -- for the sequence column (although those start with
      -- a space), and then we want to drop this header
      -- line
      (_:nls) = dropWhile (\l -> not ("--------" `isPrefixOf` l)) ls
  in unlines nls
    
-- | Return the time range covered by these schedule items.
--
--   This could be made more efficient by assuming that the
--   input is in reverse time order, but it's probably not
--   much of an improvement.
--
findTimeRange ::
  [STS]
  -- ^ This should not be an empty list
  -> (ChandraTime, ChandraTime)
  -- ^ Start and end time of this sequence.
findTimeRange [] = error "No schedule items found!"
findTimeRange scheds = (minimum stimes, maximum etimes)
  where
    stimes = fmap (siStart . fst) scheds
    etimes = fmap (siEnd . fst) scheds

-- | The three inputs are assumed to have the same length. It should be
--   enforced by the type, but too lazy to implement zip3.
--
importSchedules ::
  Seq.Seq (STSId, T.Text)
  -> Seq.Seq T.Text
  -> [[STS]]
  -> IO ()
importSchedules todo pages scheds = undefined

reportErrors :: [ParseError] -> IO ()
reportErrors emsgs = do
  hPutStrLn stderr "# ERRORS parsing responses"
  forM_ emsgs (hPrint stderr)
  exitFailure
  
-- | Extract any new schedule information and add it to the
--   system.
--
--   If there's any problem downloading or parsing information
--   then the routine will error out.
--
runIt ::
  Maybe Int  -- ^ The maximum number of new files to process
             --   (@Nothing@ means there is no limit).
  -> IO ()
runIt mmax = do
  schedList <- downloadScheduleList
  when (Seq.null schedList)
    (hPutStrLn stderr "No pages read in!" >> exitFailure)

  got <- getDownloaded
  let new = findNew schedList got
      nnew = Seq.length new
  when (nnew /= 0)
    (putStrLn ("# Found " ++ show nnew ++ " new page(s)"))

  -- It is assumed that the number of pages to download is small.
  --
  let todo = case mmax of
        Just n -> Seq.take n new
        _ -> new
        
  pages <- mapM downloadSchedule todo
  let escheds = partitionEithers (toList (fmap toSchedule pages))
  case escheds of
    ([], scheds) -> importSchedules todo pages scheds
    (emsgs, _) -> reportErrors emsgs
    
  return ()

  
main :: IO ()
main = runIt (Just 5)
  
