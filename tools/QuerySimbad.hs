{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Query SIMBAD for observations. This is to
--
--   a) catch new observations which we have not queried before
--
--   b) redo searches for old observations
--
--   option b is not fully implemented yet, although there is now
--   the beginnings of support with the --ndays flag, but it needs
--   some thought.
--
--   Usage:
--       querysimbad --cfa --cds --debug --ndays <ndays>
--
--
--   TODO:
--      I have seen some recent runs returning:
--
--      Querying SIMBAD for SMC X-1
--       -- no match found
--       -- response:
--      !! A problem occured with the script file. It may be too large (max: 31457280 bytes) null/null/null
--
--      it looks like this is being taken as a "no data" match,
--      rather than ignoring it and not adding anything to the
--      database. I think that this is an issue with the CfA
--      mirror/proxy, so I have switched to using the CDS
--      version by default.
--


import qualified Data.ByteString.Lazy.Char8 as L8
-- import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Set as S

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Functor (void)
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)

import Database.Groundhog.Postgresql

import Formatting (int, sformat)

import Network (withSocketsDo)
import Network.HTTP.Conduit

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Database (insertSimbadInfo
                , insertSimbadMatch
                , insertSimbadNoMatch
                , updateLastModified
                , putIO
                , runDb)
import Types

-- | Try and clean up SIMBAD identifiers:
--
--   NAME xxx -> xxx (seen with NAME Chandra Deep Field South)
--
cleanupName :: T.Text -> T.Text
cleanupName s =
  if "NAME " `T.isPrefixOf` s
  then T.drop 5 s
  else s

-- | Convert the target field into the value to search
--   for in SIMBAD. The checks are case insensitive.
--
--   The rules are built from looking at the names that
--   have been used and coming up with simple rules to
--   normalize the names.
--
--   *) Special case conversion from 'ArLac' to 'ar lac'
--      as this is an often-observed calibration target.
--
--   *) remove trailing word of the form
--     NORTH SOUTH EAST WEST NE SE SW NW
--     NORTHEAST SOUTHEAST SOUTHWEST NORTHWEST
--     HRC-xxx (this is to catch HRC-1 .. for the
--     Baade's Window observations)
--
--   *) removes anything after the token beginning with
--         offset
--         outskirt
--
--   *) removes a last token of "-" (this can be
--      left behind from filtering "G296.5+10.0 - SW"
--      (although for this example it doesn't matter
--       since there doesn't appear to be a match)
--
--   It could be that a multi-pass system is really needed
--   to deal with names that include nebula or filament
--   that potentially could be a valid identifier.
--
cleanTargetName :: TargetName -> TargetName
cleanTargetName tgtName =
  let tgt = fromTargetName tgtName
      lc = T.toLower
  in case T.words tgt of
    [] -> ""
    -- special case names used by CAL
    [n] | lc n == "arlac" -> "Ar Lac"
    toks -> let (ltok:rtoks) = reverse toks

                -- Remove last token, if necessary
                lltok = lc ltok
                toks2 = if (lltok `elem` compassDirs) ||
                           ("hrc-" `T.isPrefixOf` lltok)
                        then reverse rtoks
                        else toks

                -- stopping on filament could be dangerous, but not
                -- seen anything problematic so far
                isSkip x = any (`T.isPrefixOf` lc x) ["offset", "outskirt", "filament"]
                toks3 = takeWhile (not . isSkip) toks2

                (ltok3:rtoks3) = reverse toks3
                toks4 = if ltok3 == "-" then reverse rtoks3 else toks3

                cleanName = T.unwords toks4
            in if "E0102-72" `T.isPrefixOf` cleanName 
               then TN "1E 0102.2-7219"
               else TN cleanName

-- | Compass directions to remove (in lower case).
compassDirs :: [T.Text]
compassDirs = ["ne", "se", "sw", "nw"
              , "north", "east", "south", "west"
              , "northeast", "southeast", "northwest", "southwest"]


-- Note that I choose to use a text-based format for returning the data,
-- rather than a VoTable, since it's easier to create a parser for the
-- former.
--
-- TODO:
--    look for opportunities like "blah blah offset[...]" where "blah blah"
--    is a match; of course, how do we then encode/match this to
--    ScienceObservation? How about "M31 BHXNe"
--

-- TODO: XXX how to clean up the name ??

-- object name, search term, and time of search; used to 
-- create the SimbadMatch/NoMatch fields, since SimbadMatch
-- requires the database key to create
type SearchResults = (TargetName, TargetName, UTCTime)

querySIMBAD ::
  SimbadLoc
  -> Bool        -- ^ @True@ for debug output
  -> TargetName  -- ^ object name
  -> IO (SearchResults, Maybe SimbadInfo)
querySIMBAD sloc f objname = do
  T.putStrLn ("Querying SIMBAD for " <> fromTargetName objname)
  let -- we POST the script to SIMBAD
      script = "format object \"%MAIN_ID\t%OTYPE(3)\t%OTYPE(V)\t%COO(d;A D)\\n\"\n"
               <> "query id "
               <> encodeUtf8 (fromTargetName searchTerm)
               <> "\n"

      uri = simbadBase sloc <> "sim-script"

      -- TODO: "clean" the target name of known "issues"
      --       that make Simbad matches fail
      searchTerm = cleanTargetName objname

  when f (T.putStrLn ">> Script:" >> print script)

  cTime <- getCurrentTime

#if defined(MIN_VERSION_http_client) && MIN_VERSION_http_client(0,4,31)
  req <- parseRequest uri
#else
  req <- parseUrl uri
#endif

  let hdrs = ("User-Agent", "chandraobs-obscat") : requestHeaders req
      req' = urlEncodedBody [("script", script)] $ req { requestHeaders = hdrs }

  mgr <- newManager tlsManagerSettings
  rsp <- httpLbs req' mgr
  let body = decodeUtf8 (L8.toStrict (responseBody rsp))
  
      -- TODO: need to handle data that does not match expectations
      --       eg if there's an error field, display it and return nothing ...
      dropUntilNext c = drop 1 . dropWhile (not . c)
      ls = dropWhile T.null $ dropUntilNext (("::data::" :: T.Text) `T.isPrefixOf`) (T.lines body)

  when f (T.putStrLn ">> Response:" >> T.putStrLn body >> T.putStrLn ">> object info:" >> print ls)
  let rval = listToMaybe ls >>= parseObject 
  -- TODO: should have displayed the error string so this can be ignored
  when (isNothing rval) (T.putStrLn " -- no match found" >> T.putStrLn " -- response:" >> T.putStrLn body)

  let searchRes = (objname, searchTerm, cTime)

  case rval of
    Just (sName, sType3, sType) -> 
        let si = SimbadInfo {
                          smiName = sName
                        , smiType3 = sType3
                        , smiType = sType
                        }
        in return (searchRes, Just si)

    _ -> return (searchRes, Nothing)

-- | Assume we have a line from SIMBAD using the script interface using the
--   format given in querySIMBAD.
parseObject :: T.Text -> Maybe (TargetName, SimbadType, SIMCategory)
parseObject txt = 
  let toks = T.splitOn "\t" txt
      toT s = fromMaybe
              (error ("Simbad Type > 3 characters! <" <> T.unpack s <> ">"))
              (toSimbadType s)
  in case toks of
    [name, otype3, otype, _] ->
      Just (TN (cleanupName name), toT otype3, otype)
    _ -> Nothing

slen :: [a] -> T.Text
slen = showInt . length

showInt :: Int -> T.Text
showInt = sformat int


{-
-- | Given a "sorted" list (on a), return
--   the data grouped by a. The return array
--   is not guaranteed to be ordered on a.
--
--   For now I do not take advantage of the
--   sorted nature of the input list.
--
groupSorted :: Eq a => [(a,b)] -> [(a, [b])]
groupSorted xs = 
  let f (a1,_) (a2,_) = a1 == a2
      g ys = let (as,bs) = unzip ys
             in (head as, bs)
  in map g $ groupBy f xs
-}

{-
--   This is a specialized version of
--   `Data.List.groupBy`, and hopefully more
--   efficient (but not tested).
--
groupSorted :: Eq a => [(a,b)] -> [(a, [b])]
groupSorted [] = []
groupSorted ((a,b):xs) = go a [b] [] xs
  where
    go a bs out [] = (a, bs) : out
    go a bs out ((a1,b1):xs)
      | a == a1   = go a (b1:bs) out xs
      | otherwise = go a1 [b1] ((a,bs):out) xs

-}

blag :: (MonadIO m) => Bool -> T.Text -> m ()
blag f = when f . putIO

-- | The flag is @True@ to get debug output from the @querySIMBAD@ calls.
--   The Maybe argument controls the number of days back to use for
--   updating the old versions: Nothing means none, 180 means ~ 6 months
--   back.
--
-- Need to identify
--
--    A  new observations for which we already have
--       a match (ie the target name has a
--       SimbadMatch/NoMatch record).
--
--    B  new observations for which there is
--       no SimbadMatch/SimbadNoMatch record.
--
--    C  "old" observations, which should be retried
--
--    D  those SimbadNoMatch records for which we have 
--       changed the name to search on
--
-- Not all options are currently supported, and it would be better
-- to do this within the database query, where possible.
--
-- Notes:
-- 
-- This code is not transactional, in that it relies on there
-- being no other database updates running when this one is.
--
updateDB :: SimbadLoc -> Maybe Int -> Bool -> IO ()
updateDB sloc mndays f = withSocketsDo $ do
  case sloc of
    SimbadCfA -> T.putStrLn "# Using CfA SIMBAD mirror"
    SimbadCDS -> T.putStrLn "# Using CDS SIMBAD"

  T.putStrLn "# Querying the database"

  obs <- runDb (project SoTargetField
                (CondEmpty `orderBy` [Asc SoTargetField]
                 `distinctOn` SoTargetField))

  matchTargets <- runDb (project SmmTargetField CondEmpty)

  noMatchTargets <- runDb (project SmnTargetField CondEmpty)

  -- these numbers aren't that useful, since the number of
  -- obsids and targets aren't the same, but leave for now
  T.putStrLn ("# " <> slen obs <> " obsids / " <>
              slen matchTargets <> " targets " <>
              slen noMatchTargets <> " no match ")

  -- Do steps A and B - ie identify those fields for which
  -- we have no Simbad information.

  {-
  let allTgs = groupSorted obs

      allSet = S.fromList $ map fst allTgs
  -}

  let allSet = S.fromList obs
      matchSet = S.fromList matchTargets
      noMatchSet = S.fromList noMatchTargets

      unidSet = allSet `S.difference` (matchSet `S.union` noMatchSet)

      -- tgs = filter ((`S.member` unidSet) . fst) allTgs

  T.putStrLn ("# -> " <> showInt (S.size unidSet) <> " have no Simbad info")

  -- Could do all the database changes at once, but let's see
  -- how this works out.
  --
  -- It would also be best to run all the SIMBAD queries at once, in
  -- terms of not having to create/tear down the HTTP manager, but
  -- leave that for a later revision (the assumption here is that
  -- there are not going to be too many calls to SIMBAD).
  --
  forM_ (S.toList unidSet) $ 
    \tgt -> do
      (searchRes, minfo) <- querySIMBAD sloc f tgt
      let tname = _2 searchRes
          tnameT = fromTargetName tname
      runDb $ do
      case minfo of
        Just si -> do
          blag f (">> inserting SimbadInfo for " <> fromTargetName (smiName si))
          (key, cleanFlag) <- insertSimbadInfo si

          blag f (">> and SimbadMatch with target=" <> tnameT)
          void (insertSimbadMatch (toM searchRes key))

          -- If a SimbadInfo structure already exists (and
          -- matches si) then cleanFlag will be True (if
          -- it doesn't match si then there's a run-time
          -- error, which is not very nice).
          --
          -- I have no idea what I meant by the following
          -- deletion: is this to indicate that a previously
          -- unknown source is now known, so we need to
          -- delete the SimbadNoMatch field? I am not 100%
          -- convinced that smiName si is the correct search
          -- term for SmnTargetField. In insertSimbadNoMatch,
          -- the smiName field is set to the first element
          -- of searchRes, which is the same as tgt. The
          -- SimbadInfo structure returned by querySIMBAD
          -- has the smiName field set to cleanupName ran
          -- on the name returned by SIMBAD. So it's not
          -- clear that if the description above is correct
          -- that it's doing what I want.
          --
          when cleanFlag $ do
            -- display what we are deleting, as a check
            mans <- select ((SmnTargetField ==. smiName si)
                            `limitTo` 1)
            case mans of
              (SimbadNoMatch {..}:_) -> do
                let stxt = "Target: <"
                           <> fromTargetName smnTarget
                           <> "> search term: <"
                           <> smnSearchTerm
                           <> "> at "
                           <> T.pack (show smnLastChecked)
                liftIO (T.putStrLn ("&&&&& deleting " <> stxt))
                delete (SmnTargetField ==. smiName si)

              _ -> return ()
      
        _ -> do
          blag f (">> Inserting SimbadNoMatch for target=" <> tnameT)
          void (insertSimbadNoMatch (toNM searchRes))

      -- Update the last-modified date after each transaction;
      -- in production this should not matter, as this tool should not
      -- be running against the production server, but for now
      -- do it "properly"
      --
      liftIO getCurrentTime >>= updateLastModified

  {-
  forM_ tgs $ 
    \(tgt, obsids) -> do
      (searchRes, minfo) <- querySIMBAD f tgt
      case minfo of
        Just si -> doDB $ do
                        key <- insert si
                        insertSimbadMatch $ toM searchRes key

        _ -> doDB $ insertSimbadNoMatch $ toNM searchRes
  -}

  -- Now try and do C and D; this is separated out of the above
  -- for ease of initial implimentation, but should try and
  -- amalgamate where possible.
  --
  updateOldRecords sloc mndays f


-- Technically should look for "updated search term" matches
-- even when ndays == Nothing, but easier to use the same logic.
--
updateOldRecords :: SimbadLoc -> Maybe Int -> Bool -> IO ()
updateOldRecords _ Nothing _ = return ()
updateOldRecords sloc (Just ndays) f = do

  T.putStrLn "\n"
  T.putStrLn ">>> Searching for old and updated records"
  T.putStrLn (">>>   ndays = " <> showInt ndays)

  noMatchFields <- runDb (select (CondEmpty
                                  `orderBy`
                                  [Asc SmnLastCheckedField]))

  tNow <- getCurrentTime
  let dTime = -1 * 3600 * 24 * ndays
      tOld = addUTCTime (fromIntegral dTime) tNow 
      isOld SimbadNoMatch{..} = smnLastChecked <= tOld
      old = takeWhile isOld noMatchFields

      -- those queries which are old enough that the logic
      -- used to "clean up" the query has changed; note that
      -- this need not match one-to-one with the old check,
      -- so can not just use (isOld || isChanged) in a
      -- takeWhile statement. However, since we know that the
      -- old ones are going to be re-queried, we can just limit
      -- the search to "new" cases. The takeWhile and dropWhile
      -- searched could be combined, but I leave that for the
      -- compiler at the moment.
      --
      isChanged SimbadNoMatch{..} =
        fromTargetName (cleanTargetName smnTarget) /= smnSearchTerm
      changed = filter isChanged (dropWhile isOld noMatchFields)

  T.putStrLn ("# Old queries to be redone: " <> (slen old))
  T.putStrLn ("# Changed queries: "          <> (slen changed))

  -- This is *very* similar to the previous version
  forM_ (old ++ changed) $ 
    \SimbadNoMatch{..} -> do
      (searchRes, minfo) <- querySIMBAD sloc f smnTarget
      let tname = _2 searchRes
          tnameT = fromTargetName tname
      runDb $ do
      -- delete the old result; it could be updated but easiest
      -- at the moment just to create a new one.
      delete (SmnTargetField ==. smnTarget)
      case minfo of
        Just si -> do
          blag f (">> Adding SimbadInfo for " <> fromTargetName (smiName si))
          (key, cleanFlag) <- insertSimbadInfo si
          when cleanFlag
            (liftIO (T.putStrLn "&&&&&&& errr, need to delete something"))
          blag f (">> and SimbadMatch with target=" <> tnameT)
          void (insertSimbadMatch (toM searchRes key))

        _ -> do
          blag f (">> Updating SimbadNoMatch for target=" <> tnameT)
          void (insertSimbadNoMatch (toNM searchRes))

      -- Update the last-modified date after each transaction;
      -- in production this should not matter, as this tool should not
      -- be running against the production server, but for now
      -- do it "properly"
      --
      liftIO getCurrentTime >>= updateLastModified
     
    
toNM :: SearchResults -> SimbadNoMatch
toNM (a,b,c) = SimbadNoMatch {
                 smnTarget = a 
               , smnSearchTerm = fromTargetName b
               , smnLastChecked = c
               }

toM :: SearchResults -> DefaultKey SimbadInfo -> SimbadMatch
toM (a,b,c) k = SimbadMatch {
                  smmTarget = a
                , smmSearchTerm = fromTargetName b
                , smmInfo = k
                , smmLastChecked = c
                }

usage :: IO ()
usage = do
  pName <- getProgName
  hPutStrLn stderr ("Usage: " ++ pName ++ " --cfa --cds --debug --ndays <int>")
  -- hPutStrLn stderr "\n       default is --cfa and no debug"
  hPutStrLn stderr "\n       default is --cds, no debug, and no days"
  exitFailure

-- For now don't bother reporting on the actual error
--
-- I've seen recent problems with the CfA mirror, so switch back to
-- CDS for now.
--
parseArgs :: [String] -> Maybe (SimbadLoc, Maybe Int, Bool)
-- parseArgs = go (SimbadCfA, False)
parseArgs = go (SimbadCDS, Nothing, False)
  where
    go ans [] = Just ans
    go (sloc, mndays, dbg) (x:xs)
      | x == "--cds" = go (SimbadCDS, mndays, dbg) xs
      | x == "--cfa" = go (SimbadCfA, mndays, dbg) xs
      | x == "--debug" = go (sloc, mndays, True) xs
      | x == "--ndays" = case xs of
        (y1:ys) -> case maybeRead y1 of
          nd@(Just _) -> go (sloc, nd, dbg) ys
          Nothing -> Nothing
        _ -> Nothing
      | otherwise = Nothing

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Just (sloc, mndays, debug) -> updateDB sloc mndays debug
    _ -> usage

