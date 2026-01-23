{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

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
--      Avoid un-needed queries (e.g. when querying the same source
--      multiple times), which can happen when handling multiple
--      names used by CAL for the same source (as an example).
--
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

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as L8
-- import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Set as S

import qualified Network.HTTP.Conduit as NHC

import Control.Monad (forM_, unless, when)

import Data.Functor (void)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Functor.Contravariant ((>$<))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)

import Rel8 ((==.)
            , DBOrd, Columns, Context, Name, Transpose, Table, TableSchema
            , Result, Query, Expr
            , Delete(..), Returning(..), Insert(..), OnConflict(..)
            , aggregate
            , asc
            , countStar
            , delete
            , distinctOnBy
            , each
            , in_
            , insert
            , lit
            , litExpr
            , orderBy
            , select
            , showDelete
            , values
            )

import Formatting (int, sformat)

import Hasql.Connection (Connection, release)

import Network.HTTP.Types.Header (Header)

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.Read (readMaybe)

import Database (updateLastModified
                , getConnection
                , getSize
                , runDb
                , runDbMaybe
                , runDb1
                , runDb_
                )
import Types (SimbadMatch(..)
             , SimbadNoMatch(..)
             , SimbadInfo(..)
             , SimbadLoc(..)
             , simbadBase
             , SimbadType
             , toSimbadType
             , SIMCategory(..)
             , SimbadInfoKey
             , toSI, siExpr
             , toSM, smExpr
             , toSNM, snmExpr
             , TargetName
             , fromTargetName
             , toTargetName
             , sorTarget
             , scienceObsRawSchema
             , simbadMatchSchema
             , simbadNoMatchSchema
             , simbadInfoSchema
              --
             , _2
             )

userAgent :: Header
userAgent = ("User-Agent", "chandraobs dburke@cfa.harvard.edu")

-- Database code was in lib/Database.hs, but it's very specialised
-- so has been moved here.

-- | Checks that the data is not known about before inserting it.
--
--   Returns the key for the item and a flag indicating whether
--   the key already exists (so previous SimbadNoMatch may need
--   to be deleted).
--
--   This will update the existing SimbadInfo record if it has
--   changed (i.e. the designation has changed). The flag
--   will be True in this case.
--
--   TODO: There's no reason to return a flag, as the action
--         needed should be done here.
--
insertSimbadInfo ::
  Connection
  -> SimbadInfo Result
  -> IO (SimbadInfoKey, Bool)
insertSimbadInfo conn sm = do

  -- There's only one constraint on SimbadInfo. Do we have to worry about
  -- conflicts? With Groundhog the code was written to try and over-write
  -- if there was a conflict, but this was not well tested and it's not
  -- clear that the code correctly handled related tables that referenced
  -- the smiId entry. So for now just fall over.
  --
  let ins = Insert { into = simbadInfoSchema
                   , rows = values [ siExpr sm ]
                   , onConflict = Abort
                   , returning = Returning smiId
            }

  res <- runDbMaybe conn (insert ins)
  case res of
    Just idval -> pure (idval, False)
    _ -> error "Unable to insert SimbadInfo" -- TODO
    

-- | Returns True if the database was updated.
--
--   This originally checked that the smnTargetField was not known,
--   but now it just checks the full row.
--
insertSimbadMatch :: Connection -> SimbadMatch Result -> IO Bool
insertSimbadMatch conn sm =
  let ins = Insert { into = simbadMatchSchema
                   , rows = values [ smExpr sm ]
                   , onConflict = DoNothing
                   , returning = Returning smmId
                   }

  in do
    vals <- runDb conn (insert ins)
    pure (not (null vals))


-- | Returns True if the database was updated.
--
--   This originally checked that the smnTargetField was not known,
--   but now it just checks the full row.
--
insertSimbadNoMatch :: Connection -> SimbadNoMatch Result -> IO Bool
insertSimbadNoMatch conn sm =
  let ins = Insert { into = simbadNoMatchSchema
                   , rows = values [ snmExpr sm ]
                   , onConflict = DoNothing
                   , returning = Returning smnId
                   }

  in do
    vals <- runDb conn (insert ins)
    pure (not (null vals))


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
--   * Special cases:
--       Cheshire Cat Lens -> Cheshire Cat
--
--   * NAME xxx -> xxx
--
--   * xxx (yyy) -> xxx
--     There are some times that the yyy may be more useful,
--     but assume that xxx is sufficient
--
--   * xxx/yyy -> xxx
--     These are used for multiple sources, but let's just
--     pick the first one.
--
cleanTargetName :: TargetName -> TargetName
cleanTargetName tgtName =
  let tgt = fromTargetName tgtName
      tgt1 = cleanupName tgt
      tgt2 = removeSeparator '(' tgt1
      tgt3 = removeSeparator '/' tgt2

  in toTargetName $ if tgt == "Cheshire Cat Lens"
                    then "Cheshire Cat"
                    else tgt3

{-

-- | Convert the target field into the value to search
--   for in SIMBAD. The checks are case insensitive.
--
--   The rules are built from looking at the names that
--   have been used and coming up with simple rules to
--   normalize the names. However, they are now out of date,
--   as the target names have been reworked.
--
--   *) Special case conversion from 'ArLac' to 'ar lac'
--      as this is an often-observed calibration target.
--      (and some other cal-type targets).
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
--   *) removes "[...]" if "[" is not the first charactre since this is
--      often used in CAL observations - e.g. "E0102-72[S2,-120,-5,0,0]"
--      although there is '0532-710 [N206]' but this doesn't resolve
--      in SIMBAD as the full name anyway.
--
--      This rule probably simplifies a bunch of other tests, but
--      have not evaluated them yet.
--
--   It could be that a multi-pass system is really needed
--   to deal with names that include nebula or filament
--   that potentially could be a valid identifier.
--
-- TODO: need a better scheme for the "special cases"
--
oldCleanTargetName :: TargetName -> TargetName
oldCleanTargetName tgtName =
  let tgt = removeSquareBrackets tgtName

      lc = T.toLower

      isOffset x = any (`T.isSuffixOf` x)
                   [",YOFF=2", ",YOFF=5", ",YOFF=10", ",YOFF=15",
                    ",ZOFF=2", ",ZOFF=5", ",ZOFF=10", ",ZOFF=15"]
      removeOffset = T.dropEnd 1 . T.dropWhileEnd (/= ',')
                       
  in case T.words tgt of
    [] -> ""

    -- special case names used by CAL
    [n] | lc n `elem` ["arlac", "arlac,hrc-s,ao2", "arlac,hrc-i,ao2a"] -> "Ar Lac"

    [n] | isOffset n -> toTargetName (removeOffset n)
          
    -- HDF NORTH is one where we don't want to remove the compass
    -- direction (although doesn't really make a difference to the
    -- search).
    --
    [a, b] | a == "HDF" && b == "NORTH" -> "HDF NORTH"

    [n, _, ao] | lc n == "vega," && lc ao == "ao2" -> "Vega"
    
    toks -> let (ltok:rtoks) = reverse toks

                -- Remove last token, if necessary
                lltok = lc ltok
                toks2 = if (lltok `elem` compassDirs) ||
                           ("hrc-" `T.isPrefixOf` lltok)
                        then reverse rtoks
                        else toks

                -- stopping on filament could be dangerous, but not
                -- seen anything problematic so far
                isSkip x = any (`T.isPrefixOf` lc x)
                           ["offset", "outskirt", "filament"]
                toks3 = takeWhile (not . isSkip) toks2

                (ltok3:rtoks3) = reverse toks3
                toks4 = if ltok3 == "-" then reverse rtoks3 else toks3

                -- Remove trailing [I3,-120,0,0,0] (and S3) seen with
                -- Betelgeuse. Could use T.stripSuffix instead, and
                -- could just focus on the last element of toks4
                --
                trail x = if any (`T.isSuffixOf` x)
                             ["[I3,-120,0,0,0]", "[S3,-120,0,0,0]"]
                          then T.dropEnd 15 x
                          else x
                toks5 = map trail toks4
    
                cleanName = T.unwords toks5
            in if "E0102-72" `T.isPrefixOf` cleanName 
               then toTargetName "1E 0102.2-7219"
               else if "CAS A," `T.isPrefixOf` cleanName ||
                       "CAS A[" `T.isPrefixOf` cleanName ||
                       "CAS A " `T.isPrefixOf` cleanName
                    then toTargetName "Cassiopeia A"
                    else if "G21.5-09" `T.isPrefixOf` cleanName ||
                            "G21.5-0.9" `T.isPrefixOf` cleanName
                         then toTargetName "PSR J1833-1034"
                         else toTargetName cleanName

-- | Compass directions to remove (in lower case).
compassDirs :: [T.Text]
compassDirs = ["ne", "se", "sw", "nw"
              , "north", "east", "south", "west"
              , "northeast", "southeast", "northwest", "southwest"]


-- Only clean cases like "xx [sdfsfsf" (no check for trailing ']');
-- we don't yet have any with multiple '[' characters, but leave
-- them in if we find them.
--
removeSquareBrackets :: TargetName -> T.Text
removeSquareBrackets = removeSeparator '[' . fromTargetName

-}

-- Remove (... text
removeSeparator :: Char -> T.Text -> T.Text
removeSeparator sep orig =
  let firstChar = maybe ' ' fst (T.uncons orig)
      cleaned = case T.split (== sep) orig of
        [x, _] -> T.strip x
        _ -> orig

  in if firstChar == sep then orig else cleaned


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

-- Do we have a planet name. This is not intended to be robust.
-- It is assumed that the target name has already been cleaned up.
--
isPlanet :: TargetName -> Bool
isPlanet tn =
  let txt = T.toLower (fromTargetName (cleanTargetName tn))
  in txt `elem` ["mercury", "venus", "mars", "jupiter",
                 "saturn", "uranus", "neptune", "pluto"]

-- Note that I fake planet searches to always fail, since we know that
-- SIMBAD does not support planetary bodies. This probably needs work.
--
querySIMBAD ::
  NHC.Manager
  -> SimbadLoc
  -> Bool        -- ^ @True@ for debug output
  -> TargetName  -- ^ object name
  -> Int         -- ^ current iteration number
  -> Int         -- ^ Maximum iteration number
  -> IO (SearchResults, Maybe (SimbadInfo Result))
querySIMBAD _ _ f objname _ _ | isPlanet objname = do
                                  -- special case planets
                                  when f (T.putStrLn (">> Hard-coding planet: " <> fromTargetName objname))
                                  cTime <- getCurrentTime
                                  let searchRes = (objname, objname, cTime)
                                  pure (searchRes, Nothing)

querySIMBAD mgr sloc f objname cur total = do
  T.putStrLn ("[" <> showInt cur <> "/" <> showInt total <>
              "] Querying SIMBAD for " <> fromTargetName objname)
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

  req <- NHC.parseRequest uri

  let hdrs = userAgent : NHC.requestHeaders req
      req' = req { NHC.requestHeaders = hdrs }
      req'' = NHC.urlEncodedBody [("script", script)] req'

  rsp <- NHC.httpLbs req'' mgr
  let body = decodeUtf8 (L8.toStrict (NHC.responseBody rsp))
  
      -- TODO: need to handle data that does not match expectations
      --       eg if there's an error field, display it and return nothing ...
      dropUntilNext c = drop 1 . dropWhile (not . c)
      ls = dropWhile T.null $ dropUntilNext (("::data::" :: T.Text) `T.isPrefixOf`) (T.lines body)

  when f (T.putStrLn ">> Response:" >> T.putStrLn body >> T.putStrLn ">> object info:" >> print ls)
  let rval = listToMaybe ls >>= parseObject
      
  let searchRes = (objname, searchTerm, cTime)

  case rval of
    Just (sName, sType3, sType) ->
      pure (searchRes, Just (toSI sName sType3 sType))

    Nothing -> do
      -- TODO: should have displayed the error string so this can be ignored
      T.putStrLn " -- no match found"
      -- >> T.putStrLn " -- response:" >> T.putStrLn body
      pure (searchRes, Nothing)


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
      Just (toTargetName (cleanupName name), toT otype3, SIMCategory otype)
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


blag :: Bool -> T.Text -> IO ()
blag f = when f . T.putStrLn


getDistinct :: (Columns (Transpose Expr names) ~ Columns names,
                Context names ~ Name,
                Context (Transpose Expr names) ~ Expr,
                Transpose Name names ~ names,
                Transpose Name (Transpose Expr names) ~ names,
                Transpose Expr (Transpose Expr names) ~ Transpose Expr names,
                Table Expr (Transpose Expr names),
                Table Name names,
                DBOrd a) =>
               TableSchema names
               -> (Transpose Expr names -> Expr a)
               -> Query (Expr a)
getDistinct schema proj =
            fmap proj
            $ distinctOnBy proj (proj >$< asc)
            $ each schema
            

-- | All the target names (sorted).
--
allTargetsQ :: Query (Expr TargetName)
allTargetsQ = getDistinct scienceObsRawSchema sorTarget

-- | Targets that have a SIMBAD match (sorted).
--
matchTargetsQ :: Query (Expr TargetName)
matchTargetsQ = getDistinct simbadMatchSchema smmTarget

-- | Targets that do not have a SIMBAD match (sorted).
--
noMatchTargetsQ :: Query (Expr TargetName)
noMatchTargetsQ = getDistinct simbadNoMatchSchema smnTarget


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
-- There is no attempt to support running on Windows (although
-- this is only relevant if using network pre 2.6.1.0).
--
updateDB :: SimbadLoc -> Maybe Int -> Bool -> IO ()
updateDB sloc mndays f = do
  case sloc of
    SimbadCfA -> T.putStrLn "# Using CfA SIMBAD mirror"
    SimbadCDS -> T.putStrLn "# Using CDS SIMBAD"

  T.putStrLn "# Querying the database"

  conn <- getConnection

  obsTargets <- runDb conn (select allTargetsQ)
  matchTargets <- runDb conn (select matchTargetsQ)
  noMatchTargets <- runDb conn (select noMatchTargetsQ)

  -- Do steps A and B - ie identify those fields for which
  -- we have no Simbad information.
  --
  -- The *Targets fields should have no duplicates, but go via
  -- a set to make it easier to combine.
  --
  let allSet = S.fromList obsTargets
      matchSet = S.fromList matchTargets
      noMatchSet = S.fromList noMatchTargets

      delMatchSet = matchSet `S.difference` allSet
      delNoMatchSet = noMatchSet `S.difference` allSet

      setLen = showInt . S.size

  -- these numbers aren't that useful, since the number of
  -- obsids and targets aren't the same, but leave for now
  T.putStrLn ("# " <> setLen allSet <> " unique targets / " <>
              setLen matchSet <> " names " <>
              setLen noMatchSet <> " no match ")

  T.putStrLn ("# - " <> setLen delNoMatchSet <> " no-matches to delete")
  T.putStrLn ("# - " <> setLen delMatchSet <> " matches to delete")

  let deleteEntry label schema elems proj = unless (null elems) $ do
        let del = Delete { from = schema
                         , using = pure ()
                         , deleteWhere = \_ row -> proj row `in_` (fmap litExpr (S.toList elems))
                         -- , returning = NoReturning
                         , returning = Returning proj
                         }

        deleted <- runDb conn (delete del)
        T.putStrLn ("# cleanup: removed " <> T.pack (show (length deleted)) <> " " <> label)
        when (length deleted /= S.size elems) $
          T.putStrLn "### WARNING: numbers do not match"

  deleteEntry "no-matches" simbadNoMatchSchema delNoMatchSet smnTarget
  deleteEntry "matches" simbadMatchSchema delMatchSet smmTarget

  -- How can we clean up the SimbadInfo table? The only real way
  -- is that if smmTarget is no longer valid then we can delete
  -- the SimbadMatch and **if no other matches** the associated
  -- SimBadInfo entry. We have deleted the SimbadMatch entry
  -- above, but how do we clean out un-referenced items?
  --
  -- This is not quick, so only enable when I want to check.
  -- It has not been updated to use Rel8
  {-
  matches :: [SimbadMatch] <- select CondEmpty
  minfos <- mapM get (smmInfo <$> matches)
  let infos = catMaybes minfos

  allInfos <- project SmiNameField CondEmpty

  let wantSet = S.fromList (map smiName infos)
      haveSet = S.fromList allInfos

      delSet = haveSet `S.difference` wantSet

  unless (S.null delSet) $ do
    put ("# cleanup: removing " <> setLen delSet <> " simbad records")
    forM_ delSet $ \n -> delete (SmiNameField ==. n)
  -}

  -- Update the last-modified date if necessary
  --
  unless (S.null delNoMatchSet && S.null delMatchSet {- && S.null delSet -}) $
    getCurrentTime >>= updateLastModified conn

  let unidSet = allSet `S.difference` (matchSet `S.union` noMatchSet)
      nUnid = S.size unidSet

  -- NOTE: no attempt to cleanup the SimbadInfo table, which is
  --       going to be a bit awkward to do as we remove the SimbadMatch
  --       fields
  unless (nUnid == 0) $
    T.putStrLn ("# -> " <> showInt nUnid <> " have no Simbad info")

  -- Process each request separately.
  --
  mgr <- NHC.newManager NHC.tlsManagerSettings

  forM_ (zip [1..] (S.toList unidSet)) $ 
    \(ctr, tgt) -> do
      (searchRes, minfo) <- querySIMBAD mgr sloc f tgt ctr nUnid
      let tname = _2 searchRes
          tnameT = fromTargetName tname

      case minfo of
        Just si -> do
          blag f (">> inserting SimbadInfo for " <> fromTargetName (smiName si))
          (key, cleanFlag) <- insertSimbadInfo conn si

          blag f (">> and SimbadMatch with target=" <> tnameT)
          inserted <- insertSimbadMatch conn (toM searchRes key)
          when (inserted) $ 
              getCurrentTime >>= updateLastModified conn

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
          {-XXXX to restore
          when cleanFlag $ do
            let getTargetQ = do
              row <- each simbadNoMatchSchema
              where_ (smnTarget row ==. lit (smiName si))
              pure row
              
            T.putStrLn "### NOTE: is this correct?"
            -- display what we are deleting, as a check
            mans <- runDbMaybe conn (query getTargetQ)
            case mans of
              (Just SimbadNoMatch {..}) -> do
                let stxt = "Target: <"
                           <> fromTargetName smnTarget
                           <> "> search term: <"
                           <> smnSearchTerm
                           <> "> at "
                           <> T.pack (show smnLastChecked)

                    del = Delete { from = from simbadNoMatchSchema
                                 , using = pure ()
                                 , deletWhere \_ row -> smnTargetField row ==. lit (smiName si)
                                 , returning = NoReturning
                                 }
                    
                T.putStrLn ("&&&&& deleting " <> stxt)
                runDb con (delete dal)
                delete (SmnTargetField ==. smiName si)

              Nothing -> pure ()
          XXX-}
          
        _ -> do
          blag f (">> Inserting SimbadNoMatch for target=" <> tnameT)
          inserted <- insertSimbadNoMatch conn (toNM searchRes)
          when (inserted) $ 
              getCurrentTime >>= updateLastModified conn
        
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
  case mndays of
    Just mdays -> updateOldRecords conn mgr sloc mdays f
    _ -> pure ()

  release conn


-- Technically should look for "updated search term" matches
-- even when ndays == Nothing, but easier to use the same logic.
--
updateOldRecords :: Connection -> NHC.Manager -> SimbadLoc -> Int -> Bool -> IO ()
updateOldRecords conn mgr sloc ndays f = do

  T.putStrLn "\n"
  T.putStrLn ">>> Searching for old and updated records"
  T.putStrLn (">>>   ndays = " <> showInt ndays)

  let qry = orderBy (smnLastChecked >$< asc)
            $ each simbadNoMatchSchema
  noMatchFields <- runDb conn (select qry)

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
      -- compiler at the moment. It is also not clear whether the
      -- logic here is actually correct ...
      --
      isChanged SimbadNoMatch{..} =
        fromTargetName (cleanTargetName smnTarget) /= smnSearchTerm
      changed = filter isChanged (dropWhile isOld noMatchFields)

  T.putStrLn ("# Old queries to be redone: " <> slen old)
  T.putStrLn ("# Changed queries:          " <> slen changed)

  -- TODO: is todo guaranteed to not contain repeated elements?
  let todo = old ++ changed
      ntodo = length todo

  -- This is *very* similar to the previous version
  --
  -- NOTE: the changed handling does not appear to work
  --       (look for "Pluto (134340)").
  --
  forM_ (zip [1..] todo) $ 
    \(ctr, smn) -> do
      T.putStrLn (" [" <> showInt ctr <> "]  target='" <> fromTargetName (smnTarget smn) <> "'")
      
      (searchRes, minfo) <- querySIMBAD mgr sloc f (smnTarget smn) ctr ntodo
      let tname = _2 searchRes
          tnameT = fromTargetName tname

      let del = Delete { from = simbadNoMatchSchema
                       , using = pure ()
                       , deleteWhere = \_ row -> smnTarget row ==. lit (smnTarget smn)
                       , returning = NoReturning
                       }
      runDb_ conn (delete del)

      case minfo of
        Just si -> do
          blag f (">> Adding SimbadInfo for " <> fromTargetName (smiName si))
          (key, cleanFlag) <- insertSimbadInfo conn si
          
          when cleanFlag (T.putStrLn "&&&&&&& errr, need to delete something")
          blag f (">> and SimbadMatch with target=" <> tnameT)
          void (insertSimbadMatch conn (toM searchRes key))

        _ -> do
          blag f (">> Updating SimbadNoMatch for target=" <> tnameT)
          void (insertSimbadNoMatch conn (toNM searchRes))

      -- Try and update the database per change. Is this sensible?
      getCurrentTime >>= updateLastModified conn

  let nUnknown1 = length noMatchFields
      countNoMatch = getSize simbadNoMatchSchema
      
  nUnknown2 <- fromIntegral <$> runDb1 conn (select countNoMatch)

  unless (nUnknown2 == nUnknown1) $ do
    T.putStrLn ""
    T.putStrLn ("# Total number of unknown: "
                <> showInt nUnknown1 <> " -> " <> showInt nUnknown2)


toNM :: SearchResults -> SimbadNoMatch Result
toNM (a,b,c) = toSNM a (fromTargetName b) c

toM :: SearchResults -> SimbadInfoKey -> SimbadMatch Result
toM (a,b,c) k = toSM a (fromTargetName b) k c


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
        (y1:ys) -> case readMaybe y1 of
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
