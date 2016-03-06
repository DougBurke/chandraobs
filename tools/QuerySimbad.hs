{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Query SIMBAD for observations. This is to
--
--   a) catch new observations which we have not queried before
--
--   b) redo searches for old observations
--
--   not fully implemented yet
--
--   Usage:
--       querysimbad --cfa --cds --debug
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
--      database.
--


import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as L8
-- import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO)

import Data.Char (toLower)
import Data.Functor (void)
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, isNothing, listToMaybe)

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Data.Monoid ((<>), mconcat)
#else
import Data.Monoid ((<>))
#endif

import Data.Time (UTCTime, getCurrentTime)

import Database.Groundhog.Postgresql

import Network (withSocketsDo)
import Network.HTTP.Conduit

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Database (insertSimbadInfo
                , insertSimbadMatch
                , insertSimbadNoMatch
                , putIO
                , runDb)
import Types

-- | Try and clean up SIMBAD identifiers:
--
--   NAME xxx -> xxx (seen with NAME Chandra Deep Field South)
--
cleanupName :: String -> String
cleanupName s =
  if "NAME " `isPrefixOf` s
  then drop 5 s
  else s

-- | Convert the target field into the value to search
--   for in SIMBAD
--
cleanTargetName :: String -> String
cleanTargetName tgt = 
  let lc = map toLower
      toks = words tgt
  in unwords $ takeWhile (not . ("offset" `isPrefixOf`) . lc) toks

-- use ADS mirror first.
--
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
type SearchResults = (String, String, UTCTime)

querySIMBAD ::
  SimbadLoc
  -> Bool    -- ^ @True@ for debug output
  -> String  -- ^ object name
  -> IO (SearchResults, Maybe SimbadInfo)
querySIMBAD sloc f objname = do
  putStrLn ("Querying SIMBAD for " ++ objname)
  let -- we POST the script to SIMBAD
      script = mconcat [
                 "format object \"%MAIN_ID\t%OTYPE(3)\t%OTYPE(V)\t%COO(d;A D)\\n\"\n"
                 , "query id ", BS8.pack searchTerm, "\n" ]

      uri = simbadBase sloc <> "sim-script"

      -- TODO: "clean" the target name of known "issues"
      --       that make Simbad matches fail
      searchTerm = cleanTargetName objname

  when f (putStrLn ">> Script:" >> print script)

  cTime <- getCurrentTime
  req <- parseUrl uri
  let hdrs = ("User-Agent", "chandraobs-obscat") : requestHeaders req
      req' = urlEncodedBody [("script", script)] $ req { requestHeaders = hdrs }

  mgr <- newManager tlsManagerSettings
  rsp <- httpLbs req' mgr
  let body = L8.unpack (responseBody rsp)
      -- TODO: need to handle data that does not match expectations
      --       eg if there's an error field, display it and return nothing ...
      dropUntilNext c = drop 1 . dropWhile (not . c)
      ls = dropWhile null $ dropUntilNext ("::data::" `isPrefixOf`) (lines body)

  when f (putStrLn ">> Response:" >> putStrLn body >> putStrLn ">> object info:" >> print ls)
  let rval = listToMaybe ls >>= parseObject 
  -- TODO: should have displayed the error string so this can be ignored
  when (isNothing rval) (putStrLn " -- no match found" >> putStrLn " -- response:" >> putStrLn body)

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
parseObject :: String -> Maybe (String, SimbadType, String)
parseObject txt = 
  let toks = splitOn "\t" txt
      toT s = fromMaybe (error ("Simbad Type > 3 characters! <" ++ s ++ ">"))
                     $ toSimbadType s
  in case toks of
    [name, otype3, otype, _] -> Just (cleanupName name, toT otype3, otype)
    _ -> Nothing

slen :: [a] -> String
slen = show . length

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

-- not pulling in Lens for this, so hard code it for our needs
_2 :: (a, b, c) -> b
_2 (_, b, _) = b

blag :: (MonadIO m) => Bool -> String -> m ()
blag f = when f . putIO

-- | The flag is @True@ to get debug output from the @querySIMBAD@ calls.
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
updateDB :: SimbadLoc -> Bool -> IO ()
updateDB sloc f = withSocketsDo $ do
  case sloc of
    SimbadCfA -> putStrLn "# Using CfA SIMBAD mirror"
    SimbadCDS -> putStrLn "# Using CDS SIMBAD"

  putStrLn "# Querying the database"

  obs <- runDb (project SoTargetField
                (CondEmpty `orderBy` [Asc SoTargetField]
                 `distinctOn` SoTargetField))

  matchTargets <- runDb (project SmmTargetField CondEmpty)
  noMatchTargets <- runDb (project SmnTargetField CondEmpty)

  -- these numbers aren't that useful, since the number of
  -- obsids and targets aren't the same, but leave for now
  putStrLn ("# " ++ slen obs ++ " obsids / " ++ 
            slen matchTargets ++ " targets " ++
            slen noMatchTargets ++ " no match ")

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

  putStrLn ("# -> " ++ show (S.size unidSet) ++ " have no Simbad info")

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
      runDb $ case minfo of
               Just si -> do
                          blag f (">> inserting SimbadInfo for " ++ smiName si)
                          (key, cleanFlag) <- insertSimbadInfo si
                          blag f (">> and SimbadMatch with target=" ++ tname)
                          void (insertSimbadMatch (toM searchRes key))

                          -- TODO: is this correct?
                          when cleanFlag
                            (delete (SmnTargetField ==. smiName si))
      
               _ -> do
                 blag f (">> Inserting SimbadNoMatch for target=" ++ tname)
                 void (insertSimbadNoMatch (toNM searchRes))
                 return ()

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

toNM :: SearchResults -> SimbadNoMatch
toNM (a,b,c) = SimbadNoMatch {
                 smnTarget = a 
               , smnSearchTerm = b
               , smnLastChecked = c
               }

toM :: SearchResults -> DefaultKey SimbadInfo -> SimbadMatch
toM (a,b,c) k = SimbadMatch {
                  smmTarget = a
                , smmSearchTerm = b
                , smmInfo = k
                , smmLastChecked = c
                }

usage :: IO ()
usage = do
  pName <- getProgName
  hPutStrLn stderr ("Usage: " ++ pName ++ " --cfa --cds --debug")
  -- hPutStrLn stderr "\n       default is --cfa and no debug"
  hPutStrLn stderr "\n       default is --cds and no debug"
  exitFailure

-- For now don't bother reporting on the actual error
--
-- I've seen recent problems with the CfA mirror, so switch back to
-- CDS for now.
--
parseArgs :: [String] -> Maybe (SimbadLoc, Bool)
-- parseArgs = go (SimbadCfA, False)
parseArgs = go (SimbadCDS, False)
  where
    go ans [] = Just ans
    go (sloc, dbg) (x:xs) | x == "--cds" = go (SimbadCDS, dbg) xs
                          | x == "--cfa" = go (SimbadCfA, dbg) xs
                          | x == "--debug" = go (sloc, True) xs
                          | otherwise = Nothing

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Just (sloc, debug) -> updateDB sloc debug
    _ -> usage

