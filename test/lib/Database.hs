{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Database (getConnection
                , runDb_, runDb1, runDbMaybe
                , updateLastModified, updateLastModifiedS
                ) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Hasql.Session as S

import Control.Monad (guard)

import Data.Functor.Contravariant ((>$<))
import Data.List (uncons)
import Data.Maybe (fromMaybe)
import Data.Time.Calendar (Day(..))
import Data.Time.Clock (UTCTime(..))

import Hasql.Connection (Connection, Settings, acquire, settings)

import Network.URI (URI(..)
                   , URIAuth(..)
                   , parseAbsoluteURI
                   )

import Rel8 ( Statement, Serializable, Expr, Query, Result
            , Delete(..), Insert(..)
            , OnConflict(..) , Returning(..)
            , run, run_, run1, runMaybe
            , delete, insert, select
            , lit, orderBy, values
            , each
            , desc, nullsLast
            )

import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (stderr)

import Types (MetaData(..), metaDataSchema
             , fromMetaData, toMetaData)


-- See also Web.Heroku. Can we just not use the URL directly?
--
separateDatabase :: String -> Maybe Settings
separateDatabase = go
  where
    getPort (':':port) = Just port
    getPort _ = Nothing

    -- this is only in Data.List from base 4.19.0.0, so use
    -- the example from the documentation, as we expect the
    -- input to be finite.
    unsnoc xs = (\(hd, tl) -> (reverse tl, hd)) <$> uncons (reverse xs)

    isColon = (== ':')
    getUserInfo uinfo =
      let (user, rest) = break isColon uinfo
      in do
        guard (user /= "")
        (_, rest2) <- uncons rest
        (password, lastchar) <- unsnoc rest2
        guard (password /= "")
        guard (lastchar == '@')
        pure (user, password)

    getDb path = do
      (a, b) <- uncons path
      guard (a == '/')
      guard (b /= "")
      pure b

    go env = do
      uri <- parseAbsoluteURI env
      
      guard (uriScheme uri == "postgres:")
      auth <- uriAuthority uri
      (user, password) <- getUserInfo (uriUserInfo auth)
      port <- getPort (uriPort auth)
      db <- getDb (uriPath uri)

      let hostBS = B8.pack (uriRegName auth)
          userBS = B8.pack user
          portW = read port
          passBS = B8.pack password
          dbaseBS = B8.pack db
          
      pure (settings hostBS portW userBS passBS dbaseBS)
      

listify1 :: T.Text -> NE.NonEmpty T.Text
listify1 = NE.singleton

listify2 :: T.Text -> T.Text -> NE.NonEmpty T.Text
listify2 a b = a NE.:| [b]

quickError :: NE.NonEmpty T.Text -> IO a
quickError (f NE.:| rs) = do
  T.hPutStrLn stderr ("ERROR: " <> f)
  mapM_ (T.hPutStrLn stderr) rs
  exitFailure

quickError1 :: T.Text -> IO a
quickError1 = quickError . listify1

quickError2 :: T.Text -> T.Text -> IO a
quickError2 a = quickError . listify2 a


getSettings :: IO Settings
getSettings = do
  murl <- lookupEnv "DATABASE_URL"
  case murl of
    Just url -> case separateDatabase url of
      Just x -> pure x
      _ -> quickError2 "invalid DATABASE_URL" (T.pack url)
           
    _ -> quickError1 "no DATABASE_URL environment variable"
    

getConnection :: IO Connection
getConnection = do
  dbSettings <- getSettings
  resp <- acquire dbSettings
  case resp of
    Right conn -> pure conn
    Left (Just emsg) -> quickError1 (T.pack (B8.unpack emsg))
    _ -> quickError1 "Unable to connect to database"


---

-- | Run a query or two.
--
runDb ::
  Serializable exprs a
  => Connection
  -> Statement (Query exprs)
  -> IO [a]
runDb conn stmt = do
  res <- S.run (S.statement () (run stmt)) conn
  case res of
    Right ans -> pure ans
    Left err -> quickError2 "Database error:" (T.pack (show err))

runDb_ ::
  Connection
  -> Statement exprs
  -> IO ()
runDb_ conn stmt = do
  res <- S.run (S.statement () (run_ stmt)) conn
  case res of
    Right ans -> pure ans
    Left err -> quickError2 "Database error:" (T.pack (show err))

runDb1 ::
  Serializable exprs a
  => Connection
  -> Statement (Query exprs)
  -> IO a
runDb1 conn stmt = do
  res <- S.run (S.statement () (run1 stmt)) conn
  case res of
    Right ans -> pure ans
    Left err -> quickError2 "Database error:" (T.pack (show err))

-- | It is assumed the query returns 0 or 1 rows. This may be different
--   to the groundhog version (need to check).
--
runDbMaybe ::
  Serializable exprs a
  => Connection
  -> Statement (Query exprs)
  -> IO (Maybe a)
runDbMaybe conn stmt = do
  res <- S.run (S.statement () (runMaybe stmt)) conn
  case res of
    Right ans -> pure ans
    Left err -> quickError2 "Database error:" (T.pack (show err))

---

-- | Update the last-modified field with the time.
--
--   Some would say that this should be done by Postgres itself, with
--   triggers, and they'd be right.
--
updateLastModified :: Connection -> UTCTime -> IO ()
updateLastModified conn lastMod = runDb_ conn (updateLastModifiedS lastMod)

updateLastModifiedS :: UTCTime -> Statement ()
updateLastModifiedS lastMod =
  let new = toMetaData lastMod

      del = delete $ Delete
        { from = metaDataSchema
        , using = pure ()
        , deleteWhere = \_ _ -> lit True
        , returning = NoReturning
        }
      ins = insert $ Insert
        { into = metaDataSchema
        , rows = values [ lit new ]
        , onConflict = Abort
        , returning = NoReturning
        }

      query = del >> ins

  in query

  
-- | When was the database last modified.
--
getLastModified :: Connection -> IO (Maybe UTCTime)
getLastModified conn = runDbMaybe conn getLastModifiedS

getLastModifiedS :: Statement (Query (Expr UTCTime))
getLastModifiedS =
  let query = fmap mdLastModified
              $ orderBy (mdLastModified >$< desc)
              $ each metaDataSchema

  in select query

-- | The time used when no last-modified date can be accessed from
--   the database. Ideally would use the current time, as this seems
--   the safest, but just hard code a simple value (as this should not
--   be used anyware).
--
dummyLastMod :: UTCTime
dummyLastMod = UTCTime (ModifiedJulianDay 0) 0


getLastModifiedFixed :: Connection -> IO UTCTime
getLastModifiedFixed conn = fromMaybe dummyLastMod <$> getLastModified conn
