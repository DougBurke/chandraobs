{-# LANGUAGE CPP #-}

-- | Report the currently running observation.

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Control.Applicative ((<$>))
#endif

import Database.Groundhog.Postgresql

import Data.Maybe (fromMaybe)
import Data.Time (getCurrentTime)

import Database (getObsInfo, reportSize)
import Types

main :: IO ()
main = do
  now <- getCurrentTime
  putStrLn $ "The current time is: " ++ show now
  res <- withPostgresqlConn "user=postgres password=postgres dbname=chandraobs host=127.0.0.1" $ 
    runDbConn $ do
      handleMigration
      _ <- reportSize
      getObsInfo

  case res of
    Nothing -> putStrLn "ERROR: no observation was found."
    Just oi -> reportOI oi

reportOI :: ObsInfo -> IO ()
reportOI oi = do
  let p m a = putStrLn m >> print a 
      rep = either (p "# Non-science observation") (p "# Science observation")

  putStrLn "\n### Current observation:"
  rep $ oiCurrentObs oi
  
  putStrLn "\n### Previous observation:"
  fromMaybe (putStrLn "NO DATA FOUND") $ rep <$> oiPrevObs oi

  putStrLn "\n### Next observation:"
  fromMaybe (putStrLn "NO DATA FOUND") $ rep <$> oiNextObs oi
    
