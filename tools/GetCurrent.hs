
-- | Report the currently running observation.

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO, liftIO)

import Database.Groundhog.Postgresql

import Data.Maybe (fromMaybe)
import Data.Time (getCurrentTime)

import Database (getObsInfo)
import Types

reportSize :: (MonadIO m, PersistBackend m) => m ()
reportSize = do
  n1 <- countAll (undefined :: ScheduleItem)
  liftIO $ putStrLn $ "Number of scheduled items: " ++ show n1
  n2 <- countAll (undefined :: ScienceObs)
  liftIO $ putStrLn $ "Number of science obs    : " ++ show n2
  n3 <- countAll (undefined :: NonScienceObs)
  liftIO $ putStrLn $ "Number of non-science obs: " ++ show n3

main :: IO ()
main = do
  now <- getCurrentTime
  putStrLn $ "The current time is: " ++ show now
  res <- withPostgresqlConn "user=postgres password=postgres dbname=chandraobs host=127.0.0.1" $ 
    runDbConn $ do
      handleMigration
      reportSize
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
    
