{-# LANGUAGE CPP #-}

-- | Report the currently running observation.

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Control.Applicative ((<$>))
#endif

import Data.Maybe (fromMaybe)
import Data.Time (getCurrentTime)

import Database (getObsInfo, reportSize, runDb)
import Types

main :: IO ()
main = do
  now <- getCurrentTime
  putStrLn ("The current time is: " ++ show now)
  res <- runDb (reportSize >> getObsInfo)

  case res of
    Nothing -> putStrLn "ERROR: no observation was found."
    Just oi -> reportOI oi

reportOI :: ObsInfo -> IO ()
reportOI oi = do
  let p m a = putStrLn m >> print a 
      noData = putStrLn "NO DATA FOUND"
      rep = either (p "# Non-science observation") (p "# Science observation")
      printObs xs = fromMaybe noData (rep <$> xs)
        
  putStrLn "\n### Current observation:"
  rep (oiCurrentObs oi)

  putStrLn "\n### Previous observation:"
  printObs (oiPrevObs oi)

  putStrLn "\n### Next observation:"
  printObs (oiNextObs oi)
