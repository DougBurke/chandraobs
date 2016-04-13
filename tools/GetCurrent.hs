{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Report the currently running observation.

import qualified Data.Text as T
import qualified Data.Text.IO as T

#if (!defined(__GLASGOW_HASKELL__)) || (__GLASGOW_HASKELL__ < 710)
import Control.Applicative ((<$>))
#endif

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Time (getCurrentTime)

import Database (getObsInfo, reportSize, runDb)
import Types

main :: IO ()
main = do
  now <- getCurrentTime
  T.putStrLn ("The current time is: " <> T.pack (show now))
  res <- runDb (reportSize >> getObsInfo)

  case res of
    Nothing -> T.putStrLn "ERROR: no observation was found."
    Just oi -> reportOI oi

reportOI :: ObsInfo -> IO ()
reportOI oi = do
  let p m a = T.putStrLn m >> print a 
      noData = T.putStrLn "NO DATA FOUND"
      rep = either (p "# Non-science observation") (p "# Science observation")
      printObs xs = fromMaybe noData (rep <$> xs)
        
  T.putStrLn "\n### Current observation:"
  rep (oiCurrentObs oi)

  T.putStrLn "\n### Previous observation:"
  printObs (oiPrevObs oi)

  T.putStrLn "\n### Next observation:"
  printObs (oiNextObs oi)
