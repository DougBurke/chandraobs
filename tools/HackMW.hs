{-# LANGUAGE OverloadedStrings #-}

--
-- Convert the Milky Way outline into the coords used in the sky
-- projection: alpha -> 180 - alpha and then reverse the array.
--

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

{-

//Milky way
{
  "type": "FeatureCollection",
  "features": [{
    "type": "Feature",
    "id": "",       // ol 1-5
    "properties": {},
    "geometry": {
      "type": "MultiPolygon",
      "coordinates":[[[[],[]],[[],[]]]]
    }
  }, { } ]
}

-}

-- | Need to recurse down through the object copying over everything
--   until get to the features.geometry.coordinates field.
--
hackObj :: A.Object -> A.Object
hackObj = HM.mapWithKey hackMap

hackMap :: T.Text -> A.Value -> A.Value
hackMap _ (A.Object ohm) = A.Object (HM.mapWithKey hackMap ohm)
hackMap k a@(A.Array oa) | k == "features" = A.Array (V.map hackArray oa)
                         | k == "coordinates" = A.Array (V.map changeCoords1 oa)
                         | otherwise = a
hackMap _ v = v

hackArray :: A.Value -> A.Value
hackArray (A.Object ohm) = A.Object (HM.mapWithKey hackMap ohm)
hackArray v = v

--   "coordinates":
--       [ [ [[],[]], [[],[]] ] ]
--
changeCoords1 :: A.Value -> A.Value
changeCoords1 (A.Array xs) = A.Array (V.map changeCoords2 xs)
changeCoords1 ys = error ("coord1: Expected vector, got " ++ show ys)

changeCoords2 :: A.Value -> A.Value
changeCoords2 (A.Array xs) = A.Array (V.reverse (V.map changeCoords3 xs))
changeCoords2 ys = error ("coord2: Expected vector, got " ++ show ys)

changeCoords3 :: A.Value -> A.Value
changeCoords3 (A.Array xs) = A.Array (hackCoords xs)
changeCoords3 ys = error ("coord3: Expected vector, got " ++ show ys)

-- Assumed to be a 2-element array
hackCoords :: V.Vector A.Value -> V.Vector A.Value
hackCoords xs =
  let n = V.length xs
      x0 = case V.head xs of
        A.Number x -> x
        y -> error ("expected number, got " ++ show y)
      nxs = xs V.// [(0, A.Number (180.0 - x0))]
  in if n == 2
     then nxs
     else error ("hackCoords: Expected 2-element vector, got " ++
                 show n)

convert :: String -> IO ()
convert infile = do
  cts <- LB8.readFile infile
  let mobj = A.eitherDecode' cts
  case mobj of
    Right obj -> do
      let nobj = hackObj obj
          lbs = A.encode (A.Object nobj)

      LB8.putStrLn lbs

    Left msg -> do
      hPutStrLn stderr ("ERROR: unable to parse " ++
                        infile ++ " as JSON object")
      hPutStrLn stderr ("       " ++ msg)
      exitFailure
         
usage :: IO ()
usage = do
  pName <- getProgName
  hPutStrLn stderr ("Usage: " ++ pName ++ " infile")
  exitFailure
  
main :: IO ()
main = do
  args <- getArgs
  case args of
    [infile] -> convert infile
    _ -> usage
    
