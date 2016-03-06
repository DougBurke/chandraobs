{-# LANGUAGE OverloadedStrings #-}

--
-- Convert the JSON geo files into the coords used in the sky
-- projection: alpha -> 180 - alpha and then reverse the array.
--
-- This is a hack and so hard-coded based on the inputs:
--    Milky Way
--    Constellation boundaries
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

//DSOs
{
  "type": "FeatureCollection",
  "features": [{
    "type": "Feature",
    "id": "",       // short designator
    "properties": {
      "name": "",   // most common designator
      "type": "",   // object type: gg, g, s, s0, ds, i, e, oc, gc, dn, bn, sfr, rn, pn, snr
      "mag": "",    // apparent magnitude, 999 if n.a.
      "dim": ""     // angular dimensions in arcminutes
    },
    "geometry": {  
      "type": "Point",
      "coordinates": []
    }
  }, { } ]
}

//Boundaries
{
  "type": "FeatureCollection",
  "features": [{
    "type": "Feature",
    "id": "",           // 3-letter designator of contellation
    "geometry": {  
      "type": "Polygon",
      "coordinates": [[[],[]]]
    },
  }, { } ]
}

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
--   This will error out if the structure does not match (rather than
--   indicating this in the return type).
--
hackObj :: A.Object -> A.Object
hackObj = HM.mapWithKey hackMap

hackMap :: T.Text -> A.Value -> A.Value
hackMap k (A.Object ohm) | k == "geometry" =
                             let gt = ohm HM.! "type"  -- will error out
                             in A.Object (HM.mapWithKey (hackCoordMap gt) ohm)
                         | otherwise = A.Object (HM.mapWithKey hackMap ohm)
hackMap k a@(A.Array oa) | k == "features" = A.Array (V.map hackArray oa)
                         -- | k == "coordinates" = A.Array (V.map changeCoords3 oa)
                         | otherwise = a
hackMap _ v = v

hackCoordMap :: A.Value -> T.Text -> A.Value -> A.Value
hackCoordMap gType k ov =
  if k == "coordinates"
  then let startFunc = case gType of
             A.String "Point" -> changeCoords0
             A.String "Polygon" -> changeCoords2
             A.String "MultiPolygon" -> changeCoords3
             _ -> error ("Unsupported geometry type: " ++ show (gType))
       in startFunc ov
  else ov

hackArray :: A.Value -> A.Value
hackArray (A.Object ohm) = A.Object (HM.mapWithKey hackMap ohm)
hackArray v = v

changeCoords3 :: A.Value -> A.Value
changeCoords3 (A.Array xs) = A.Array (V.map changeCoords2 xs)
changeCoords3 ys = error ("coord3: Expected vector, got " ++ show ys)

changeCoords2 :: A.Value -> A.Value
changeCoords2 (A.Array xs) = A.Array (V.map changeCoords1 xs)
changeCoords2 ys = error ("coord2: Expected vector, got " ++ show ys)

changeCoords1 :: A.Value -> A.Value
changeCoords1 (A.Array xs) = A.Array (V.reverse (V.map changeCoords0 xs))
changeCoords1 ys = error ("coord1: Expected vector, got " ++ show ys)

changeCoords0 :: A.Value -> A.Value
changeCoords0 (A.Array xs) = A.Array (hackCoords xs)
changeCoords0 ys = error ("coord0: Expected vector, got " ++ show ys)

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
    
