{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

--
-- Usage:
--    ./getcalendar infile
--
-- Aim:
--
-- Write, to calendar-<n>.svg, the calendar images from
-- https://chandraobservatory.herokuapp.com/search/calendar/ which
-- must have been saved as infile (since we need to evaluate the
-- javascript to create the SVG elements).
--

module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

import Control.Monad (forM_)

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (stderr)

import Text.HTML.TagSoup (Tag(..)
                         , isTagOpenName
                         , isTagCloseName
                         , fromAttrib
                         , parseTags
                         , partitions
                         , renderTags
                         )


-- some stuff taken from http://bl.ocks.org/mbostock/4060606
--
-- Hmm. I can't seem to set the background for some reason
csstexts :: [T.Text]
csstexts = [ "svg {"
           , "  background-color: white;"
           , "  font-family: sans-serif;"
           , "  font-size: 14px;"
           , "  shape-rendering: crispEdges;"
           , "}"
           , ".day {"
           , "  fill: rgba(153, 153, 153, 0.2);"
           , "  stroke: #ccc;"
           , "}"
           , ".month {"
           , "  fill: none;"
           , "  stroke: #000;"
           , "  stroke-width: 2px;"
           , "}"
           , ".q0-9 { fill:rgb(247,251,255); }"
           , ".q1-9 { fill:rgb(222,235,247); }"
           , ".q2-9 { fill:rgb(198,219,239); }"
           , ".q3-9 { fill:rgb(158,202,225); }"
           , ".q4-9 { fill:rgb(107,174,214); }"
           , ".q5-9 { fill:rgb(66,146,198); }"
           , ".q6-9 { fill:rgb(33,113,181); }"
           , ".q7-9 { fill:rgb(8,81,156); }"
           , ".q8-9 { fill:rgb(8,48,107); }"
           ]

-- We want to find <svg class="year" ...> elements
--
findSVGs :: [Tag T.Text] -> [(Int, [Tag T.Text])]
findSVGs tags =
  let want cls t = isTagOpenName "svg" t && fromAttrib "class" t == cls

      -- assume our elements are closed
      isOpen name = not . isTagCloseName name
      svg (year, els) =
        let orig:rest = takeWhile (isOpen "svg") els

            -- add a xmlns attribute
            fixed = case orig of
              TagOpen n a -> TagOpen n (changeAttr a)
              _ -> error "WAT"

            -- add the XMLNS link and tweak the height
            changeAttr a = tweakHeight a <>
                           [("xmlns", "http://www.w3.org/2000/svg")]

            tweakHeight = map conv
              where
                conv ("height", h_old) = ("height", h_new)
                  where
                    h = getInt h_old + cheight
                    h_new = T.pack (show h)

                conv other = other

            -- to simplify things we just add a g tag to
            -- shift by the height of the colorbar
            --
            gopen = TagOpen "g" [("transform",
                                  "translate(0," <>
                                  T.pack (show cheight) <>
                                  ")")]
            gclose = TagClose "g"
                
        in (year,
            [fixed] <> extra year <>
            [gopen] <> rest <> [gclose] <>
            [TagClose "svg"])

      -- hard code the CSS colours
      css = [ TagOpen "style" []
            , TagText (T.unlines csstexts)
            , TagClose "style"
            ]

      -- Extract the colorbar block and identify the height
      colorbar_svg = case partitions (want "colorbar") tags of
        x:_ -> takeWhile (isOpen "svg") x
        [] -> error "no colorbar found!"

      -- We also want to tweak the line
      --   "Number of observations started in a day:"
      -- to
      --   "Number of Chandra observations started in a day:"
      --
      (colorbar, cheight) = case colorbar_svg of
        (TagOpen "svg" svg_attr:TagOpen "g" g_attr:rest) ->
          let ntags = TagOpen "g" g_attr : takeWhile (isOpen "g") rest

              height_txt = getAttr svg_attr "height"

              height :: Int
              height = getInt height_txt

              swapText (TagText orig) =
                let cleaned = case orig of
                      "Number of observations started in a day:" -> "Number of Chandra observations started in a day:"
                      _ -> orig
                      
                in TagText cleaned
              swapText other = other
              
          in (map swapText ntags <> [TagClose "g"], height)
          
        _ -> error "Unexpected tags"

      getInt t = case T.decimal t of
        Right (h, x) | x == T.empty -> h
                     | otherwise -> error "extra term after int"
        _ -> error "unable to parse int"
        
      calendars = reverse (partitions (want "year") tags)

      addYear year = [TagOpen "g" [("transform", "translate(10,10)")],
                      TagOpen "text" [("x", "0"),
                                      ("y", "0"),
                                      ("text-anchor", "start"),
                                      ("dy", "1em"),
                                      ("class", "label")
                                     ],
                      TagText ("Year " <> T.pack (show year)),
                      TagClose "text",
                      TagClose "g"]

      extra year = css <>
                   -- for now do not show the year marker
                   -- addYear year <>
                   colorbar

  in map svg (zip [1::Int ..] calendars)


-- assumes attribute exists
getAttr :: [(T.Text, T.Text)] -> T.Text -> T.Text
getAttr attrs name = case lookup name attrs of
  Just x -> x
  Nothing -> error "Unable to find attribute"
  
  
process :: T.Text -> IO ()
process html = do
  let svg_args = findSVGs (parseTags html)
  putStrLn $ "Number: " <> show (length svg_args)
  forM_ svg_args $ \(n, svg) -> do
    let fname = "calendar-" <> idx <> ".svg"
        idx = (if n < 10 then "0" else "") <> show n
    T.writeFile fname (renderTags svg)
    putStrLn $ "Created: " <> fname
  

readFromFile :: FilePath -> IO()
readFromFile infile = T.readFile infile >>= process
  
usage :: IO ()
usage = do
  pName <- T.pack <$> getProgName
  T.hPutStrLn stderr ("Usage: " <> pName <> " infile")
  exitFailure


main :: IO ()
main = do
  args <- getArgs
  case args of
    [infile] -> readFromFile infile
    _ -> usage
