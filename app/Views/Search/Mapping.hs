{-# LANGUAGE OverloadedStrings #-}

-- | Show the mapping between proposal and object cateories.

module Views.Search.Mapping (indexPage) where

import qualified Prelude as P
import Prelude (($), (+), (++), Int, String, fst, length, snd, zip)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Map.Strict as M

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Aeson ((.=))
import Data.Functor (void)
import Data.List (nub)
import Data.Maybe (fromJust)
import Data.Monoid ((<>), mconcat)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Utils (defaultMeta, d3Meta, renderFooter
             , jsScript, cssLink)
import Views.Record (CurrentPage(..), mainNavBar)

indexPage ::
  M.Map (String, String) Int
  -- ^ The proposal category, SIMBAD type, and counts.
  -> Html
indexPage cts =
  let jsLoad = "createMapping(mapinfo);"
  in docTypeHtml ! lang "en-US" $
    head (H.title "Chandra observations: what goes where?"
          <> defaultMeta
          <> d3Meta
          <> jsScript "/js/d3.sankey.js"
          <> jsScript "/js/mapping-view.js"
          <> cssLink "/css/mapping.css"
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    (body ! onload jsLoad)
     (mainNavBar CPExplore
      <> renderMatches cts
      <> renderFooter
     )

renderMatches ::
  M.Map (String, String) Int
  -> Html
renderMatches cts = 
  let svgBlock = do
        div ! id "mapping" $ ""
        script ! type_ "text/javascript" $ do
          void "var mapinfo = "
          toHtml (LB8.unpack (Aeson.encode jsonCts))
          ";"

      names = M.keys cts
      propNames = nub (P.map fst names)
      simNames = nub (P.map snd names)

      nprop = length propNames
      zero = 0 :: Int
      propMap = M.fromList (zip propNames [zero..])
      simMap = M.fromList (zip simNames [nprop, nprop+1..])
      
      makeName n = Aeson.object [ "name" .= n ]
      getVal n m = fromJust (M.lookup n m)
      makeLink ((prop,stype), c) =
        Aeson.object [ "source" .= getVal prop propMap
                     , "target" .= getVal stype simMap
                     , "value" .= c ]
                   
      jsonCts = Aeson.object [
        "nodes" .= P.map makeName (propNames ++ simNames)
        , "links" .= P.map makeLink (M.toList cts)
        ]
                
  in div ! A.id "mappingBlock" $ do
    h2 $ toHtml ("What kind of objects are looked at by Chandra?"::String)

    p $ mconcat
        [ "The left-hand column shows the "
        , (a ! href "/search/category/") "proposal categories"
        , " and the right-hand column the "
        , (a ! href "/search/type/") "SIMBAD categories"
        , " for the objects observed by Chandra "
        , "(since SIMBAD does not include solar-system objects, the "
        , "solar system category is not included)."
        , "The connections between the two show the number of "
        , "observations made; "
        , strong "note"
        , " that this is "
        , strong "not"
        , " the number of objects, as an object can be observed multiple "
        , "times by Chandra. "
        , "The format used for this display is known as a "
        , (a ! href "http://en.wikipedia.org/wiki/Sankey_diagram") "Sankey diagram"
        , "."
        ]

    svgBlock

