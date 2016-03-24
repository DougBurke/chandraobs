{-# LANGUAGE OverloadedStrings #-}

-- | Show the mapping between proposal and object cateories.
--
--   Unlike many of the other pages this relies on AJAX to load
--   the data on the page, so there's not much here.
--

module Views.Search.Mapping (indexPage) where

-- import qualified Prelude as P
import Prelude (($), String)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>), mconcat)

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Utils (defaultMeta, d3Meta, renderFooter
             , jsScript, cssLink)
import Views.Record (CurrentPage(..), mainNavBar)

indexPage :: Html
indexPage =
  let jsLoad = "createMapping();"
  in docTypeHtml ! lang "en-US" $
    head (H.title "Chandra observations: what goes where?"
          <> defaultMeta
          <> d3Meta
          <> jsScript "/js/d3.sankey.js"
          <> jsScript "/js/mapping-view.js"
          <> cssLink "/css/mapping.css"
          <> (cssLink "/css/main.css" ! A.title "Default")
          )
    <>
    (body ! onload jsLoad)
     (mainNavBar CPExplore
      <> renderMatches
      <> renderFooter
     )

renderMatches :: Html
renderMatches = 
  div ! A.id "mappingBlock" $ do
    h2 (toHtml ("What kind of objects are looked at by Chandra?"::String))

    p (mconcat
       [ "The left-hand column shows the "
       , (a ! href "/search/category/") "proposal categories"
       , " and the right-hand column the "
       , (a ! href "/search/type/") "SIMBAD types"
       , " for the objects observed by Chandra "
       , "(since SIMBAD does not include solar-system objects, the "
       , "solar system category is not included)."
       , "The connections between the two show the number of "
       , "observations made; "
       , strong "note"
       , " that this is "
       , strong "not"
       , " the number of objects, as an object can be observed multiple "
       , "times by Chandra. Not all objects have a SIMBAD type due to the "
       , "way I match them up (and this is in large part because the target name "
       , "field created by the observers is not designed for machine lookups, but "
       , "for the observer). "
       , "The names are links that will show the relevant subsample (namely the "
       , "objects in the proposal category or SIMBAD type). "
       , "The format used for this display is known as a "
       , (a ! href "http://en.wikipedia.org/wiki/Sankey_diagram") "Sankey diagram"
       , "."
       ])

    -- SVG goes here
    -- TODO: add some sort of 'page needs javascript' text element
    (div ! id "mapping") ""

