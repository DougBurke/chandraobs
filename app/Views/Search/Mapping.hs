{-# LANGUAGE OverloadedStrings #-}

-- | Show the mapping between proposal and object cateories.
--
--   Unlike many of the other pages this relies on AJAX to load
--   the data on the page, so there's not much here.
--

module Views.Search.Mapping (indexPage) where

-- import qualified Prelude as P
import Prelude (($), Maybe(Nothing))

import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>))

import Text.Blaze.Html5 hiding (map, title)
import Text.Blaze.Html5.Attributes hiding (title)

import Utils (defaultMeta, d3Meta, renderFooter
             , jsScript, cssLink
             , basicTypeLinkSearch
             , categoryLinkSearch
             , dquote
             )
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
      <> (div ! id "explorebox") renderMatches
      <> renderFooter
     )

renderMatches :: Html
renderMatches = 
  div ! A.id "mappingBlock" $ do
    h2 "Breaking down the time spent in each Chandra category"

    p ("The left-hand column shows the "
       <> (a ! href "/search/category/") "proposal categories"
       <> " and the right-hand column the "
       <> (a ! href "/search/type/") "SIMBAD types"
       <> " for the objects observed by Chandra. "
       <> "The connections between the two indicate the amount of time "
       <> "spent observing these sources; they can be selected to view "
       <> "the observations, and if you hover over a band "
       <> "then you can find out the number of sources as well as the "
       <> "observing time. Similarly, hovering over the bands on the "
       <> "left and right-hand sides will display the total time for "
       <> "that category (left) or SIMBAD type (right), and the names "
       <> "are links to view the respective sub-samples. "
       <> "Not all objects have a SIMBAD type due to the "
       <> "way I match them up (and this is in large part because the target name "
       <> "field created by the observers is not designed for machine lookups, but "
       <> "for the observer), and these have been assigned to the "
       <> dquote (basicTypeLinkSearch Nothing)
       <> " category; as you can see, there are a lot of them! "
       <> "This group also contains all the "
       <> categoryLinkSearch "SOLAR SYSTEM" ("Solar System" :: T.Text)
       <> " observations, since SIMBAD does not include solar-system objects in its "
       <> "database. ")

    p ("The format used for this display is known as a "
       <> (a ! href "http://en.wikipedia.org/wiki/Sankey_diagram") "Sankey diagram"
       <> ". The bands at the left and right can be dragged vertically "
       <> "to aid visibility in case the layout algorithm does not "
       <> "do a good job. As a reminder, this only covers a "
       <> (a ! href "/search/calendar/") (em "small" <> " fraction")
       <> " of the Chandra archive, and the results are not guaranteed to be "
       <> "correct!")

    -- TODO: add some sort of 'page needs javascript' text element
    (div ! id "mapping") ""

