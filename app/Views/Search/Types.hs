{-# LANGUAGE OverloadedStrings #-}

-- | Search on SIMBAD object type.

module Views.Search.Types (indexPage, dependencyPage
                          , matchPage
                          , matchDependencyPage
                          , renderDependencyJSON) where

import qualified Prelude as P
import Prelude ((.), ($), (==), (+), Int, String
               , compare, error, fst, lookup, mapM_, maybe
               , null, snd, sum, uncurry, unzip)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Aeson ((.=))
import Data.Function (on)
import Data.Functor (void)
import Data.List (groupBy, intersperse, sortBy)
import Data.Monoid ((<>), mconcat)

import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (title)

import Types (Schedule(..), SimbadType(..), SimbadTypeInfo, SimbadCode(..)
             , simbadLabels, _2)
import Utils (defaultMeta, skymapMeta, d3Meta, renderFooter
             , jsScript , cssLink
             , typeLinkSearch
             , typeDLinkSearch
             , typeDLinkURI
             , getNumObs)
import Views.Record (CurrentPage(..), mainNavBar)
import Views.Render (makeSchedule)

-- | A simple tabular view of the explicit object types
indexPage :: 
  [(SimbadTypeInfo, Int)]
  -> Html
indexPage objs =
  docTypeHtml ! lang "en-US" $
    head (H.title "Chandra observations by object type"
          <> defaultMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    body
     (mainNavBar CPExplore
      <> renderTypes objs
      <> renderFooter
     )

-- | Show the "full" object hierarcy (at least, as much as we have)
--   and allow the user to zoom around in it.
--
dependencyPage :: 
  [(SimbadTypeInfo, Int)]
  -> Html
dependencyPage objs =
  docTypeHtml ! lang "en-US" $
    head (H.title "Chandra observations (dendogram view of SIMBAD objects)"
          <> defaultMeta
          <> d3Meta
          <> jsScript "/js/simbad-tree.js"
          <> cssLink "/css/simbad-tree.css"
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    (body ! onload "createTree(typeinfo);")
     (mainNavBar CPExplore
      <> renderDependency objs
      <> renderFooter
     )

-- TODO: combine with Schedule.schedPage

matchPage :: 
  SimbadTypeInfo
  -> Schedule  -- the observations that match this type, organized into a "schedule"
  -> Html
matchPage typeInfo sched =
  let lbl = niceType typeInfo
  in docTypeHtml ! lang "en-US" $
    head (H.title ("Chandra observations of " <> H.toHtml lbl)
          <> defaultMeta
          <> skymapMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    (body ! onload "createMap(obsinfo);")
     (mainNavBar CPExplore
      <> renderMatches lbl sched []
      <> renderFooter
     )

matchDependencyPage :: 
  [SimbadTypeInfo] -- ^ guaranteed not to be empty
  -> Schedule  -- the observations that match this type, organized into a "schedule"
  -> Html
matchDependencyPage typeInfos sched =
  let typeInfo0 = P.head typeInfos
      lbl = niceType typeInfo0

  in docTypeHtml ! lang "en-US" $
    head (H.title ("Chandra observations of " <> H.toHtml lbl)
          <> defaultMeta
          <> skymapMeta
          <> (cssLink "/css/main.css" ! A.title  "Default")
          )
    <>
    (body ! onload "createMap(obsinfo);")
     (mainNavBar CPExplore
      <> renderMatches lbl sched (P.tail typeInfos)
      <> renderFooter
     )

niceType :: (SimbadType, String) -> String
niceType (SimbadType "reg", _) = "Area of the sky"
niceType (_, l) = l

-- | TODO: combine table rendering with Views.Schedule
--
--   TODO: convert the long version of SimbadType to a nice string (may want to send in SimbadType here to match on)
renderMatches ::
  String           -- ^ SIMBAD type, as a string
  -> Schedule      -- ^ non-empty list of matches
  -> [SimbadTypeInfo]  -- ^ children of this type included in the page (if any)
  -> Html
renderMatches lbl (Schedule cTime _ done mdoing todo simbad) children = 
  let (svgBlock, tblBlock) = makeSchedule cTime done mdoing todo simbad

      -- TODO: rewrite, re-position, and make links
      toLink = P.uncurry typeDLinkSearch 
      typeLbls = intersperse ", " (P.map toLink children)
      childTxt = case typeLbls of
        [] -> "."
        [c] -> "; this includes the " <> c <> " type."
        _ -> "; the following types are included: "
             <> mconcat typeLbls <> "."
      

  in div ! A.id "scheduleBlock" $ do
    h2 (toHtml lbl)

    svgBlock

    -- TODO: improve English here
    p $ mconcat
        [ "This page shows the observations of "
        , toHtml lbl
        , " objects by Chandra"
        , childTxt
        , "The object type is based on the target name created by the "
        , "observer, and is often not sufficient to identify it in "
        , (a ! href "http://cds.u-strasbg.fr/cgi-bin/Otype?X") "SIMBAD"
        , ", which is why not all observations have a type (it is also "
        , "true that the Chandra field of view is large enough to contain "
        , "more objects than just the observation target!). "
          -- assume the schedule is all science observations
        , toHtml (getNumObs done mdoing todo)
        , ", and the format used here is the same as that of the "
        , (a ! href "/schedule") "schedule view"
        , "."
        ]

    tblBlock

-- | Render the list of object types
renderTypes ::
  [(SimbadTypeInfo, Int)]
  -> Html
renderTypes objs = 
  let toRow (sti,n) = tr $ do
                        td (uncurry typeLinkSearch sti)
                        td (toHtml n)

      sobjs = sortBy (compare `on` (snd.fst)) objs
      str :: String -> H.Html
      str = toHtml
  in div $ do
    p $ do
      str "The target names set by the proposal writers were used to identify "
      str "the object types using "
      (a ! href "http://cds.u-strasbg.fr/cgi-bin/Otype?X") "the SIMBAD database"
      str ". Not all objects could be found, so the following list is "
      str "incomplete (and does not include so-called serendipitous sources, "
      str "that is, those sources that are near-enough to the target to also "
      str "be observed by Chandra). The table below does not indicate the "
      str "SIMBAD hierarchy; to see how these object types are related visit "
      (a ! href "/search/dtype/") "the SIMBAD dendogram view"
      str "."
    table $ do
             thead $ tr $ do
               th "Object Type"
               th "Number of objects"
             tbody $
               mapM_ toRow sobjs


-- | Silently remove any for which there's no code, adds elements
--   with 0 values to match the full SIMBAD hierarchy, and
--   then groups by the simbad code.
--
addCode ::
  [(SimbadTypeInfo, Int)]
  -> [((SimbadType, T.Text), SimbadCode, Int)]
addCode user =
  let user2 = P.map (\((st, _), n) -> (st, n)) user
      out st txt sc n = ((st, txt), sc, n)
      conv (sc, st, txt) = maybe (out st txt sc 0) (out st txt sc) (lookup st user2)
  in P.map conv simbadLabels

toTree ::
  [((SimbadType, T.Text), SimbadCode, Int)]
  -- ^ this is assumed to be the full hierarchy, so can have elements
  --   with a count of zero.
  -> Aeson.Value
toTree sl =
  let -- just hard code things for now
      g x = groupBy ((==) `on` (x . _2))
      g1 = g _sc1
      g2 = g _sc2
      g3 = g _sc3

      c1 = g1 sl

      short = fromSimbadType . fst
      mkLink = B8.unpack . typeDLinkURI . fst

      addChildren xs cs obj =
        Aeson.object (if null xs then obj else "children" .= cs : obj)

      makeObj st sc ntot =
        let lvl = _scLevel sc
        in [ "name" .= snd st
           , "level" .= lvl
           , "shortName" .= short st
           , "searchLink" .= mkLink st
           , "size" .= ntot]

      -- the assumption is that the first element in this list is the level-1
      -- value
      toChild1 [] = error "empty list"
      toChild1 ((st,sc,n):xs) =
        let obj = makeObj st sc ntot
            l2 = g2 xs
            ntot = n + sum ns
            (ns, cs) = unzip (P.map toChild2 l2)
        in addChildren xs cs obj
        
      toChild2 [] = (0, Aeson.object [])
      toChild2 ((st,sc,n):xs) =
        let obj = makeObj st sc ntot
            l3 = g3 xs
            ntot = n + sum ns
            (ns, cs) = unzip (P.map toChild3 l3)
        in (ntot, addChildren xs cs obj)
        
      toChild3 [] = (0, Aeson.object [])
      toChild3 ((st,sc,n):xs) =
        let obj = makeObj st sc ntot
            ntot = n + sum ns
            (ns, cs) = unzip (P.map toChild4 xs)
        in (ntot, addChildren xs cs obj)
        
      toChild4 (st,sc,n) =
        let obj = makeObj st sc n
        in (n, Aeson.object obj)
        
  in Aeson.object [ "name" .= ("all" :: String),
                    "children" .= P.map toChild1 c1 ]

-- | Create the SIMBAD dependency graph and render it.
--
renderDependency ::
  [(SimbadTypeInfo, Int)]
  -> Html
renderDependency objs = 
  let xs = toTree (addCode objs)
        
      svgBlock = do
        div ! id "tree" $ ""
        script ! type_ "text/javascript" $ do
                   void "var typeinfo = "
                   toHtml (LB8.unpack (Aeson.encode xs))
                   ";"

      str :: String -> H.Html
      str = toHtml

  in div $ do
    p $ do
      str "The target names set by the proposal writers were used to identify "
      str "the object types using "
      (a ! href "http://cds.u-strasbg.fr/cgi-bin/Otype?X") "the SIMBAD database"
      str ". Not all objects could be found, so the following list is "
      str "incomplete (and does not include so-called serendipitous sources, "
      str "that is, those sources that are near-enough to the target to also "
      str "be observed by Chandra). The "
      (a ! href "https://en.wikipedia.org/wiki/Dendrogram") "dendogram view"
      str " is used to show the SIMBAD hierarchy. Selecting a circle will "
      str "close or open the children of the item (i.e. those types that are "
      str "more specific than the selected item); a filled circle shows that "
      str "the item has been closed (or hidden). "
      -- note what the numbers show and how the links work
      str "The number after a name indicates the number of objects that "
      str "have this type, or are a descendent of this type, and "
      str "selecting the type will show the observations of these "
      str "objects. "
      str "For a simpler view, which just lists the SIMBAD types but does "
      str "not show the hierarchy, is available at "
      (a ! href "/search/type/") "the SIMBAD types view"
      str "."

    svgBlock

-- | for testing; may want to support something like this more generally
renderDependencyJSON ::
  [(SimbadTypeInfo, Int)]
  -> Aeson.Value
renderDependencyJSON = toTree . addCode
  
