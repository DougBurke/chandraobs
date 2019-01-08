{-# LANGUAGE OverloadedStrings #-}

-- | Search on SIMBAD object type, including "no SIMBAD info was identified".
--
--   There are two views (for those with a SIMBAD type):
--      - just that type
--      - the type and all the children of that type
--
--   For the "has no SIMBAD type" there's obviously no children.
--
module Views.Search.Types (indexPage, dependencyPage
                          , matchPage
                          , matchDependencyPage
                          , renderDependencyJSON) where

import qualified Prelude as P
import Prelude ((.), ($), (==), (+), Int, Maybe(Just)
               , compare, error, fst, lookup, mapM_, maybe, mconcat
               , null, snd, sum, uncurry, unzip)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Aeson ((.=))
import Data.Function (on)
import Data.List (groupBy, intersperse, sortBy)

import Data.Monoid ((<>))

import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (title)

import API (typeLinkSearch
           , typeDLinkSearch
           , typeDLinkURI
           , categoryLinkSearch
           , jsScript , cssLink)
import Layout (defaultMeta, d3Meta
              , dquote, floatableTable)
import Types (RestrictedSchedule
             , SimbadType
             , fromSimbadType
             , SimbadTypeInfo
             , SimbadCode
             , sc1
             , sc2
             , sc3
             , scLevel
             , SIMCategory
             , simbadLabels
             , noSimbadLabel
             , _2)
import Utils (getNumObsRestricted
             , getScienceTimeRestricted
             , toJSVarObj
             )
import Views.Record (CurrentPage(..), singleColBody)
import Views.Render (standardRestrictedSchedulePage
                    , standardExplorePage)

-- | A simple tabular view of the explicit object types
indexPage :: 
  [(SimbadTypeInfo, Int)]
  -> Html
indexPage objs =
  let hdrTitle = "Chandra observations by object type"
      bodyBlock = renderTypes objs
      mid = Just "explorebox"
      mcss = Just "/css/simbad.css"
  in standardExplorePage mcss hdrTitle bodyBlock mid


-- | Show the "full" object hierarcy (at least, as much as we have)
--   and allow the user to zoom around in it.
--
--   TODO: how can I add in the unidentified sources?
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
     (singleColBody CPExplore
      ((div ! id "explorebox") (renderDependency objs)))

-- TODO: combine with Schedule.schedPage

matchPage :: 
  SimbadTypeInfo
  -> RestrictedSchedule
  -> Html
matchPage typeInfo sched =
  let hdrTitle = "Chandra observations of " <> H.toHtml lbl
      lbl = niceType typeInfo

      pageTitle = if lbl == noSimbadLabel
                  then "Unidentified sources"
                  else toHtml lbl

      mainBlock = renderMatches lbl sched []
      
  in standardRestrictedSchedulePage sched CPExplore hdrTitle pageTitle mainBlock

matchDependencyPage :: 
  [SimbadTypeInfo] -- ^ guaranteed not to be empty
  -> RestrictedSchedule
  -> Html
matchDependencyPage typeInfos sched =
  let hdrTitle = "Chandra observations of " <> H.toHtml lbl
      typeInfo0 = P.head typeInfos
      lbl = niceType typeInfo0

      pageTitle = if lbl == noSimbadLabel
                  then "Unidentified sources"
                  else toHtml lbl

      mainBlock = renderMatches lbl sched (P.tail typeInfos)
      
  in standardRestrictedSchedulePage sched CPExplore hdrTitle pageTitle mainBlock


niceType :: (SimbadType, SIMCategory) -> SIMCategory
niceType (s, l) | fromSimbadType s == "reg" = "Area of the sky"
                | P.otherwise               = l


renderMatches ::
  SIMCategory          -- ^ SIMBAD type, as a string
  -> RestrictedSchedule
  -> [SimbadTypeInfo]  -- ^ children of this type included in the page (if any)
  -> Html
renderMatches lbl sched children = 
  let scienceTime = getScienceTimeRestricted sched
      nobs = toHtml (getNumObsRestricted sched)
      schedLink = ", and the format used here is the same as that of the "
                  <> (a ! href "/schedule") "schedule view"
                  <> "."

      -- TODO: rewrite, re-position, and make links
      toLink = P.uncurry typeDLinkSearch 
      typeLbls = intersperse ", " (P.map toLink children)
      childTxt = case typeLbls of
        [] -> ""
        [c] -> "; this includes the " <> c <> " type"
        _ -> "; the following types are included: "
             <> mconcat typeLbls
      
      -- TODO: improve English here
      introText =
        if lbl == noSimbadLabel
        then [ "This page shows the Chandra observations of objects for "
             , "which no identification could be found in SIMBAD"
             , scienceTime
             , ". This "
             , "includes "
             , categoryLinkSearch "SOLAR SYSTEM" ("Solar System" :: T.Text)
             , " objects, since SIMBAD does not track "
             , "these objects, but most of the objects could not be identified "
             , "from the target name supplied by the Observer. The current system "
             , "used to match to SIMBAD is very simple, and so misses out on "
             , "a large number of "
             , dquote "obvious"
             , " matches, but there are also a lot of target fields which are "
             , "hard to match to SIMBAD."
             , nobs
             , schedLink
             ]

        else [ "This page shows the observations of "
             , toHtml lbl
             , " objects by Chandra"
               -- TODO: improve the English here; need to rework childTxt
               --       now added the total observation time.
             , scienceTime
             , childTxt
             , ". The object type is based on the target name created by the "
             , "observer, and is often not sufficient to identify it in "
             , (a ! href "http://cds.u-strasbg.fr/cgi-bin/Otype?X") "SIMBAD"
             , ", which is why not all observations have a type (it is also "
             , "true that the Chandra field of view is large enough to contain "
             , "more objects than just the observation target!). "
               -- assume the schedule is all science observations
             , nobs
             , schedLink
             ]
                  
  in p (mconcat introText)

nameInfo :: H.Html
nameInfo =
  "The target names set by the proposal writers were used to identify " <>
  "the object types using " <>
  (a ! href "http://cds.u-strasbg.fr/cgi-bin/Otype?X") "the SIMBAD database"
  <> ". "

-- | Render the list of object types, and include a link to the "unidentified"
--   source, but at the moment this is separate, since I do not have a good
--   count of the number of targets in the latter case, rather than number
--   of observations.
--
renderTypes ::
  [(SimbadTypeInfo, Int)]
  -> Html
renderTypes objs = 
  let toRow (sti,n) = tr (tdObject sti <> tdNum n)
                        
      tdObject = td . uncurry typeLinkSearch
      tdNum n = (td ! A.title (toValue lbl)) (toHtml n)

      lbl = "Number of objects" :: T.Text

      tableHead = tr (th "Object Type" <>
                      th (toHtml lbl))

      unidRow =
        td ((a ! href "/search/type/unidentified") unidLabel) <>
        (td ! A.title (toValue lbl)) "lots"
        
      tableBody = 
        tr unidRow <>
        mapM_ toRow sobjs

      sobjs = sortBy (compare `on` (snd.fst)) objs

      unidLabel = "Unidentified sources"

      pText = 
        nameInfo
        <> "Not all objects could be found, in which case the object "
        <> "was added to the "
        <> dquote unidLabel
        <> " category (this includes so-called serendipitous sources, "
        <> "that is, those sources that are near-enough to the target to also "
        <> "be observed by Chandra). The table below does not indicate the "
        <> "SIMBAD hierarchy; to see how these object types are related visit "
        <> (a ! href "/search/dtype/") "the SIMBAD dendogram view"
        <> "."
      
  in div (p pText
          <> floatableTable (thead tableHead <> tbody tableBody))


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
      conv (sc, st, txt) =
        maybe (out st txt sc 0) (out st txt sc) (lookup st user2)
  in P.map conv simbadLabels

toTree ::
  [((SimbadType, T.Text), SimbadCode, Int)]
  -- ^ this is assumed to be the full hierarchy, so can have elements
  --   with a count of zero.
  -> Aeson.Value
toTree sl =
  let -- just hard code things for now
      g x = groupBy ((==) `on` (x . _2))
      g1 = g sc1
      g2 = g sc2
      g3 = g sc3

      c1 = g1 sl

      short = fromSimbadType . fst
      mkLink = B8.unpack . typeDLinkURI . fst

      addChildren xs cs obj =
        Aeson.object (if null xs then obj else "children" .= cs : obj)

      makeObj st sc ntot =
        let lvl = scLevel sc
        in [ "name" .= snd st
           , "level" .= lvl
           , "shortName" .= short st
           , "searchLink" .= mkLink st
           , "size" .= ntot ]

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
        
  in Aeson.object [ "name" .= ("all" :: T.Text),
                    "children" .= P.map toChild1 c1 ]


-- | Create the SIMBAD dependency graph and render it.
--
renderDependency ::
  [(SimbadTypeInfo, Int)]
  -> Html
renderDependency objs = 
  let xs = toTree (addCode objs)
        
      svgBlock = do
        (div ! id "tree") ""
        toJSVarObj "typeinfo" xs

      -- TODO: would like to add in a link to the unidentified sources
      pText =
        nameInfo
        <> "Not all objects could be found, so the following list is "
        <> "incomplete (and does not include so-called serendipitous sources, "
        <> "that is, those sources that are near-enough to the target to also "
        <> "be observed by Chandra). The "
        <> (a ! href "https://en.wikipedia.org/wiki/Dendrogram") "dendogram view"
        <> " is used to show the SIMBAD hierarchy. Selecting a circle will "
        <> "close or open the children of the item (i.e. those types that are "
        <> "more specific than the selected item); a filled circle shows that "
        <> "the item has been closed (or hidden). "
        -- note what the numbers show and how the links work
        <> "The number after a name indicates the number of objects that "
        <> "have this type, or are a descendent of this type, and "
        <> "selecting the type will show the observations of these "
        <> "objects. "
        <> "For a simpler view, which just lists the SIMBAD types but does "
        <> "not show the hierarchy, is available at "
        <> (a ! href "/search/type/") "the SIMBAD types view"
        <> "."

  in div (p pText <> svgBlock)

-- | for testing; may want to support something like this more generally
renderDependencyJSON ::
  [(SimbadTypeInfo, Int)]
  -> Aeson.Value
renderDependencyJSON = toTree . addCode
