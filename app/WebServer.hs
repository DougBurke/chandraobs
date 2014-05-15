{-# LANGUAGE OverloadedStrings #-}

-- | A test webserver.
-- 
-- As this is a test server, all the HTML is crammed
-- into this module. Once I have worked out what I
-- want, things will get separated out and cleaned
-- up.
--
module Main where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

{-
import Control.Concurrent.STM (STM, TVar
                              , atomically, newTVar, readTVar
                              , writeTVar)

import Control.Monad.IO.Class (liftIO)
-}

import Data.Default (def)
import Data.Maybe (isJust)
import Data.Monoid ((<>), mconcat)

import Network.HTTP.Types (StdMethod(HEAD))
-- import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Handler.Warp (defaultSettings, setPort)

import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStrLn, stderr)

import Text.Blaze.Html.Renderer.Text

import Web.Scotty

import PersistentTypes
import HackData

readInt :: String -> Maybe Int
readInt s = case reads s of
              [(v,[])] -> Just v
              _ -> Nothing

production :: 
  Int  -- ^ The port number to use
  -> Options
production p = def { verbose = 0
                   , settings = setPort p defaultSettings }

development :: Options
development = def

uerror :: String -> IO ()
uerror msg = do
  hPutStrLn stderr $ "ERROR: " ++ msg
  hFlush stderr
  exitFailure

-- I use the presence of the PORT environment variable to decide
-- between production and test environments. The Scotty documentations
-- suggest calling setFdCacheDuration on the settings field, to change
-- the value from 0, but do not really explain the implications of why
-- it is set to 0 in the first place.
--
main :: IO ()
main = do
  mports <- lookupEnv "PORT"
  let eopts = case mports of
                Just ports -> case readInt ports of
                                Just port -> Right $ production port
                                _ -> Left $ "Invalid PORT argument: " ++ ports

                _ -> Right development
 
  case eopts of
    Left emsg -> uerror emsg
    Right opts -> scottyOpts opts webapp

webapp :: ScottyM ()
webapp = do

    -- Need to find out how the static directory gets copied
    -- over by cabal
    --
    -- middleware logStdoutDev-- An invalid port number foe
    middleware $ staticPolicy (noDots >-> addBase "static")

    get "/" $ redirect "/index.html"
    get "/index.html" $ fromBlaze introPage

    -- TODO: is this correct for HEAD; or should it just 
    --       set the redirect header?
    addroute HEAD "/" $ standardResponse >> redirect "/index.html"
    addroute HEAD "/index.html" standardResponse

    get "/about.html" $ fromBlaze aboutPage
    addroute HEAD "/about.html" standardResponse

    get "/wwt.html" $ fromBlaze (wwtPage currentRecord)

fromBlaze :: H.Html -> ActionM ()
fromBlaze = html . renderHtml

-- A placeholder in case we want to set up any 
-- response settings.
standardResponse :: ActionM ()
standardResponse = return ()

defaultMeta :: H.Html
defaultMeta = H.meta H.! A.httpEquiv "Content-Type"
                     H.! A.content "text/html; charset=UTF-8"

-- The uninformative landing page
introPage :: H.Html
introPage =
  let initialize = "initialize()"
  in H.docTypeHtml $
    H.head (H.title "Welcome" <>
            defaultMeta <>
            (H.script H.! A.src "js/main.js") "" <>
            H.link H.! A.href   "css/main.css"
                   H.! A.type_  "text/css" 
                   H.! A.rel    "stylesheet"
                   H.! A.title  "Default"
                   H.! A.media  "all"
            )
    <>
    (H.body H.! A.onload initialize)
     (H.p "Hello world!" <>
      H.p "The next observation is:" <>
      renderRecord currentRecord <>
      H.p ("Information on " <> 
           (H.a H.! A.href "http://burro.cwru.edu/Academics/Astr306/Coords/coords.html") "Astronomical coordinate systems" <>
           ".")
     )

-- The uninformative about page  
aboutPage :: H.Html
aboutPage = 
  H.docTypeHtml $
   let txt = "This is " <> H.em "not" <> 
             " the space-ninja rocket ship start-up you were looking for."
       lnk = "Back " <> (H.a H.! A.href "/index.html" $ "home") <> "."
   in H.head (H.title "About this mysterious site" <> 
              defaultMeta) <>
      H.body (H.p txt <> H.p lnk)

{-
-- | A simple counter that is persistent within a session but
--   not between sessions.
--
newtype Counter = Counter Integer
  deriving (Eq, Ord, Show)

-- | Return a new counter, set to 0
newCounter :: IO (TVar Counter)
newCounter = atomically $ newTVar $ Counter 0

-- | Increase the value by 1  and return
--   the new counter.
incCounter :: TVar Counter -> STM Counter
incCounter ctrV = do
  -- could use modifyTVar' but want to return
  -- the new value
  Counter oval <- readTVar ctrV
  let n = Counter $! oval + 1
  writeTVar ctrV n
  return n

-}

-- How to display a record? For now, just pick one
-- in the middle (ie that length testSchedule > 5).
--
-- assume that currentRecord occurs within testShedule
--
currentRecord :: Record
currentRecord = testSchedule !! 2

nRecords :: Int
nRecords = length testSchedule

nBefore, nAfter :: Int
nBefore = length $ takeWhile (/= currentRecord) testSchedule
nAfter = length $ drop 1 $ dropWhile (/= currentRecord) testSchedule

renderRecord :: Record -> H.Html
renderRecord rs = 
  let cts = if isJust (recordSequence rs)
            then renderObsId rs
            else renderSpecial rs
  in (H.div H.! A.class_ "observation") cts <>
     H.p ("There are " <> H.toHtml nBefore <> " previous observations.") <>
     H.p ("There are " <> H.toHtml nAfter <> " remaining observations.")

renderObsId :: Record -> H.Html
renderObsId rs = 
  H.p ("Target: " <> H.toHtml (recordTarget rs))
  <>
  H.p ("ObsId: " <> H.toHtml (recordObsname rs))
  <>
  renderLocation rs
  <>
  renderLinks rs

renderSpecial :: Record -> H.Html
renderSpecial rs = 
  H.p ("Target: " <> H.toHtml (recordTarget rs))
  <>
  H.p ("Name: " <> H.toHtml (recordObsname rs))
  <>
  renderLocation rs

-- Display the location fields
renderLocation :: Record -> H.Html
renderLocation rs = 
  let roll = recordRoll rs
      pitch = recordPitch rs
      slew = recordSlew rs
      ra = recordRa rs
      dec = recordDec rs
  in (H.div H.! A.class_ "location") 
      (H.span ("RA: " <> H.toHtml ra) <>
       H.span ("Dec: " <> H.toHtml dec) <>
       H.span ("Roll: " <> H.toHtml roll) <>
       H.span ("Pitch: " <> H.toHtml pitch) <>
       H.span ("Slew: " <> H.toHtml slew))

-- Display the DSS/... links. For now assume they
-- all exist. Note that it looks like these links
-- can be autogenerated from the sequence and obsid
-- values instead of included in the record.
--
-- Actually, trying out a carousel-style display,
-- where the user can flip between the three. The default
-- styling for button is nicer than input[type='button'],
-- but semantically the latter is a better fit, since
-- only one item can be active at a time.
--
renderLinks :: Record -> H.Html

renderLinks rs = 
  let mh = do
        dss <- recordDss rs
        pspc <- recordPspc rs
        rass <- recordRass rs
        return (dss, pspc, rass)

      link :: String -> String -> Bool -> H.Html
      link lbl uri f = 
        let base = H.img H.! A.src    (H.toValue uri)
                         H.! A.alt    (H.toValue lbl)
                         H.! A.width  (H.toValue (680::Int))
                         H.! A.height (H.toValue (680::Int))
                         H.! A.id     (H.toValue lbl)
                         H.! A.class_ (if f then "active" else "inactive")
        in base

      imgSel :: String -> Bool -> H.Html
      imgSel lbl f = 
        let idName = H.toValue (lbl++"button")
            base = H.input H.! A.type_ "radio"
                           H.! A.name  "imgtype"
                           H.! A.value (H.toValue lbl)
                           H.! A.id idName
                           H.! A.onclick
                               ("switchImage('" <> H.toValue lbl <> "')")
        in (if f then base H.! A.checked "checked" else base)
           <> (H.label H.! A.for idName) (H.toHtml lbl)

      form = H.div H.! A.class_ "radiobuttons" $
              mconcat [ "View: "
                      , imgSel "DSS" True
                      , imgSel "RASS" False
                      , imgSel "PSPC" False
                      , " or in "
                      , (H.a H.! A.href "/wwt.html") "WWT"
                      ]

  in case mh of
    Nothing -> ""
    Just (dss, pspc, rass) ->
       form <>
       (H.div H.! A.class_ "links")
        (link "DSS" dss True <>
         link "PSPC" pspc False <>
         link "RASS" rass False) 
{-
renderLinks rs = 
  let mh = do
        dss <- recordDss rs
        pspc <- recordPspc rs
        rass <- recordRass rs
        return (dss, pspc, rass)

      link lbl uri = H.a H.! A.href (H.toValue uri) $ lbl

  in case mh of
    Nothing -> ""
    Just (dss, pspc, rass) ->
       (H.div H.! A.class_ "links")
        (H.span (link "DSS" dss) <>
         H.span (link "PSPC" pspc) <>
         H.span (link "RASS" rass)) 
-}

{-

complete WWT page?

code on using WWT/HTML5

http://www.worldwidetelescope.org/docs/Samples/displaycode.htm?codeExample=WWTWebClientPolyHtml5.html

-}

-- | Create a WWT view of the observation
wwtPage :: 
  Record
  -> H.Html
wwtPage rs =
  let ra = recordRa rs
      dec = recordDec rs
      roll = recordRoll rs
      name = recordTarget rs
      initialize = "initialize(" <> H.toValue ra <> "," <> H.toValue dec <> "," <> H.toValue roll <> ",\"" <> H.toValue name <> "\")" 

      host = (H.div H.! A.id "WorldWideTelescopeControlHost") canvas
      canvas = (H.div H.! A.id "WWTCanvas" 
                      H.! A.style "width: 700px; height: 700px; border-style: none; border-width: 0px") ""

      zoomSource = 
        H.button H.! A.id "jump"
                 H.! A.name "jump"
                 H.! A.value "jump"
                 H.! A.type_ "button"
                 H.! A.onclick "resetLocation();"
           $ "Jump to Source"

      fov = 
        H.span "View Instrument outline" <>
        H.input H.! A.id "fov" 
                H.! A.type_ "checkbox"
                H.! A.checked "checked"
                H.! A.onclick "toggleFOV();"

      crossHair = 
        H.span "View cross hair" <>
        H.input H.! A.id "crosshairs" 
                H.! A.type_ "checkbox"
                H.! A.checked "checked"
                H.! A.onclick "toggleCrosshairs();"

      constellation = 
        H.span "Show constellations" <>
        H.input H.! A.id "constellations" 
                H.! A.type_ "checkbox"
                H.! A.checked "checked"
                H.! A.onclick "toggleConstellations();"

      boundaries = 
        H.span "Show constellation boundaries" <>
        H.input H.! A.id "boundaries" 
                H.! A.type_ "checkbox"
                H.! A.checked "checked"
                H.! A.onclick "toggleBoundaries();"

      controls = mconcat [zoomSource, fov, crossHair, constellation, boundaries]

  in H.docTypeHtml $
    H.head 
     (H.title "View in the World Wide Telescope" <>
      defaultMeta <>
      (H.script H.! A.src "http://www.worldwidetelescope.org/scripts/wwtsdk.aspx") "" <>
      (H.script H.! A.src "js/wwt.js") ""
     )
    <>
    (H.body H.! A.onload initialize)
     (mconcat 
        [ H.p "The instrument outline is ACIS-I (approx)"
        , (H.div H.! A.style "float: left;") controls
        , host
        ])


