{-# LANGUAGE OverloadedStrings #-}

-- | The "about" page.

module About (aboutPage)
       where

-- import qualified Prelude as P
import Prelude (($), (==), maybe)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>))

import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (item, title)

import API (cssLink)
import Database (DBInfo)
import Git (fromCommitId, gitCommitId)
import Layout (defaultMeta)
import Utils (showInt)
import Types (toChandraTime, showCTime, showExpTime)
import Views.Record (CurrentPage(..)
                    , singleColBody)

{-
aboutPage :: 
  UTCTime     -- current time
  -> ObsInfo 
  -> (Maybe SimbadInfo, (Maybe Proposal, SortedList StartTimeOrder ScienceObs))
  -- other observations in the proposal
  -> Html
-}
aboutPage ::
  DBInfo
  -> Html
aboutPage dbInfo =
  docTypeHtml ! lang "en-US" $
    head (H.title "What is the 'What is Chandra doing now?' site?" <>
          defaultMeta <>
          (cssLink "/css/main.css" ! A.title "Default") <>
          cssLink "/css/about.css"
          )
    <>
    body
     (singleColBody CPAbout
       ((div ! class_ "explanation")
               (introSection
                <> orbitSection
                <> dbSection dbInfo
                <> changelog
                <> furtherSection)))


alink :: AttributeValue -> Html -> Html
alink url = a ! href url


introSection :: Html
introSection = do
  
   h1 "What is the 'What is Chandra doing now?' site?"

   (p ! class_ "caveat") (
     "The information on this site is provided for educational, "
     <> em "not"
     <> " scientific, purposes. It is kept as up-to-date as possible, but "
     <> "due to the way Chandra is controlled - it is semi-autonomous and "
     <> "we are not in constant contact with it - there is no way to "
     <> "guarantee the current status of Chandra.")

   p ("The Chandra X-ray Observatory - hereafter Chandra - is "
      <> "one of NASA's four great observatories, the other three being: "
      <> "the "
      <> alink "https://hubblesite.org/" "Hubble Space Telescope"
      <> "; the "
      <> alink "https://heasarc.gsfc.nasa.gov/docs/cgro/"
      "Compton Gamma-Ray Observatory"
      <> "; and the "
      <> alink "http://www.spitzer.caltech.edu/" "Spitzer Space Telescope"
      <> ". It is in orbit around the Earth and taking observations, "
      <> "commanded by the Chandra X-ray Center based on proposals made by "
      <> "professional Astronomers. This site aims to tell you "
      <> "what observation Chandra is performing now, and provide "
      <> "some context for what it is doing.")

   p ("For science observations, information on the target, the "
      <> "reason for observing the target (the abstract of the proposal), "
      <> "and "
      <> alink "views.html" "a variety of views"
      <> " of the target are given. Chandra also regularly performs "
      <> "sets of calibration observations, which are "
      <> "indicated as such, and have no science abstract or imaging data. "
      <> "There are times when Chandra is not taking data, "
      <> "either because the orbit of Chandra is taking it through the "
      <> alink "https://www-spof.gsfc.nasa.gov/Education/Iradbelt.html"
      "radiation belts around Earth"
      <> " - in which case the instruments are moved out of the focal plane "
      <> "of the telescope to prevent radiation damage - or "
      <> "because the telescope has to change its direction to point "
      <> "to a new target. This latter case is known as "
      <> alink "https://en.wikipedia.org/wiki/Slew_%28spacecraft%29" "slewing"
      <> " and for Chandra this is a slow process, "
      <> "since it is a large, heavy, spacecraft and so fast moves would "
      <> "make it harder to obtain the pointing accuracy needed "
      <> "to maximise the high spatial resolution of the telescope. "
      <> "This is in contrast to some missions such as the "
      <> alink "https://swift.gsfc.nasa.gov/about_swift/"
      "Swift Gamma-Ray Burst Mission"
      <> ", which is designed to be able to "
      <> alink "https://swift.gsfc.nasa.gov/help/swiftfaq.html#slew"
      "slew very quickly"
      <> " (as it's primary science goal is to observe sources which "
      <> "vary " <> em "very" <> " quickly).")
      

orbitSection :: Html
orbitSection = do

  (h2 ! id "orbit") "Where is Chandra?"

  p ("The "
     <> alink "https://chandra.harvard.edu/about/tracking.html"
     "Tracking Chandra"
     <> " page provides information on Chandra's orbit. "
     <> "The recent trend in placing telescopes into orbit has included "
     <> "a Low-Earth Orbit - such as used by the Hubble Space Telescope and "
     <> alink "https://en.wikipedia.org/wiki/Suzaku_(satellite)"
     "the Suzaku X-ray telescope"
     <> " - and at the "
     <> alink "https://en.wikipedia.org/wiki/L2_point#L2"
     "L2 point of the Sun-Earth system"
     <> " - such as the "
     <> alink "https://en.wikipedia.org/wiki/Herschel_Space_Observatory"
     "Herschel Space Observatory"
     <> " and "
     <> alink "https://en.wikipedia.org/wiki/Gaia_(spacecraft)" "Gaia"
     <> ". The Chandra orbit is different, in that it is a highly "
     <> "elliptical orbit around the Earth that extends out (at apogee, "
     <> "the furthest point from Earth) to almost one-third the way to "
     <> "the Moon. It takes just over 64 hours to complete one orbit, "
     <> "with only about 15% of the time close to Earth, where the radiation "
     <> "environment is hazardous to the instruments on board (so they are "
     <> "moved out of the focal plane of the telescope to protect them).")

dbSection ::
  DBInfo
  -- ^ Number of science observations, proposals, total length of the science
  --   observations, and when was the database last modified.
  -> Html
dbSection (nScience, nProp, tScience, mLastMod) = do

  (h2 ! id "database") "Database"

  -- TODO: improve the following, describing how the
  --       database is scraped
  --
  -- p ("Something about the database.")

  -- Not convinced going via ChandraTime is a good idea
  let obs = "observation" <> if nScience == 1 then "" else "s"
      prop = "proposal" <> if nProp == 1 then "" else "s"

      -- TODO: would it be nicer to say "updated x days ago"?
      --       probably need to extract the logic used in the
      --       obsid rendering
      --
      noData = "There appears to be no data in the database! "
               <> "Hopefully it is being loaded up with updated information, "
               <> "so try reloading this page after a minute or so."
               
      cts = maybe noData
            (\t -> "The database was last updated on "
                   <> showCTime (toChandraTime t)
                   <> " and contains "
                   <> showInt nScience
                   <> " scientific " <> obs
                   <> ". The total length of these " <> obs
                   <> " is "
                   <> showExpTime tScience
                   <> ". The observations are from " <> showInt nProp
                   <> " " <> prop <> "."
            )
            mLastMod

  -- logic here is ugly
  p (toHtml (if nScience == 0 then noData else cts))
  

changelog :: Html
changelog = do

  (h2 ! id "changelog") "Changes to the website"

  p ("A list of some of the recent changes to the website - useful if "
     <> "a page or link you were using has changed - can be found on "
     <> "the "
     <> alink "changelog.html" "recent changes to the website"
     <> " page.")
    

furtherSection :: Html
furtherSection = do

  (h1 ! id "further") "Further information and credits"

  p ("More information on the Chandra X-ray Observatory can be "
     <> "found on its official "
     <> alink "https://chandra.harvard.edu/" "home page"
     <> ".")

  p ("The tour functionality provided on the "
     <> alink "/index.html" "main page"
     <> " is provided by the "
     <> alink "http://bootstraptour.com/" "Bootstrap Tour"
     <> " JavaScript library; this was inspired by  the "
     -- <> alink "http://adslabs.org/adsabs/" "ADS Labs search page"
     <> alink "http://ads.harvard.edu/" "ADS Labs search page"
     <> " (which has since changed and no-longer provides a tour). "
     <> "The table sorting on the "
     <> alink "/schedule/index.html" "Schedule page"
     <> " is done by the "
     <> alink "http://tablesorter.com/docs/" "TableSorter jQuery plugin"
     <> " and "
     <> alink "https://d3js.org/" "D3"
     <> " is used to display the scheduled observations on the sky, "
     <> "following the approach used for the "
     <> alink "https://xrtpub.harvard.edu/photo/map/" "Chandra Sky Map"
     <> ". The outline of the Milky Way, shown on the "
     <> alink "/schedule/" "schedule view"
     <> " pages, and the constellation boundaries, on the "
     <> alink "/search/constellation/" "individual Constellation pages"
     <> ", are taken from the "
     <> alink "https://github.com/ofrohn/d3-celestial" "d3-celestial"
     <> " project by Olaf Frohn (the Milky outline is based on the data "
     <> " from the "
     <> alink "http://www.skymap.com/milkyway_cat.htm"
     "Milky Way Outline Catalog"
     <> " from Jose R. Vieira). The "
     <> alink "/search/timeline.html" "timeline view"
     <> ", along with its faceted browsing, would not be possible "
     <> "without the "
     <> alink "https://github.com/simile-widgets/exhibit/"
     "Exhibit widget framework"
     <> ". At least one icon comes from the free version of "
     <> alink "https://fontawesome.com/" "Font Awesome"
     <> "."
    )

  p ("The " <> alink "views.html#WWT" "interactive view" <>
     " uses the WebGL version of the American Astronomical Society's "
     <> alink "https://www.worldwidetelescope.org/webclient/"
     "WorldWide Telescope"
     <> " - aka WWT - web client. The background images have been "
     <> "provided by a variety of institutions and groups:")

  let thumb url = img ! class_ "thumbnail" ! src url
      hdr url title = dt (alink url title)

      item url cts = (dd ! class_ "term") (thumb url <> cts)
      
  (dl ! class_ "credits") (
    hdr "https://www.nasa.gov/mission_pages/planck/index.html" "Planck"
    <>
    item "https://www.worldwidetelescope.org/wwtweb/thumbnail.aspx?name=planck"
       (p ("Planck is a European Space Agency mission, with significant " <>
           "participation from NASA. NASAs Planck Project Office is based at " <>
           "JPL. JPL contributed mission-enabling technology for both of " <>
           "Plancks science instruments. European, Canadian and U.S. Planck " <>
           "scientists work together to analyze the Planck data."))
    
    <>
    {- Is this URL dead now? -}
    hdr "http://lwa.nrl.navy.mil/VLSS/" "VLA Low-frequency Sky Survey (VLSS)"
    <>
    item "https://www.worldwidetelescope.org/wwtweb/thumbnail.aspx?name=VLA"
       (p ("VLSS Cohen, A. S.; Lane, W. M.; Cotton, W. D.; Kassim, N. E.; Lazio, " <>
           "T. J. W.; Perley, R. A.; Condon, J. J.; Erickson, W. C.; " <>
           "Served From NASA Skyview"))

    {-
    <>
    hdr "http://wise.ssl.berkeley.edu/" "WISE"
    <>
    dd (thumb "https://www.worldwidetelescope.org/wwtweb/thumbnail.aspx?name=wise"
        <>
        p "NASA/JPL-Caltech/UCLA")
    -}

    <>
    hdr "https://www.ipac.caltech.edu/2mass/" "2MASS"
    <>
    item "https://www.worldwidetelescope.org/wwtweb/thumbnail.aspx?name=2mass"
       (p ("This publication makes use of data products from the Two Micron " <>
           "All Sky Survey, which is a joint project of the University of " <>
           "Massachusetts and the Infrared Processing and Analysis " <>
           "Center/California Institute of Technology, funded by the National " <>
           "Aeronautics and Space Administration and the National Science " <>
           "Foundation."))

    <>
    hdr "https://gsss.stsci.edu/Acknowledgements/DataCopyrights.htm" "DSS"
    <>
    item "https://www.worldwidetelescope.org/thumbnails/DSS.png"
       (p "Copyright DSS Consortium")

    {-
    <>
    hdr "https://www.astro.princeton.edu/~dfink/halpha/processing.html" "Hα"
    <>
    dd (thumb "https://www.worldwidetelescope.org/wwtweb/thumbnail.aspx?name=halpha"
        <>
        p ("Image Courtesy Douglas Finkbeiner. The full-sky H-alpha map (6' " <>
           "FWHM resolution) is a composite of the Virginia Tech Spectral " <>
           "line Survey (VTSS) in the north and the Southern H-Alpha Sky " <>
           "Survey Atlas (SHASSA) in the south. The Wisconsin H-Alpha Mapper " <>
           "(WHAM) survey provides a stable zero-point over 3/4 of the sky on " <>
           "a one degree scale."))
    -}
    
    <>
    hdr "http://www.xray.mpe.mpg.de/cgi-bin/rosat/rosat-survey" "RASS"
    <>
    item "https://www.worldwidetelescope.org/wwtweb/thumbnail.aspx?name=rosatcolor"
       (p ("This is a composite of three RASS3 surveys from the ROSAT Data " <>
           "Archive of the Max-Planck-Institut fur extraterrestrische Physik " <>
           "(MPE) at Garching, Germany. TOAST-formatted data was obtained " <>
           "from NASA's SkyView Virtual Telescope. Red is soft band " <>
           "(RASS3sb), Green is broad band (RASS3bb), Blue is hard band " <>
           "(RASS3hb)"))

    <>
    hdr "https://www.nasa.gov/mission_pages/GLAST/main/index.html" "Fermi"
    <>
    item "https://www.worldwidetelescope.org/wwtweb/thumbnail.aspx?name=FermiYearThree"
       (p "NASA and the FERMI-LAT Team.")
    )
    
  let gitURL = "https://github.com/DougBurke/chandraobs/commit/"
               <> gitTxt
      gitTxt = fromCommitId gitCommitId
      
  p ("The web site "
     <> alink "https://github.com/DougBurke/chandraobs"
     "code is available on GitHub"
    <> ", is coded in "
    <> alink "https://www.haskell.org/" "Haskell"
    <> ", and runs on the "
    <> alink "https://www.heroku.com/" "Heroku platform"
    <> ". The version of the code used to create this site can "
    <> "be found on GitHub at: "
    <> alink (toValue gitURL) (toHtml gitTxt)
    <> ".")

  p ("For questions on this site try either "
     <> alink "https://mastodon.social/@dburke" "@dburke"
     <> " (Mastodon) or the "
     <> alink "https://github.com/DougBurke/chandraobs/issues"
     "issues page"
     <> " for the project.")
