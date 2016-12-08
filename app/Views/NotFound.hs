{-# LANGUAGE OverloadedStrings #-}

-- | The 404 page.

module Views.NotFound (notFoundPage, errPage) where

-- import qualified Prelude as P
import Prelude (($))

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>))

import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (title)

import Utils (defaultMeta, cssLink, renderFooter)
import Views.Record (CurrentPage(..), twitterDiv
                    , mainNavBar)

notFoundPage :: Html -> Html
notFoundPage fact =
  docTypeHtml ! lang "en-US" $
    head (H.title "What is Chandra doing? I am not sure!" <>
          defaultMeta <>
          (cssLink "/css/main.css" ! A.title "Default")
          )
    <>
    body
     (mainNavBar CPOther
      <> notFoundDiv fact
      <> twitterDiv)
     <> renderFooter

notFoundDiv :: Html -> Html
notFoundDiv fact = (div ! id "mainBar") (notFoundParas fact)
  
notFoundParas :: Html -> Html
notFoundParas fact = 
  (p ! class_ "error")  
  ("Unfortunately there was an error and I don't know what to do. "
   <> "To take your mind off this calamity, how about a fun "
   <> "Chandra fact:")
  <> (p ! class_ "fact") fact

errPage :: Html
errPage = do
  h1 "Oops, something isn't right"
  p $ "There has been some sort of error - probably the "
      <> "database and the web server not getting along - "
      <> "which means you've ended up here. Hopefully it is "
      <> "just due to an update to the database and it will "
      <> "be finished in a few minutes, after which time "
      <> "the site should be back to normal - except with "
      <> "even more data to explore! - but if it doesn't "
      <> "get fixed within 10 minutes, send a note to me "
      <> "at "
      <> (a ! href "https://twitter.com/doug_burke") "@doug_burke"
      <> " or via the "
      <> (a ! href "https://bitbucket.org/doug_burke/chandraobs/issues?status=new&status=open") "issue tracker"
      <> "."
  p $ "You could while away the time by "
      <> (a ! href "http://joshworth.com/dev/pixelspace/pixelspace_solarsystem.html") "exploring the Solar System"
      <> " from the comfort of your web browser. If you scroll "
      <> "out to Jupiter - which shows remarkable dedication - "
      <> "you should have spent enough time for any database updates to have "
      <> "run their course!"
      
