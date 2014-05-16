{-# LANGUAGE OverloadedStrings #-}

-- | The about page.
-- 
module Views.About (aboutPage) where

-- import qualified Prelude as P
import Prelude (($))

import qualified Text.Blaze.Html5 as H
-- import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>))

import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (title)

import Utils (defaultMeta)

-- The uninformative about page  
aboutPage :: Html
aboutPage = 
  docTypeHtml $
   let txt = "This is " <> em "not" <> 
             " the space-ninja rocket ship start-up you were looking for."
       lnk = "Back " <> (a ! href "/index.html" $ "home") <> "."
   in head (H.title "About this mysterious site" <> 
            defaultMeta) <>
      body (p txt <> p lnk)

