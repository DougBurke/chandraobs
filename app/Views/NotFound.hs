{-# LANGUAGE OverloadedStrings #-}

-- | The 404 page.

module Views.NotFound (notFoundPage) where

-- import qualified Prelude as P
import Prelude (($))

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>))

import Text.Blaze.Html5 hiding (title)
import Text.Blaze.Html5.Attributes hiding (title)

import Utils (defaultMeta)
import Views.Record (CurrentPage(..), renderTwitter
                    , mainNavBar)

notFoundPage :: Html -> Html
notFoundPage fact =
  docTypeHtml ! lang "en-US" $
    head (H.title "What is Chandra doing? I am not sure!" <>
          defaultMeta <>
           link ! href   "/css/main.css"
                ! type_  "text/css" 
                ! rel    "stylesheet"
                ! A.title  "Default"
                ! media  "all"
          )
    <>
    body
     (mainNavBar CPOther
      <> (div ! class_ "error")  
           ("Unfortunately I don't know what to do. Whilst I am thinking, "
            <> "how about this fun Chandra fact:")
      <> (div ! class_ "fact") fact
      <> renderTwitter
     )

