{-# LANGUAGE OverloadedStrings #-}
module Page.Common where

import Prelude
import Data.Monoid (mempty)
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes

htmlContainer :: Html -> Html
htmlContainer f = docTypeHtml ! lang "en" $ do
    H.head $ do
        meta ! name "viewport" ! content "width=device-width, initial-scale=1"
        link ! href "//maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap.min.css" ! rel "stylesheet"
        script ! src "//code.jquery.com/jquery-2.1.1.min.js" $ mempty
        script ! src "//maxcdn.bootstrapcdn.com/bootstrap/3.3.1/js/bootstrap.min.js" $ mempty
    body $ H.div ! class_ "container" $ H.div ! class_ "row" $
        H.div ! class_ "col-md-12" $ fieldset $ f

