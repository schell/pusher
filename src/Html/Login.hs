{-# LANGUAGE OverloadedStrings #-}
module Html.Login where

import Prelude
import Html.Url
import Html.Common
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

loginHtml :: String -> Html
loginHtml r = let login = toValue $ Uri UrlUserLogin [("r", r)] in
    guestContainer $ H.form ! method "POST" ! action login $ do
        legend "Login:"
        H.div ! class_ "form-group" $ do
            H.label ! for "authname" $ "Username"
            input ! type_ "text" ! class_ "form-control" ! A.id "authname"
                  ! name "authname" ! placeholder "your username"
        H.div ! class_ "form-group" $ do
            H.label ! for "authpass" $ "Password"
            input ! type_ "password" ! class_ "form-control" ! A.id "authpass"
                  ! name "authpass" ! placeholder "your password"

        button ! type_ "submit" ! class_ "btn btn-default" $ "Submit"

