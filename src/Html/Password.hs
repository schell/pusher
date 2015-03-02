{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Html.Password where

import Prelude
import Html.Common
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

passwordHtml :: Html
passwordHtml =
    userContainer $ H.form ! method "POST" ! action "/user/password" $ do
        legend "New password"
        H.div ! class_ "form-group" $ do
            H.label ! for "pass" $ "Password:"
            input ! type_ "password" ! class_ "form-control" ! A.id "pass"
                  ! name "pass"
        H.div ! class_ "form-group" $ do
            H.label ! for "passCheck" $ "Password again:"
            input ! type_ "password" ! class_ "form-control" ! A.id "passCheck"
                  ! name "passCheck"

        button ! type_ "submit" ! class_ "btn btn-default" $ "Submit"
