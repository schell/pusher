{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Html.NewUser where

import Prelude
import Html.Common
import Html.Url
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

newUserHtml :: Html
newUserHtml =
    userContainer $ H.form ! method "POST" ! action (toValue UrlUserAdd) $ do
        legend "New/Updated User Details:"
        H.div ! class_ "form-group" $ do
            H.label ! for "name" $ "New user's name"
            input ! type_ "text" ! class_ "form-control" ! A.id "name"
                  ! name "name" ! placeholder "username to add"
        H.div ! class_ "form-group" $ do
            H.label ! for "pass" $ "New user's password"
            input ! type_ "password" ! class_ "form-control" ! A.id "pass"
                  ! name "pass" ! placeholder "password for new user"
        H.div ! class_ "form-group" $ do
            H.label ! for "lvl" $ "New user's level"
            select ! class_ "form-control" ! A.id "lvl" ! name "lvl" $ do
                option "0"
                option "1"
                option "2"
            H.span ! A.id "helpBlock" ! class_ "help-block" $
                "0 - superuser, 1 - user, 2 - peon"
        fieldset $ do
            legend "Optional Starting Bucket/Creds"
            H.div ! class_ "form-group" $ do
                H.label ! for "bucket" $ "New user's aws bucket"
                input ! type_ "text" ! class_ "form-control" ! A.id "bucket"
                      ! name "bucket" ! placeholder "aws bucket"
            H.div ! class_ "form-group" $ do
                H.label ! for "key" $ "New user's aws key"
                input ! type_ "text" ! class_ "form-control" ! A.id "key"
                      ! name "key" ! placeholder "aws key"
            H.div ! class_ "form-group" $ do
                H.label ! for "secret" $ "New user's aws secret"
                input ! type_ "text" ! class_ "form-control" ! A.id "secret"
                      ! name "secret" ! placeholder "aws secret"

        button ! type_ "submit" ! class_ "btn btn-default" $ "Submit"
