{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Html.UserAddBucket where

import Prelude
import Html.Common
import Html.Url
import Text.Blaze.Html5
import Data.Text (Text)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

userAddBucketHtml :: Text -> Html
userAddBucketHtml username =
    userContainer $ H.form ! method "POST" ! action (toValue UrlBucketAdd) $ do
        legend "Add another bucket credential"
        p ! class_ "help-block" $
            "You can add buckets for other users if your level is less than theirs or if you are the super user with level 0."
        H.div ! class_ "form-group" $ do
            H.label ! for "username" $ "Username"
            input ! type_ "text" ! class_ "form-control" ! A.id "username"
                  ! name "username" ! value (toValue username)
        H.div ! class_ "form-group" $ do
            H.label ! for "bucket" $ "S3 bucket"
            input ! type_ "text" ! class_ "form-control" ! A.id "bucket"
                  ! name "bucket" ! placeholder "s3 bucket"
        H.div ! class_ "form-group" $ do
            H.label ! for "key" $ "S3 key"
            input ! type_ "text" ! class_ "form-control" ! A.id "key"
                  ! name "key" ! placeholder "s3 key"
        H.div ! class_ "form-group" $ do
            H.label ! for "secret" $ "S3 secret"
            input ! type_ "text" ! class_ "form-control" ! A.id "secret"
                  ! name "secret" ! placeholder "s3 secret"

        button ! type_ "submit" ! class_ "btn btn-default" $ "Submit"
