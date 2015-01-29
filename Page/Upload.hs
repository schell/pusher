{-# LANGUAGE OverloadedStrings #-}
module Page.Upload where

import Prelude
import Page.Common
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

uploadPage :: Html
uploadPage = htmlContainer $ H.form ! method "POST" ! action "/upload"
                                    ! enctype "multipart/form-data" $ do
    legend "Upload a file:"
    H.div ! class_ "form-group" $ do
        H.label ! for "file" $ "Choose file"
        input ! type_ "file" ! A.id "file" ! name "file"
        H.label ! for "bucket" $ "Upload to bucket:"
        input ! type_ "text" ! class_ "form-control" ! A.id "bucket" ! name "bucket" ! placeholder "bucket"
    fieldset $ do
        legend "Optional"
        H.div ! class_ "form-group" $ do
            H.label ! for "key" $ "With key"
            input ! type_ "text" ! class_ "form-control" ! A.id "key" ! name "key" ! placeholder "/some/key.name"
        H.div ! class_ "form-group" $ do
            H.label ! for "acl" $ "ACL"
            select ! A.id "acl" ! name "acl" $ do
                -- Maybe make this a bounded enum mapM over all
                -- Acl constructors
                option "AclPrivate"
                option "AclPublicRead"
                option "AclPublicReadWrite"
                option "AclAuthenticatedRead"
                option "AclBucketOwnerRead"
                option "AclBucketOwnerFullControl"
                option "AclLogDeliveryWrite"
        H.div ! class_ "form-group" $ do
            H.label ! for "content-type" $ "Content-Type"
            input ! type_ "text" ! class_ "form-control" ! A.id "content-type" ! name "content-type`" ! placeholder ""
        H.div ! class_ "form-group" $ do
            H.label ! for "key" $ "Content-Encoding"
            input ! type_ "text" ! class_ "form-control" ! A.id "content-encoding" ! name "content-encodeing" ! placeholder ""
    legend "Authorization:"
    H.div ! class_ "form-group" $ do
        H.label ! for "name" $ "Your username"
        input ! type_ "text" ! class_ "form-control" ! A.id "name" ! name "name" ! placeholder "your username"
    H.div ! class_ "form-group" $ do
        H.label ! for "pass" $ "Your password"
        input ! type_ "password" ! class_ "form-control" ! A.id "pass" ! name "pass" ! placeholder "your password"
    button ! type_ "submit" ! class_ "btn btn-default" $ "Submit"

