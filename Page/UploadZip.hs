{-# LANGUAGE OverloadedStrings #-}
module Page.UploadZip where

import Prelude
import Page.Common
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

uploadZipPage :: Html
uploadZipPage = htmlContainer $ H.form ! method "POST" ! action "/upload-zip"
                                    ! enctype "multipart/form-data" $ do
    legend "Upload a tar.gz archive:"
    p ! class_ "help-block" $
        "The tar.gz will be unarchived and expanded at s3. Any gzipped files will be assumed to have a gzip content-encoding."
    H.div ! class_ "form-group" $ do
        H.label ! for "file" $ "Choose file"
        input ! type_ "file" ! accept "application/x-gzip" ! A.id "file" ! name "file"
    H.div ! class_ "form-group" $ do
        H.label ! for "bucket" $ "Upload to bucket:"
        input ! type_ "text" ! class_ "form-control" ! A.id "bucket" ! name "bucket" ! placeholder "bucket"
    H.div ! class_ "form-group" $ do
        H.label ! for "key" $ "With key"
        input ! type_ "text" ! class_ "form-control" ! A.id "key" ! name "key" ! placeholder "/some/key"
    fieldset $ do
        legend "Optional"
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
    legend "Authorization:"
    H.div ! class_ "form-group" $ do
        H.label ! for "name" $ "Your username"
        input ! type_ "text" ! class_ "form-control" ! A.id "name" ! name "name" ! placeholder "your username"
    H.div ! class_ "form-group" $ do
        H.label ! for "pass" $ "Your password"
        input ! type_ "password" ! class_ "form-control" ! A.id "pass" ! name "pass" ! placeholder "your password"
    button ! type_ "submit" ! class_ "btn btn-default" $ "Submit"

