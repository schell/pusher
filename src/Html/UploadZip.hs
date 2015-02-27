{-# LANGUAGE OverloadedStrings #-}
module Html.UploadZip where

import Prelude
import Aws.S3
import Html.Common
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

uploadZipHtml :: [Bucket] -> Html
uploadZipHtml bs = userContainer $ H.form ! method "POST" ! action "/upload-zip"
                                          ! enctype "multipart/form-data" $ do
    legend "Upload a tar.gz archive:"
    p ! class_ "help-block" $
        "The tar.gz will be unarchived and expanded at s3. Any gzipped files will be assumed to have a gzip content-encoding."
    H.div ! class_ "form-group" $ do
        H.label ! for "file" $ "Choose file"
        input ! type_ "file" ! accept "application/x-gzip" ! A.id "file" ! name "file"
    H.div ! class_ "form-group" $ do
        H.label ! for "bucket" $ "Upload to bucket:"
        br
        selectBucket "bucket" bs
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

    button ! type_ "submit" ! class_ "btn btn-default" $ "Submit"

