{-# LANGUAGE OverloadedStrings #-}
module Html.Upload where

import Prelude
import Aws.S3
import Html.Common
import Html.Url
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

uploadHtml :: [Bucket] -> Html
uploadHtml bs =
    userContainer $ H.form ! method "POST" ! action (toValue UrlUploadFile)
                                    ! enctype "multipart/form-data" $ do
        legend "Upload a file:"
        H.div ! class_ "form-group" $ do
            H.label ! for "file" $ "Choose file"
            input ! type_ "file" ! A.id "file" ! name "file"
            H.label ! for "bucket" $ "Upload to bucket:"
            br
            selectBucket "bucket" bs
        fieldset $ do
            legend "Optional"
            H.div ! class_ "form-group" $ do
                H.label ! for "key" $ "With key"
                input ! type_ "text" ! class_ "form-control" ! A.id "key"
                      ! name "key" ! placeholder "/some/key.name"
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
                input ! type_ "text" ! class_ "form-control"
                      ! A.id "content-type" ! name "content-type`" ! placeholder ""
            H.div ! class_ "form-group" $ do
                H.label ! for "key" $ "Content-Encoding"
                input ! type_ "text" ! class_ "form-control"
                      ! A.id "content-encoding" ! name "content-encodeing"
                      ! placeholder ""

        button ! type_ "submit" ! class_ "btn btn-default" $ "Submit"
