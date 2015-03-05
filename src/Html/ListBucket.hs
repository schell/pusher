{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Html.ListBucket where

import Prelude as P
import Aws.S3
import Html.Url
import Html.Common
import Text.Blaze.Html5 as H
import Control.Monad
import Data.Text as T
import Text.Blaze.Html5.Attributes as A

listBucketFormHtml :: [Bucket] -> Html
listBucketFormHtml bs =
    userContainer $ H.form ! method "POST" ! action (toValue UrlBucketList)
                           ! enctype "multipart/form-data" $ do
        legend "List a bucket:"
        H.div ! class_ "form-group" $ do
            H.label ! for "bucket" $ "Bucket:"
            br
            selectBucket "bucket" bs
        H.div ! class_ "form-group" $ do
            H.label ! for "prefix" $ "Directory:"
            input ! type_ "text" ! class_ "form-control" ! A.id "prefix" ! name "prefix"

        button ! type_ "submit" ! class_ "btn btn-default" $ "Submit"

listBucketHtml :: [Text] -> Html
listBucketHtml ks = userContainer $ table ! class_ "table table-striped" $ do
    thead $ tr $ do
        th $ "Key"
    tbody $ forM_ ks (tr . td . toHtml)
