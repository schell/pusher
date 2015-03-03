{-# LANGUAGE OverloadedStrings #-}
module Html.Cloudfront where

import Prelude
import Types
import Control.Monad
import Html.Url
import Text.Blaze.Html5
import qualified Data.Map.Strict as M
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

cloudfrontHtml :: CFDistros -> Html
cloudfrontHtml cfds = H.form ! method "POST" ! action (toValue UrlBucketLinkCF) $ do
    legend "Linked Cloudfront Distros:"
    forM_ (M.toList cfds) $ \(bucket, distro) -> do
        let bucketValue = toValue bucket
        H.div ! class_ "form-group" $ do
            H.label ! for bucketValue $ toHtml bucket
            input ! type_ "text" ! class_ "form-control" ! A.id bucketValue
                  ! name bucketValue ! value (toValue distro)

    button ! type_ "submit" ! class_ "btn btn-default" $ "Submit"

