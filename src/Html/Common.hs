{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Html.Common where

import Prelude
import Html.Url
import Html.Bootstrap
import Aws.S3
import Control.Monad
import Data.Monoid (mempty)
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

instance ToMarkup S3Error where
    toMarkup err = H.div ! class_ "row" $ do
        H.h3 "S3 Error"
        H.pre $ H.toHtml $ show err

headNavContentHtml :: Html -> Html -> Html -> Html
headNavContentHtml h n f =
    docTypeHtml ! lang "en" $ do
        h
        body $ do
            n
            H.div ! class_ "container" $ H.div ! class_ "row" $
                H.div ! class_ "col-md-12" $ fieldset $ f

standardHead :: Html
standardHead = H.head $ do
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    link ! href "//maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css"
         ! rel "stylesheet"
    link ! href "//maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap.min.css"
         ! rel "stylesheet"
    script ! src "//code.jquery.com/jquery-2.1.1.min.js" $ mempty
    script ! src "//maxcdn.bootstrapcdn.com/bootstrap/3.3.1/js/bootstrap.min.js" $ mempty


userContainer :: Html -> Html
userContainer f = headNavContentHtml
    standardHead
    loggedInHeader
    f

guestContainer :: Html -> Html
guestContainer f = headNavContentHtml
    standardHead
    loggedOutHeader
    f

val :: Url -> AttributeValue
val = toValue

loggedInHeader :: Html
loggedInHeader = nav ! class_ "navbar navbar-default" $
    H.div ! class_ "container-fluid" $ do
        H.div ! class_ "navbar-header" $
            a ! class_ "navbar-brand" ! href (val UrlHome) $
                i ! class_ "fa fa-home" $ mempty
        ul ! class_ "nav navbar-nav" $ do
            liDropdown (faIcon "file" $ do
                           void $ " file"
                           H.span ! class_ "caret" $ mempty)
                       (do liIcon (val UrlUploadFile) "upload" " upload a file"
                           liIcon (val UrlUploadTarball) "file-archive-o" " upload a tarball"
                           liIcon (val UrlCopyFile) "file-o" " copy a file"
                           liIcon (val UrlCopyFolder) "files-o" " copy a folder")
            liDropdown (faIcon "cloud" $ do
                           void $ " bucket"
                           H.span ! class_ "caret" $ mempty)
                       (do liIcon (val UrlBucketList) "list-ol" " list a bucket"
                           liIcon (val UrlBucketAdd) "plus" " add a bucket"
                           liIcon (val UrlBucketLinkCF) "cloud" " link cloudfront")
            liDropdown (faIcon "user" $ do
                           void $ " user"
                           H.span ! class_ "caret" $ mempty)
                       (do liIcon (val UrlUserSettings) "cogs" " user settings"
                           liIcon (val UrlUserPassword) "key" " update password"
                           liIcon (val UrlUserAdd) "user-plus" " add user")

        H.ul ! class_ "nav navbar-nav navbar-right" $ do
            liIcon (val UrlLogView) "file-text" " view log"
            liIcon (val UrlUserLogout) "sign-out" " logout"

loggedOutHeader :: Html
loggedOutHeader = nav ! class_ "navbar navbar-default" $
    H.div ! class_ "container-fluid" $ do
        H.div ! class_ "navbar-header" $
            a ! class_ "navbar-brand" ! href (val UrlHome) $
                i ! class_ "fa fa-home" $ mempty
        H.ul ! class_ "nav navbar-nav navbar-right" $ do
            li $ a ! href (val UrlUserLogin) $ do
                i ! class_ "fa fa-sign-in" $ mempty
                " login"

selectBucket :: AttributeValue -> [Bucket] -> Html
selectBucket idName bs = do
    select ! A.id idName ! name idName $ forM_ bs $ \bucket ->
        option $ toHtml bucket

