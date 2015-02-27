{-# LANGUAGE OverloadedStrings #-}
module Html.Common where

import Prelude
import Data.Monoid (mempty)
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes

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

loggedInHeader :: Html
loggedInHeader = nav ! class_ "navbar navbar-default" $
    H.div ! class_ "container-fluid" $ do
        H.div ! class_ "navbar-header" $
            a ! class_ "navbar-brand" ! href "/" $
                i ! class_ "fa fa-home" $ mempty
        H.ul ! class_ "nav navbar-nav" $ do
            li $ a ! href "/upload" $ do
                i ! class_ "fa fa-upload" $ mempty
                " upload a file"
            li $ a ! href "/upload-zip" $ do
                i ! class_ "fa fa-file-archive-o" $ mempty
                " upload a tarball"
            li $ a ! href "/copy" $ do
                i ! class_ "fa fa-file-o" $ mempty
                " copy a file"
            li $ a ! href "/copy-folder" $ do
                i ! class_ "fa fa-files-o" $ mempty
                " copy a folder"
            li $ a ! href "/user-add-bucket" $ do
                i ! class_ "fa fa-cloud" $ mempty
                " add a bucket"

        H.ul ! class_ "nav navbar-nav navbar-right" $ do
            li $ a ! href "/user" $ do
                i ! class_ "fa fa-user-plus" $ mempty
                " add user"
            li $ a ! href "/log" $ do
                i ! class_ "fa fa-file-text" $ mempty
                " view log"
            li $ a ! href "/logout" $ do
                i ! class_ "fa fa-sign-out" $ mempty
                " logout"

loggedOutHeader :: Html
loggedOutHeader = nav ! class_ "navbar navbar-default" $
    H.div ! class_ "container-fluid" $ do
        H.div ! class_ "navbar-header" $
            a ! class_ "navbar-brand" ! href "/" $
                i ! class_ "fa fa-home" $ mempty
        H.ul ! class_ "nav navbar-nav navbar-right" $ do
            li $ a ! href "/login" $ do
                i ! class_ "fa fa-sign-in" $ mempty
                " login"
