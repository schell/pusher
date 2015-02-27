{-# LANGUAGE OverloadedStrings #-}
module Html.CopyFolder where

import Prelude
import Html.Common
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

copyFolderHtml :: Html
copyFolderHtml =
    userContainer $ H.form ! method "POST" ! action "/copy-folder"
                                    ! enctype "multipart/form-data" $ do
        p ! class_ "help-block" $
            "This copies the contents of frombucket/fromkey/ to tobucket/tokey/"
        legend "From:"
        H.div ! class_ "form-group" $ do
            H.label ! for "bucket" $ "Source bucket:"
            input ! type_ "text" ! class_ "form-control" ! A.id "bucket"
                  ! name "bucket" ! placeholder "bucket"
        H.div ! class_ "form-group" $ do
            H.label ! for "from" $ "Source key:"
            input ! type_ "text" ! class_ "form-control" ! A.id "from"
                  ! name "from" ! placeholder "from"
        legend "To:"
        H.div ! class_ "form-group" $ do
            H.label ! for "toBucket" $ "Target bucket:"
            input ! type_ "text" ! class_ "form-control" ! A.id "bucket"
                  ! name "toBucket" ! placeholder "toBucket"
        H.div ! class_ "form-group" $ do
            H.label ! for "to" $ "Target key:"
            input ! type_ "text" ! class_ "form-control" ! A.id "to"
                  ! name "to" ! placeholder "to"

        button ! type_ "submit" ! class_ "btn btn-default" $ "Submit"
