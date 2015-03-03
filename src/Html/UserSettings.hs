{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Html.UserSettings where

import Types
import Control.Monad
import Html.Common
import Html.Url
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Data.Map.Strict as M

userSettingsHtml :: UserDetail -> Html
userSettingsHtml UserDetail{..} =
    userContainer $ dl ! class_ "dl-horizontal" $ do
        dt "Name"
        dd $ toHtml userName
        dt "Password"
        dd $ a ! href (toValue UrlUserPassword) $ "update password"
        dt "Level"
        dd $ toHtml $ show userLevel
        dt "Buckets"
        dd $ do forM_ (M.keys userCreds) $ \bucket -> toHtml bucket >> br
                a ! href (toValue UrlBucketAdd) $ "add another..."

