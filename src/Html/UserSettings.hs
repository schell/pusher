{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Html.UserSettings where

import Types
import Control.Monad
import Html.Common
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Data.Map.Strict as M
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

userSettingsHtml :: UserDetail -> Html
userSettingsHtml UserDetail{..} =
    userContainer $ dl ! class_ "dl-horizontal" $ do
        dt "Name"
        dd $ toHtml userName
        dt "Password"
        dd $ a ! href "/user/password" $ "update password"
        dt "Level"
        dd $ toHtml $ show userLevel
        dt "Buckets"
        dd $ do forM_ (M.keys userCreds) $ \bucket -> toHtml bucket >> br
                a ! href "/user-add-bucket" $ "add another..."

