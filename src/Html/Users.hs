{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Html.Users where

import Types
import Html.Common
import Text.Blaze.Html5
import Control.Monad
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Blaze.Html5.Attributes

usersHtml :: M.Map UserName UserDetail -> Html
usersHtml m = userContainer $ table ! class_ "table table-striped" $ do
    thead $ tr $ do
        th $ "Name"
        th $ "Buckets"
    tbody $ forM_ (M.toList m) userTR

userTR :: (UserName, UserDetail) -> Html
userTR (n, detail) = tr $ do
    td $ toHtml n
    td $ toHtml $ T.intercalate ", " $ M.keys $ userCreds detail
