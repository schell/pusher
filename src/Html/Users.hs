{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Html.Users where

import Types
import Html.Common
import Text.Blaze.Html5
import Data.String
import Control.Monad
import qualified Data.Map as M
import qualified Text.Blaze.Html5 as H
import qualified Data.Text as T
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

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
