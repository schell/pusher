{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Html.Task where

import Prelude as P
import Types
import Control.Monad
import Html.Common
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes

taskLinkHtml :: UniqueID -> Html
taskLinkHtml uid = userContainer $ p $ do
    void $ "The task is running and can be checked at "
    a ! href (toValue ("/task/" ++ show uid)) $
        toHtml $ unwords ["Task", show uid]

taskHtml :: String -> Html
taskHtml out = userContainer $ table ! class_ "table table-striped" $ do
    thead $ tr $ do
        th "Files"
    tbody $ forM_ (lines out) $ \ln -> do
        tr $ td $ toHtml ln
