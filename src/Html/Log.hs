{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Html.Log where

import Prelude as P
import Types
import Html.Common
import Text.Blaze.Html5
import Control.Monad
import qualified Data.Text.Lazy as LT
import Text.Blaze.Html5.Attributes

logHtml :: [LogEntry] -> Html
logHtml es = userContainer $ table ! class_ "table table-striped" $ do
    thead $ tr $ do
        th $ "User"
        th $ "Time"
        th $ "Path"
        th $ "Params"
    tbody $ forM_ es entryTR

entryTR :: LogEntry -> Html
entryTR LogEntry{..} = tr $ do
    td $ toHtml logUser
    td $ toHtml $ show logTime
    td $ toHtml logPath
    td $ toHtml shownParams
        where shownParams = LT.intercalate "&" $ P.map (\(k,v) -> LT.concat [k,"=",v]) logParams
