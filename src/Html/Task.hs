{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Html.Task where

import Prelude as P
import Types
import Html.Url
import Control.Monad
import Html.Common
import Data.Text (Text)
import qualified Data.Text as T
import Text.Blaze
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes

taskLinkHtml :: UniqueID -> Html
taskLinkHtml uid = p $ do
    void $ "The task is running and can be checked at "
    a ! href (toValue (Uri UrlTask [("task", show uid)])) $
        toHtml $ unwords ["Task", show uid]

taskHtml :: Task -> Html
taskHtml = userContainer . toHtml

cfUrl :: Text -> Html
cfUrl cf = a ! href (toValue $ url `T.append` cf) ! target "_blank" $
    "Invalidate these files at their cloudfront distribution."
    where url = "https://console.aws.amazon.com/cloudfront/home?region=us-east-1#distribution-settings:"

instance ToMarkup Task where
    toMarkup (CompletedTask tus (b', mcf) utc) = H.div $ do
        H.p $ do toHtml $ "This task successfully completed at " ++ show utc
                 br
                 toHtml $ unwords [ show $ length tus
                                  , "files were uploaded to"
                                  , T.unpack b'
                                  ]
                 br
                 maybe (return ()) cfUrl mcf
        H.pre $ forM_ tus toHtml
    toMarkup (PendingTask tus) = H.div $ do
        H.p $ "This task is currently pending. Refresh for updates."
        H.pre $ forM_ tus toHtml

instance ToMarkup TaskUpdate where
    toMarkup (TaskError e stdin stdout) = dl ! class_ "dl-horizontal" $ do
        dd "Error"
        dt $ toHtml $ show e
        dd "stdin"
        dt $ toHtml stdin
        dd "stdout"
        dt $ toHtml stdout
    toMarkup (TaskSuccess str) = toHtml $ str ++ "\n"

