{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Html.Task where

import Prelude as P
import Types
import Control.Monad
import Html.Common
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Data.ByteString.Char8 as B

taskLinkHtml :: UniqueID -> Html
taskLinkHtml uid = userContainer $ p $ do
    void $ "The task is running and can be checked at "
    a ! href (toValue ("/task/" ++ show uid)) $
        toHtml $ unwords ["Task", show uid]

taskHtml :: [B.ByteString] -> Html
taskHtml out = userContainer $ H.div $ pre $ toHtml $ B.unpack $ foldr B.append "" out
