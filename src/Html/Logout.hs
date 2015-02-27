{-# LANGUAGE OverloadedStrings #-}
module Html.Logout where

import Prelude
import Html.Common
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
--import Text.Blaze.Html5.Attributes
--import qualified Text.Blaze.Html5.Attributes as A

logoutHtml :: Html
logoutHtml = guestContainer $ H.div "You have been logged out."
