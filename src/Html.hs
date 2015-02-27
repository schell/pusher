{-# LANGUAGE OverloadedStrings #-}
module Html (
  blaze,
  blazePretty,
  module H
) where

import Web.Scotty.Trans
import Html.Common as H
import Html.NewUser as H
import Html.UserAddBucket as H
import Html.Upload as H
import Html.UploadZip as H
import Html.Copy as H
import Html.CopyFolder as H
import Html.Login as H
import Html.Logout as H
import Html.Log as H
import Html.Users as H
import Text.Blaze.Html5
import qualified Text.Blaze.Html.Renderer.Utf8 as Utf8
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty
import qualified Data.ByteString.Lazy.Char8 as B

-- | Render some Blaze Html
--
blaze :: (ScottyError e, Monad m) => Html -> ActionT e m ()
blaze h = do
  setHeader "Content-Type" "text/html"
  raw $ Utf8.renderHtml h

blazePretty :: (ScottyError e, Monad m) => Html -> ActionT e m ()
blazePretty h = do
    setHeader "Content-Type" "text/html"
    raw $ B.pack $ Pretty.renderHtml h
