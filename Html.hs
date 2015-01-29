{-# LANGUAGE OverloadedStrings #-}
module Html (
  blaze,
  blazePretty
) where

import           Web.Scotty.Trans
import           Text.Blaze.Html (Html)
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
