{-# LANGUAGE OverloadedStrings #-}
module Html.Bootstrap where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Blaze.Html5
--import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

role :: AttributeValue -> Attribute
role = customAttribute "role"

dataToggle :: AttributeValue -> Attribute
dataToggle = dataAttribute "toggle"

ariaExpanded :: Bool -> Attribute
ariaExpanded = customAttribute "aria-expanded" . toTruth

toTruth :: Bool -> AttributeValue
toTruth t = if t then "true" else "false"

liDropdown :: Html -> Html -> Html
liDropdown toggle lst = li ! class_ "dropdown" $ do
    a ! href "#" ! class_ "dropdown-toggle" ! role "button"
      ! dataToggle "dropdown" ! ariaExpanded False $ do
        toggle
        ul ! class_ "dropdown-menu" ! role "menu" $
            lst

liIcon :: AttributeValue -> Text -> Text -> Html
liIcon hrf icn txt = li $ a ! href hrf $ faIcon icn $ toHtml txt

faIcon :: Text -> Html -> Html
faIcon icn htmel = do
    i ! class_ (toValue $ "fa fa-" `T.append` icn) $ mempty
    htmel
