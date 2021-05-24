{-# LANGUAGE TemplateHaskell #-}

module Const where

import Prelude ( ($), String )
import Data.Text.Lazy ( fromStrict )
import Data.Text.Internal.Lazy ( Text )
import Data.Text.Encoding ( decodeUtf8 )
import Data.FileEmbed  ( embedFile )

applicationName :: String
applicationName = "osplit"

version :: String
version = "0.1.0.0"

mainCSS :: Text
mainCSS = fromStrict $ decodeUtf8 $(embedFile "static/main.css")

viewCSS :: Text
viewCSS = fromStrict $ decodeUtf8 $(embedFile "static/view.css")

mainJS :: Text
mainJS = fromStrict $ decodeUtf8 $(embedFile "static/main.js")

viewJS :: Text
viewJS = fromStrict $ decodeUtf8 $(embedFile "static/view.js")

gamepadJS :: Text
gamepadJS = fromStrict $ decodeUtf8 $(embedFile "static/gamepad.js")

websocketJS :: Text
websocketJS = fromStrict $ decodeUtf8 $(embedFile "static/websocket.js")
