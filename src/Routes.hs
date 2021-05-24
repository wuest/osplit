{-# LANGUAGE OverloadedStrings #-}

module Routes ( routes )
where

import Prelude
import Data.Text.Lazy ( Text )
import Control.Monad.Reader ( ReaderT )

import Web.Scotty.Trans
import Text.Blaze.Html5 ( Html )
import Text.Blaze.Html.Renderer.Text

import qualified Const
import qualified View
import qualified Data.Sqlite as DB

type Router = ScottyT Text DB.DBPoolM ()
type Response = ActionT Text DB.DBPoolM ()

blaze :: Html -> Response
blaze = html . renderHtml

mainCSS :: Response
mainCSS = do
    setHeader "Content-Type" "text/css"
    text Const.mainCSS

viewCSS :: Response
viewCSS = do
    setHeader "Content-Type" "text/css"
    text Const.viewCSS

mainJS :: Response
mainJS = do
    setHeader "Content-Type" "application/javascript"
    text Const.mainJS

viewJS :: Response
viewJS = do
    setHeader "Content-Type" "application/javascript"
    text Const.viewJS

websocketJS :: Response
websocketJS = do
    setHeader "Content-Type" "application/javascript"
    text Const.websocketJS

gamepadJS :: Response
gamepadJS = do
    setHeader "Content-Type" "application/javascript"
    text Const.gamepadJS

index :: Response
index = blaze View.index

viewIndex :: Response
viewIndex = blaze View.viewIndex

customIndex :: Response
customIndex = do
    name <- param "ref"
    blaze $ View.index' name

routes :: Router
routes = do
-- Health Check
    get "/_ping" $ text "OK"

-- Static Content
    get "/main.css" mainCSS
    get "/view.css" viewCSS
    get "/main.js" mainJS
    get "/view.js" viewJS
    get "/websocket.js" websocketJS
    get "/gamepad.js" gamepadJS

-- Public routes
    get "/" index
    get "/view" viewIndex
    get "/view/:ref" customIndex
