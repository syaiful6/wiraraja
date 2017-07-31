{-# LANGUAGE OverloadedStrings #-}

module Web.Wiraraja.Application where

import qualified Network.HTTP.Types as H
import           Network.Wai (Application, responseBuilder)

import           Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)

import           Web.Wiraraja.Routing (Match, end, routeMiddleware)
import           Web.Wiraraja.Layout (page404, homePage)

data Page = Home

routePage :: Match Page
routePage = Home <$ end

page404Handler :: Application
page404Handler _ send = send $ responseBuilder
    H.status404
    [("Content-Type", "text/html")]
    (renderHtmlBuilder page404)

homeHandler :: Application
homeHandler _ send = send $ responseBuilder
    H.status200
    [("Content-Type", "text/html")]
    (renderHtmlBuilder homePage)

routingToApp :: Page -> Application
routingToApp page = case page of
    Home -> homeHandler

application :: Application
application = routeMiddleware (routingToApp <$> routePage) page404Handler
