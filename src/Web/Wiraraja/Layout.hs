{-# LANGUAGE OverloadedStrings #-}

module Web.Wiraraja.Layout where

import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

layout :: String -> [H.Html] -> H.Html -> H.Html
layout title hdrs bdy = H.docTypeHtml $ do
    H.head $ do
        H.title $ H.toHtml title
        sequence_ hdrs
    H.body $ do
        bdy

homePage :: H.Html
homePage = layout "Wiraraja" [] $ do
    H.div ! A.class_ "container" $ do
        H.h1 "Wiraraja"

page404 :: H.Html
page404 = layout "404 NotFound" [] $ do
    H.div ! A.class_ "container" $ do
        H.h1 "404 Not Found"
