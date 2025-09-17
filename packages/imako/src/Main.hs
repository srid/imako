{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}

module Main where

import Htmx.Lucid.Core
import Lucid
import Main.Utf8 qualified as Utf8
import Web.Scotty qualified as S

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    S.scotty 3000 $ do
      S.get "/" $ do
        S.html $ renderText homePage

      S.post "/button-click" $ do
        S.html $ renderText $ p_ "Button was clicked! 🎉"

homePage :: Html ()
homePage = html_ $ do
  head_ $ do
    title_ "Hello World - Imako"
    meta_ [charset_ "UTF-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
    script_ [src_ "https://cdn.tailwindcss.com"] ("" :: Text)
    script_ [src_ "https://unpkg.com/htmx.org@1.9.10"] ("" :: Text)

  body_ [class_ "bg-gray-100 min-h-screen flex items-center justify-center"] $ do
    div_ [class_ "bg-white p-8 rounded-lg shadow-lg max-w-md w-full"] $ do
      h1_ [class_ "text-3xl font-bold text-gray-800 mb-6 text-center"] "Hello World!"

      p_
        [class_ "text-gray-600 mb-6 text-center"]
        "Welcome to Imako - a simple web app built with Scotty, Lucid2, and htmx."

      div_ [class_ "text-center"] $ do
        button_
          [ class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded transition duration-200"
          , hxPost_ "/button-click"
          , hxTarget_ "#result"
          , hxSwap_ "innerHTML"
          ]
          "Click me!"

        div_ [id_ "result", class_ "mt-4 text-center text-gray-700"] ""
