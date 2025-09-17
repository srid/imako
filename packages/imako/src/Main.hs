{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}

module Main where

import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Htmx.Lucid.Core
import Imako.CLI qualified as CLI
import Lucid
import Main.Utf8 qualified as Utf8
import Network.HTTP.Types.Status (status404)
import Ob qualified
import Options.Applicative (execParser)
import System.FilePath (takeBaseName)
import Text.Pandoc.Definition (Pandoc)
import Web.Scotty qualified as S

type Note = Either Text (Maybe Aeson.Value, Pandoc)

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    options <- liftIO $ execParser CLI.opts
    putTextLn "Starting web server on http://localhost:3000"
    Ob.withLiveNotebook options.path $ \notebookVar -> do
      S.scotty 3000 $ do
        S.get "/" $ do
          notebook <- liftIO $ readTVarIO notebookVar
          S.html $ renderText $ homePage notebook

        S.get "/note/:path" $ do
          notePath <- S.captureParam "path"
          notebook <- liftIO $ readTVarIO notebookVar
          case Map.lookup notePath notebook of
            Just note -> S.html $ renderText $ noteView notePath note
            Nothing -> S.status status404 >> S.text "Note not found"

-- | Main page showing all notes
homePage :: Ob.Notebook -> Html ()
homePage notebook = html_ $ do
  head_ $ do
    title_ "Imako - Notebook Viewer"
    meta_ [charset_ "UTF-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
    script_ [src_ "https://cdn.tailwindcss.com"] ("" :: Text)
    script_ [src_ "https://unpkg.com/htmx.org@1.9.10"] ("" :: Text)

  body_ [class_ "bg-gray-50 min-h-screen"] $ do
    div_ [class_ "flex h-screen"] $ do
      -- Main content area (left side - scrollable)
      div_ [class_ "flex-1 overflow-y-auto p-6"] $ do
        h1_ [class_ "text-3xl font-bold text-gray-800 mb-8"] "📓 Notebook"
        div_ [id_ "notes-grid", class_ "grid gap-4 md:grid-cols-2 lg:grid-cols-3"] $ do
          forM_ (Map.toList notebook) $ \(path, note) -> do
            noteCard path note

      -- Sticky detail panel (right side - fixed width, scrollable)
      div_ [class_ "w-1/3 min-w-[400px] border-l border-gray-200 bg-white flex flex-col"] $ do
        div_ [class_ "p-4 border-b border-gray-200 bg-gray-50"] $ do
          h2_ [class_ "text-lg font-semibold text-gray-800"] "Note Details"
        div_ [class_ "flex-1 overflow-y-auto"] $ do
          div_ [id_ "note-detail", class_ "p-6"] $ do
            div_ [class_ "text-center text-gray-500 mt-20"] $ do
              p_ [class_ "text-sm"] "Click on a note to view its content here"

-- | Individual note card
noteCard :: FilePath -> Note -> Html ()
noteCard path note =
  div_
    [ class_ "bg-white rounded-lg border border-gray-200 p-4 hover:shadow-md transition-shadow cursor-pointer"
    , hxGet_ ("/note/" <> toText path)
    , hxTarget_ "#note-detail"
    , hxSwap_ "innerHTML"
    ]
    $ do
      h3_ [class_ "font-semibold text-gray-800 mb-2"] $ toHtml $ takeBaseName path
      case note of
        Left err -> p_ [class_ "text-red-500 text-sm"] $ toHtml err
        Right (frontmatter, _pandoc) -> do
          case frontmatter of
            Just fm -> div_ [class_ "text-xs text-gray-500 mb-2"] $ toHtml (show fm :: Text)
            Nothing -> mempty
          p_ [class_ "text-gray-600 text-sm"] "Click to view content..."

-- | Detailed note view
noteView :: FilePath -> Note -> Html ()
noteView path note =
  div_ [class_ "bg-white rounded-lg border border-gray-200 p-6"] $ do
    h2_ [class_ "text-2xl font-bold text-gray-800 mb-4"] $ toHtml $ takeBaseName path
    case note of
      Left err -> p_ [class_ "text-red-500"] $ toHtml err
      Right (frontmatter, pandoc) -> do
        case frontmatter of
          Just fm ->
            div_ [class_ "bg-gray-50 rounded p-3 mb-4"] $ do
              h4_ [class_ "font-semibold text-gray-700 mb-2"] "Frontmatter:"
              pre_ [class_ "text-sm text-gray-600"] $ toHtml (show fm :: Text)
          Nothing -> mempty
        div_ [class_ "prose max-w-none"] $ do
          p_ [class_ "text-gray-700"] $ toHtml (show pandoc :: Text)
