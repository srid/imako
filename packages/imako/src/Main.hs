module Main where

import Data.Acid (AcidState, openLocalState, query, update)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Htmx.Lucid.Core
import Imako.GraphDB
import Lucid
import Main.Utf8 qualified as Utf8
import Web.Scotty qualified as S

main :: IO ()
main = Utf8.withUtf8 $ do
  db <- openLocalState emptyGraphDB
  initSampleData db

  S.scotty 3000 $ do
    S.get "/" $ do
      nodes <- liftIO $ query db GetAllNodes
      edges <- liftIO $ query db GetAllEdges
      S.html $ renderText $ page nodes edges

    S.post "/add-node" $ do
      body <- S.formParam "body"
      nodeId <- liftIO nextRandom
      liftIO $ update db (AddNode $ Node nodeId body)
      nodes <- liftIO $ query db GetAllNodes
      edges <- liftIO $ query db GetAllEdges
      S.html $ renderText $ grid nodes edges

-- | Initialize with sample data if empty
initSampleData :: AcidState GraphDB -> IO ()
initSampleData db = do
  nodes <- query db GetAllNodes
  when (null nodes) $ do
    [id1, id2, id3] <- replicateM 3 nextRandom
    let samples =
          [ Node id1 "Learn Haskell fundamentals"
          , Node id2 "Monads are monoids in the category of endofunctors"
          , Node id3 "Build a web app with Scotty"
          ]
    mapM_ (update db . AddNode) samples
    void $ update db (AddEdge id2 id1)
    void $ update db (AddEdge id1 id3)

-- | Main page
page :: [Node] -> [(UUID, UUID)] -> Html ()
page nodes edges = html_ $ do
  head_ $ do
    title_ "Imako"
    meta_ [charset_ "UTF-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
    script_ [src_ "https://cdn.tailwindcss.com"] ("" :: Text)
    script_ [src_ "https://unpkg.com/htmx.org@1.9.10"] ("" :: Text)

  body_ [class_ "bg-gray-50 min-h-screen p-4"] $
    div_ [class_ "max-w-6xl mx-auto"] $ do
      searchBox
      grid nodes edges

-- | Search-style input box
searchBox :: Html ()
searchBox =
  form_ [hxPost_ "/add-node", hxTarget_ "#grid", hxSwap_ "outerHTML", class_ "mb-6"] $
    input_
      [ type_ "text"
      , name_ "body"
      , placeholder_ "Take a note..."
      , class_ "w-full max-w-md p-3 border border-gray-300 rounded-lg shadow-sm focus:ring-2 focus:ring-blue-500 focus:border-blue-500 outline-none"
      , required_ ""
      ]

-- | Masonry grid of cards
grid :: [Node] -> [(UUID, UUID)] -> Html ()
grid nodes edges =
  div_ [id_ "grid", class_ "columns-1 sm:columns-2 md:columns-3 lg:columns-4 xl:columns-5 gap-4"] $
    forM_ nodes $
      \node -> card node (edgesFor node edges nodes)

-- | Note card
card :: Node -> [Text] -> Html ()
card node connections =
  div_ [class_ "break-inside-avoid mb-4"] $
    div_ [class_ "bg-white rounded-lg border border-gray-200 p-4 hover:shadow-md transition-shadow"] $ do
      p_ [class_ "text-gray-800 text-sm whitespace-pre-wrap"] $ toHtml $ nodeBody node
      unless (null connections) $
        div_ [class_ "mt-2 pt-2 border-t border-gray-100 text-xs text-blue-600"] $ do
          span_ "→ "
          span_ $ toHtml $ T.intercalate ", " connections

-- | Find connected nodes
edgesFor :: Node -> [(UUID, UUID)] -> [Node] -> [Text]
edgesFor node allEdges allNodes =
  [ nodeBody target
  | (from, to) <- allEdges
  , from == nodeId node
  , target <- allNodes
  , nodeId target == to
  ]
