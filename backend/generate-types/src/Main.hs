{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Generate TypeScript types from Haskell ToJSON instances.

Usage:
  generate-types protocol  -- Generates frontend/src/types.ts
  generate-types ast       -- Generates frontend/src/components/markdown/types.ts

Run via justfile:
  just generate-types

This ensures type-safety between the Haskell backend and TypeScript frontend.
-}
module Main where

import Data.Aeson.TypeScript.TH
import Data.List qualified
import Imako.API.Protocol (protocolTsDeclarations)
import Imako.Core.FolderTree (folderTreeTsDeclarations)
import Ob.Task (taskTsDeclarations)
import Ob.Task.Properties (priorityTsDeclarations)
import Text.Pandoc.Definition.TypeScript (pandocTsDeclarations)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["protocol"] -> putStrLn generateProtocol
    ["ast"] -> putStrLn generateAst
    _ -> do
      putStrLn "Usage: generate-types <command>"
      putStrLn ""
      putStrLn "Commands:"
      putStrLn "  protocol  Generate protocol types (frontend/src/types.ts)"
      putStrLn "  ast       Generate AST types (frontend/src/components/markdown/types.ts)"

-- | Generate protocol types for frontend/src/types.ts
generateProtocol :: String
generateProtocol =
  protocolHeader <> formatTSDeclarations' defaultFormattingOptions protocolDecls
  where
    protocolDecls =
      mconcat
        [ taskTsDeclarations
        , priorityTsDeclarations
        , folderTreeTsDeclarations
        , protocolTsDeclarations
        ]

    protocolHeader :: String
    protocolHeader =
      Data.List.unlines
        [ "/**"
        , " * AUTO-GENERATED TypeScript types from Haskell."
        , " * DO NOT EDIT MANUALLY."
        , " *"
        , " * Regenerate with: just generate-types"
        , " *"
        , " * Source Haskell files:"
        , " *   - Task, TaskStatus: packages/ob/src/Ob/Task.hs"
        , " *   - Priority: packages/ob/src/Ob/Task/Properties.hs"
        , " *   - FolderNode: packages/imako/src/Imako/Core/FolderTree.hs"
        , " *   - Protocol types: packages/imako/src/Imako/API/Protocol.hs"
        , " */"
        , ""
        ]

-- | Generate AST types for frontend/src/components/markdown/types.ts
generateAst :: String
generateAst =
  astHeader <> formatTSDeclarations' defaultFormattingOptions pandocTsDeclarations
  where
    astHeader :: String
    astHeader =
      Data.List.unlines
        [ "/**"
        , " * AUTO-GENERATED AST types from Haskell."
        , " * DO NOT EDIT MANUALLY."
        , " *"
        , " * Regenerate with: just generate-types"
        , " *"
        , " * Source: packages/ob/src/Text/Pandoc/Definition/TypeScript.hs"
        , " *"
        , " * These types match Pandoc's Block/Inline AST for markdown rendering."
        , " * Uses discriminated unions with \"tag\" field for pattern matching."
        , " */"
        , ""
        ]
