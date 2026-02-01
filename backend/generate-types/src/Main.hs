{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Generate TypeScript types from Haskell ToJSON instances.

Usage:
  cabal run generate-types > frontend/src/types.ts
  -- or --
  just generate-types

This ensures type-safety between the Haskell backend and TypeScript frontend.
-}
module Main where

import Data.Aeson (defaultOptions)
import Data.Aeson.TypeScript.Internal (TSDeclaration (..), TSField (..))
import Data.Aeson.TypeScript.TH
import Data.Bool qualified as B
import Data.List qualified
import Data.Maybe qualified as M
import Data.Time (Day)
import Imako.API.Protocol (NotesData (..), Query (..), ServerMessage (..), TasksData (..), VaultInfo (..))
import Imako.Core.Filter (Filter (..))
import Imako.Core.FolderTree (FolderNode (..))
import Ob.Task (Task (..), TaskStatus (..))
import Ob.Task.Properties (Priority (..))
import Text.Pandoc.Definition (Pandoc)

-- | Helper to create a required TSField (works around relude/base Bool conflict)
reqField :: String -> String -> TSField
reqField name typ = TSField B.False name typ M.Nothing

-- | Helper to create an optional TSField
optField :: String -> String -> TSField
optField name typ = TSField B.True name typ M.Nothing

-- Derive TypeScript instances for types with generic ToJSON
$(deriveTypeScript defaultOptions ''TaskStatus)
$(deriveTypeScript defaultOptions ''Priority)
$(deriveTypeScript defaultOptions ''Filter)

-- Day is serialized as a string (ISO date)
instance TypeScript Day where
  getTypeScriptType _ = "string"
  getTypeScriptDeclarations _ = [] -- No separate declaration needed

-- Task has a custom ToJSON, so we need to manually define what it looks like
-- This matches the JSON output from Task.hs ToJSON instance
instance TypeScript Task where
  getTypeScriptType _ = "Task"
  getTypeScriptDeclarations _ =
    [ TSInterfaceDeclaration
        { interfaceName = "Task"
        , interfaceGenericVariables = []
        , interfaceMembers =
            [ reqField "description" "string"
            , reqField "sourceNote" "string"
            , reqField "status" "TaskStatus"
            , optField "dueDate" "string"
            , optField "scheduledDate" "string"
            , optField "startDate" "string"
            , optField "completedDate" "string"
            , reqField "priority" "Priority"
            , reqField "tags" "string[]"
            , reqField "parentBreadcrumbs" "string[]"
            ]
        , interfaceDoc = M.Nothing
        }
    ]

$(deriveTypeScript defaultOptions ''FolderNode)
$(deriveTypeScript defaultOptions ''Query)
$(deriveTypeScript defaultOptions ''VaultInfo)
$(deriveTypeScript defaultOptions ''TasksData)
$(deriveTypeScript defaultOptions ''NotesData)
$(deriveTypeScript defaultOptions ''ServerMessage)

main :: IO ()
main = do
  putStrLn header
  putStrLn $ formatTSDeclarations' defaultFormattingOptions allDecls
  where
    allDecls =
      mconcat
        [ getTypeScriptDeclarations (Proxy @TaskStatus)
        , getTypeScriptDeclarations (Proxy @Priority)
        , getTypeScriptDeclarations (Proxy @Task)
        , getTypeScriptDeclarations (Proxy @Filter)
        , getTypeScriptDeclarations (Proxy @FolderNode)
        , getTypeScriptDeclarations (Proxy @Query)
        , getTypeScriptDeclarations (Proxy @VaultInfo)
        , getTypeScriptDeclarations (Proxy @TasksData)
        , getTypeScriptDeclarations (Proxy @NotesData)
        , getTypeScriptDeclarations (Proxy @ServerMessage)
        ]

    header :: String
    header =
      Data.List.unlines
        [ "/**"
        , " * AUTO-GENERATED TypeScript types from Haskell."
        , " * DO NOT EDIT MANUALLY."
        , " *"
        , " * Regenerate with: cabal run generate-types > frontend/src/types.ts"
        , " * Or: just generate-types"
        , " *"
        , " * Source Haskell files:"
        , " *   - Task, TaskStatus: packages/ob/src/Ob/Task.hs"
        , " *   - Priority: packages/ob/src/Ob/Task/Properties.hs"
        , " *   - FolderNode: packages/imako/src/Imako/Core/FolderTree.hs"
        , " *   - Filter: packages/imako/src/Imako/Core/Filter.hs"
        , " *   - Protocol types: packages/imako/src/Imako/API/Protocol.hs"
        , " */"
        , ""
        ]
