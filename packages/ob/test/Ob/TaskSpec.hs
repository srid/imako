{-# LANGUAGE OverloadedRecordDot #-}

module Ob.TaskSpec where

import Ob.Markdown (parseMarkdown)
import Ob.Task
import Test.Hspec

spec :: Spec
spec = do
  describe "extractTasks" $ do
    it "parses task lists from markdown" $ do
      let markdownContent =
            unlines
              [ "# Sample Tasks"
              , ""
              , "- [ ] Incomplete task"
              , "- [X] Completed task"
              , "- [ ] Another incomplete task"
              ]

      case parseMarkdown "test.md" markdownContent of
        Left err -> expectationFailure $ "Failed to parse markdown: " <> show err
        Right (_, pandoc) -> do
          let tasks = extractTasks "test.md" pandoc
          map (\t -> (extractText t.description, t.isCompleted)) tasks
            `shouldBe` [ ("Incomplete task", False)
                       , ("Completed task", True)
                       , ("Another incomplete task", False)
                       ]

    it "parses obsidian-tasks plugin format with metadata" $ do
      let markdownContent =
            unlines
              [ "# Project Tasks"
              , ""
              , "- [ ] Review pull request 📅 2024-01-15 ⏳ 2024-01-10 #urgent #review"
              , "- [x] Setup CI pipeline 🔺 ✅ 2024-01-08 #devops"
              , "- [ ] Write documentation 📅 2024-01-20 #docs"
              ]

      case parseMarkdown "project.md" markdownContent of
        Left err -> expectationFailure $ "Failed to parse markdown: " <> show err
        Right (_, pandoc) -> do
          let tasks = extractTasks "project.md" pandoc
          map (\t -> (extractText t.description, t.isCompleted)) tasks
            `shouldBe` [ ("Review pull request", False)
                       , ("Setup CI pipeline", True)
                       , ("Write documentation", False)
                       ]
