{-# LANGUAGE OverloadedRecordDot #-}

module Ob.TaskSpec where

import Data.Time (fromGregorian)
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

    it "extracts obsidian-tasks properties correctly" $ do
      let markdownContent =
            unlines
              [ "# Project Tasks"
              , ""
              , "- [ ] Review pull request 📅 2024-01-15 ⏳ 2024-01-10 #urgent #review"
              , "- [x] Setup CI pipeline 🔺 ✅ 2024-01-08 #devops"
              , "- [ ] Write documentation 📅 2024-01-20 ⏬ #docs"
              ]

      case parseMarkdown "project.md" markdownContent of
        Left err -> expectationFailure $ "Failed to parse markdown: " <> show err
        Right (_, pandoc) -> do
          let tasks = extractTasks "project.md" pandoc
          length tasks `shouldBe` 3

          case tasks of
            [task1, task2, task3] -> do
              -- Test first task properties
              extractText task1.description `shouldBe` "Review pull request"
              task1.isCompleted `shouldBe` False
              task1.scheduledDate `shouldBe` Just (fromGregorian 2024 1 10)
              task1.dueDate `shouldBe` Just (fromGregorian 2024 1 15)
              task1.completedDate `shouldBe` Nothing
              task1.priority `shouldBe` Normal
              task1.tags `shouldBe` ["urgent", "review"]

              -- Test second task properties
              extractText task2.description `shouldBe` "Setup CI pipeline"
              task2.isCompleted `shouldBe` True
              task2.scheduledDate `shouldBe` Nothing
              task2.dueDate `shouldBe` Nothing
              task2.completedDate `shouldBe` Just (fromGregorian 2024 1 8)
              task2.priority `shouldBe` High
              task2.tags `shouldBe` ["devops"]

              -- Test third task properties
              extractText task3.description `shouldBe` "Write documentation"
              task3.isCompleted `shouldBe` False
              task3.scheduledDate `shouldBe` Nothing
              task3.dueDate `shouldBe` Just (fromGregorian 2024 1 20)
              task3.completedDate `shouldBe` Nothing
              task3.priority `shouldBe` Lowest
              task3.tags `shouldBe` ["docs"]
            _ -> expectationFailure "Expected exactly 3 tasks"
