{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Ob.TaskSpec where

import Data.Time (fromGregorian)
import NeatInterpolation (text)
import Ob.Markdown (parseMarkdown)
import Ob.Task
import Ob.Task.Properties (TaskProperties (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "extractTasks" $ do
    it "parses task lists from markdown" $ do
      let markdownContent =
            [text|
        # Sample Tasks

        - [ ] Incomplete task
        - [X] Completed task
        - [ ] Another incomplete task
        |]

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
            [text|
        # Project Tasks

        - [ ] Review pull request 📅 2024-01-15 ⏳ 2024-01-10 #urgent #review
        - [x] Setup CI pipeline 🔺 ✅ 2024-01-08 #devops
        - [ ] Write documentation 📅 2024-01-20 #docs
        |]

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
            [text|
        # Project Tasks

        - [ ] Review pull request 📅 2024-01-15 ⏳ 2024-01-10 #urgent #review
        - [x] Setup CI pipeline 🔺 ✅ 2024-01-08 #devops
        - [ ] Write documentation 📅 2024-01-20 ⏬ #docs
        |]

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
              task1.properties.scheduledDate `shouldBe` Just (fromGregorian 2024 1 10)
              task1.properties.dueDate `shouldBe` Just (fromGregorian 2024 1 15)
              task1.properties.completedDate `shouldBe` Nothing
              task1.properties.priority `shouldBe` Normal
              task1.properties.tags `shouldBe` ["urgent", "review"]

              -- Test second task properties
              extractText task2.description `shouldBe` "Setup CI pipeline"
              task2.isCompleted `shouldBe` True
              task2.properties.scheduledDate `shouldBe` Nothing
              task2.properties.dueDate `shouldBe` Nothing
              task2.properties.completedDate `shouldBe` Just (fromGregorian 2024 1 8)
              task2.properties.priority `shouldBe` High
              task2.properties.tags `shouldBe` ["devops"]

              -- Test third task properties
              extractText task3.description `shouldBe` "Write documentation"
              task3.isCompleted `shouldBe` False
              task3.properties.scheduledDate `shouldBe` Nothing
              task3.properties.dueDate `shouldBe` Just (fromGregorian 2024 1 20)
              task3.properties.completedDate `shouldBe` Nothing
              task3.properties.priority `shouldBe` Lowest
              task3.properties.tags `shouldBe` ["docs"]
            _ -> expectationFailure "Expected exactly 3 tasks"

    it "parses tasks with links" $ do
      let markdownContent =
            [text|
        # Tasks with Links

        - [ ] Check out [this article](https://example.com)
        - [ ] https://github.com/some/repo
        - [ ] Visit [GitHub](https://github.com) and [GitLab](https://gitlab.com)
        |]

      case parseMarkdown "links.md" markdownContent of
        Left err -> expectationFailure $ "Failed to parse markdown: " <> show err
        Right (_, pandoc) -> do
          let tasks = extractTasks "links.md" pandoc
          map (extractText . description) tasks
            `shouldBe` [ "Check out this article"
                       , "https://github.com/some/repo"
                       , "Visit GitHub and GitLab"
                       ]

    it "extracts nested task lists" $ do
      let markdownContent =
            [text|
        # Project Tasks

        - [ ] Main task
          - [ ] Subtask 1
          - [x] Subtask 2
        - [ ] Another main task
          - [ ] Another subtask
        |]

      case parseMarkdown "nested.md" markdownContent of
        Left err -> expectationFailure $ "Failed to parse markdown: " <> show err
        Right (_, pandoc) -> do
          let tasks = extractTasks "nested.md" pandoc
          map (\t -> (extractText t.description, t.isCompleted)) tasks
            `shouldBe` [ ("Main task", False)
                       , ("Another main task", False)
                       , ("Subtask 1", False)
                       , ("Subtask 2", True)
                       , ("Another subtask", False)
                       ]
