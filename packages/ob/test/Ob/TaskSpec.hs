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
          map (\t -> (extractText t.description, t.status)) tasks
            `shouldBe` [ ("Incomplete task", Incomplete)
                       , ("Completed task", Completed)
                       , ("Another incomplete task", Incomplete)
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
          map (\t -> (extractText t.description, t.status)) tasks
            `shouldBe` [ ("Review pull request", Incomplete)
                       , ("Setup CI pipeline", Completed)
                       , ("Write documentation", Incomplete)
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
              task1.status `shouldBe` Incomplete
              task1.properties.scheduledDate `shouldBe` Just (fromGregorian 2024 1 10)
              task1.properties.dueDate `shouldBe` Just (fromGregorian 2024 1 15)
              task1.properties.completedDate `shouldBe` Nothing
              task1.properties.priority `shouldBe` Normal
              task1.properties.tags `shouldBe` ["urgent", "review"]

              -- Test second task properties
              extractText task2.description `shouldBe` "Setup CI pipeline"
              task2.status `shouldBe` Completed
              task2.properties.scheduledDate `shouldBe` Nothing
              task2.properties.dueDate `shouldBe` Nothing
              task2.properties.completedDate `shouldBe` Just (fromGregorian 2024 1 8)
              task2.properties.priority `shouldBe` High
              task2.properties.tags `shouldBe` ["devops"]

              -- Test third task properties
              extractText task3.description `shouldBe` "Write documentation"
              task3.status `shouldBe` Incomplete
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
          map (\t -> (extractText t.description, t.status, t.parentTasks)) tasks
            `shouldBe` [ ("Main task", Incomplete, [])
                       , ("Subtask 1", Incomplete, ["Main task"])
                       , ("Subtask 2", Completed, ["Main task"])
                       , ("Another main task", Incomplete, [])
                       , ("Another subtask", Incomplete, ["Another main task"])
                       ]

    it "tracks only parent tasks in breadcrumb, not plain list items" $ do
      let markdownContent =
            [text|
        # Project

        - Project context
          - [/] Feature A
            - Plain item
              - [ ] Deep task
        |]

      case parseMarkdown "context.md" markdownContent of
        Left err -> expectationFailure $ "Failed to parse markdown: " <> show err
        Right (_, pandoc) -> do
          let tasks = extractTasks "context.md" pandoc
          map (\t -> (extractText t.description, t.parentTasks)) tasks
            `shouldBe` [ ("Feature A", [])
                       , ("Deep task", ["Feature A"])
                       ]

    it "preserves quoted text in task description" $ do
      let markdownContent =
            [text|
        # Tasks

        - [ ] "In Progress" tasks should be shown as well
        - [ ] Fix the "bug" in parser
        - [ ] Use 'single quotes' too
        |]

      case parseMarkdown "quotes.md" markdownContent of
        Left err -> expectationFailure $ "Failed to parse markdown: " <> show err
        Right (_, pandoc) -> do
          let tasks = extractTasks "quotes.md" pandoc
          map (extractText . description) tasks
            `shouldBe` [ "\"In Progress\" tasks should be shown as well"
                       , "Fix the \"bug\" in parser"
                       , "Use 'single quotes' too"
                       ]

    it "preserves inline formatting in task description" $ do
      let markdownContent =
            [text|
        # Tasks

        - [ ] This has _italic_ and **bold** text
        - [ ] Code with `inline code` element
        - [ ] Check [this link](https://example.com)
        |]

      case parseMarkdown "format.md" markdownContent of
        Left err -> expectationFailure $ "Failed to parse markdown: " <> show err
        Right (_, pandoc) -> do
          let tasks = extractTasks "format.md" pandoc
          -- Just verify we can extract text without errors
          -- The actual HTML rendering is tested visually
          map (extractText . description) tasks
            `shouldBe` [ "This has italic and bold text"
                       , "Code with inline code element"
                       , "Check this link"
                       ]

    it "tracks parent start dates for future task filtering" $ do
      let markdownContent =
            [text|
        # Tasks

        - [ ] Parent with future start 🛫 2030-01-01
          - [ ] Child task without start date
        - [ ] Another parent without start
          - [ ] Another child
        |]

      case parseMarkdown "future.md" markdownContent of
        Left err -> expectationFailure $ "Failed to parse markdown: " <> show err
        Right (_, pandoc) -> do
          let tasks = extractTasks "future.md" pandoc
          case tasks of
            [parent1, child1, parent2, child2] -> do
              -- Parent with future start should have it in properties
              parent1.properties.startDate `shouldBe` Just (fromGregorian 2030 1 1)
              parent1.parentStartDates `shouldBe` []

              -- Child should inherit parent's start date in parentStartDates
              child1.properties.startDate `shouldBe` Nothing
              child1.parentStartDates `shouldBe` [Just (fromGregorian 2030 1 1)]

              -- Parent without start
              parent2.properties.startDate `shouldBe` Nothing
              parent2.parentStartDates `shouldBe` []

              -- Child of parent without start
              child2.properties.startDate `shouldBe` Nothing
              child2.parentStartDates `shouldBe` [Nothing]
            _ -> expectationFailure $ "Expected 4 tasks but got " <> show (length tasks)
