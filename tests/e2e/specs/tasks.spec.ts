/**
 * Tasks page tests.
 *
 * Verifies task rendering, status display, folder tree, and hierarchical nesting.
 *
 * Expected vault state (example/):
 * Filters hide Completed/Cancelled tasks by default.
 * Visible tasks (17 total):
 * - Notes/Tasks.md: 13 visible (2 completed/cancelled hidden)
 *   - 6 are nested subtasks rendered inside their parents
 * - Projects/Active.md: 4 visible (1 completed hidden)
 */

import { test, expect, TaskExpectation } from "../dsl";

// All visible tasks from the example vault (Completed/Cancelled hidden by default)
const EXPECTED_TASKS: TaskExpectation[] = [
  // Notes/Tasks.md — top-level
  { text: "Add mobile support", status: "Incomplete" },
  { text: "Alice: move Box to her place!", status: "Incomplete" },
  { text: "Explore offline mode", status: "Incomplete" },
  { text: "Implement backlinks feature", status: "Incomplete", contains: ["feature"] },
  { text: "Review the Imako implementation plan", status: "Incomplete" },
  { text: "Write E2E tests for core features", status: "Incomplete" },
  { text: "Research Playwright best practices", status: "InProgress", contains: ["research"] },
  // Notes/Tasks.md — nested subtasks
  { text: "Pack the box", status: "Incomplete" },
  { text: "Schedule pickup", status: "Incomplete" },
  { text: "Write task rendering tests", status: "InProgress" },
  { text: "Write navigation tests", status: "Incomplete" },
  { text: "Read official docs", status: "Incomplete" },
  { text: "Try page object pattern", status: "Incomplete" },
  // Projects/Active.md
  { text: "Add live sync tests", status: "Incomplete" },
  { text: "Add screenshots", status: "Incomplete" },
  { text: "Complete E2E test infrastructure", status: "Incomplete" },
  { text: "Write user guide", status: "Incomplete" },
];

const EXPECTED_FOLDERS = ["Notes", "Projects"];

test.describe("Tasks Page", () => {
  test.beforeEach(async ({ app }) => {
    await app.navigateTo("/tasks");
  });

  test("renders visible tasks with correct content", async ({ app }) => {
    const tasks = app.tasks();
    await tasks.waitForTasks();

    // Verify all visible tasks are rendered with correct content
    await tasks.verifyTasks(EXPECTED_TASKS);
  });

  test("displays folder tree with correct folders", async ({ app }) => {
    const tree = app.folderTree();

    // Verify exact folder count and names
    const folders = tree.folders();
    await expect(folders).toHaveCount(EXPECTED_FOLDERS.length);

    for (const folderName of EXPECTED_FOLDERS) {
      await expect(folders.filter({ hasText: folderName })).toBeVisible();
    }
  });

  test("nested tasks are rendered inside their parent", async ({ app }) => {
    const tasks = app.tasks();
    await tasks.waitForTasks();

    // "Pack the box" should be nested inside "Alice: move Box to her place!"
    const aliceParent = tasks.taskItems().filter({ hasText: "Alice: move Box to her place!" }).first();
    // The parent task-item should contain child task-items
    const packChild = aliceParent.locator("[data-testid='task-item']").filter({ hasText: "Pack the box" });
    await expect(packChild).toBeVisible();

    const scheduleChild = aliceParent.locator("[data-testid='task-item']").filter({ hasText: "Schedule pickup" });
    await expect(scheduleChild).toBeVisible();
  });

  test("hides children and grandchildren of future-dated parents", async ({ app }) => {
    const tasks = app.tasks();
    await tasks.waitForTasks();

    // Future parent and its descendants should NOT be visible by default
    const futureTexts = ["Plan 2090 conference", "Book venue", "Research catering options"];
    for (const text of futureTexts) {
      await expect(tasks.taskItems().filter({ hasText: text })).toHaveCount(0);
    }

    // Toggle "Future tasks" filter — now they should appear in the DOM
    await tasks.toggleFilter("Future tasks");
    // All 3 tasks (parent + child + grandchild) should now exist
    for (const text of futureTexts) {
      await expect(tasks.taskItems().filter({ hasText: text }).first()).toBeAttached();
    }

    // Toggle off again — they should disappear
    await tasks.toggleFilter("Future tasks");
    for (const text of futureTexts) {
      await expect(tasks.taskItems().filter({ hasText: text })).toHaveCount(0);
    }
  });
});
