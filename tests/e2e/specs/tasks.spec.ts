/**
 * Tasks page tests.
 *
 * Verifies task rendering, status display, folder tree, and parent breadcrumbs.
 *
 * Expected vault state (example/):
 * Filters hide Completed/Cancelled tasks by default.
 * Visible tasks (17 total):
 * - Notes/Tasks.md: 13 visible (2 completed/cancelled hidden)
 *   - 6 are nested subtasks with parent breadcrumbs
 * - Projects/Active.md: 4 visible (1 completed hidden)
 *
 * Note: Tags are rendered with 🏷️ emoji, not # symbol
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
  { text: "Pack the box", status: "Incomplete", breadcrumbs: ["Alice: move Box to her place!"] },
  { text: "Schedule pickup", status: "Incomplete", breadcrumbs: ["Alice: move Box to her place!"] },
  { text: "Write task rendering tests", status: "InProgress", breadcrumbs: ["Write E2E tests for core features"] },
  { text: "Write navigation tests", status: "Incomplete", breadcrumbs: ["Write E2E tests for core features"] },
  { text: "Read official docs", status: "Incomplete", breadcrumbs: ["Research Playwright best practices"] },
  { text: "Try page object pattern", status: "Incomplete", breadcrumbs: ["Research Playwright best practices"] },
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

  test("nested tasks show parent breadcrumbs", async ({ app }) => {
    const tasks = app.tasks();
    await tasks.waitForTasks();

    // Verify nested tasks have correct breadcrumbs
    const nestedTasks = EXPECTED_TASKS.filter((t) => t.breadcrumbs);
    for (const task of nestedTasks) {
      const item = tasks.taskItems().filter({ hasText: task.text });
      const crumbs = item.locator("[data-testid='task-breadcrumbs']");
      await expect(crumbs).toBeVisible();
      await expect(crumbs).toContainText(task.breadcrumbs!.join(" › "));
    }

    // Top-level tasks should NOT have breadcrumbs
    const topLevelCount = EXPECTED_TASKS.filter((t) => !t.breadcrumbs).length;
    const allBreadcrumbs = tasks.taskItems().locator("[data-testid='task-breadcrumbs']");
    // Only nested tasks should have breadcrumbs (4 nested tasks)
    await expect(allBreadcrumbs).toHaveCount(nestedTasks.length);
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
