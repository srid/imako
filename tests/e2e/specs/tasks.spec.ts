/**
 * Tasks page tests.
 *
 * Verifies task rendering, status display, and folder tree.
 *
 * Expected vault state (example/):
 * Filters hide Completed/Cancelled tasks by default.
 * Visible tasks (11 total):
 * - Notes/Tasks.md: 7 visible (2 completed/cancelled hidden)
 * - Projects/Active.md: 4 visible (1 completed hidden)
 *
 * Note: Tags are rendered with 🏷️ emoji, not # symbol
 */

import { test, expect, TaskExpectation } from "../dsl";

// All visible tasks from the example vault (Completed/Cancelled hidden by default)
const EXPECTED_TASKS: TaskExpectation[] = [
  // Notes/Tasks.md
  { text: "Add mobile support", status: "Incomplete" },
  { text: "move", status: "Incomplete", contains: ["Alice", "Box", "to her place!"] },
  { text: "Explore offline mode", status: "Incomplete" },
  { text: "Implement backlinks feature", status: "Incomplete", contains: ["feature"] },
  { text: "Review the Imako implementation plan", status: "Incomplete" },
  { text: "Write E2E tests for core features", status: "Incomplete" },
  { text: "Research Playwright best practices", status: "InProgress", contains: ["research"] },
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
});
