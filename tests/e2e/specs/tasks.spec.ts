/**
 * Vault tasks tests.
 *
 * Verifies task rendering, status display, folder tree, and hierarchical nesting
 * in the unified vault browser.
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

test.describe("Vault Tasks", () => {
  test.beforeEach(async ({ app }) => {
    // Root "/" now loads the vault page with all tasks visible
    await app.navigateTo("/");
    const vault = app.vault();
    await vault.waitForVault();
  });

  test("renders visible tasks with correct content at root", async ({ app }) => {
    const tasks = app.tasks();
    await tasks.waitForTasks();

    // Verify all visible tasks are rendered with correct content
    await tasks.verifyTasks(EXPECTED_TASKS);
  });

  test("displays folder tree with correct folders in sidebar", async ({ app }) => {
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

  test("show tasks toggle hides and shows tasks", async ({ app }) => {
    const vault = app.vault();
    const tasks = app.tasks();
    await tasks.waitForTasks();

    // Initially tasks are visible
    const initialCount = await tasks.taskCount();
    expect(initialCount).toBeGreaterThan(0);

    // Toggle tasks off
    await vault.toggleShowTasks();

    // Tasks should be hidden
    await expect(tasks.taskItems()).toHaveCount(0);

    // Toggle tasks back on
    await vault.toggleShowTasks();

    // Tasks should reappear
    await tasks.waitForTasks();
    expect(await tasks.taskCount()).toBe(initialCount);
  });

  test("folder selection scopes tasks to that subtree", async ({ app }) => {
    const vault = app.vault();
    const tasks = app.tasks();
    await tasks.waitForTasks();

    // Select "Notes" folder
    await vault.selectFolder("Notes");

    // Should only see Notes tasks (not Projects tasks)
    await expect(tasks.taskItems().filter({ hasText: "Add mobile support" })).toBeVisible();
    await expect(tasks.taskItems().filter({ hasText: "Add live sync tests" })).toHaveCount(0);

    // Select root — should see all tasks again
    await vault.selectRoot();
    await expect(tasks.taskItems().filter({ hasText: "Add live sync tests" })).toBeVisible();
  });

  test("file selection shows tasks and note content", async ({ app }) => {
    const vault = app.vault();

    // Select a file with tasks
    await vault.selectFile("Tasks.md");

    // Should show tasks for this file
    const tasks = app.tasks();
    await tasks.waitForTasks();
    await expect(tasks.taskItems().filter({ hasText: "Add mobile support" })).toBeVisible();

    // Should show note content
    const note = app.note();
    await note.waitForContent();
  });

  test("inline search filters the folder tree", async ({ app }) => {
    const vault = app.vault();
    const tree = app.folderTree();

    // Initially all files visible
    const initialFiles = await tree.files().count();
    expect(initialFiles).toBeGreaterThan(0);

    // Filter by a file name
    await vault.filterTree("Tasks");

    // Only matching files should be visible
    await expect(tree.files().filter({ hasText: "Tasks.md" })).toBeVisible();

    // Clear filter
    await vault.clearFilter();

    // All files should be visible again
    await expect(tree.files()).toHaveCount(initialFiles);
  });
});
