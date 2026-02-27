/**
 * Vault tasks tests.
 *
 * Verifies task rendering, status display, folder tree, and hierarchical nesting
 * in the unified vault browser.
 *
 * Expected vault state (example/):
 * Filters hide Completed/Cancelled tasks by default.
 * Visible tasks (24 total):
 * - Notes/Tasks.md: 13 visible (2 completed/cancelled hidden)
 *   - 6 are nested subtasks rendered inside their parents
 * - Projects/Active.md: 4 visible (1 completed hidden)
 * - Projects/Team/Backend.md: 2 visible (1 completed hidden)
 * - Projects/Team/Frontend.md: 1 visible (1 completed hidden)
 * - Daily/2026-02-17.md: 2 visible (1 completed hidden)
 * - Daily/2026-02-19.md: 2 visible (1 completed hidden)
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
  // Daily/2026-02-17.md
  { text: "Schedule team sync meeting", status: "Incomplete" },
  { text: "Update documentation", status: "Incomplete" },
  // Daily/2026-02-19.md
  { text: "Implement CalendarWidget component", status: "Incomplete" },
  { text: "Build JournalView for main panel", status: "Incomplete" },
  // Projects/Team/Backend.md
  { text: "Set up CI pipeline", status: "Incomplete" },
  { text: "Add health check endpoint", status: "Incomplete" },
  // Projects/Team/Frontend.md
  { text: "Build dashboard page", status: "Incomplete" },
];

const EXPECTED_FOLDERS = ["Daily", "Notes", "Projects"];

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

    // Verify folder count (3 top-level + 1 nested Team subfolder = 4)
    const folders = tree.folders();
    await expect(folders).toHaveCount(4);

    // Verify top-level folders exist by name
    for (const folderName of EXPECTED_FOLDERS) {
      await expect(folders.filter({ hasText: folderName }).first()).toBeVisible();
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

    // Wait for vault model to stabilize (live-sync tests may still be settling)
    await expect.poll(
      () => tasks.taskCount(),
      { timeout: 5000, message: "task count should stabilize" }
    ).toBe(24);
    const initialCount = 24;

    // Toggle tasks off
    await vault.toggleShowTasks();

    // Tasks should be hidden
    await expect(tasks.taskItems()).toHaveCount(0);

    // Toggle tasks back on
    await vault.toggleShowTasks();

    // Tasks should reappear with exact same count
    await tasks.waitForTasks();
    expect(await tasks.taskCount()).toBe(initialCount);
  });

  test("folder chevron toggles expand without navigating, label navigates without toggling", async ({ app }) => {
    const vault = app.vault();
    await vault.waitForVault();
    const tasks = app.tasks();
    await tasks.waitForTasks();
    const tree = app.folderTree();

    // Initially folder is expanded (default open)
    expect(await vault.isFolderOpen("Notes")).toBe(true);

    // Click chevron — should collapse, but NOT navigate (root tasks still visible)
    await vault.toggleFolder("Notes");
    expect(await vault.isFolderOpen("Notes")).toBe(false);
    // Verify we're still at root (all task groups visible)
    await expect(tasks.taskItems().filter({ hasText: "Add live sync tests" })).toBeVisible();

    // Click chevron again — should expand, still no navigation
    await vault.toggleFolder("Notes");
    expect(await vault.isFolderOpen("Notes")).toBe(true);

    // Note the folder's open state before label click
    const openBefore = await vault.isFolderOpen("Notes");

    // Click folder label — should navigate to Notes folder, NOT toggle expand/collapse
    await vault.selectFolder("Notes");

    // The folder's expand state should be unchanged
    expect(await vault.isFolderOpen("Notes")).toBe(openBefore);

    // Should see only Notes-scoped tasks
    await expect(tasks.taskItems().filter({ hasText: "Add mobile support" })).toBeVisible();
    await expect(tasks.taskItems().filter({ hasText: "Add live sync tests" })).toHaveCount(0);
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

  test("slash key focuses filter and Escape clears it", async ({ app }) => {
    const vault = app.vault();
    await vault.waitForVault();
    const filterInput = vault.filterInput();

    // Filter should not be focused initially
    await expect(filterInput).not.toBeFocused();

    // Press "/" to focus filter
    await app.page.keyboard.press("/");
    await expect(filterInput).toBeFocused();

    // Type something
    await filterInput.fill("Tasks");
    await expect(filterInput).toHaveValue("Tasks");

    // Press Escape to clear and blur
    await app.page.keyboard.press("Escape");
    await expect(filterInput).toHaveValue("");
    await expect(filterInput).not.toBeFocused();
  });

  test("subfolder tasks are grouped under a collapsible folder header", async ({ app }) => {
    const tasks = app.tasks();
    await tasks.waitForTasks();

    // "Projects" contains a "Team" subfolder — tasks should be grouped under it
    // Navigate to Projects folder to scope down
    await app.page.locator("aside [data-testid='folder-label']").filter({ hasText: "Projects" }).click();

    // The Team subfolder should render as a folder-tasks-group
    const teamGroup = tasks.folderTaskGroup("Team");
    await expect(teamGroup).toBeVisible();

    // Inside the folder group, there should be file-tasks-group entries
    const filesInTeamGroup = teamGroup.locator("[data-testid='file-tasks-group']");
    await expect(filesInTeamGroup).toHaveCount(2); // Backend.md, Frontend.md

    // Tasks within the folder group should be visible
    await expect(teamGroup.locator("[data-testid='task-item']").filter({ hasText: "Set up CI pipeline" })).toBeVisible();
    await expect(teamGroup.locator("[data-testid='task-item']").filter({ hasText: "Build dashboard page" })).toBeVisible();

    // Direct file tasks (Active.md) should NOT be inside the Team folder group
    const activeFileGroup = tasks.fileTaskGroups().filter({ hasText: "Active.md" });
    await expect(activeFileGroup).toBeVisible();
    // Active.md should be a direct file group, not inside Team
    await expect(activeFileGroup.locator("[data-testid='task-item']").filter({ hasText: "Complete E2E test infrastructure" })).toBeVisible();
  });
});
