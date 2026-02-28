/**
 * Vault tasks tests.
 *
 * Verifies task rendering, status display, folder tree, and hierarchical nesting.
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
// Note: subtasks (indented items) are not extracted as separate task-items;
// they are part of the parent task's description.
const EXPECTED_TASKS: TaskExpectation[] = [
  // Notes/Tasks.md — top-level only (subtasks not extracted)
  { text: "Add mobile support", status: "Incomplete" },
  { text: "Alice: move Box to her place!", status: "Incomplete" },
  { text: "Explore offline mode", status: "Incomplete" },
  { text: "Implement backlinks feature", status: "Incomplete" },
  { text: "Review the Imako implementation plan", status: "Incomplete" },
  { text: "Write E2E tests for core features", status: "Incomplete" },
  { text: "Research Playwright best practices", status: "InProgress" },
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
    await app.navigateTo("/");
    const vault = app.vault();
    await vault.waitForVault();
  });

  test("renders visible tasks with correct content at root", async ({ app }) => {
    const tasks = app.tasks();
    await tasks.waitForTasks();
    await tasks.verifyTasks(EXPECTED_TASKS);
  });

  test("displays folder tree with correct folders in sidebar", async ({ app }) => {
    const tree = app.folderTree();

    // Verify folder count (3 top-level + 1 nested Team subfolder = 4)
    const folders = tree.folders();
    await expect(folders).toHaveCount(4);

    for (const folderName of EXPECTED_FOLDERS) {
      await expect(folders.filter({ hasText: folderName }).first()).toBeVisible();
    }
  });

  test("task items contain expected text content", async ({ app }) => {
    const tasks = app.tasks();
    await tasks.waitForTasks();

    // Key tasks should be visible as task items
    await expect(tasks.taskItems().filter({ hasText: "Add mobile support" })).toBeVisible();
    await expect(tasks.taskItems().filter({ hasText: "Schedule team sync meeting" })).toBeVisible();
    await expect(tasks.taskItems().filter({ hasText: "Set up CI pipeline" })).toBeVisible();
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
    await app.navigateTo("/");
    const vault2 = app.vault();
    await vault2.waitForVault();
    const tasks2 = app.tasks();
    await tasks2.waitForTasks();
    await expect(tasks2.taskItems().filter({ hasText: "Add live sync tests" })).toBeVisible();
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

  test("subfolder tasks are grouped under a collapsible folder header", async ({ app }) => {
    const tasks = app.tasks();
    await tasks.waitForTasks();

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
    await expect(activeFileGroup.locator("[data-testid='task-item']").filter({ hasText: "Complete E2E test infrastructure" })).toBeVisible();
  });
});
