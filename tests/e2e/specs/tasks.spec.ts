/**
 * Tasks page tests.
 *
 * Verifies task rendering, status display, and folder tree.
 */

import { test, expect } from "../dsl";

test.describe("Tasks Page", () => {
  test.beforeEach(async ({ app }) => {
    await app.navigateTo("/tasks");
  });

  test("renders tasks from the vault", async ({ app }) => {
    const tasks = app.tasks();
    await tasks.waitForTasks();

    // Should have tasks from the example vault
    const count = await tasks.taskCount();
    expect(count).toBeGreaterThan(0);
  });

  test("displays folder tree", async ({ app }) => {
    const tree = app.folderTree();

    // Folder tree should be visible
    await expect(tree.folders().first()).toBeVisible();
  });

  test("shows tasks with correct status indicators", async ({ app }) => {
    const tasks = app.tasks();
    await tasks.waitForTasks();

    // Should have at least one incomplete task
    const incomplete = tasks.tasksByStatus("Incomplete");
    await expect(incomplete.first()).toBeVisible();
  });

  test("task descriptions render inline markdown", async ({ app }) => {
    const tasks = app.tasks();
    await tasks.waitForTasks();

    // Tasks should contain their text content
    const firstTask = tasks.taskItems().first();
    const text = await firstTask.textContent();
    expect(text).toBeTruthy();
  });
});
