/**
 * Journals (Daily Notes) E2E tests.
 *
 * Verifies daily notes are displayed in the vault tree and navigable.
 *
 * NOTE: Calendar widget and journal view are not yet implemented in Dioxus.
 * These tests verify the underlying daily notes functionality that IS available:
 * - Daily notes appear in the folder tree
 * - Daily notes are navigable and renderable
 * - Daily notes contain tasks
 *
 * Expected vault state (example/):
 * - .obsidian/daily-notes.json: { "folder": "Daily", "format": "YYYY-MM-DD" }
 * - Daily/2026-02-17.md, 2026-02-18.md, 2026-02-19.md
 */

import { test, expect } from "../dsl";

test.describe("Journals - Daily Notes", () => {
  test("daily notes folder is visible in sidebar", async ({ app }) => {
    await app.navigateTo("/");
    const vault = app.vault();
    await vault.waitForVault();

    const tree = app.folderTree();
    const dailyFolder = tree.folders().filter({ hasText: "Daily" });
    await expect(dailyFolder).toBeVisible();
  });

  test("daily notes files are listed in sidebar", async ({ app }) => {
    await app.navigateTo("/");
    const tree = app.folderTree();
    await expect(tree.files().filter({ hasText: "2026-02-17.md" })).toBeVisible();
    await expect(tree.files().filter({ hasText: "2026-02-18.md" })).toBeVisible();
    await expect(tree.files().filter({ hasText: "2026-02-19.md" })).toBeVisible();
  });

  test("navigating to a daily note shows its content", async ({ app }) => {
    await app.navigateTo("/p/Daily%2F2026-02-17.md");
    const note = app.note();
    await note.waitForContent();
  });

  test("daily notes contain tasks", async ({ app }) => {
    await app.navigateTo("/p/Daily%2F2026-02-17.md");
    const tasks = app.tasks();
    await tasks.waitForTasks();
    await expect(tasks.taskItems().filter({ hasText: "Schedule team sync meeting" })).toBeVisible();
  });

  test("daily folder scopes tasks correctly", async ({ app }) => {
    await app.navigateTo("/p/Daily");
    const vault = app.vault();
    await vault.waitForVault();

    const tasks = app.tasks();
    await tasks.waitForTasks();
    // Daily tasks should be visible
    await expect(tasks.taskItems().filter({ hasText: "Schedule team sync meeting" })).toBeVisible();
    // Non-daily tasks should NOT be visible
    await expect(tasks.taskItems().filter({ hasText: "Add mobile support" })).toHaveCount(0);
  });
});
