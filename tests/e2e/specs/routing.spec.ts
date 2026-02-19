/**
 * URL routing tests.
 *
 * Verifies SPA routing conventions: deep links, refresh persistence,
 * browser back/forward, and wikilink navigation staying in vault context.
 */

import { test, expect } from "../dsl";

test.describe("URL Routing", () => {
  test("deep link to a file selects it in the tree and shows detail", async ({ app }) => {
    // Navigate directly to a file URL
    await app.navigateTo("/p/Notes%2FTasks.md");
    const vault = app.vault();
    await vault.waitForVault();

    // File should be selected in the sidebar (has active styling)
    const tree = app.folderTree();
    const selectedFile = tree.files().filter({ hasText: "Tasks.md" });
    await expect(selectedFile).toBeVisible();

    // Detail pane should show the file's content
    const note = app.note();
    await note.waitForContent();
  });

  test("deep link to a folder scopes tasks", async ({ app }) => {
    // Navigate directly to a folder URL
    await app.navigateTo("/p/Notes");
    const vault = app.vault();
    await vault.waitForVault();

    // Should show only Notes tasks (not Projects)
    const tasks = app.tasks();
    await tasks.waitForTasks();
    await expect(tasks.taskItems().filter({ hasText: "Add mobile support" })).toBeVisible();
    await expect(tasks.taskItems().filter({ hasText: "Add live sync tests" })).toHaveCount(0);
  });

  test("page refresh preserves selected path", async ({ app }) => {
    await app.navigateTo("/");
    const vault = app.vault();
    await vault.waitForVault();

    // Select a file
    await vault.selectFile("Tasks.md");

    // Verify URL changed
    await app.page.waitForURL("**/p/**");

    // Reload the page
    await app.page.reload();
    await app.waitForConnection();
    await vault.waitForVault();

    // Note content should still be visible (same file selected)
    const note = app.note();
    await note.waitForContent();
  });

  test("browser back/forward navigates selection history", async ({ app }) => {
    // Build history: root → folder → file
    await app.navigateTo("/");
    const vault = app.vault();
    await vault.waitForVault();

    await app.navigateTo("/p/Notes");
    await vault.waitForVault();
    const tasks = app.tasks();
    await tasks.waitForTasks();
    // Notes folder: should not show Projects tasks
    await expect(tasks.taskItems().filter({ hasText: "Add live sync tests" })).toHaveCount(0);

    await app.navigateTo("/p/Notes%2FTasks.md");
    await vault.waitForVault();
    const note = app.note();
    await note.waitForContent();

    // Go back to Notes folder
    await app.page.goBack();
    await app.page.waitForURL("**/p/Notes");
    await tasks.waitForTasks();
    await expect(tasks.taskItems().filter({ hasText: "Add mobile support" })).toBeVisible();
    await expect(tasks.taskItems().filter({ hasText: "Add live sync tests" })).toHaveCount(0);

    // Go back to root
    await app.page.goBack();
    await expect(tasks.taskItems().filter({ hasText: "Add live sync tests" })).toBeVisible();

    // Go forward to Notes folder
    await app.page.goForward();
    await app.page.waitForURL("**/p/Notes");
    await expect(tasks.taskItems().filter({ hasText: "Add live sync tests" })).toHaveCount(0);
  });

  test("wikilink click navigates within vault browser", async ({ app }) => {
    // Navigate to a file with wikilinks
    await app.navigateTo("/p/Notes%2FWikilinks.md");
    const vault = app.vault();
    await vault.waitForVault();
    const note = app.note();
    await note.waitForContent();

    // Click on the Welcome wikilink
    const welcomeLink = note.wikilinks().filter({ hasText: "Welcome" });
    await welcomeLink.click();

    // Should navigate to /p/ path (stays in vault browser)
    await app.page.waitForURL("**/p/**Welcome**");

    // Sidebar should still be visible (didn't leave vault context)
    await expect(vault.detailPane()).toBeVisible();
  });
});
