/**
 * URL routing tests.
 *
 * Verifies routing conventions: deep links, refresh persistence,
 * browser back/forward, and wikilink navigation.
 */

import { test, expect } from "../dsl";

test.describe("URL Routing", () => {
  test("deep link to a file selects it in the tree and shows detail", async ({ app }) => {
    await app.navigateTo("/p/Notes%2FTasks.md");
    const vault = app.vault();
    await vault.waitForVault();

    const tree = app.folderTree();
    const selectedFile = tree.files().filter({ hasText: "Tasks.md" });
    await expect(selectedFile).toBeVisible();

    const note = app.note();
    await note.waitForContent();
  });

  test("deep link to a folder scopes tasks", async ({ app }) => {
    await app.navigateTo("/p/Notes");
    const vault = app.vault();
    await vault.waitForVault();

    const tasks = app.tasks();
    await tasks.waitForTasks();
    await expect(tasks.taskItems().filter({ hasText: "Add mobile support" })).toBeVisible();
    await expect(tasks.taskItems().filter({ hasText: "Add live sync tests" })).toHaveCount(0);
  });

  test("page refresh preserves selected path", async ({ app }) => {
    await app.navigateTo("/");
    const vault = app.vault();
    await vault.waitForVault();

    await vault.selectFile("Tasks.md");
    await app.page.waitForURL("**/p/**");

    await app.page.reload();
    await app.waitForConnection();
    await vault.waitForVault();

    const note = app.note();
    await note.waitForContent();
  });

  test("browser back/forward navigates selection history", async ({ app }) => {
    await app.navigateTo("/");
    const vault = app.vault();
    await vault.waitForVault();

    await app.navigateTo("/p/Notes");
    await vault.waitForVault();
    const tasks = app.tasks();
    await tasks.waitForTasks();
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
    await app.navigateTo("/p/Notes%2FWikilinks.md");
    const vault = app.vault();
    await vault.waitForVault();
    const note = app.note();
    await note.waitForContent();

    const welcomeLink = note.wikilinks().filter({ hasText: "Welcome" });
    await welcomeLink.click();

    await app.page.waitForURL("**/p/**Welcome**");
    await expect(vault.detailPane()).toBeVisible();
  });
});
