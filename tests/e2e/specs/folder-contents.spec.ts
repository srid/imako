/**
 * Folder contents display tests.
 *
 * Verifies that selecting a folder in the sidebar shows its children
 * (subfolders + files) as clickable items in the main panel.
 */

import { test, expect } from "../dsl";

test.describe("Folder Contents", () => {
  test("root view shows folder contents listing for root folders", async ({ app }) => {
    // Navigate to a folder that has subfolders
    await app.navigateTo("/p/Notes");
    const vault = app.vault();
    await vault.waitForVault();

    await expect(vault.folderContents()).toBeVisible();
    const fileCount = await vault.folderContentsFiles().count();
    expect(fileCount).toBeGreaterThan(0);
  });

  test("folder view shows its files in contents listing", async ({ app }) => {
    await app.navigateTo("/p/Notes");
    const vault = app.vault();
    await vault.waitForVault();

    await expect(vault.folderContents()).toBeVisible();
    const fileCount = await vault.folderContentsFiles().count();
    expect(fileCount).toBeGreaterThan(0);

    await expect(vault.folderContentsFiles().filter({ hasText: "Tasks.md" })).toBeVisible();
  });

  test("clicking a file in folder contents navigates to it", async ({ app }) => {
    await app.navigateTo("/p/Notes");
    const vault = app.vault();
    await vault.waitForVault();
    await expect(vault.folderContents()).toBeVisible();

    await vault.folderContentsFiles().filter({ hasText: "Tasks.md" }).click();

    const note = app.note();
    await note.waitForContent();
  });

  test("clicking a subfolder in contents navigates deeper", async ({ app }) => {
    await app.navigateTo("/p/Projects");
    const vault = app.vault();
    await vault.waitForVault();
    await expect(vault.folderContents()).toBeVisible();

    // Click on the Team subfolder
    const teamFolder = vault.folderContentsFolders().filter({ hasText: "Team" });
    await expect(teamFolder).toBeVisible();
    await teamFolder.click();

    await app.page.waitForURL("**/p/**Team**");
  });
});
