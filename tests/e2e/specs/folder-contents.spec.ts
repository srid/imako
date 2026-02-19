/**
 * Folder contents display tests.
 *
 * Verifies that selecting a folder in the sidebar shows its children
 * (subfolders + files) as clickable items in the main panel.
 *
 * Uses deep-link navigation to avoid fragile sidebar selection with large vaults.
 */

import { test, expect } from "../dsl";

test.describe("Folder Contents", () => {
  test("root view shows folder contents listing", async ({ app }) => {
    const vault = app.vault();
    await vault.waitForVault();

    // Root is selected by default — should show contents section
    await expect(vault.folderContents()).toBeVisible();

    // Should have at least one folder listed (any vault has at least Notes or Projects)
    const folderCount = await vault.folderContentsFolders().count();
    expect(folderCount).toBeGreaterThan(0);
  });

  test("folder view shows its files in contents listing", async ({ app }) => {
    // Navigate directly to the Notes folder via deep link
    await app.navigateTo("/p/Notes");
    const vault = app.vault();
    await vault.waitForVault();

    // Notes folder should show its files in the contents listing
    await expect(vault.folderContents()).toBeVisible();
    const fileCount = await vault.folderContentsFiles().count();
    expect(fileCount).toBeGreaterThan(0);

    // Should list known files
    await expect(vault.folderContentsFiles().filter({ hasText: "Tasks.md" })).toBeVisible();
  });

  test("clicking a file in folder contents navigates to it and selects it in sidebar", async ({ app }) => {
    // Navigate to Notes folder
    await app.navigateTo("/p/Notes");
    const vault = app.vault();
    await vault.waitForVault();
    await expect(vault.folderContents()).toBeVisible();

    // Click on Tasks.md in the contents listing
    await vault.folderContentsFiles().filter({ hasText: "Tasks.md" }).click();

    // Should navigate to the file's detail view (note content loads)
    const note = app.note();
    await note.waitForContent();

    // Sidebar should highlight the file as selected (has active styling)
    const sidebarFile = app.folderTree().files().filter({ hasText: "Tasks.md" });
    await expect(sidebarFile).toHaveClass(/text-accent-600/);
  });

  test("clicking a subfolder in contents navigates deeper and syncs sidebar", async ({ app }) => {
    // Navigate to root via deep link
    await app.navigateTo("/p/Notes");
    const vault = app.vault();
    await vault.waitForVault();
    await expect(vault.folderContents()).toBeVisible();

    // Click on a known file in the main panel's contents listing
    await vault.folderContentsFiles().filter({ hasText: "Welcome.md" }).click();

    // URL should update
    await app.page.waitForURL("**/p/**Welcome**");

    // Sidebar should highlight the file — the selected item gets accent styling
    const sidebarFile = app.folderTree().files().filter({ hasText: "Welcome.md" });
    await expect(sidebarFile).toHaveClass(/text-accent-600/);
  });
});
