/**
 * Connection and hydration tests.
 *
 * Verify WebSocket connection, initial data loading,
 * and page-level integration.
 */

import { test, expect } from "../dsl";

test.describe("Connection & Hydration", () => {
  test("connects to WebSocket and displays vault name", async ({ app }) => {
    await app.navigateTo("/");
    const name = await app.vaultName();
    expect(name.length).toBeGreaterThan(0);
  });

  test("loads vault at root and shows folder tree", async ({ app }) => {
    await app.navigateTo("/");
    const vault = app.vault();
    await vault.waitForVault();

    // Verify folder tree is visible in sidebar
    const tree = app.folderTree();
    const files = tree.files();
    await expect(files.first()).toBeVisible();
  });
});
