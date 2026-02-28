/**
 * Live sync tests.
 *
 * NOTE: WebSocket-based live sync is not yet implemented in the Dioxus SSR app.
 * The old implementation pushed file system changes to the browser via WebSocket.
 * The current SSR app serves data from server functions at request time.
 *
 * These tests verify that the server functions return fresh data on page reload,
 * which is the SSR equivalent of "live sync" — changes on disk are reflected
 * when the page is re-requested.
 */

import { test, expect } from "../dsl";
import * as fs from "fs";
import * as path from "path";

const VAULT_PATH = path.join(process.cwd(), "..", "example");
const SYNC_TIMEOUT = 10000;

test.describe("Data Freshness (SSR)", () => {
  test.describe.configure({ mode: 'serial' });

  test("new task appears after page reload", async ({ app }) => {
    await app.navigateTo("/");
    const vault = app.vault();
    await vault.waitForVault();
    const tasks = app.tasks();
    await tasks.waitForTasks();

    const initialCount = await tasks.taskCount();

    const tasksFile = path.join(VAULT_PATH, "Notes", "Tasks.md");
    const original = fs.readFileSync(tasksFile, "utf-8");

    try {
      fs.writeFileSync(tasksFile, original + "\n- [ ] New task from E2E test\n");

      // Reload page to get fresh SSR data
      await app.page.reload();
      await app.waitForConnection();
      await vault.waitForVault();
      await tasks.waitForTasks();

      const newCount = await tasks.taskCount();
      expect(newCount).toBeGreaterThan(initialCount);
    } finally {
      fs.writeFileSync(tasksFile, original);
    }
  });

  test("note content updates after page reload", async ({ app }) => {
    await app.navigateTo("/p/Notes%2FWelcome.md");
    const note = app.note();
    await note.waitForContent();

    const welcomeFile = path.join(VAULT_PATH, "Notes", "Welcome.md");
    const original = fs.readFileSync(welcomeFile, "utf-8");

    try {
      const marker = `E2E-TEST-MARKER-${Date.now()}`;
      fs.writeFileSync(welcomeFile, original + `\n\n${marker}\n`);

      await app.page.reload();
      await app.waitForConnection();
      await note.waitForContent();

      await expect(app.page.locator(`text=${marker}`)).toBeVisible({
        timeout: SYNC_TIMEOUT,
      });
    } finally {
      fs.writeFileSync(welcomeFile, original);
    }
  });

  test("new file appears in folder tree after reload", async ({ app }) => {
    await app.navigateTo("/");
    const vault = app.vault();
    await vault.waitForVault();
    const tree = app.folderTree();

    const newFile = path.join(VAULT_PATH, "Notes", "NewNote.md");

    try {
      fs.writeFileSync(newFile, "# New Note\n\nHello from E2E!\n");

      await app.page.reload();
      await app.waitForConnection();
      await vault.waitForVault();

      await expect(tree.files().filter({ hasText: "NewNote.md" })).toBeVisible({
        timeout: SYNC_TIMEOUT,
      });
    } finally {
      if (fs.existsSync(newFile)) fs.unlinkSync(newFile);
    }
  });

  test("deleted file disappears from folder tree after reload", async ({ app }) => {
    const tempFile = path.join(VAULT_PATH, "Notes", "ToDelete.md");
    fs.writeFileSync(tempFile, "# To Delete\n\nTemporary file.\n");

    try {
      await app.navigateTo("/");
      const vault = app.vault();
      await vault.waitForVault();
      const tree = app.folderTree();

      await expect(tree.files().filter({ hasText: "ToDelete.md" })).toBeVisible({
        timeout: SYNC_TIMEOUT,
      });

      fs.unlinkSync(tempFile);

      await app.page.reload();
      await app.waitForConnection();
      await vault.waitForVault();

      await expect(tree.files().filter({ hasText: "ToDelete.md" })).toHaveCount(0, {
        timeout: SYNC_TIMEOUT,
      });
    } finally {
      if (fs.existsSync(tempFile)) fs.unlinkSync(tempFile);
    }
  });
});
