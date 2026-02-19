/**
 * Live sync tests.
 *
 * Verifies that file system changes propagate to the UI
 * via WebSocket push. Uses purely declarative waiting
 * (expect.poll / auto-retrying assertions) — no waitForTimeout.
 */

import { test, expect } from "../dsl";
import * as fs from "fs";
import * as path from "path";

// Path to the example vault (tests run from tests/ directory)
const VAULT_PATH = path.join(process.cwd(), "..", "example");

/** Poll timeout for file-system → WebSocket → UI propagation */
const SYNC_TIMEOUT = 10000;

test.describe("Live Sync", () => {
  // ─── Content modification ────────────────────────────────────────

  test("new task appears when file is modified", async ({ app }) => {
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

      await expect.poll(
        () => tasks.taskCount(),
        { timeout: SYNC_TIMEOUT }
      ).toBeGreaterThan(initialCount);
    } finally {
      fs.writeFileSync(tasksFile, original);
    }
  });

  test("note content updates when file changes", async ({ app }) => {
    await app.navigateTo("/p/Notes%2FWelcome.md");
    const note = app.note();
    await note.waitForContent();

    const welcomeFile = path.join(VAULT_PATH, "Notes", "Welcome.md");
    const original = fs.readFileSync(welcomeFile, "utf-8");

    try {
      const marker = `E2E-TEST-MARKER-${Date.now()}`;
      fs.writeFileSync(welcomeFile, original + `\n\n${marker}\n`);

      await expect(app.page.locator(`text=${marker}`)).toBeVisible({
        timeout: SYNC_TIMEOUT,
      });
    } finally {
      fs.writeFileSync(welcomeFile, original);
    }
  });

  // ─── Structural: file creation ────────────────────────────────────

  test("new file appears in folder tree", async ({ app }) => {
    await app.navigateTo("/");
    const vault = app.vault();
    await vault.waitForVault();
    const tree = app.folderTree();

    const newFile = path.join(VAULT_PATH, "Notes", "NewNote.md");

    try {
      fs.writeFileSync(newFile, "# New Note\n\nHello from E2E!\n");

      await expect(tree.files().filter({ hasText: "NewNote.md" })).toBeVisible({
        timeout: SYNC_TIMEOUT,
      });
    } finally {
      if (fs.existsSync(newFile)) fs.unlinkSync(newFile);
    }
  });

  test("new file with tasks updates task count at root", async ({ app }) => {
    await app.navigateTo("/");
    const vault = app.vault();
    await vault.waitForVault();
    const tasks = app.tasks();
    await tasks.waitForTasks();

    const initialCount = await tasks.taskCount();
    const newFile = path.join(VAULT_PATH, "Notes", "ExtraWork.md");

    try {
      fs.writeFileSync(newFile, "# Extra\n\n- [ ] Brand new task in new file\n");

      await expect.poll(
        () => tasks.taskCount(),
        { timeout: SYNC_TIMEOUT }
      ).toBeGreaterThan(initialCount);
    } finally {
      if (fs.existsSync(newFile)) fs.unlinkSync(newFile);
    }
  });

  // ─── Structural: file deletion ────────────────────────────────────

  test("deleted file disappears from folder tree", async ({ app }) => {
    const tempFile = path.join(VAULT_PATH, "Notes", "ToDelete.md");
    fs.writeFileSync(tempFile, "# To Delete\n\nTemporary file.\n");

    try {
      await app.navigateTo("/");
      const vault = app.vault();
      await vault.waitForVault();
      const tree = app.folderTree();

      // Verify file appears
      await expect(tree.files().filter({ hasText: "ToDelete.md" })).toBeVisible({
        timeout: SYNC_TIMEOUT,
      });

      // Delete it
      fs.unlinkSync(tempFile);

      // Verify it disappears
      await expect(tree.files().filter({ hasText: "ToDelete.md" })).toHaveCount(0, {
        timeout: SYNC_TIMEOUT,
      });
    } finally {
      if (fs.existsSync(tempFile)) fs.unlinkSync(tempFile);
    }
  });

  // ─── Structural: file rename ──────────────────────────────────────

  test("renamed file shows new name in folder tree", async ({ app }) => {
    const oldFile = path.join(VAULT_PATH, "Notes", "BeforeRename.md");
    const newFile = path.join(VAULT_PATH, "Notes", "AfterRename.md");
    fs.writeFileSync(oldFile, "# Before\n\nRename test.\n");

    try {
      await app.navigateTo("/");
      const vault = app.vault();
      await vault.waitForVault();
      const tree = app.folderTree();

      await expect(tree.files().filter({ hasText: "BeforeRename.md" })).toBeVisible({
        timeout: SYNC_TIMEOUT,
      });

      fs.renameSync(oldFile, newFile);

      await expect(tree.files().filter({ hasText: "BeforeRename.md" })).toHaveCount(0, {
        timeout: SYNC_TIMEOUT,
      });
      await expect(tree.files().filter({ hasText: "AfterRename.md" })).toBeVisible({
        timeout: SYNC_TIMEOUT,
      });
    } finally {
      if (fs.existsSync(oldFile)) fs.unlinkSync(oldFile);
      if (fs.existsSync(newFile)) fs.unlinkSync(newFile);
    }
  });

  // ─── Scoped view updates ──────────────────────────────────────────

  test("task added while viewing a folder updates scoped tasks", async ({ app }) => {
    await app.navigateTo("/p/Notes");
    const vault = app.vault();
    await vault.waitForVault();
    const tasks = app.tasks();
    await tasks.waitForTasks();

    const initialCount = await tasks.taskCount();

    const tasksFile = path.join(VAULT_PATH, "Notes", "Tasks.md");
    const original = fs.readFileSync(tasksFile, "utf-8");

    try {
      fs.writeFileSync(tasksFile, original + "\n- [ ] Scoped folder task test\n");

      await expect.poll(
        () => tasks.taskCount(),
        { timeout: SYNC_TIMEOUT }
      ).toBeGreaterThan(initialCount);
    } finally {
      fs.writeFileSync(tasksFile, original);
    }
  });

  test("task added while viewing a file updates file tasks", async ({ app }) => {
    await app.navigateTo("/p/Notes%2FTasks.md");
    const vault = app.vault();
    await vault.waitForVault();
    const tasks = app.tasks();
    await tasks.waitForTasks();

    const initialCount = await tasks.taskCount();

    const tasksFile = path.join(VAULT_PATH, "Notes", "Tasks.md");
    const original = fs.readFileSync(tasksFile, "utf-8");

    try {
      fs.writeFileSync(tasksFile, original + "\n- [ ] File-scoped task test\n");

      await expect.poll(
        () => tasks.taskCount(),
        { timeout: SYNC_TIMEOUT }
      ).toBeGreaterThan(initialCount);
    } finally {
      fs.writeFileSync(tasksFile, original);
    }
  });

  // ─── Task indicator dot ───────────────────────────────────────────

  test("file gains task dot when first task is added", async ({ app }) => {
    await app.navigateTo("/");
    const vault = app.vault();
    await vault.waitForVault();

    // Welcome.md has no tasks initially — no dot
    expect(await vault.fileHasTaskDot("Welcome.md")).toBe(false);

    const welcomeFile = path.join(VAULT_PATH, "Notes", "Welcome.md");
    const original = fs.readFileSync(welcomeFile, "utf-8");

    try {
      fs.writeFileSync(welcomeFile, original + "\n- [ ] New task to trigger dot\n");

      await expect.poll(
        () => vault.fileHasTaskDot("Welcome.md"),
        { timeout: SYNC_TIMEOUT }
      ).toBe(true);
    } finally {
      fs.writeFileSync(welcomeFile, original);
    }
  });

  // ─── Cross-file updates ───────────────────────────────────────────

  test("sidebar updates when a different file is modified", async ({ app }) => {
    // View Tasks.md, then modify Welcome.md — sidebar dot should appear
    await app.navigateTo("/p/Notes%2FTasks.md");
    const vault = app.vault();
    await vault.waitForVault();

    expect(await vault.fileHasTaskDot("Welcome.md")).toBe(false);

    const welcomeFile = path.join(VAULT_PATH, "Notes", "Welcome.md");
    const original = fs.readFileSync(welcomeFile, "utf-8");

    try {
      fs.writeFileSync(welcomeFile, original + "\n- [ ] Cross-file sidebar test\n");

      await expect.poll(
        () => vault.fileHasTaskDot("Welcome.md"),
        { timeout: SYNC_TIMEOUT }
      ).toBe(true);
    } finally {
      fs.writeFileSync(welcomeFile, original);
    }
  });

  // ─── Rapid successive changes ─────────────────────────────────────

  test("rapid successive changes resolve to final state", async ({ app }) => {
    await app.navigateTo("/p/Notes%2FWelcome.md");
    const note = app.note();
    await note.waitForContent();

    const welcomeFile = path.join(VAULT_PATH, "Notes", "Welcome.md");
    const original = fs.readFileSync(welcomeFile, "utf-8");

    try {
      // Write 3 times rapidly
      fs.writeFileSync(welcomeFile, original + "\n\nRAPID-CHANGE-1\n");
      fs.writeFileSync(welcomeFile, original + "\n\nRAPID-CHANGE-2\n");
      fs.writeFileSync(welcomeFile, original + "\n\nRAPID-CHANGE-3-FINAL\n");

      // Only the final marker should be visible
      await expect(app.page.locator("text=RAPID-CHANGE-3-FINAL")).toBeVisible({
        timeout: SYNC_TIMEOUT,
      });
      await expect(app.page.locator("text=RAPID-CHANGE-1")).toHaveCount(0);
      await expect(app.page.locator("text=RAPID-CHANGE-2")).toHaveCount(0);
    } finally {
      fs.writeFileSync(welcomeFile, original);
    }
  });
});
