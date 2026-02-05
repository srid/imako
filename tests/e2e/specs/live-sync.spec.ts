/**
 * Live sync tests.
 *
 * Verifies that file system changes propagate to the UI
 * via WebSocket push.
 */

import { test, expect, waitForSync } from "../dsl";
import * as fs from "fs";
import * as path from "path";

// Path to the example vault (relative to project root)
const VAULT_PATH = path.join(process.cwd(), "example");

test.describe("Live Sync", () => {
  test("new task appears when file is modified", async ({ app }) => {
    await app.navigateTo("/tasks");
    const tasks = app.tasks();
    await tasks.waitForTasks();

    const initialCount = await tasks.taskCount();

    // Add a new task to the Tasks.md file
    const tasksFile = path.join(VAULT_PATH, "Notes", "Tasks.md");
    const original = fs.readFileSync(tasksFile, "utf-8");

    try {
      const modified = original + "\n- [ ] New task from E2E test\n";
      fs.writeFileSync(tasksFile, modified);

      // Wait for sync
      await waitForSync(app.page);

      // Task count should increase
      const newCount = await tasks.taskCount();
      expect(newCount).toBeGreaterThan(initialCount);
    } finally {
      // Restore original file
      fs.writeFileSync(tasksFile, original);
    }
  });

  test("note content updates when file changes", async ({ app }) => {
    await app.navigateTo("/n/Notes%2FWelcome.md");
    const note = app.note();
    await note.waitForContent();

    const welcomeFile = path.join(VAULT_PATH, "Notes", "Welcome.md");
    const original = fs.readFileSync(welcomeFile, "utf-8");

    try {
      // Add a unique marker
      const marker = `E2E-TEST-MARKER-${Date.now()}`;
      const modified = original + `\n\n${marker}\n`;
      fs.writeFileSync(welcomeFile, modified);

      // Wait for sync and verify content updated
      await waitForSync(app.page);
      await expect(app.page.locator(`text=${marker}`)).toBeVisible({
        timeout: 5000,
      });
    } finally {
      // Restore original file
      fs.writeFileSync(welcomeFile, original);
    }
  });
});
