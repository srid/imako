/**
 * Backlinks tests.
 *
 * Verifies that backlinks (notes linking TO the current note) are
 * displayed correctly, based on the wikilink graph.
 *
 * Expected wikilink graph (example/):
 * - Notes/Welcome.md → Tasks, Projects/Active
 * - Notes/Wikilinks.md → Welcome, Tasks, Projects/Active
 * - Projects/Active.md → Notes/Welcome
 * - Notes/Tasks.md → (Alice, Box — broken, no backlink edges)
 *
 * Therefore:
 * - Welcome: backlinks from Wikilinks, Active
 * - Tasks: backlinks from Welcome, Wikilinks
 * - Active: backlinks from Welcome, Wikilinks
 * - Wikilinks: no backlinks
 */

import { test, expect } from "../dsl";

test.describe("Backlinks", () => {
  test("Welcome note shows backlinks from Wikilinks and Active", async ({ app }) => {
    await app.navigateTo("/p/Notes%2FWelcome.md");
    const note = app.note();
    await note.waitForContent();

    const section = note.backlinksSection();
    await expect(section).toBeVisible();

    const items = note.backlinkItems();
    await expect(items).toHaveCount(2);
    await expect(items.filter({ hasText: "Notes/Wikilinks.md" })).toBeVisible();
    await expect(items.filter({ hasText: "Projects/Active.md" })).toBeVisible();
  });

  test("Tasks note shows backlinks from Welcome and Wikilinks", async ({ app }) => {
    await app.navigateTo("/p/Notes%2FTasks.md");
    const note = app.note();
    await note.waitForContent();

    const section = note.backlinksSection();
    await expect(section).toBeVisible();

    const items = note.backlinkItems();
    await expect(items).toHaveCount(2);
    await expect(items.filter({ hasText: "Notes/Welcome.md" })).toBeVisible();
    await expect(items.filter({ hasText: "Notes/Wikilinks.md" })).toBeVisible();
  });

  test("Active note shows backlinks from Welcome and Wikilinks", async ({ app }) => {
    await app.navigateTo("/p/Projects%2FActive.md");
    const note = app.note();
    await note.waitForContent();

    const section = note.backlinksSection();
    await expect(section).toBeVisible();

    const items = note.backlinkItems();
    await expect(items).toHaveCount(2);
    await expect(items.filter({ hasText: "Notes/Welcome.md" })).toBeVisible();
    await expect(items.filter({ hasText: "Notes/Wikilinks.md" })).toBeVisible();
  });

  test("Wikilinks note has no backlinks section", async ({ app }) => {
    await app.navigateTo("/p/Notes%2FWikilinks.md");
    const note = app.note();
    await note.waitForContent();

    const section = note.backlinksSection();
    await expect(section).not.toBeVisible();
  });

  test("clicking a backlink navigates to that note", async ({ app }) => {
    await app.navigateTo("/p/Notes%2FWelcome.md");
    const note = app.note();
    await note.waitForContent();

    // Click the Wikilinks backlink
    const wikilinksBacklink = note.backlinkItems().filter({ hasText: "Notes/Wikilinks.md" });
    await wikilinksBacklink.click();

    // Should navigate to the Wikilinks note
    await app.page.waitForURL("**/p/Notes%2FWikilinks.md");
  });
});
