/**
 * Notes page tests.
 *
 * Verifies markdown rendering, wikilinks, and navigation.
 *
 * Expected vault state (example/):
 * - Notes/Welcome.md: Main note with headings, code blocks, and wikilinks
 * - Notes/Wikilinks.md: Note with 6 wikilinks, including 2 broken ones
 */

import { test, expect } from "../dsl";

const WELCOME_HEADINGS = [
  "Welcome to Imako",
  "About Imako",
  "Features",
];

test.describe("Notes Page", () => {
  test("renders note content with correct title", async ({ app }) => {
    await app.navigateTo("/n/Notes%2FWelcome.md");
    const note = app.note();
    await note.waitForContent();

    // Verify the main heading
    const h1 = note.content().locator("h1");
    await expect(h1).toHaveText("Welcome to Imako");
  });

  test("renders all expected headings", async ({ app }) => {
    await app.navigateTo("/n/Notes%2FWelcome.md");
    const note = app.note();
    await note.waitForContent();

    // Verify heading count and content
    const headings = note.headings();
    await expect(headings).toHaveCount(WELCOME_HEADINGS.length);

    for (const heading of WELCOME_HEADINGS) {
      await expect(headings.filter({ hasText: heading })).toBeVisible();
    }
  });

  test("renders code block with correct content", async ({ app }) => {
    await app.navigateTo("/n/Notes%2FWelcome.md");
    const note = app.note();
    await note.waitForContent();

    // Verify code block exists and contains expected content
    const codeBlocks = note.codeBlocks();
    await expect(codeBlocks.first()).toBeVisible();
    await expect(codeBlocks.first()).toContainText("Imako");
  });

  test("renders correct number of wikilinks", async ({ app }) => {
    await app.navigateTo("/n/Notes%2FWikilinks.md");
    const note = app.note();
    await note.waitForContent();

    // Wikilinks.md has 6 wikilinks
    const wikilinks = note.wikilinks();
    await expect(wikilinks).toHaveCount(6);
  });

  test("renders correct broken wikilinks", async ({ app }) => {
    await app.navigateTo("/n/Notes%2FWikilinks.md");
    const note = app.note();
    await note.waitForContent();

    // 2 broken wikilinks: "Nonexistent" and "Missing Page"
    const broken = note.brokenWikilinks();
    await expect(broken).toHaveCount(2);
    await expect(broken.filter({ hasText: "Nonexistent" })).toBeVisible();
    await expect(broken.filter({ hasText: "Missing Page" })).toBeVisible();
  });

  test("wikilink navigation navigates to correct page", async ({ app }) => {
    await app.navigateTo("/n/Notes%2FWikilinks.md");
    const note = app.note();
    await note.waitForContent();

    // Click on the Welcome wikilink
    const welcomeLink = note.wikilinks().filter({ hasText: "Welcome" });
    await welcomeLink.click();

    // Should navigate to the Welcome note
    await app.page.waitForURL("**/n/Notes%2FWelcome.md");
  });
});
