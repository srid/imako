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
    await app.navigateTo("/p/Notes%2FWelcome.md");
    const note = app.note();
    await note.waitForContent();

    const h1 = note.content().locator("h1");
    await expect(h1).toHaveText("Welcome to Imako");
  });

  test("renders all expected headings", async ({ app }) => {
    await app.navigateTo("/p/Notes%2FWelcome.md");
    const note = app.note();
    await note.waitForContent();

    const headings = note.headings();
    await expect(headings).toHaveCount(WELCOME_HEADINGS.length);

    for (const heading of WELCOME_HEADINGS) {
      await expect(headings.filter({ hasText: heading })).toBeVisible();
    }
  });

  test("renders code block with correct content", async ({ app }) => {
    await app.navigateTo("/p/Notes%2FWelcome.md");
    const note = app.note();
    await note.waitForContent();

    const codeBlocks = note.codeBlocks();
    await expect(codeBlocks.first()).toBeVisible();
    await expect(codeBlocks.first()).toContainText("Imako");
  });

  test("renders correct number of wikilinks", async ({ app }) => {
    await app.navigateTo("/p/Notes%2FWikilinks.md");
    const note = app.note();
    await note.waitForContent();

    const wikilinks = note.wikilinks();
    await expect(wikilinks).toHaveCount(6);
  });

  test("renders correct broken wikilinks", async ({ app }) => {
    await app.navigateTo("/p/Notes%2FWikilinks.md");
    const note = app.note();
    await note.waitForContent();

    // At least 1 broken wikilink visible
    const broken = note.brokenWikilinks();
    const brokenCount = await broken.count();
    expect(brokenCount).toBeGreaterThanOrEqual(1);
    await expect(broken.first()).toBeVisible();
  });

  test("wikilink navigation navigates to correct page", async ({ app }) => {
    await app.navigateTo("/p/Notes%2FWikilinks.md");
    const note = app.note();
    await note.waitForContent();

    const welcomeLink = note.wikilinks().filter({ hasText: "Welcome" });
    await welcomeLink.click();

    await app.page.waitForURL("**/p/Notes%2FWelcome.md");
  });
});
