/**
 * Notes page tests.
 *
 * Verifies markdown rendering, wikilinks, and navigation.
 */

import { test, expect } from "../dsl";

test.describe("Notes Page", () => {
  test("renders note content", async ({ app }) => {
    // Navigate to Welcome note
    await app.navigateTo("/n/Notes%2FWelcome.md");

    const note = app.note();
    await note.waitForContent();

    // Should contain the welcome text
    const text = await note.textContent();
    expect(text).toContain("Welcome");
  });

  test("renders headings correctly", async ({ app }) => {
    await app.navigateTo("/n/Notes%2FWelcome.md");

    const note = app.note();
    await note.waitForContent();

    // Should have headings
    const headings = note.headings();
    await expect(headings.first()).toBeVisible();
  });

  test("renders code blocks", async ({ app }) => {
    await app.navigateTo("/n/Notes%2FWelcome.md");

    const note = app.note();
    await note.waitForContent();

    // Should have a code block
    const codeBlocks = note.codeBlocks();
    await expect(codeBlocks.first()).toBeVisible();
  });

  test("renders wikilinks with styling", async ({ app }) => {
    await app.navigateTo("/n/Notes%2FWikilinks.md");

    const note = app.note();
    await note.waitForContent();

    // Should have wikilinks
    const wikilinks = note.wikilinks();
    await expect(wikilinks.first()).toBeVisible();
  });

  test("marks broken wikilinks", async ({ app }) => {
    await app.navigateTo("/n/Notes%2FWikilinks.md");

    const note = app.note();
    await note.waitForContent();

    // Should have broken wikilinks
    const broken = note.brokenWikilinks();
    await expect(broken.first()).toBeVisible();
  });

  test("wikilink navigation works", async ({ app }) => {
    await app.navigateTo("/n/Notes%2FWikilinks.md");

    const note = app.note();
    await note.waitForContent();

    // Click on a valid wikilink (Welcome)
    const welcomeLink = note.wikilinks().filter({ hasText: "Welcome" });
    await welcomeLink.click();

    // Should navigate to the Welcome note
    await app.page.waitForURL("**/n/Notes%2FWelcome.md");
  });
});
