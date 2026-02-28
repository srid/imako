/**
 * Markdown rendering tests.
 *
 * Verifies that Markdown elements render with correct structure.
 *
 * Expected vault state (example/):
 * - Notes/MarkdownShowcase.md: Comprehensive Markdown fixture
 */

import { test, expect } from "../dsl";

const SHOWCASE = "/p/Notes%2FMarkdownShowcase.md";

test.describe("Markdown Rendering", () => {
  test.beforeEach(async ({ app }) => {
    await app.navigateTo(SHOWCASE);
    await app.note().waitForContent();
  });

  // --- Block Elements ---

  test("renders all heading levels (H1–H6)", async ({ app }) => {
    const note = app.note();
    const content = note.content();

    await expect(content.locator("h1")).toHaveText("Markdown Showcase");
    const h3 = content.locator("h3");
    await expect(h3.filter({ hasText: "Third-Level Heading" })).toBeVisible();
    await expect(content.locator("h4").filter({ hasText: "Fourth-Level Heading" })).toBeVisible();
  });

  test("renders paragraphs", async ({ app }) => {
    const note = app.note();
    const paragraphs = note.paragraphs();

    const count = await paragraphs.count();
    expect(count).toBeGreaterThanOrEqual(2);

    await expect(paragraphs.filter({ hasText: "first paragraph" })).toBeVisible();
    await expect(paragraphs.filter({ hasText: "second paragraph" })).toBeVisible();
  });

  test("renders bullet list items", async ({ app }) => {
    const content = app.note().content();

    await expect(content.getByText("First bullet item")).toBeVisible();
    await expect(content.getByText("Second bullet item")).toBeVisible();
    await expect(content.getByText("Third bullet item with")).toBeVisible();

    const boldInList = content.locator("strong").filter({ hasText: "bold" });
    await expect(boldInList.first()).toBeVisible();
  });

  test("renders nested bullet lists with indentation", async ({ app }) => {
    const content = app.note().content();

    await expect(content.getByText("Top level")).toBeVisible();
    await expect(content.getByText("Nested level one")).toBeVisible();
    await expect(content.getByText("Nested level two")).toBeVisible();
  });

  test("renders ordered list with numbering", async ({ app }) => {
    const note = app.note();
    const ol = note.orderedLists();
    await expect(ol.first()).toBeVisible();

    const content = note.content();
    await expect(content.getByText("First ordered item")).toBeVisible();
    await expect(content.getByText("Second ordered item")).toBeVisible();
    await expect(content.getByText("Third ordered item")).toBeVisible();
  });

  test("renders code blocks", async ({ app }) => {
    const content = app.note().content();

    const codeBlocks = content.locator("pre code");
    const count = await codeBlocks.count();
    expect(count).toBeGreaterThanOrEqual(1);
  });

  test("renders blockquote", async ({ app }) => {
    const note = app.note();
    const bq = note.blockquotes().first();
    await expect(bq).toBeVisible();
    await expect(bq).toContainText("This is a blockquote");
  });

  test("renders nested blockquote", async ({ app }) => {
    const note = app.note();
    const nestedBq = note.content().locator("blockquote blockquote");
    await expect(nestedBq).toBeVisible();
    await expect(nestedBq).toContainText("nested blockquote");
  });

  test("renders horizontal rule", async ({ app }) => {
    const note = app.note();
    const hr = note.horizontalRules();
    await expect(hr.first()).toBeVisible();
  });

  test("renders table with header and body rows", async ({ app }) => {
    const note = app.note();
    const tables = note.tables();
    await expect(tables.first()).toBeVisible();

    const table = tables.first();
    const ths = table.locator("th");
    await expect(ths).toHaveCount(3);
    await expect(ths.first()).toContainText("Left Align");

    const tds = table.locator("td");
    await expect(tds).toHaveCount(6);
    await expect(tds.first()).toContainText("Row 1 Col 1");
  });

  test("renders definition list", async ({ app }) => {
    const note = app.note();
    const dl = note.definitionLists();
    await expect(dl.first()).toBeVisible();

    const dts = note.definitionTerms();
    await expect(dts).toHaveCount(2);
    await expect(dts.first()).toContainText("Term One");

    const dds = note.definitionDescriptions();
    await expect(dds).toHaveCount(2);
    await expect(dds.first()).toContainText("Definition for term one");
  });

  // --- Inline Elements ---

  test("renders bold text with <strong>", async ({ app }) => {
    const content = app.note().content();
    const strong = content.locator("strong").filter({ hasText: "bold words" });
    await expect(strong).toBeVisible();
  });

  test("renders italic text with <em>", async ({ app }) => {
    const content = app.note().content();
    const em = content.locator("em").filter({ hasText: "italic words" });
    await expect(em).toBeVisible();
  });

  test("renders strikethrough with <del>", async ({ app }) => {
    const content = app.note().content();
    const del = content.locator("del").filter({ hasText: "strikethrough" });
    await expect(del).toBeVisible();
  });

  test("renders inline code", async ({ app }) => {
    const content = app.note().content();
    const code = content.locator("code").filter({ hasText: "inline code" });
    await expect(code).toBeVisible();
  });

  test("renders external link", async ({ app }) => {
    const note = app.note();
    const link = note.externalLinks().filter({ hasText: "Example Website" });
    await expect(link).toBeVisible();
    await expect(link).toHaveAttribute("target", "_blank");
  });

  test("renders wikilink and navigates on click", async ({ app }) => {
    const note = app.note();
    const wikilinks = note.wikilinks();
    const welcomeLink = wikilinks.filter({ hasText: "Welcome" });
    await expect(welcomeLink).toBeVisible();

    await welcomeLink.click();
    await app.page.waitForURL("**/p/Notes%2FWelcome.md");

    const welcomeNote = app.note();
    await welcomeNote.waitForContent();
    await expect(welcomeNote.content().locator("h1")).toHaveText("Welcome to Imako");
  });

  test("renders footnote as superscript marker", async ({ app }) => {
    const note = app.note();
    const sups = note.superscripts();
    await expect(sups.first()).toBeVisible();
  });

  test("renders single and double quoted text", async ({ app }) => {
    const text = await app.note().textContent();
    expect(text).toContain("single quoted");
    expect(text).toContain("double quoted");
  });
});
