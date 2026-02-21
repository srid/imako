/**
 * Markdown rendering tests.
 *
 * Verifies that every Markdown element renders with correct structure and styling.
 *
 * Expected vault state (example/):
 * - Notes/MarkdownShowcase.md: Comprehensive Markdown fixture with all elements
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

    // H1 (title)
    await expect(content.locator("h1")).toHaveText("Markdown Showcase");

    // H3
    const h3 = content.locator("h3");
    await expect(h3.filter({ hasText: "Third-Level Heading" })).toBeVisible();

    // H4
    await expect(content.locator("h4").filter({ hasText: "Fourth-Level Heading" })).toBeVisible();

    // H5 — BlockRenderer renders H5 and H6 as <h5>
    const h5 = content.locator("h5");
    await expect(h5.filter({ hasText: "Fifth-Level Heading" })).toBeVisible();
    await expect(h5.filter({ hasText: "Sixth-Level Heading" })).toBeVisible();
  });

  test("heading styles: font-weight is bold", async ({ app }) => {
    const h1 = app.note().content().locator("h1");
    // font-weight can be "700" or "bold" depending on browser
    await expect(h1).toHaveCSS("font-weight", /^(700|bold)$/);
  });

  test("renders paragraphs with proper spacing", async ({ app }) => {
    const note = app.note();
    const paragraphs = note.paragraphs();

    // Should have multiple paragraphs
    const count = await paragraphs.count();
    expect(count).toBeGreaterThanOrEqual(2);

    // Check that first and second paragraphs under "Paragraphs" section exist
    await expect(paragraphs.filter({ hasText: "first paragraph" })).toBeVisible();
    await expect(paragraphs.filter({ hasText: "second paragraph" })).toBeVisible();
  });

  test("renders bullet list items", async ({ app }) => {
    const content = app.note().content();

    // Verify bullet items contain expected text
    await expect(content.getByText("First bullet item")).toBeVisible();
    await expect(content.getByText("Second bullet item")).toBeVisible();
    await expect(content.getByText("Third bullet item with")).toBeVisible();

    // Bold inside list item
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

    // Verify items
    const content = note.content();
    await expect(content.getByText("First ordered item")).toBeVisible();
    await expect(content.getByText("Second ordered item")).toBeVisible();
    await expect(content.getByText("Third ordered item")).toBeVisible();
  });

  test("renders code blocks with syntax highlighting", async ({ app }) => {
    const content = app.note().content();

    // 14 code blocks total in expanded MarkdownShowcase
    const codeBlocks = content.locator("pre code");
    const count = await codeBlocks.count();
    expect(count).toBe(14);

    // Wait for Shiki to finish rendering (highlighted spans appear)
    const firstShikiBlock = content.locator(".code-block").first();
    const highlightedSpans = firstShikiBlock.locator("span[style]");
    await expect(highlightedSpans.first()).toBeVisible({ timeout: 5000 });

    // Verify Haskell content present
    await expect(firstShikiBlock).toContainText("fib");

    // Verify Shiki pre element has the expected classes
    const shikiPre = firstShikiBlock.locator("pre.shiki");
    await expect(shikiPre).toBeVisible();
  });

  test("syntax highlighting: markdown block with embedded bash injection", async ({ app }) => {
    const content = app.note().content();

    // The markdown code block is lang="markdown" and contains embedded bash
    // Shiki's TextMate markdown grammar supports bash injection in fenced blocks
    const markdownBlock = content.locator(".code-block").filter({ hasText: "My Haskell Project" });
    await expect(markdownBlock).toBeVisible({ timeout: 5000 });

    // The embedded bash ("cabal run") should have highlighted spans
    const bashSpans = markdownBlock.locator("span[style]").filter({ hasText: "cabal" });
    await expect(bashSpans.first()).toBeVisible();
  });

  test("code blocks have background and padding", async ({ app }) => {
    const pre = app.note().content().locator("pre").first();
    await expect(pre).toBeVisible();

    // Verify background and padding via toHaveCSS (avoids fragile getComputedStyle)
    await expect(pre).not.toHaveCSS("background-color", "rgba(0, 0, 0, 0)");
    await expect(pre).not.toHaveCSS("padding", "0px");
  });

  test("renders blockquote with left border", async ({ app }) => {
    const note = app.note();
    const bq = note.blockquotes().first();
    await expect(bq).toBeVisible();
    await expect(bq).toContainText("This is a blockquote");

    // Verify left border and italic via toHaveCSS (avoids fragile getComputedStyle)
    await expect(bq).not.toHaveCSS("border-left-width", "0px");
    await expect(bq).toHaveCSS("font-style", "italic");
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

    // Header cells
    const ths = table.locator("th");
    await expect(ths).toHaveCount(3);
    await expect(ths.first()).toContainText("Left Align");

    // Body rows
    const tds = table.locator("td");
    await expect(tds).toHaveCount(6); // 2 rows × 3 cols
    await expect(tds.first()).toContainText("Row 1 Col 1");
  });

  test("renders definition list", async ({ app }) => {
    const note = app.note();
    const dl = note.definitionLists();
    await expect(dl.first()).toBeVisible();

    // Terms
    const dts = note.definitionTerms();
    await expect(dts).toHaveCount(2);
    await expect(dts.first()).toContainText("Term One");

    // Definitions
    const dds = note.definitionDescriptions();
    await expect(dds).toHaveCount(2);
    await expect(dds.first()).toContainText("Definition for term one");
  });

  // --- Inline Elements ---

  test("renders bold text with <strong>", async ({ app }) => {
    const content = app.note().content();
    const strong = content.locator("strong").filter({ hasText: "bold words" });
    await expect(strong).toBeVisible();
    await expect(strong).toHaveCSS("font-weight", /^(700|bold)$/);
  });

  test("renders italic text with <em>", async ({ app }) => {
    const content = app.note().content();
    const em = content.locator("em").filter({ hasText: "italic words" });
    await expect(em).toBeVisible();
    await expect(em).toHaveCSS("font-style", "italic");
  });

  test("renders strikethrough with <del>", async ({ app }) => {
    const content = app.note().content();
    const del = content.locator("del").filter({ hasText: "strikethrough" });
    await expect(del).toBeVisible();
  });

  test("renders inline code with background", async ({ app }) => {
    const content = app.note().content();
    const code = content.locator("code").filter({ hasText: "inline code" });
    await expect(code).toBeVisible();
    await expect(code).not.toHaveCSS("background-color", "rgba(0, 0, 0, 0)");
  });

  test("renders combined bold + nested italic", async ({ app }) => {
    const content = app.note().content();
    // Bold wrapping nested italic
    const strongWithItalic = content.locator("strong em");
    await expect(strongWithItalic.first()).toBeVisible();
  });

  test("renders external link with target=_blank", async ({ app }) => {
    const note = app.note();
    const link = note.externalLinks().filter({ hasText: "Example Website" });
    await expect(link).toBeVisible();
    await expect(link).toHaveAttribute("target", "_blank");
    await expect(link).toHaveAttribute("rel", "noopener");
  });

  test("renders wikilink and navigates on click", async ({ app }) => {
    const note = app.note();
    const wikilinks = note.wikilinks();
    const welcomeLink = wikilinks.filter({ hasText: "Welcome" });
    await expect(welcomeLink).toBeVisible();

    // Click should navigate to Welcome note via SPA
    await welcomeLink.click();
    await app.page.waitForURL("**/p/Notes%2FWelcome.md");

    // Verify the Welcome note content loaded
    const welcomeNote = app.note();
    await welcomeNote.waitForContent();
    await expect(welcomeNote.content().locator("h1")).toHaveText("Welcome to Imako");
  });

  test("renders footnote as superscript marker", async ({ app }) => {
    const note = app.note();
    // Footnotes render as <sup> with [*]
    const sups = note.superscripts();
    await expect(sups.first()).toBeVisible();
    await expect(sups.first()).toContainText("[*]");
  });

  test("renders inline math with KaTeX", async ({ app }) => {
    const content = app.note().content();
    // KaTeX renders into .katex elements
    const katex = content.locator(".katex");
    await expect(katex.first()).toBeVisible();
  });

  test("renders display math as block", async ({ app }) => {
    const content = app.note().content();
    // Both inline (E=mc²) and display (integral) math should render
    const allKatex = content.locator(".katex");
    await expect(allKatex).toHaveCount(2);
    // Second KaTeX element is the display math (integral)
    await expect(allKatex.nth(1)).toContainText("∫");
  });

  test("renders single and double quoted text", async ({ app }) => {
    const text = await app.note().textContent();
    expect(text).toContain("single quoted");
    expect(text).toContain("double quoted");
  });
});
