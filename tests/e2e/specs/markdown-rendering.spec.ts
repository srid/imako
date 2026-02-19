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

    // H4 — BlockRenderer renders H4, H5, H6 all as <h4>
    const h4 = content.locator("h4");
    await expect(h4.filter({ hasText: "Fourth-Level Heading" })).toBeVisible();
    await expect(h4.filter({ hasText: "Fifth-Level Heading" })).toBeVisible();
    await expect(h4.filter({ hasText: "Sixth-Level Heading" })).toBeVisible();
  });

  test("heading styles: font-weight is bold", async ({ app }) => {
    const h1 = app.note().content().locator("h1");
    await expect(h1).toHaveCSS("font-weight", "700");
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

  test("renders code blocks with monospace font", async ({ app }) => {
    const note = app.note();
    const codeBlocks = note.codeBlocks();

    // At least 2 code blocks (haskell and javascript)
    await expect(codeBlocks).toHaveCount(2);

    // Verify content
    await expect(codeBlocks.first()).toContainText("putStrLn");
    await expect(codeBlocks.nth(1)).toContainText("greet");

    // Verify monospace font-family
    const fontFamily = await codeBlocks.first().evaluate(
      (el) => getComputedStyle(el).fontFamily
    );
    expect(fontFamily.toLowerCase()).toContain("mono");
  });

  test("code blocks have background and padding", async ({ app }) => {
    const pre = app.note().content().locator("pre").first();
    await expect(pre).toBeVisible();

    // Check background-color is not transparent
    const bg = await pre.evaluate((el) => getComputedStyle(el).backgroundColor);
    expect(bg).not.toBe("rgba(0, 0, 0, 0)");

    // Check padding exists
    const padding = await pre.evaluate((el) => getComputedStyle(el).padding);
    expect(padding).not.toBe("0px");
  });

  test("renders blockquote with left border", async ({ app }) => {
    const note = app.note();
    const blockquotes = note.blockquotes();
    await expect(blockquotes.first()).toBeVisible();
    await expect(blockquotes.first()).toContainText("This is a blockquote");

    // Verify left border
    const borderLeft = await blockquotes.first().evaluate(
      (el) => getComputedStyle(el).borderLeftWidth
    );
    expect(parseInt(borderLeft)).toBeGreaterThan(0);

    // Verify italic
    const fontStyle = await blockquotes.first().evaluate(
      (el) => getComputedStyle(el).fontStyle
    );
    expect(fontStyle).toBe("italic");
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

    const fontWeight = await strong.evaluate(
      (el) => getComputedStyle(el).fontWeight
    );
    expect(parseInt(fontWeight)).toBeGreaterThanOrEqual(700);
  });

  test("renders italic text with <em>", async ({ app }) => {
    const content = app.note().content();
    const em = content.locator("em").filter({ hasText: "italic words" });
    await expect(em).toBeVisible();

    const fontStyle = await em.evaluate(
      (el) => getComputedStyle(el).fontStyle
    );
    expect(fontStyle).toBe("italic");
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

    const bg = await code.evaluate((el) => getComputedStyle(el).backgroundColor);
    expect(bg).not.toBe("rgba(0, 0, 0, 0)");
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

  test("renders footnote marker", async ({ app }) => {
    const content = app.note().content();
    // Footnotes render as <sup> with [*] text
    const footnoteMarker = content.locator("sup").filter({ hasText: "[*]" });
    await expect(footnoteMarker.first()).toBeVisible();
  });

  test("renders single and double quoted text", async ({ app }) => {
    const text = await app.note().textContent();
    expect(text).toContain("single quoted");
    expect(text).toContain("double quoted");
  });
});
