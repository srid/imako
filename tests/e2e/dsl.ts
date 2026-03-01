import { type Page, type Locator, expect } from "@playwright/test";

/**
 * App DSL for Imako E2E tests.
 *
 * Wraps Playwright page interactions with domain-specific methods.
 * Follows the Imperative DSL pattern: separates "what" (test logic)
 * from "how" (DOM selectors).
 */
export class App {
  constructor(public readonly page: Page) {}

  /** Navigate to the app root and wait for the sidebar to load. */
  async goto() {
    await this.page.goto("/");
    await this.waitForSidebar();
  }

  /** Navigate to a note path and wait for content to render. */
  async gotoNote(path: string) {
    await this.page.goto(`/p/${path}`);
    await this.waitForSidebar();
    await this.waitForContent();
  }

  /** Navigate to a folder path. */
  async gotoFolder(path: string) {
    await this.page.goto(`/p/${path}`);
    await this.waitForSidebar();
  }

  /** Wait for the sidebar folder tree to be visible. */
  async waitForSidebar() {
    await expect(this.sidebar()).toBeVisible({ timeout: 15_000 });
    await expect(
      this.sidebar().locator("details").first()
    ).toBeVisible({ timeout: 15_000 });
  }

  /** Wait for note content to finish loading. */
  async waitForContent() {
    await expect(
      this.content().locator(".markdown-content")
    ).toBeVisible({ timeout: 15_000 });
  }

  // --- Scoped Containers ---

  sidebar(): Locator {
    return this.page.locator("aside");
  }

  content(): Locator {
    return this.page.locator("main");
  }

  // --- Sidebar Interactions ---

  /** Get all folder names in the sidebar (text only, no emoji). */
  async sidebarFolderNames(): Promise<string[]> {
    // Each folder: <summary> <Link> <span>📁</span> <span>{name}</span> </Link> </summary>
    // The name is the second span inside the link inside summary
    const nameSpans = this.sidebar().locator("summary a span:nth-child(2)");
    const count = await nameSpans.count();
    const names: string[] = [];
    for (let i = 0; i < count; i++) {
      const text = await nameSpans.nth(i).textContent();
      if (text) names.push(text.trim());
    }
    return names;
  }

  /** Click a file in the sidebar by name and wait for content. */
  async clickFile(name: string) {
    await this.sidebar().locator("a", { hasText: name }).click();
    await this.waitForContent();
  }

  // --- Content Assertions ---

  async expectHeading(text: string) {
    await expect(
      this.content().locator("h1, h2, h3, h4", { hasText: text }).first()
    ).toBeVisible({ timeout: 5_000 });
  }

  async expectText(text: string) {
    await expect(
      this.content().getByText(text, { exact: false }).first()
    ).toBeVisible({ timeout: 5_000 });
  }

  async expectElement(selector: string) {
    await expect(
      this.content().locator(selector).first()
    ).toBeVisible({ timeout: 5_000 });
  }

  currentPath(): string {
    return new URL(this.page.url()).pathname;
  }
}
