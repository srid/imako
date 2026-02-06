/**
 * Imako E2E DSL - View Abstractions
 *
 * Read-only view interfaces that wrap Playwright locators,
 * hiding DOM structure from test logic.
 */

import { Page, Locator, expect } from "@playwright/test";

/**
 * View abstraction for the Tasks page.
 */
export class TasksView {
  private readonly container: Locator;

  constructor(private readonly page: Page) {
    this.container = page.locator("main");
  }

  /**
   * Get all visible task items.
   */
  taskItems(): Locator {
    return this.container.locator("[data-testid='task-item']");
  }

  /**
   * Get task items by status.
   */
  tasksByStatus(
    status: "Incomplete" | "InProgress" | "Completed" | "Cancelled"
  ): Locator {
    return this.container.locator(`[data-status='${status}']`);
  }

  /**
   * Get the count of visible tasks.
   */
  async taskCount(): Promise<number> {
    return await this.taskItems().count();
  }

  /**
   * Wait for tasks to be loaded (at least one task visible).
   */
  async waitForTasks(): Promise<void> {
    await expect(this.taskItems().first()).toBeVisible({ timeout: 5000 });
  }

  /**
   * Get the folder tree container.
   */
  folderTree(): Locator {
    return this.container.locator("[data-testid='folder-tree']");
  }
}

/**
 * View abstraction for a Note page.
 */
export class NoteView {
  private readonly container: Locator;

  constructor(
    private readonly page: Page,
    private readonly notePath?: string
  ) {
    this.container = page.locator("main");
  }

  /**
   * Get the note content container.
   */
  content(): Locator {
    return this.container.locator("[data-testid='note-content']");
  }

  /**
   * Get all headings in the note.
   */
  headings(): Locator {
    return this.content().locator("h1, h2, h3, h4, h5, h6");
  }

  /**
   * Get all wikilinks (identified by data-wikilink attribute).
   */
  wikilinks(): Locator {
    return this.content().locator("[data-wikilink]");
  }

  /**
   * Get broken wikilinks.
   */
  brokenWikilinks(): Locator {
    return this.content().locator("[data-broken='true']");
  }

  /**
   * Get code blocks.
   */
  codeBlocks(): Locator {
    return this.content().locator("pre code");
  }

  /**
   * Get task checkboxes within the note.
   */
  taskCheckboxes(): Locator {
    return this.content().locator("input[type='checkbox']");
  }

  /**
   * Wait for note content to be loaded.
   */
  async waitForContent(): Promise<void> {
    await expect(this.content()).toBeVisible({ timeout: 5000 });
  }

  /**
   * Get the text content of the note.
   */
  async textContent(): Promise<string> {
    return (await this.content().textContent()) ?? "";
  }
}

/**
 * View abstraction for the Command Palette.
 */
export class CommandPaletteView {
  private readonly overlay: Locator;
  private readonly input: Locator;

  constructor(private readonly page: Page) {
    this.overlay = page.locator("[data-testid='command-palette']");
    this.input = this.overlay.locator("input");
  }

  /**
   * Open the command palette using keyboard shortcut.
   */
  async open(): Promise<void> {
    await this.page.keyboard.press("Control+k");
    await expect(this.overlay).toBeVisible();
  }

  /**
   * Close the command palette.
   */
  async close(): Promise<void> {
    await this.page.keyboard.press("Escape");
    await expect(this.overlay).toBeHidden();
  }

  /**
   * Search for a note by name.
   */
  async search(query: string): Promise<void> {
    await this.input.fill(query);
  }

  /**
   * Get the search results.
   */
  results(): Locator {
    return this.overlay.locator("[data-testid='palette-result']");
  }

  /**
   * Select a result by index (0-based).
   */
  async selectResult(index: number): Promise<void> {
    await this.results().nth(index).click();
  }

  /**
   * Check if the palette is open.
   */
  async isOpen(): Promise<boolean> {
    return await this.overlay.isVisible();
  }
}

/**
 * View abstraction for the Folder Tree.
 */
export class FolderTreeView {
  private readonly container: Locator;

  constructor(private readonly page: Page) {
    this.container = page.locator("[data-testid='folder-tree']");
  }

  /**
   * Get all folder nodes.
   */
  folders(): Locator {
    return this.container.locator("[data-testid='folder-node']");
  }

  /**
   * Get all file nodes.
   */
  files(): Locator {
    return this.container.locator("[data-testid='file-node']");
  }

  /**
   * Expand a folder by name.
   */
  async expandFolder(name: string): Promise<void> {
    const folder = this.container.locator(
      `[data-testid='folder-node']:has-text("${name}")`
    );
    await folder.click();
  }

  /**
   * Click on a file node.
   */
  async openFile(name: string): Promise<void> {
    const file = this.container.locator(
      `[data-testid='file-node']:has-text("${name}")`
    );
    await file.click();
  }
}
