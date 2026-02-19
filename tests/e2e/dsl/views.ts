/**
 * Imako E2E DSL - View Abstractions
 *
 * Read-only view interfaces that wrap Playwright locators,
 * hiding DOM structure from test logic.
 */

import { Page, Locator, expect } from "@playwright/test";

/**
 * Expected task for verification.
 */
export interface TaskExpectation {
  /** Text that should appear in the task description */
  text: string;
  /** Expected task status */
  status: "Incomplete" | "InProgress" | "Completed" | "Cancelled";
  /** Additional text fragments that should appear */
  contains?: string[];
}

/**
 * View abstraction for the unified vault page.
 */
export class VaultView {
  private readonly sidebar: Locator;
  private readonly detail: Locator;

  constructor(private readonly page: Page) {
    this.sidebar = page.locator("aside");
    this.detail = page.locator("main main");
  }

  /**
   * Wait for the vault to be loaded (sidebar tree visible).
   */
  async waitForVault(): Promise<void> {
    await expect(this.sidebar.locator("[data-testid='folder-tree']").first()).toBeVisible({ timeout: 5000 });
  }

  /**
   * Click a file in the sidebar tree to select it.
   */
  async selectFile(name: string): Promise<void> {
    const file = this.sidebar.locator(`[data-testid='file-node']:has-text("${name}")`);
    await file.click();
  }

  /**
   * Click a folder's icon/name to select it (navigate to it in main panel).
   */
  async selectFolder(name: string): Promise<void> {
    const label = this.sidebar.locator(`[data-testid='folder-node']`).filter({ hasText: name }).first().locator("[data-testid='folder-label']");
    await label.click();
  }

  /**
   * Click a folder's chevron arrow to toggle expand/collapse.
   */
  async toggleFolder(name: string): Promise<void> {
    const toggle = this.sidebar.locator(`[data-testid='folder-node']`).filter({ hasText: name }).first().locator("[data-testid='folder-toggle']");
    await toggle.click();
  }

  /**
   * Check if a folder node is expanded (details[open]).
   */
  async isFolderOpen(name: string): Promise<boolean> {
    const details = this.sidebar.locator(`[data-testid='folder-node']`).filter({ hasText: name }).first();
    return details.evaluate((el) => (el as HTMLDetailsElement).open);
  }

  /**
   * Click the root vault button.
   */
  async selectRoot(): Promise<void> {
    const root = this.sidebar.locator("button").first();
    await root.click();
  }

  /**
   * Get the detail pane container.
   */
  detailPane(): Locator {
    return this.detail;
  }

  /**
   * Get the folder contents listing container.
   */
  folderContents(): Locator {
    return this.detail.locator("[data-testid='folder-contents']");
  }

  /**
   * Get folder items in the folder contents listing.
   */
  folderContentsFolders(): Locator {
    return this.detail.locator("[data-testid='folder-contents-folder']");
  }

  /**
   * Get file items in the folder contents listing.
   */
  folderContentsFiles(): Locator {
    return this.detail.locator("[data-testid='folder-contents-file']");
  }

  /**
   * Check if a file task indicator dot is visible.
   */
  async fileHasTaskDot(name: string): Promise<boolean> {
    const file = this.sidebar.locator(`[data-testid='file-node']:has-text("${name}")`);
    const dot = file.locator(".rounded-full.bg-accent-500");
    return await dot.isVisible();
  }

  /**
   * Toggle the "Show tasks" button.
   */
  async toggleShowTasks(): Promise<void> {
    const button = this.page.locator("[data-testid='toggle-tasks']");
    await button.click();
  }

  /**
   * Type into the search filter.
   */
  async filterTree(query: string): Promise<void> {
    const input = this.page.locator("[data-testid='tree-filter']");
    await input.fill(query);
  }

  /**
   * Clear the search filter.
   */
  async clearFilter(): Promise<void> {
    const input = this.page.locator("[data-testid='tree-filter']");
    await input.fill("");
  }

  /**
   * Get the filter input locator (for focus/shortcut assertions).
   */
  filterInput(): Locator {
    return this.page.locator("[data-testid='tree-filter']");
  }
}

/**
 * View abstraction for tasks (in the detail pane).
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
   * Verify tasks match expected content and count.
   */
  async verifyTasks(expected: TaskExpectation[]): Promise<void> {
    // Verify exact count
    await expect(this.taskItems()).toHaveCount(expected.length);

    // Verify each task exists with correct content
    for (const task of expected) {
      // Match only the task-item whose own description contains the text
      // (not ancestor task-items that contain nested children with this text)
      const item = this.taskItems().filter({
        has: this.page.locator("[data-testid='task-description']", { hasText: task.text }),
      }).last();

      await expect(item).toBeVisible();
      await expect(item).toHaveAttribute("data-status", task.status);

      if (task.contains) {
        for (const text of task.contains) {
          await expect(item).toContainText(text);
        }
      }
    }
  }

  /**
   * Toggle a filter button by its label text.
   */
  async toggleFilter(label: string): Promise<void> {
    const button = this.page.getByRole("button", { name: label });
    await button.click();
  }

  /**
   * Get the folder tree container in the detail pane (for folder/root view).
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
   * Get paragraphs.
   */
  paragraphs(): Locator {
    return this.content().locator("p");
  }

  /**
   * Get blockquotes.
   */
  blockquotes(): Locator {
    return this.content().locator("blockquote");
  }

  /**
   * Get horizontal rules.
   */
  horizontalRules(): Locator {
    return this.content().locator("hr");
  }

  /**
   * Get tables.
   */
  tables(): Locator {
    return this.content().locator("table");
  }

  /**
   * Get ordered lists.
   */
  orderedLists(): Locator {
    return this.content().locator("ol");
  }

  /**
   * Get definition lists.
   */
  definitionLists(): Locator {
    return this.content().locator("dl");
  }

  /**
   * Get definition terms.
   */
  definitionTerms(): Locator {
    return this.content().locator("dt");
  }

  /**
   * Get definition descriptions.
   */
  definitionDescriptions(): Locator {
    return this.content().locator("dd");
  }

  /**
   * Get external links (<a> tags with href).
   */
  externalLinks(): Locator {
    return this.content().locator("a[href]");
  }

  /**
   * Get images.
   */
  images(): Locator {
    return this.content().locator("img");
  }

  /**
   * Get superscript elements.
   */
  superscripts(): Locator {
    return this.content().locator("sup");
  }

  /**
   * Get subscript elements.
   */
  subscripts(): Locator {
    return this.content().locator("sub");
  }

  /**
   * Wait for note content to be loaded.
   */
  async waitForContent(): Promise<void> {
    await expect(this.content()).toBeVisible({ timeout: 10000 });
  }

  /**
   * Get the text content of the note.
   */
  async textContent(): Promise<string> {
    return (await this.content().textContent()) ?? "";
  }
}

/**
 * View abstraction for the Folder Tree sidebar.
 */
export class FolderTreeView {
  private readonly container: Locator;

  constructor(private readonly page: Page) {
    this.container = page.locator("aside [data-testid='folder-tree']").first();
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
   * Click on a file node to select it.
   */
  async selectFile(name: string): Promise<void> {
    const file = this.container.locator(
      `[data-testid='file-node']:has-text("${name}")`
    );
    await file.click();
  }
}
