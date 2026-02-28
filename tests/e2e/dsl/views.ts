/**
 * Imako E2E DSL - View Abstractions
 *
 * Read-only view interfaces that wrap Playwright locators,
 * hiding DOM structure from test logic.
 * Ported from old SolidJS DSL to Dioxus SSR.
 */

import { Page, Locator, expect } from "@playwright/test";

/**
 * Expected task for verification.
 */
export interface TaskExpectation {
  text: string;
  status: "Incomplete" | "InProgress" | "Completed" | "Cancelled";
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
    this.detail = page.locator("main");
  }

  async waitForVault(): Promise<void> {
    await expect(this.sidebar.locator("[data-testid='folder-tree']").first()).toBeVisible({ timeout: 5000 });
  }

  async selectFile(name: string): Promise<void> {
    const file = this.sidebar.locator(`[data-testid='file-node']:has-text("${name}")`);
    await file.click();
  }

  async selectFolder(name: string): Promise<void> {
    const label = this.sidebar
      .locator("[data-testid='folder-node']")
      .filter({ hasText: name })
      .first()
      .locator("[data-testid='folder-label']");
    await label.click();
  }

  async selectRoot(): Promise<void> {
    const root = this.sidebar.locator("button").first();
    await root.click();
  }

  detailPane(): Locator { return this.detail; }
  folderContents(): Locator { return this.detail.locator("[data-testid='folder-contents']"); }
  folderContentsFolders(): Locator { return this.detail.locator("[data-testid='folder-contents-folder']"); }
  folderContentsFiles(): Locator { return this.detail.locator("[data-testid='folder-contents-file']"); }
}

/**
 * View abstraction for tasks in the detail pane.
 */
export class TasksView {
  private readonly container: Locator;

  constructor(private readonly page: Page) {
    this.container = page.locator("main");
  }

  taskItems(): Locator {
    return this.container.locator("[data-testid='task-item']");
  }

  tasksByStatus(status: "Incomplete" | "InProgress" | "Completed" | "Cancelled"): Locator {
    return this.container.locator(`[data-status='${status}']`);
  }

  async taskCount(): Promise<number> {
    return await this.taskItems().count();
  }

  async waitForTasks(): Promise<void> {
    await expect(this.taskItems().first()).toBeVisible({ timeout: 5000 });
  }

  async verifyTasks(expected: TaskExpectation[]): Promise<void> {
    // Verify at least the expected number of tasks
    const count = await this.taskItems().count();
    expect(count).toBeGreaterThanOrEqual(expected.length);

    // Verify each expected task exists with correct content
    for (const task of expected) {
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

  fileTaskGroups(): Locator {
    return this.container.locator("[data-testid='file-tasks-group']");
  }

  folderTaskGroups(): Locator {
    return this.container.locator("[data-testid='folder-tasks-group']");
  }

  folderTaskGroup(name: string): Locator {
    return this.container.locator("[data-testid='folder-tasks-group']").filter({ hasText: name });
  }
}

/**
 * View abstraction for a Note page.
 */
export class NoteView {
  private readonly container: Locator;

  constructor(private readonly page: Page) {
    this.container = page.locator("main");
  }

  content(): Locator {
    return this.container.locator("[data-testid='note-content']");
  }

  headings(): Locator {
    return this.content().locator("h1, h2, h3, h4, h5, h6");
  }

  wikilinks(): Locator {
    return this.content().locator("[data-wikilink]");
  }

  brokenWikilinks(): Locator {
    return this.content().locator("[data-broken='true']");
  }

  codeBlocks(): Locator {
    return this.content().locator("pre code");
  }

  paragraphs(): Locator {
    return this.content().locator("p");
  }

  blockquotes(): Locator {
    return this.content().locator("blockquote");
  }

  horizontalRules(): Locator {
    return this.content().locator("hr");
  }

  tables(): Locator {
    return this.content().locator("table");
  }

  orderedLists(): Locator {
    return this.content().locator("ol");
  }

  definitionLists(): Locator {
    return this.content().locator("dl");
  }

  definitionTerms(): Locator {
    return this.content().locator("dt");
  }

  definitionDescriptions(): Locator {
    return this.content().locator("dd");
  }

  externalLinks(): Locator {
    return this.content().locator("a[href]");
  }

  superscripts(): Locator {
    return this.content().locator("sup");
  }

  subscripts(): Locator {
    return this.content().locator("sub");
  }

  backlinksSection(): Locator {
    return this.container.locator("[data-testid='backlinks-section']");
  }

  backlinkItems(): Locator {
    return this.container.locator("[data-testid='backlink-item']");
  }

  async waitForContent(): Promise<void> {
    await expect(this.content()).toBeVisible({ timeout: 10000 });
  }

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

  folders(): Locator {
    return this.container.locator("[data-testid='folder-node']");
  }

  files(): Locator {
    return this.container.locator("[data-testid='file-node']");
  }

  async expandFolder(name: string): Promise<void> {
    const folder = this.container.locator(`[data-testid='folder-node']:has-text("${name}")`);
    await folder.click();
  }

  async selectFile(name: string): Promise<void> {
    const file = this.container.locator(`[data-testid='file-node']:has-text("${name}")`);
    await file.click();
  }
}
