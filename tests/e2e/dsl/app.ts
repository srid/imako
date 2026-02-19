/**
 * Imako E2E DSL - App Wrapper
 *
 * Wraps the Playwright Page to provide a high-level, imperative API
 * for interacting with the Imako application.
 */

import { Page, expect } from "@playwright/test";
import { TasksView, NoteView, FolderTreeView, VaultView } from "./views";

export class App {
  constructor(public readonly page: Page) {}

  /**
   * Wait for the WebSocket connection to be established.
   * The loading spinner should disappear once connected.
   */
  async waitForConnection(timeoutMs = 10000): Promise<void> {
    // Wait for the loading spinner to disappear
    await expect(this.page.locator("text=Connecting to vault")).toBeHidden({
      timeout: timeoutMs,
    });
    // Verify the main app container is visible
    await expect(this.page.locator(".max-w-6xl")).toBeVisible();
  }

  /**
   * Navigate to a specific route (uses hash-based routing).
   * Routes are prefixed with /#/ for HashRouter compatibility.
   */
  async navigateTo(route: "/" | `/n/${string}`): Promise<void> {
    // With HashRouter, routes become /#/path - navigate to root and let hash handle it
    const hashRoute = route === "/" ? "/" : `/#${route}`;
    await this.page.goto(hashRoute);
    await this.waitForConnection();
  }

  /**
   * Get the VaultView for interacting with the unified vault page.
   */
  vault(): VaultView {
    return new VaultView(this.page);
  }

  /**
   * Get the TasksView for interacting with tasks in the detail pane.
   */
  tasks(): TasksView {
    return new TasksView(this.page);
  }

  /**
   * Get a NoteView for a specific note path.
   */
  note(path?: string): NoteView {
    return new NoteView(this.page, path);
  }

  /**
   * Get the FolderTreeView for the sidebar tree.
   */
  folderTree(): FolderTreeView {
    return new FolderTreeView(this.page);
  }

  /**
   * Get the vault name displayed in the header.
   */
  async vaultName(): Promise<string> {
    const header = this.page.locator("header h1");
    const text = await header.textContent();
    return text?.trim() ?? "";
  }
}
