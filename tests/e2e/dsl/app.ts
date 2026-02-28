/**
 * Imako E2E DSL - App Wrapper
 *
 * High-level API for interacting with the Imako application.
 * Adapted from old SolidJS DSL for Dioxus SSR.
 */

import { Page, expect } from "@playwright/test";
import { TasksView, NoteView, FolderTreeView, VaultView } from "./views";

export class App {
  constructor(public readonly page: Page) {}

  /**
   * Navigate to a specific route.
   * In Dioxus SSR, routes are standard paths (not hash-based).
   */
  async navigateTo(route: string): Promise<void> {
    const url = route.startsWith("/") ? route : `/${route}`;
    await this.page.goto(url);
  }

  /**
   * Wait for the page to be loaded (main container visible).
   */
  async waitForConnection(timeoutMs = 10000): Promise<void> {
    await expect(this.page.locator(".max-w-6xl")).toBeVisible({ timeout: timeoutMs });
  }

  /**
   * Get the vault name from the sidebar.
   */
  async vaultName(): Promise<string> {
    return (await this.page.locator("aside button").first().textContent()) ?? "";
  }

  vault(): VaultView { return new VaultView(this.page); }
  tasks(): TasksView { return new TasksView(this.page); }
  note(): NoteView { return new NoteView(this.page); }
  folderTree(): FolderTreeView { return new FolderTreeView(this.page); }
}
