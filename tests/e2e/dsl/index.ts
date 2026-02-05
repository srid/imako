/**
 * Imako E2E DSL - Main Exports
 *
 * Provides the scenario() helper and exports all DSL components.
 */

import { test as base, Page, BrowserContext } from "@playwright/test";
import { App } from "./app";

export { App } from "./app";
export {
  TasksView,
  NoteView,
  CommandPaletteView,
  FolderTreeView,
} from "./views";

/**
 * Extended test fixture with Imako-specific helpers.
 */
interface ImakoFixtures {
  app: App;
}

/**
 * Extended Playwright test with Imako DSL fixtures.
 */
export const test = base.extend<ImakoFixtures>({
  app: async ({ page }, use) => {
    const app = new App(page);
    await page.goto("/");
    await app.waitForConnection();
    await use(app);
  },
});

export { expect } from "@playwright/test";

/**
 * Sync wait constant for WebSocket propagation.
 */
export const SYNC_WAIT = 500;

/**
 * Wait for a file system change to propagate through the backend.
 */
export async function waitForSync(page: Page): Promise<void> {
  await page.waitForTimeout(SYNC_WAIT);
}
