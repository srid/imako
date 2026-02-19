/**
 * Imako E2E DSL - Main Exports
 *
 * Provides the scenario() helper and exports all DSL components.
 */

import { test as base } from "@playwright/test";
import { App } from "./app";

export { App } from "./app";
export {
  TasksView,
  TaskExpectation,
  NoteView,
  VaultView,
  FolderTreeView,
  JournalView,
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
