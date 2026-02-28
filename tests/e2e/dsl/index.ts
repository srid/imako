/**
 * Imako E2E DSL - Main Exports
 */

import { test as base } from "@playwright/test";
import { App } from "./app";

export { App } from "./app";
export { TasksView, TaskExpectation, NoteView, VaultView, FolderTreeView } from "./views";

interface ImakoFixtures {
  app: App;
}

export const test = base.extend<ImakoFixtures>({
  app: async ({ page }, use) => {
    const app = new App(page);
    await page.goto("/");
    await app.waitForConnection();
    await use(app);
  },
});

export { expect } from "@playwright/test";
