import { defineConfig } from "@playwright/test";

export default defineConfig({
  testDir: "./e2e",
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  workers: process.env.CI ? 1 : undefined,
  reporter: "list",
  use: {
    baseURL: "http://127.0.0.1:6006",
    trace: "on-first-retry",
  },
  // No webServer block — start the server manually with `just run`
  // before running tests. This avoids dx serve hot-reload crashes.
});
