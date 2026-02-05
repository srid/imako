import { defineConfig, devices } from "@playwright/test";

/**
 * Playwright configuration for Imako E2E tests.
 * Servers are managed externally by process-compose (via `just e2e-servers`).
 *
 * @see https://playwright.dev/docs/test-configuration
 */
export default defineConfig({
  testDir: "./e2e/specs",
  fullyParallel: false,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  workers: 1,
  reporter: process.env.CI ? "github" : "list",
  timeout: 30000,

  use: {
    baseURL: "http://localhost:5173",
    trace: "on-first-retry",
    screenshot: "only-on-failure",
  },

  projects: [
    {
      name: "chromium",
      use: { ...devices["Desktop Chrome"] },
    },
  ],

  // Servers are managed by process-compose (nix run .#e2e-servers)
  // No webServer config needed here
});

