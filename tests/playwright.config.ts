import { defineConfig, devices } from "@playwright/test";

// Base URL configurable via env var (for e2e tests on different ports)
const baseURL = process.env.E2E_BASE_URL || "http://localhost:5173";

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
    baseURL,
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

