import { defineConfig, devices } from "@playwright/test";

/**
 * Playwright configuration for Imako E2E tests.
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

  webServer: [
    {
      // Backend: Nix package with the example vault
      command: "cd .. && nix run .#imako -- ./example",
      url: "http://localhost:9010",
      timeout: 300000, // 5 min for cold Nix builds
      reuseExistingServer: !process.env.CI,
    },
    {
      // Frontend: Vite dev server
      command: "cd .. && just frontend-dev",
      url: "http://localhost:5173",
      timeout: 30000,
      reuseExistingServer: !process.env.CI,
      cwd: __dirname,
    },
  ],
});
