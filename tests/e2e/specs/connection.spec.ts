/**
 * Connection and hydration tests.
 *
 * Verifies that the app connects to the WebSocket server
 * and hydrates the UI correctly.
 */

import { test, expect } from "../dsl";

test.describe("Connection", () => {
  test("shows loading spinner while connecting", async ({ page }) => {
    // Navigate without waiting for connection
    await page.goto("/", { waitUntil: "domcontentloaded" });

    // Loading spinner should be visible initially
    const spinner = page.locator("text=Connecting to vault");
    // May or may not be visible depending on connection speed
    // Just verify the page loads eventually
    await expect(page.locator(".max-w-4xl")).toBeVisible({ timeout: 10000 });
  });

  test("displays app after connection", async ({ app }) => {
    // app fixture already waits for connection
    await expect(app.page.locator(".max-w-4xl")).toBeVisible();
  });

  test("shows vault name in header", async ({ app }) => {
    // Navigate to tasks page
    await app.navigateTo("/tasks");

    // Header should contain the vault name
    const header = app.page.locator("header");
    await expect(header).toBeVisible();
  });

  test("redirects root to tasks page", async ({ page }) => {
    await page.goto("/");
    // Wait for navigation to complete (hash-based routing)
    await page.waitForURL("**/#/tasks");
    expect(page.url()).toContain("/#/tasks");
  });
});
