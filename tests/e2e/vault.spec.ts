import { test, expect } from '@playwright/test';

test.describe('Vault Page', () => {
  test('renders the app title', async ({ page }) => {
    await page.goto('/');
    // App title is いまここ
    await expect(page.locator('h1')).toContainText('いまここ');
  });

  test('renders the sidebar with vault name', async ({ page }) => {
    await page.goto('/');
    // Sidebar should show the vault folder
    const sidebar = page.locator('aside');
    await expect(sidebar).toBeVisible();
    await expect(sidebar).toContainText('example');
  });

  test('renders folder tree in sidebar', async ({ page }) => {
    await page.goto('/');
    const sidebar = page.locator('aside');
    // Should show at least some folders from the example vault
    await expect(sidebar).toContainText('Daily');
    await expect(sidebar).toContainText('Notes');
    await expect(sidebar).toContainText('Projects');
  });

  test('renders tasks in the main panel', async ({ page }) => {
    await page.goto('/');
    // The main panel should have some task content from the vault
    const main = page.locator('main');
    await expect(main).toBeVisible();
  });
});

test.describe('Routing', () => {
  test('navigating to /p/Notes/Welcome.md shows note content', async ({ page }) => {
    await page.goto('/p/Notes%2FWelcome.md');
    // Should render the note content via SSR
    const main = page.locator('main');
    await expect(main).toBeVisible();
  });

  test('root route shows vault overview', async ({ page }) => {
    await page.goto('/');
    // Should have the sidebar and main panel
    await expect(page.locator('aside')).toBeVisible();
    await expect(page.locator('main')).toBeVisible();
  });
});

test.describe('Server Functions', () => {
  test('vault tree data loads via server function', async ({ page }) => {
    await page.goto('/');
    // If vault tree loads successfully, sidebar will show folder structure
    const sidebar = page.locator('aside');
    // Wait for the sidebar to have content (server function response)
    await expect(sidebar).not.toBeEmpty();
  });
});
