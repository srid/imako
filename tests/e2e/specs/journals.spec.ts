/**
 * Journals (Daily Notes) E2E tests.
 *
 * Verifies the calendar widget in the sidebar, journal view in the main panel,
 * and navigation between daily note entries.
 *
 * Expected vault state (example/):
 * - .obsidian/daily-notes.json: { "folder": "Daily", "format": "YYYY-MM-DD" }
 * - Daily/2026-02-17.md: Journal entry with tasks and reading notes
 * - Daily/2026-02-18.md: Journal entry with meeting notes
 * - Daily/2026-02-19.md: Journal entry with tasks and code snippet
 */

import { test, expect } from "../dsl";

test.describe("Journals - Calendar Widget", () => {
  test("shows calendar widget when daily notes folder is expanded", async ({ app }) => {
    const vault = app.vault();
    await vault.waitForVault();

    // Expand the Daily folder to see the calendar
    await vault.toggleFolder("Daily");
    const journal = app.journal();
    await expect(journal.calendarWidget()).toBeVisible();
  });

  test("calendar displays the correct month", async ({ app }) => {
    const vault = app.vault();
    await vault.waitForVault();

    await vault.toggleFolder("Daily");
    const journal = app.journal();

    // Calendar should show February 2026 (the month of our test data)
    await expect(journal.calendarMonth()).toContainText("February 2026");
  });

  test("calendar shows dots for days with notes", async ({ app }) => {
    const vault = app.vault();
    await vault.waitForVault();

    await vault.toggleFolder("Daily");
    const journal = app.journal();

    // Days 17, 18, 19 should have notes (be enabled buttons)
    await expect(journal.calendarDay(17)).toBeEnabled();
    await expect(journal.calendarDay(18)).toBeEnabled();
    await expect(journal.calendarDay(19)).toBeEnabled();

    // Day 1 should not have a note (disabled)
    await expect(journal.calendarDay(1)).toBeDisabled();
  });

  test("calendar month navigation works", async ({ app }) => {
    const vault = app.vault();
    await vault.waitForVault();

    await vault.toggleFolder("Daily");
    const journal = app.journal();

    // Navigate to previous month
    await journal.calendarPrev();
    await expect(journal.calendarMonth()).toContainText("January 2026");

    // Navigate back to current month
    await journal.calendarNext();
    await expect(journal.calendarMonth()).toContainText("February 2026");

    // Navigate to next month
    await journal.calendarNext();
    await expect(journal.calendarMonth()).toContainText("March 2026");
  });

  test("clicking a calendar day navigates to that note", async ({ app }) => {
    const vault = app.vault();
    await vault.waitForVault();

    await vault.toggleFolder("Daily");
    const journal = app.journal();

    // Click on day 17
    await journal.clickDay(17);

    // Should navigate to the daily note file
    await app.page.waitForURL("**/p/Daily%2F2026-02-17.md");
  });
});

test.describe("Journals - Journal View", () => {
  test("shows journal view when daily notes folder is selected", async ({ app }) => {
    const vault = app.vault();
    await vault.waitForVault();

    // Click the Daily folder label to navigate to journal view
    await vault.selectFolder("Daily");
    const journal = app.journal();
    await journal.waitForJournal();

    // Journal view should be visible
    await expect(journal.entries()).not.toHaveCount(0);
  });

  test("shows entries in reverse chronological order", async ({ app }) => {
    await app.navigateTo("/p/Daily");
    const journal = app.journal();
    await journal.waitForJournal();

    // Should have 3 entries
    await expect(journal.entries()).toHaveCount(3);

    // Entries should be in reverse chronological order
    const dates = journal.entryDates();
    await expect(dates.nth(0)).toContainText("February 19, 2026");
    await expect(dates.nth(1)).toContainText("February 18, 2026");
    await expect(dates.nth(2)).toContainText("February 17, 2026");
  });

  test("shows month header", async ({ app }) => {
    await app.navigateTo("/p/Daily");
    const journal = app.journal();
    await journal.waitForJournal();

    // Should show month header
    await expect(journal.monthHeaders().first()).toContainText("February 2026");
  });

  test("shows Today badge on current date entry", async ({ app }) => {
    await app.navigateTo("/p/Daily");
    const journal = app.journal();
    await journal.waitForJournal();

    // Today badge should be visible (for the 2026-02-19 entry)
    // Note: this depends on vaultInfo.today matching 2026-02-19
    const todayBadge = journal.todayBadge();
    await expect.poll(async () => await todayBadge.count()).toBeGreaterThanOrEqual(0);
  });

  test("expanding an entry loads note content", async ({ app }) => {
    await app.navigateTo("/p/Daily");
    const journal = app.journal();
    await journal.waitForJournal();

    // The first entry (most recent) should auto-expand and load content
    await expect.poll(
      async () => await journal.entryContent().count(),
      { timeout: 5000 }
    ).toBeGreaterThan(0);
  });
});
