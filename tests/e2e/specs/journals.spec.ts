/**
 * Journals (Daily Notes) E2E tests.
 *
 * Verifies the calendar widget in the sidebar and calendar view in the main panel.
 *
 * Expected vault state (example/):
 * - .obsidian/daily-notes.json: { "folder": "Daily", "format": "YYYY-MM-DD" }
 * - Daily/2026-02-17.md, 2026-02-18.md, 2026-02-19.md
 */

import { test, expect } from "../dsl";

test.describe("Journals - Sidebar Calendar", () => {
  test("shows calendar widget when daily notes folder is expanded", async ({ app }) => {
    const vault = app.vault();
    await vault.waitForVault();

    await vault.toggleFolder("Daily");
    const journal = app.journal();
    await expect(journal.calendarWidget()).toBeVisible();
  });

  test("calendar displays the correct month", async ({ app }) => {
    const vault = app.vault();
    await vault.waitForVault();

    await vault.toggleFolder("Daily");
    const journal = app.journal();
    await expect(journal.calendarMonth()).toContainText("February 2026");
  });

  test("calendar shows enabled days for notes and disabled for empty", async ({ app }) => {
    const vault = app.vault();
    await vault.waitForVault();

    await vault.toggleFolder("Daily");
    const journal = app.journal();

    // Days with notes should be enabled
    await expect(journal.calendarDay(17)).toBeEnabled();
    await expect(journal.calendarDay(18)).toBeEnabled();
    await expect(journal.calendarDay(19)).toBeEnabled();

    // Day without a note should be disabled
    await expect(journal.calendarDay(1)).toBeDisabled();
  });

  test("calendar month navigation works", async ({ app }) => {
    const vault = app.vault();
    await vault.waitForVault();

    await vault.toggleFolder("Daily");
    const journal = app.journal();

    await journal.calendarPrev();
    await expect(journal.calendarMonth()).toContainText("January 2026");

    await journal.calendarNext();
    await expect(journal.calendarMonth()).toContainText("February 2026");

    await journal.calendarNext();
    await expect(journal.calendarMonth()).toContainText("March 2026");
  });

  test("clicking a calendar day navigates to that note", async ({ app }) => {
    const vault = app.vault();
    await vault.waitForVault();

    await vault.toggleFolder("Daily");
    const journal = app.journal();

    await journal.clickDay(17);
    await app.page.waitForURL("**/p/Daily%2F2026-02-17.md");
  });
});

test.describe("Journals - Main Panel Calendar", () => {
  test("shows calendar view when Daily folder is selected", async ({ app }) => {
    const vault = app.vault();
    await vault.waitForVault();

    await vault.selectFolder("Daily");
    const journal = app.journal();
    await journal.waitForJournal();
  });

  test("shows month grids with correct header", async ({ app }) => {
    await app.navigateTo("/p/Daily");
    const journal = app.journal();
    await journal.waitForJournal();

    // Should have a February month header
    await expect(journal.monthHeaders().first()).toContainText("February");
  });

  test("month grid days with notes are clickable", async ({ app }) => {
    await app.navigateTo("/p/Daily");
    const journal = app.journal();
    await journal.waitForJournal();

    // Day 19 should be enabled (has a note)
    const main = app.page.locator("[data-testid='journal-view']");
    const day19 = main.locator("[data-testid='calendar-day-19']");
    await expect(day19).toBeEnabled();

    // Click navigates to the note
    await day19.click();
    await app.page.waitForURL("**/p/Daily%2F2026-02-19.md");
  });
});
