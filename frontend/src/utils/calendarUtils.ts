/**
 * Shared calendar utility functions.
 *
 * Used by both CalendarWidget (sidebar) and JournalView (main panel).
 */

export const DAYS = ["Mo", "Tu", "We", "Th", "Fr", "Sa", "Su"] as const;

export interface ParsedDate {
  year: number;
  /** 1-indexed month */
  month: number;
  day: number;
}

/**
 * Parse an ISO date string like "2026-02-19" into year/month/day.
 */
export function parseISODate(iso: string): ParsedDate {
  const [y, m, d] = iso.split("-").map(Number);
  return { year: y, month: m, day: d };
}

/**
 * Build a Monday-start calendar grid for a given year/month.
 * Returns an array of (day | null) cells, padded to complete weeks.
 */
export function buildCalendarGrid(year: number, month: number): (number | null)[] {
  const m = month - 1; // JS 0-indexed
  const firstDay = new Date(year, m, 1);
  const startDow = (firstDay.getDay() + 6) % 7; // Monday=0
  const daysInMonth = new Date(year, m + 1, 0).getDate();

  const cells: (number | null)[] = [];
  for (let i = 0; i < startDow; i++) cells.push(null);
  for (let d = 1; d <= daysInMonth; d++) cells.push(d);
  while (cells.length % 7 !== 0) cells.push(null);
  return cells;
}

/**
 * Check if a day matches a given "today" date.
 */
export function isDayToday(year: number, month: number, day: number, today: ParsedDate): boolean {
  return year === today.year && month === today.month && day === today.day;
}
