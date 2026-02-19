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
 * Parse a "YYYY-MM-DD.md" filename into year/month/day.
 * Returns null if the filename doesn't match the expected pattern.
 */
export function parseDateFilename(f: string): ParsedDate | null {
  const m = f.match(/^(\d{4})-(\d{2})-(\d{2})\.md$/);
  if (!m) return null;
  return { year: +m[1], month: +m[2], day: +m[3] };
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

/**
 * Build a filename from year/month/day, e.g. "2026-02-19.md".
 */
export function dateToFilename(year: number, month: number, day: number): string {
  const mm = String(month).padStart(2, "0");
  const dd = String(day).padStart(2, "0");
  return `${year}-${mm}-${dd}.md`;
}
