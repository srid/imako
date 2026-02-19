import { Component, createMemo, For } from "solid-js";
import type { FolderNode } from "@/types";

interface JournalViewProps {
  /** The daily notes folder node */
  node: FolderNode;
  /** Path of the daily notes folder (e.g. "Daily") */
  folderPath: string;
  /** Today's date as ISO string, e.g. "2026-02-19" */
  today: string;
  /** Navigate to a vault path */
  onSelectPath: (path: string) => void;
}

interface ParsedNote {
  year: number;
  month: number; // 1-indexed
  day: number;
  filename: string;
}

const DAYS = ["Mo", "Tu", "We", "Th", "Fr", "Sa", "Su"];

/**
 * Parse YYYY-MM-DD.md filenames into structured data.
 */
function parseNoteFiles(files: string[]): ParsedNote[] {
  const notes: ParsedNote[] = [];
  for (const f of files) {
    const m = f.match(/^(\d{4})-(\d{2})-(\d{2})\.md$/);
    if (m) {
      notes.push({ year: +m[1], month: +m[2], day: +m[3], filename: f });
    }
  }
  return notes;
}

/**
 * Group parsed notes by year → month.
 */
function groupByYearMonth(notes: ParsedNote[]): { year: number; months: { month: number; days: ParsedNote[] }[] }[] {
  const yearMap = new Map<number, Map<number, ParsedNote[]>>();
  for (const n of notes) {
    if (!yearMap.has(n.year)) yearMap.set(n.year, new Map());
    const monthMap = yearMap.get(n.year)!;
    if (!monthMap.has(n.month)) monthMap.set(n.month, []);
    monthMap.get(n.month)!.push(n);
  }

  // Sort years descending, months ascending within each year
  const result: { year: number; months: { month: number; days: ParsedNote[] }[] }[] = [];
  const sortedYears = [...yearMap.keys()].sort((a, b) => b - a);
  for (const year of sortedYears) {
    const monthMap = yearMap.get(year)!;
    const sortedMonths = [...monthMap.keys()].sort((a, b) => b - a);
    result.push({
      year,
      months: sortedMonths.map((month) => ({
        month,
        days: monthMap.get(month)!,
      })),
    });
  }
  return result;
}

/**
 * Full calendar view for the daily notes folder.
 * Shows all months that have notes as mini calendar grids.
 * Zero server requests — renders entirely from the folder file list.
 */
export const JournalView: Component<JournalViewProps> = (props) => {
  const parsed = createMemo(() => parseNoteFiles(Object.keys(props.node.files)));
  const grouped = createMemo(() => groupByYearMonth(parsed()));

  const todayParts = createMemo(() => {
    const [y, m, d] = props.today.split("-").map(Number);
    return { year: y, month: m, day: d };
  });

  return (
    <div data-testid="journal-view">
      <For each={grouped()}>
        {(yearGroup) => (
          <div class="mb-8">
            {/* Year header */}
            <h2 class="text-xl font-bold text-stone-700 dark:text-stone-200 mb-4 flex items-center gap-2">
              <span class="text-accent-500">📅</span>
              {yearGroup.year}
            </h2>

            {/* Month grids in a responsive grid */}
            <div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-4">
              <For each={yearGroup.months}>
                {(monthGroup) => (
                  <MonthGrid
                    year={yearGroup.year}
                    month={monthGroup.month}
                    notes={monthGroup.days}
                    today={todayParts()}
                    folderPath={props.folderPath}
                    onSelectPath={props.onSelectPath}
                  />
                )}
              </For>
            </div>
          </div>
        )}
      </For>
    </div>
  );
};

/**
 * A single month calendar grid with clickable days.
 */
const MonthGrid: Component<{
  year: number;
  month: number;
  notes: ParsedNote[];
  today: { year: number; month: number; day: number };
  folderPath: string;
  onSelectPath: (path: string) => void;
}> = (props) => {
  const monthName = createMemo(() => {
    const d = new Date(props.year, props.month - 1);
    return d.toLocaleDateString("en-US", { month: "long" });
  });

  const noteDays = createMemo(() => {
    const map = new Map<number, string>();
    for (const n of props.notes) {
      map.set(n.day, n.filename);
    }
    return map;
  });

  // Build 6-row calendar grid
  const calendarDays = createMemo(() => {
    const y = props.year;
    const m = props.month - 1; // JS 0-indexed
    const firstDay = new Date(y, m, 1);
    const startDow = (firstDay.getDay() + 6) % 7; // Monday=0
    const daysInMonth = new Date(y, m + 1, 0).getDate();

    const cells: (number | null)[] = [];
    for (let i = 0; i < startDow; i++) cells.push(null);
    for (let d = 1; d <= daysInMonth; d++) cells.push(d);
    while (cells.length % 7 !== 0) cells.push(null);
    return cells;
  });

  const isToday = (day: number) =>
    props.year === props.today.year &&
    props.month === props.today.month &&
    day === props.today.day;

  return (
    <div class="rounded-xl border border-stone-200 dark:border-stone-700 bg-white dark:bg-stone-900/50 p-4 transition-all">
      {/* Month name */}
      <h3
        data-testid="journal-month-header"
        class="text-sm font-bold text-stone-600 dark:text-stone-300 mb-3 text-center"
      >
        {monthName()}
      </h3>

      {/* Day-of-week headers */}
      <div class="grid grid-cols-7 gap-0 mb-1">
        <For each={DAYS}>
          {(d) => (
            <span class="text-center text-[10px] font-medium text-stone-400 dark:text-stone-500 leading-5">
              {d}
            </span>
          )}
        </For>
      </div>

      {/* Day cells */}
      <div class="grid grid-cols-7 gap-0.5">
        <For each={calendarDays()}>
          {(day) => {
            if (day === null) {
              return <span class="w-full aspect-square" />;
            }

            const filename = () => noteDays().get(day);
            const hasNote = () => !!filename();
            const today = () => isToday(day);

            return (
              <button
                data-testid={`calendar-day-${day}`}
                onClick={() => {
                  const f = filename();
                  if (f) {
                    const path = props.folderPath ? `${props.folderPath}/${f}` : f;
                    props.onSelectPath(path);
                  }
                }}
                disabled={!hasNote()}
                class={`
                  w-full aspect-square flex flex-col items-center justify-center rounded-md text-xs transition-all relative
                  ${today()
                    ? "bg-accent-100 dark:bg-accent-900/30 text-accent-600 dark:text-accent-400 font-bold ring-1 ring-accent-400"
                    : hasNote()
                      ? "text-stone-700 dark:text-stone-200 hover:bg-accent-50 dark:hover:bg-accent-900/20 cursor-pointer font-medium"
                      : "text-stone-300 dark:text-stone-600 cursor-default"
                  }
                `}
              >
                {day}
                {/* Note indicator dot */}
                {hasNote() && (
                  <span class="absolute bottom-0.5 w-1 h-1 rounded-full bg-accent-500" />
                )}
              </button>
            );
          }}
        </For>
      </div>
    </div>
  );
};
