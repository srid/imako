import { Component, createMemo, For } from "solid-js";
import type { FolderNode } from "@/types";
import { parseDateFilename, parseISODate, type ParsedDate } from "@/utils/calendarUtils";
import { MonthCalendar } from "@/components/MonthCalendar";

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
  month: number;
  day: number;
  filename: string;
}

/**
 * Group parsed notes by year → month (both descending).
 */
function groupByYearMonth(notes: ParsedNote[]): { year: number; months: { month: number; days: ParsedNote[] }[] }[] {
  const yearMap = new Map<number, Map<number, ParsedNote[]>>();
  for (const n of notes) {
    if (!yearMap.has(n.year)) yearMap.set(n.year, new Map());
    const monthMap = yearMap.get(n.year)!;
    if (!monthMap.has(n.month)) monthMap.set(n.month, []);
    monthMap.get(n.month)!.push(n);
  }

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
  const parsed = createMemo(() => {
    const notes: ParsedNote[] = [];
    for (const f of Object.keys(props.node.files)) {
      const p = parseDateFilename(f);
      if (p) notes.push({ ...p, filename: f });
    }
    return notes;
  });
  const grouped = createMemo(() => groupByYearMonth(parsed()));
  const todayParts = createMemo(() => parseISODate(props.today));

  const handleClickDay = (filename: string) => {
    const path = props.folderPath ? `${props.folderPath}/${filename}` : filename;
    props.onSelectPath(path);
  };

  return (
    <div data-testid="journal-view">
      <For each={grouped()}>
        {(yearGroup) => (
          <div class="mb-8">
            <h2 class="text-xl font-bold text-stone-700 dark:text-stone-200 mb-4 flex items-center gap-2">
              <span class="text-accent-500">📅</span>
              {yearGroup.year}
            </h2>

            <div class="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-4">
              <For each={yearGroup.months}>
                {(monthGroup) => {
                  const noteDays = createMemo(() => {
                    const map = new Map<number, string>();
                    for (const n of monthGroup.days) {
                      map.set(n.day, n.filename);
                    }
                    return map;
                  });

                  return (
                    <div class="rounded-xl border border-stone-200 dark:border-stone-700 bg-white dark:bg-stone-900/50 p-4 transition-all">
                      <MonthCalendar
                        year={yearGroup.year}
                        month={monthGroup.month}
                        noteDays={noteDays()}
                        today={todayParts()}
                        onClickDay={handleClickDay}
                        showHeader={true}
                      />
                    </div>
                  );
                }}
              </For>
            </div>
          </div>
        )}
      </For>
    </div>
  );
};
