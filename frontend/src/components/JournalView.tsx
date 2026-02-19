import { Component, createMemo, For } from "solid-js";
import { parseISODate, type ParsedDate } from "@/utils/calendarUtils";
import { MonthCalendar } from "@/components/MonthCalendar";

interface JournalViewProps {
  /** Map of filename → ISO date string (from FolderNode.dailyNoteDates) */
  dailyNoteDates: Record<string, string>;
  /** Path of the daily notes folder (e.g. "Daily") */
  folderPath: string;
  /** Today's date as ISO string, e.g. "2026-02-19" */
  today: string;
  /** Navigate to a vault path */
  onSelectPath: (path: string) => void;
}

interface NoteEntry {
  filename: string;
  date: ParsedDate;
}

/**
 * Group note entries by year → month (both descending).
 */
function groupByYearMonth(entries: NoteEntry[]): { year: number; months: { month: number; entries: NoteEntry[] }[] }[] {
  const yearMap = new Map<number, Map<number, NoteEntry[]>>();
  for (const e of entries) {
    if (!yearMap.has(e.date.year)) yearMap.set(e.date.year, new Map());
    const monthMap = yearMap.get(e.date.year)!;
    if (!monthMap.has(e.date.month)) monthMap.set(e.date.month, []);
    monthMap.get(e.date.month)!.push(e);
  }

  const result: { year: number; months: { month: number; entries: NoteEntry[] }[] }[] = [];
  const sortedYears = [...yearMap.keys()].sort((a, b) => b - a);
  for (const year of sortedYears) {
    const monthMap = yearMap.get(year)!;
    const sortedMonths = [...monthMap.keys()].sort((a, b) => b - a);
    result.push({
      year,
      months: sortedMonths.map((month) => ({
        month,
        entries: monthMap.get(month)!,
      })),
    });
  }
  return result;
}

/**
 * Full calendar view for the daily notes folder.
 * Shows all months that have notes as mini calendar grids.
 * Uses structured data from the backend — zero parsing.
 */
export const JournalView: Component<JournalViewProps> = (props) => {
  const entries = createMemo(() => {
    const result: NoteEntry[] = [];
    for (const [filename, dateStr] of Object.entries(props.dailyNoteDates)) {
      result.push({ filename, date: parseISODate(dateStr) });
    }
    return result;
  });
  const grouped = createMemo(() => groupByYearMonth(entries()));
  const todayParts = createMemo(() => parseISODate(props.today));

  const handleClickDay = (vaultPath: string) => {
    props.onSelectPath(vaultPath);
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
                    for (const e of monthGroup.entries) {
                      const vaultPath = props.folderPath ? `${props.folderPath}/${e.filename}` : e.filename;
                      map.set(e.date.day, vaultPath);
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
