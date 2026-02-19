import { Component, createMemo, For } from "solid-js";
import { DAYS, buildCalendarGrid, isDayToday, type ParsedDate } from "@/utils/calendarUtils";

interface MonthCalendarProps {
  year: number;
  /** 1-indexed month */
  month: number;
  /** Map of day number → filename for days that have notes */
  noteDays: Map<number, string>;
  /** Today's parsed date */
  today: ParsedDate;
  /** Called when a day with a note is clicked; receives the filename */
  onClickDay: (filename: string) => void;
  /** Currently selected filename (for highlight), sidebar only */
  selectedFile?: string | null;
  /** Whether to show the month name header (used in main panel grids) */
  showHeader?: boolean;
}

/**
 * Reusable month calendar grid.
 * Used by CalendarWidget (sidebar, single month with navigation)
 * and JournalView (main panel, multiple months).
 */
export const MonthCalendar: Component<MonthCalendarProps> = (props) => {
  const grid = createMemo(() => buildCalendarGrid(props.year, props.month));

  const monthName = createMemo(() => {
    const d = new Date(props.year, props.month - 1);
    return d.toLocaleDateString("en-US", { month: "long" });
  });

  return (
    <div>
      {/* Optional month name header */}
      {props.showHeader && (
        <h3
          data-testid="journal-month-header"
          class="text-sm font-bold text-stone-600 dark:text-stone-300 mb-3 text-center"
        >
          {monthName()}
        </h3>
      )}

      {/* Day-of-week headers */}
      <div class="grid grid-cols-7 gap-0 mb-1">
        <For each={[...DAYS]}>
          {(d) => (
            <span class="text-center text-[10px] font-medium text-stone-400 dark:text-stone-500 leading-5">
              {d}
            </span>
          )}
        </For>
      </div>

      {/* Day cells */}
      <div class="grid grid-cols-7 gap-0.5">
        <For each={grid()}>
          {(day) => {
            if (day === null) {
              return <span class="w-full aspect-square" />;
            }

            const filename = () => props.noteDays.get(day);
            const hasNote = () => !!filename();
            const today = () => isDayToday(props.year, props.month, day, props.today);
            const selected = () => props.selectedFile != null && props.selectedFile === filename();

            return (
              <button
                data-testid={`calendar-day-${day}`}
                onClick={() => {
                  const f = filename();
                  if (f) props.onClickDay(f);
                }}
                disabled={!hasNote()}
                class={`
                  w-full aspect-square flex flex-col items-center justify-center rounded-md text-xs transition-all relative
                  ${selected()
                    ? "bg-accent-500 text-white font-bold"
                    : today()
                      ? "bg-accent-100 dark:bg-accent-900/30 text-accent-600 dark:text-accent-400 font-bold ring-1 ring-accent-400"
                      : hasNote()
                        ? "text-stone-700 dark:text-stone-200 hover:bg-accent-50 dark:hover:bg-accent-900/20 cursor-pointer font-medium"
                        : "text-stone-300 dark:text-stone-600 cursor-default"
                  }
                `}
              >
                {day}
                {hasNote() && !selected() && (
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
