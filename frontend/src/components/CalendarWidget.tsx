import { Component, createMemo, createSignal, For } from "solid-js";

interface CalendarWidgetProps {
  /** Set of filenames (without path) that exist as daily notes, e.g. "2026-02-19.md" */
  noteFiles: string[];
  /** ISO date string for today, e.g. "2026-02-19" */
  today: string;
  /** Called when user clicks a day that has a note */
  onSelectDate: (filename: string) => void;
  /** Currently selected filename (for highlight) */
  selectedFile?: string | null;
}

const DAYS = ["Mo", "Tu", "We", "Th", "Fr", "Sa", "Su"];

/**
 * Parse a date-like filename (e.g. "2026-02-19.md") into { year, month, day }.
 * Returns null if it doesn't match YYYY-MM-DD.md pattern.
 */
function parseDateFilename(f: string): { year: number; month: number; day: number } | null {
  const m = f.match(/^(\d{4})-(\d{2})-(\d{2})\.md$/);
  if (!m) return null;
  return { year: +m[1], month: +m[2], day: +m[3] };
}

export const CalendarWidget: Component<CalendarWidgetProps> = (props) => {
  // Current displayed month (default: today's month)
  const todayParsed = createMemo(() => {
    const [y, m, d] = props.today.split("-").map(Number);
    return { year: y, month: m, day: d };
  });

  const [viewYear, setViewYear] = createSignal(todayParsed().year);
  const [viewMonth, setViewMonth] = createSignal(todayParsed().month);

  // Set of days that have notes in the viewed month
  const noteDays = createMemo(() => {
    const s = new Set<number>();
    for (const f of props.noteFiles) {
      const p = parseDateFilename(f);
      if (p && p.year === viewYear() && p.month === viewMonth()) {
        s.add(p.day);
      }
    }
    return s;
  });

  const monthName = createMemo(() => {
    const d = new Date(viewYear(), viewMonth() - 1);
    return d.toLocaleDateString("en-US", { month: "long", year: "numeric" });
  });

  // Calendar grid: 6 weeks × 7 days
  const calendarDays = createMemo(() => {
    const y = viewYear();
    const m = viewMonth() - 1; // JS months are 0-indexed
    const firstDay = new Date(y, m, 1);
    // Monday-based: 0=Mon, 6=Sun
    const startDow = (firstDay.getDay() + 6) % 7;
    const daysInMonth = new Date(y, m + 1, 0).getDate();

    const cells: (number | null)[] = [];
    // Leading blanks
    for (let i = 0; i < startDow; i++) cells.push(null);
    // Days
    for (let d = 1; d <= daysInMonth; d++) cells.push(d);
    // Trailing blanks to fill last row
    while (cells.length % 7 !== 0) cells.push(null);

    return cells;
  });

  const prevMonth = () => {
    if (viewMonth() === 1) {
      setViewMonth(12);
      setViewYear(viewYear() - 1);
    } else {
      setViewMonth(viewMonth() - 1);
    }
  };

  const nextMonth = () => {
    if (viewMonth() === 12) {
      setViewMonth(1);
      setViewYear(viewYear() + 1);
    } else {
      setViewMonth(viewMonth() + 1);
    }
  };

  const filenameForDay = (day: number) => {
    const mm = String(viewMonth()).padStart(2, "0");
    const dd = String(day).padStart(2, "0");
    return `${viewYear()}-${mm}-${dd}.md`;
  };

  const isToday = (day: number) =>
    viewYear() === todayParsed().year &&
    viewMonth() === todayParsed().month &&
    day === todayParsed().day;

  const isSelected = (day: number) =>
    props.selectedFile === filenameForDay(day);

  return (
    <div data-testid="calendar-widget" class="select-none">
      {/* Month navigation */}
      <div class="flex items-center justify-between mb-2">
        <button
          data-testid="calendar-prev"
          onClick={prevMonth}
          class="w-6 h-6 flex items-center justify-center text-stone-400 hover:text-accent-500 dark:text-stone-500 dark:hover:text-accent-400 rounded transition-colors"
        >
          ‹
        </button>
        <span
          data-testid="calendar-month"
          class="text-xs font-semibold text-stone-600 dark:text-stone-300"
        >
          {monthName()}
        </span>
        <button
          data-testid="calendar-next"
          onClick={nextMonth}
          class="w-6 h-6 flex items-center justify-center text-stone-400 hover:text-accent-500 dark:text-stone-500 dark:hover:text-accent-400 rounded transition-colors"
        >
          ›
        </button>
      </div>

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
      <div class="grid grid-cols-7 gap-0">
        <For each={calendarDays()}>
          {(day) => {
            if (day === null) {
              return <span class="w-full aspect-square" />;
            }

            const hasNote = () => noteDays().has(day);
            const today = () => isToday(day);
            const selected = () => isSelected(day);

            return (
              <button
                data-testid={`calendar-day-${day}`}
                onClick={() => {
                  if (hasNote()) {
                    props.onSelectDate(filenameForDay(day));
                  }
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
                {/* Note indicator dot */}
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
