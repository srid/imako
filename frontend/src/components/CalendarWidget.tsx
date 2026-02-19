import { Component, createMemo, createSignal } from "solid-js";
import { parseDateFilename, parseISODate, dateToFilename } from "@/utils/calendarUtils";
import { MonthCalendar } from "@/components/MonthCalendar";

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

/**
 * Sidebar calendar widget with month navigation.
 * Thin wrapper around MonthCalendar that adds prev/next month controls.
 */
export const CalendarWidget: Component<CalendarWidgetProps> = (props) => {
  const todayParsed = createMemo(() => parseISODate(props.today));

  const [viewYear, setViewYear] = createSignal(todayParsed().year);
  const [viewMonth, setViewMonth] = createSignal(todayParsed().month);

  // Build map of day → filename for the currently viewed month
  const noteDays = createMemo(() => {
    const map = new Map<number, string>();
    for (const f of props.noteFiles) {
      const p = parseDateFilename(f);
      if (p && p.year === viewYear() && p.month === viewMonth()) {
        map.set(p.day, f);
      }
    }
    return map;
  });

  const monthName = createMemo(() => {
    const d = new Date(viewYear(), viewMonth() - 1);
    return d.toLocaleDateString("en-US", { month: "long", year: "numeric" });
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

      {/* Calendar grid */}
      <MonthCalendar
        year={viewYear()}
        month={viewMonth()}
        noteDays={noteDays()}
        today={todayParsed()}
        onClickDay={props.onSelectDate}
        selectedFile={props.selectedFile}
      />
    </div>
  );
};
