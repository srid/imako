import { Component, createMemo, createEffect, Show, For, createSignal } from "solid-js";
import { sendQuery } from "@/sync/websocket";
import { notesData } from "@/store";
import { AstRenderer } from "@/components/markdown";
import type { Pandoc } from "@/components/markdown";
import type { FolderNode, Task } from "@/types";
import { ObsidianEditLink } from "@/components/ObsidianEditLink";
import { TaskItem } from "@/components/TaskItem";
import { buildTaskTree } from "@/utils/taskTree";
import { isTaskVisible, showTasks } from "@/state/filters";

interface JournalViewProps {
  /** The daily notes folder node */
  node: FolderNode;
  /** Path of the daily notes folder (e.g. "Daily") */
  folderPath: string;
  /** Today's date as ISO string, e.g. "2026-02-19" */
  today: string;
}

interface JournalEntry {
  /** ISO date string, e.g. "2026-02-19" */
  date: string;
  /** Filename, e.g. "2026-02-19.md" */
  filename: string;
  /** Full path, e.g. "Daily/2026-02-19.md" */
  path: string;
  /** Date object for formatting */
  dateObj: Date;
  /** Tasks from this file */
  tasks: Task[];
}

/**
 * Parse daily note filenames into sorted journal entries.
 * O(n log n) where n = number of files in the folder.
 */
function buildEntries(node: FolderNode, folderPath: string): JournalEntry[] {
  const entries: JournalEntry[] = [];
  for (const [filename, tasks] of Object.entries(node.files)) {
    const m = filename.match(/^(\d{4})-(\d{2})-(\d{2})\.md$/);
    if (!m) continue;
    const date = `${m[1]}-${m[2]}-${m[3]}`;
    entries.push({
      date,
      filename,
      path: folderPath ? `${folderPath}/${filename}` : filename,
      dateObj: new Date(+m[1], +m[2] - 1, +m[3]),
      tasks,
    });
  }
  // Sort reverse chronological
  entries.sort((a, b) => b.date.localeCompare(a.date));
  return entries;
}

/**
 * Format a date as a nice heading, e.g. "Wednesday, February 19, 2026"
 */
function formatDateHeading(d: Date): string {
  return d.toLocaleDateString("en-US", {
    weekday: "long",
    year: "numeric",
    month: "long",
    day: "numeric",
  });
}

export const JournalView: Component<JournalViewProps> = (props) => {
  const entries = createMemo(() => buildEntries(props.node, props.folderPath));

  // Group entries by month (for the month header) — O(n) single pass
  const entriesByMonth = createMemo(() => {
    const groups: { label: string; entries: JournalEntry[] }[] = [];
    let currentLabel = "";
    for (const entry of entries()) {
      const label = entry.dateObj.toLocaleDateString("en-US", {
        month: "long",
        year: "numeric",
      });
      if (label !== currentLabel) {
        currentLabel = label;
        groups.push({ label, entries: [] });
      }
      groups[groups.length - 1].entries.push(entry);
    }
    return groups;
  });

  // Cache of loaded note ASTs by path
  const [noteCache, setNoteCache] = createSignal<Record<string, Pandoc>>({});

  // Track which note we're currently loading
  const [loadingPath, setLoadingPath] = createSignal<string | null>(null);

  // When notesData updates, cache the result and load the next entry
  createEffect(() => {
    const data = notesData();
    if (!data) return;
    const path = data.notePath;
    const ast = data.noteAst as Pandoc;
    setNoteCache((prev) => ({ ...prev, [path]: ast }));

    // Load the next entry that hasn't been cached yet
    const all = entries();
    const cached = noteCache();
    const next = all.find((e) => !(e.path in cached) && e.path !== path);
    if (next) {
      setLoadingPath(next.path);
      sendQuery({ tag: "NotesQuery", contents: next.path });
    } else {
      setLoadingPath(null);
    }
  });

  // Kick off loading the first entry
  createEffect(() => {
    const all = entries();
    if (all.length > 0 && Object.keys(noteCache()).length === 0) {
      setLoadingPath(all[0].path);
      sendQuery({ tag: "NotesQuery", contents: all[0].path });
    }
  });

  return (
    <div data-testid="journal-view" class="space-y-6">
      <For each={entriesByMonth()}>
        {(group) => (
          <div>
            {/* Month header */}
            <h2
              data-testid="journal-month-header"
              class="text-lg font-bold text-stone-700 dark:text-stone-200 mb-4 flex items-center gap-2"
            >
              <span class="text-accent-500">📅</span>
              {group.label}
            </h2>

            {/* Journal entries */}
            <div class="space-y-4">
              <For each={group.entries}>
                {(entry) => (
                  <JournalEntryCard
                    entry={entry}
                    today={props.today}
                    noteAst={noteCache()[entry.path] ?? null}
                    isLoading={loadingPath() === entry.path}
                  />
                )}
              </For>
            </div>
          </div>
        )}
      </For>

      <Show when={entries().length === 0}>
        <div class="flex items-center justify-center h-32 text-stone-400 dark:text-stone-500">
          <p class="text-sm">No daily notes found</p>
        </div>
      </Show>
    </div>
  );
};

/**
 * Individual journal entry card — always visible, non-collapsible.
 */
const JournalEntryCard: Component<{
  entry: JournalEntry;
  today: string;
  noteAst: Pandoc | null;
  isLoading: boolean;
}> = (props) => {
  const isToday = () => props.entry.date === props.today;

  const visibleTasks = createMemo(() =>
    props.entry.tasks.filter((t) => isTaskVisible(t, props.today))
  );
  const tree = createMemo(() => buildTaskTree(visibleTasks()));

  return (
    <div
      data-testid="journal-entry"
      class="rounded-xl border border-stone-200 dark:border-stone-700 bg-white dark:bg-stone-900/50 overflow-hidden transition-all hover:border-accent-300 dark:hover:border-accent-600"
    >
      {/* Header */}
      <div class="px-5 py-4 flex items-center gap-3">
        {/* Date heading */}
        <span class="flex-1 min-w-0">
          <span
            data-testid="journal-entry-date"
            class="text-sm font-semibold text-stone-700 dark:text-stone-200"
          >
            {formatDateHeading(props.entry.dateObj)}
          </span>
        </span>

        {/* Today badge */}
        <Show when={isToday()}>
          <span
            data-testid="journal-today-badge"
            class="px-2 py-0.5 text-xs font-bold rounded-full bg-accent-500 text-white"
          >
            Today
          </span>
        </Show>

        {/* Edit link */}
        <ObsidianEditLink
          filePath={props.entry.path}
          class="text-stone-400 hover:text-accent-600 dark:hover:text-accent-400"
        />
      </div>

      {/* Content — always visible */}
      <div class="px-5 pb-5 border-t border-stone-100 dark:border-stone-800">
        {/* Tasks section */}
        <Show when={showTasks() && visibleTasks().length > 0}>
          <div data-testid="journal-entry-tasks" class="mt-3 mb-4">
            <h4 class="text-xs font-semibold text-stone-500 dark:text-stone-400 uppercase tracking-wide mb-2">
              Tasks
            </h4>
            <div class="flex flex-col">
              <For each={tree()}>
                {(node) => <TaskItem node={node} today={props.today} />}
              </For>
            </div>
          </div>
        </Show>

        {/* Note content */}
        <Show
          when={props.noteAst}
          fallback={
            <Show when={props.isLoading}>
              <p class="text-sm text-stone-400 dark:text-stone-500 animate-pulse mt-3">
                Loading note…
              </p>
            </Show>
          }
        >
          <div data-testid="journal-entry-content" class="mt-3">
            <AstRenderer ast={props.noteAst!} />
          </div>
        </Show>
      </div>
    </div>
  );
};
