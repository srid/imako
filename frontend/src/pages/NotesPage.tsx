import { Component, onMount, Show, createMemo, createEffect, For } from "solid-js";
import { useParams, A } from "@solidjs/router";
import { routeData, vaultInfo } from "@/store";
import { sendQuery } from "@/sync/websocket";
import { AstRenderer } from "@/components/markdown";
import type { Pandoc } from "@/components/markdown";
import { ObsidianEditLink } from "@/components/ObsidianEditLink";

const NotesPage: Component = () => {
  const params = useParams<{ notePath?: string }>();

  // Get note path from route, decode it
  const notePath = createMemo(() => {
    const path = params.notePath;
    return path ? decodeURIComponent(path) : null;
  });

  // On mount, if no path, send a query to populate vaultInfo
  // (any query works since vaultInfo is included in every response)
  onMount(() => {
    if (!notePath()) {
      sendQuery({ tag: "TasksQuery" });
    }
  });

  // Request note when path changes
  createEffect(() => {
    const path = notePath();
    if (path) {
      sendQuery({ tag: "NotesQuery", contents: path });
    }
  });

  const notesData = createMemo(() => {
    const data = routeData();
    return data?.tag === "notes" ? data.data : null;
  });

  // Recent notes sorted by mtime
  const recentNotes = createMemo(() => {
    const notes = vaultInfo.notes;
    return Object.entries(notes)
      .sort(([, a], [, b]) => new Date(b).getTime() - new Date(a).getTime())
      .slice(0, 20);
  });

  return (
    <>
      <Show when={notePath()} fallback={
        /* Recent notes list */
        <div>
          <div class="flex items-center justify-between gap-4 mb-6">
            <h2 class="text-lg font-semibold text-stone-700 dark:text-stone-200">
              Recent Notes
            </h2>
            <span class="text-sm text-stone-400 dark:text-stone-500">
              Press <kbd class="px-1.5 py-0.5 bg-stone-100 dark:bg-stone-800 rounded text-xs">Ctrl+P</kbd> to search
            </span>
          </div>

          <div class="space-y-1">
            <For each={recentNotes()}>
              {([path, mtime]) => (
                <A
                  href={`/n/${encodeURIComponent(path)}`}
                  class="flex items-center justify-between px-3 py-2 rounded-lg hover:bg-stone-100 dark:hover:bg-stone-800 transition-colors group"
                >
                  <span class="font-medium text-stone-700 dark:text-stone-300 group-hover:text-amber-600 dark:group-hover:text-amber-400">
                    {path}
                  </span>
                  <span class="text-xs text-stone-400 dark:text-stone-500">
                    {new Date(mtime).toLocaleDateString()}
                  </span>
                </A>
              )}
            </For>
          </div>
        </div>
      }>
        {/* Note content view */}
        <Show when={notesData()} fallback={<p class="text-stone-500 dark:text-stone-400">Loading note...</p>}>
          {(data) => (
            <>
              {/* Subheader */}
              <div class="flex items-center justify-between gap-4 mb-6">
                <div class="flex items-center gap-2">
                  <h2 class="text-lg font-semibold text-stone-700 dark:text-stone-200">
                    {data().notePath}
                  </h2>
                  <ObsidianEditLink
                    filePath={data().notePath}
                    class="text-stone-400 hover:text-accent-600 dark:hover:text-accent-400"
                  />
                </div>
                <A href="/n" class="text-sm text-stone-400 hover:text-amber-500 dark:text-stone-500 dark:hover:text-amber-400">
                  ← Back
                </A>
              </div>

              {/* Rendered markdown via AST */}
              <div data-testid="note-content">
                <AstRenderer ast={data().noteAst as Pandoc} />
              </div>
            </>
          )}
        </Show>
      </Show>
    </>
  );
};

export default NotesPage;
