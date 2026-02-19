import { Component, Show, createMemo, createEffect } from "solid-js";
import { useParams } from "@solidjs/router";
import { routeData } from "@/store";
import { sendQuery } from "@/sync/websocket";
import { AstRenderer } from "@/components/markdown";
import type { Pandoc } from "@/components/markdown";
import { ObsidianEditLink } from "@/components/ObsidianEditLink";

/**
 * Direct note view — used for wikilink navigation (/n/<path>).
 * Fetches and renders a single note's content.
 */
const NotesPage: Component = () => {
  const params = useParams<{ notePath?: string }>();

  const notePath = createMemo(() => {
    const path = params.notePath;
    return path ? decodeURIComponent(path) : null;
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

  return (
    <Show when={notePath()} fallback={
      <p class="text-stone-400 dark:text-stone-500">No note selected.</p>
    }>
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
              <a href="#/" class="text-sm text-stone-400 hover:text-accent-500 dark:text-stone-500 dark:hover:text-accent-400">
                ← Back to vault
              </a>
            </div>

            {/* Rendered markdown via AST */}
            <div data-testid="note-content">
              <AstRenderer ast={data().noteAst as Pandoc} />
            </div>
          </>
        )}
      </Show>
    </Show>
  );
};

export default NotesPage;
