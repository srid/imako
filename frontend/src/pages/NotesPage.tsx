import { Component, onMount, Show, createMemo } from "solid-js";
import { routeData } from "@/store";
import { sendQuery } from "@/sync/websocket";

const NotesPage: Component = () => {
  onMount(() => {
    sendQuery("NotesQuery");
  });

  const notesData = createMemo(() => {
    const data = routeData();
    return data?.tag === "notes" ? data.data : null;
  });

  return (
    <Show when={notesData()} fallback={<p class="text-stone-500 dark:text-stone-400">Loading notes...</p>}>
      {(data) => (
        <>
          {/* Subheader */}
          <div class="flex items-center justify-between gap-4 mb-6">
            <h2 class="text-lg font-semibold text-stone-700 dark:text-stone-200">Notes</h2>
            <span class="text-sm text-stone-400 dark:text-stone-500">
              {data().noteCount} notes in vault
            </span>
          </div>

          {/* Content placeholder - will render INBOX.md in Phase 2 */}
          <p class="text-stone-500 dark:text-stone-400">
            Note rendering coming in Phase 2...
          </p>
        </>
      )}
    </Show>
  );
};

export default NotesPage;
