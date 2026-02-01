import { Component, onMount, Show, createMemo } from "solid-js";
import { routeData } from "@/store";
import { sendQuery } from "@/sync/websocket";

const NotesPage: Component = () => {
  onMount(() => {
    // Request INBOX.md (configurable later)
    sendQuery({ tag: "NotesQuery", contents: "INBOX.md" });
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
            <h2 class="text-lg font-semibold text-stone-700 dark:text-stone-200">
              {data().notePath}
            </h2>
          </div>

          {/* Rendered markdown content */}
          <div
            class="prose prose-stone dark:prose-invert max-w-none"
            innerHTML={data().noteHtml}
          />
        </>
      )}
    </Show>
  );
};

export default NotesPage;
