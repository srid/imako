import { Component, onMount, Show, createMemo } from "solid-js";
import { routeData, vaultInfo } from "@/store";
import { sendQuery } from "@/sync/websocket";
import { FilterBar } from "@/components/FilterBar";
import { FolderTree } from "@/components/FolderTree";

const TasksPage: Component = () => {
  onMount(() => {
    sendQuery("TasksQuery");
  });

  const tasksData = createMemo(() => {
    const data = routeData();
    return data?.tag === "tasks" ? data.data : null;
  });

  return (
    <Show when={tasksData()} fallback={<p class="text-stone-500 dark:text-stone-400">Loading tasks...</p>}>
      {(data) => (
        <>
          {/* Subheader: Date + Filters */}
          <div class="flex items-center justify-between gap-4 mb-6">
            <p class="text-sm text-stone-500 dark:text-stone-400">
              Today: <span class="font-medium text-stone-700 dark:text-stone-300">{vaultInfo.today}</span>
            </p>
            <FilterBar />
          </div>

          {/* Folder tree */}
          <FolderTree node={data().folderTree} today={vaultInfo.today} />
        </>
      )}
    </Show>
  );
};

export default TasksPage;
