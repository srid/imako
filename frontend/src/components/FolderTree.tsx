import { Component, For, Show, createMemo } from "solid-js";
import type { FolderNode as FolderNodeType, Task } from "@/types";
import { Icons } from "@/utils/icons";
import { isCollapsed, toggleCollapse, isTaskVisible } from "@/state/filters";
import { FileNode } from "@/components/FileNode";

/**
 * Helper to check if a folder node has any visible tasks (recursively)
 */
const folderHasVisibleTasks = (node: FolderNodeType, today: string): boolean => {
  // Check files in this folder
  for (const tasks of Object.values(node.files)) {
    if (tasks.some((t: Task) => isTaskVisible(t, today))) {
      return true;
    }
  }
  // Check subfolders recursively
  for (const subfolder of Object.values(node.subfolders)) {
    if (folderHasVisibleTasks(subfolder, today)) {
      return true;
    }
  }
  return false;
};

export const FolderTree: Component<{ node: FolderNodeType; path?: string; today: string }> = (props) => {
  // Computed folder data
  const data = createMemo(() => ({
    currentPath: props.path ?? "",
    folders: Object.entries(props.node.subfolders),
    files: Object.entries(props.node.files),
  }));

  return (
    <div class="flex flex-col gap-2">
      <For each={data().folders}>
        {([name, subnode]) => {
          const folderPath = () => `${data().currentPath}/${name}`;
          const nodeId = () => `folder:${folderPath()}`;
          return (
            <Show when={folderHasVisibleTasks(subnode, props.today)}>
              <details open={!isCollapsed(nodeId())} onToggle={(e) => {
                const isOpen = (e.target as HTMLDetailsElement).open;
                if (isOpen && isCollapsed(nodeId())) toggleCollapse(nodeId());
                else if (!isOpen && !isCollapsed(nodeId())) toggleCollapse(nodeId());
              }} class="group/folder">
                <summary class="list-none cursor-pointer -mx-2 px-3 py-2 rounded-lg bg-amber-100 dark:bg-stone-800 hover:bg-amber-200 dark:hover:bg-stone-700 flex items-center gap-2 text-sm font-semibold text-stone-700 dark:text-stone-200 select-none transition-colors">
                  <span class="w-4 h-4 flex items-center justify-center text-stone-400 dark:text-stone-500 transition-transform group-open/folder:rotate-90">
                    {Icons.chevronRight}
                  </span>
                  <span class="flex items-center gap-2">
                    <span class="text-amber-600 dark:text-amber-500">{Icons.folder}</span>
                    <span>{name}</span>
                  </span>
                </summary>
                <div class="pl-4 mt-2">
                  <FolderTree node={subnode} path={folderPath()} today={props.today} />
                </div>
              </details>
            </Show>
          );
        }}
      </For>
      <For each={data().files}>
        {([filename, tasks]) => <FileNode filename={filename} tasks={tasks} today={props.today} path={data().currentPath} />}
      </For>
    </div>
  );
};
