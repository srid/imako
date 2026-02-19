import { Component, onMount, Show, createMemo, createEffect, For, on } from "solid-js";
import { routeData, vaultInfo, selectedPath, setSelectedPath } from "@/store";
import { sendQuery } from "@/sync/websocket";
import { FolderTree } from "@/components/FolderTree";
import { TaskItem } from "@/components/TaskItem";
import { ObsidianEditLink } from "@/components/ObsidianEditLink";
import { AstRenderer } from "@/components/markdown";
import type { Pandoc } from "@/components/markdown";
import type { FolderNode, Task, NotesData } from "@/types";
import { buildTaskTree } from "@/utils/taskTree";
import { isTaskVisible, showTasks } from "@/state/filters";
import { Icons } from "@/utils/icons";

/**
 * Extract a subtree from a FolderNode by path.
 * Path format: "/folder/subfolder" or "/folder/file.md"
 */
function getSubtree(
  node: FolderNode,
  path: string
): { type: "folder"; node: FolderNode; name: string } | { type: "file"; tasks: Task[]; name: string } | null {
  const parts = path.split("/").filter(Boolean);
  let current = node;

  for (let i = 0; i < parts.length; i++) {
    const part = parts[i];
    const isLast = i === parts.length - 1;

    // Check if it's a file at this level
    if (isLast && part in current.files) {
      return { type: "file", tasks: current.files[part], name: part };
    }

    // Check if it's a folder
    if (part in current.subfolders) {
      current = current.subfolders[part];
      if (isLast) {
        return { type: "folder", node: current, name: part };
      }
    } else {
      return null;
    }
  }

  return null;
}

/**
 * Collect all tasks from a FolderNode recursively, grouped by file path.
 */
function collectTasks(
  node: FolderNode,
  basePath: string
): { path: string; tasks: Task[] }[] {
  const result: { path: string; tasks: Task[] }[] = [];

  // Files at this level
  for (const [filename, tasks] of Object.entries(node.files)) {
    if (tasks.length > 0) {
      result.push({ path: `${basePath}/${filename}`, tasks });
    }
  }

  // Recurse into subfolders
  for (const [name, subfolder] of Object.entries(node.subfolders)) {
    result.push(...collectTasks(subfolder, `${basePath}/${name}`));
  }

  return result;
}

const VaultPage: Component = () => {
  // Request vault data on mount
  onMount(() => {
    sendQuery({ tag: "VaultQuery" });
  });

  const vaultData = createMemo(() => {
    const data = routeData();
    return data?.tag === "vault" ? data.data : null;
  });

  const notesData = createMemo(() => {
    const data = routeData();
    return data?.tag === "notes" ? data.data : null;
  });

  // When a file is selected, fetch its note content
  createEffect(
    on(selectedPath, (path) => {
      if (!path) return;
      const tree = vaultData();
      if (!tree) return;
      const target = getSubtree(tree.folderTree, path);
      if (target?.type === "file") {
        // Strip leading "/" for the backend path
        const notePath = path.startsWith("/") ? path.slice(1) : path;
        sendQuery({ tag: "NotesQuery", contents: notePath });
      }
    })
  );

  // Compute what to show in the detail pane
  const detailData = createMemo(() => {
    const path = selectedPath();
    const tree = vaultData();
    if (!tree) return null;

    if (!path) {
      // Root selected — show all tasks
      return {
        type: "folder" as const,
        name: vaultInfo.vaultName || "Vault",
        taskGroups: collectTasks(tree.folderTree, ""),
      };
    }

    const target = getSubtree(tree.folderTree, path);
    if (!target) return null;

    if (target.type === "folder") {
      return {
        type: "folder" as const,
        name: target.name,
        taskGroups: collectTasks(target.node, path),
      };
    }

    return {
      type: "file" as const,
      name: target.name,
      path: path,
      tasks: target.tasks,
    };
  });

  return (
    <div class="grid grid-cols-1 md:grid-cols-[minmax(200px,1fr)_2fr] gap-6 min-h-[calc(100vh-12rem)]">
      {/* Left pane: Folder tree */}
      <aside class="border-r border-stone-200 dark:border-stone-700 pr-4 overflow-y-auto md:max-h-[calc(100vh-12rem)]">
        <Show
          when={vaultData()}
          fallback={
            <p class="text-stone-400 dark:text-stone-500 text-sm py-4">
              Loading vault…
            </p>
          }
        >
          {(data) => (
            <>
              {/* Root node - click to deselect all */}
              <button
                onClick={() => setSelectedPath(null)}
                class={`w-full text-left py-2 mb-2 text-sm font-semibold transition-colors rounded-md ${
                  selectedPath() === null
                    ? "text-accent-600 dark:text-accent-400 bg-accent-50 dark:bg-accent-900/20 px-2 -mx-2"
                    : "text-stone-700 dark:text-stone-200 hover:text-accent-600 dark:hover:text-accent-400"
                }`}
              >
                <span class="flex items-center gap-2">
                  <span class="text-accent-500">{Icons.folder}</span>
                  {vaultInfo.vaultName || "Vault"}
                </span>
              </button>
              <FolderTree node={data().folderTree} />
            </>
          )}
        </Show>
      </aside>

      {/* Right pane: Detail */}
      <main class="overflow-y-auto md:max-h-[calc(100vh-12rem)]">
        <Show when={detailData()} fallback={
          <div class="flex items-center justify-center h-full text-stone-400 dark:text-stone-500">
            <p class="text-sm">Select a file or folder to view</p>
          </div>
        }>
          {(detail) => (
            <Show
              when={detail().type === "file"}
              fallback={
                /* Folder / Root: scoped task hierarchy */
                <FolderTaskView
                  name={(detail() as any).name}
                  taskGroups={(detail() as any).taskGroups}
                  today={vaultInfo.today}
                />
              }
            >
              {/* File: tasks + note content */}
              <FileDetailView
                name={(detail() as any).name}
                path={(detail() as any).path}
                tasks={(detail() as any).tasks}
                today={vaultInfo.today}
                notesData={notesData()}
              />
            </Show>
          )}
        </Show>
      </main>
    </div>
  );
};

/**
 * Folder task view: shows tasks grouped by files, same hierarchy as original Tasks page.
 */
const FolderTaskView: Component<{
  name: string;
  taskGroups: { path: string; tasks: Task[] }[];
  today: string;
}> = (props) => {
  return (
    <Show when={showTasks()}>
      <div data-testid="folder-task-view" class="space-y-4">
        <For each={props.taskGroups}>
          {(group) => {
            const visibleTasks = createMemo(() =>
              group.tasks.filter((t) => isTaskVisible(t, props.today))
            );
            const tree = createMemo(() => buildTaskTree(visibleTasks()));

            return (
              <Show when={visibleTasks().length > 0}>
                <div data-testid="file-tasks-group">
                  <details open>
                    <summary class="list-none cursor-pointer py-1.5 flex items-center gap-2 text-sm font-medium text-stone-700 dark:text-stone-200 hover:text-accent-600 dark:hover:text-accent-400 select-none">
                      <span class="text-stone-400 dark:text-stone-500">{Icons.file}</span>
                      <span class="truncate">{group.path}</span>
                      <span class="text-xs text-stone-400 dark:text-stone-500 ml-auto flex-shrink-0">
                        {visibleTasks().length}
                      </span>
                    </summary>
                    <div class="pl-6 flex flex-col">
                      <For each={tree()}>
                        {(node) => <TaskItem node={node} today={props.today} />}
                      </For>
                    </div>
                  </details>
                </div>
              </Show>
            );
          }}
        </For>
      </div>
    </Show>
  );
};

/**
 * File detail view: shows tasks + lazily-fetched note content.
 */
const FileDetailView: Component<{
  name: string;
  path: string;
  tasks: Task[];
  today: string;
  notesData: NotesData | null;
}> = (props) => {
  const filePath = () => {
    const p = props.path;
    return p.startsWith("/") ? p.slice(1) : p;
  };

  const visibleTasks = createMemo(() =>
    props.tasks.filter((t) => isTaskVisible(t, props.today))
  );
  const tree = createMemo(() => buildTaskTree(visibleTasks()));

  return (
    <div class="space-y-6">
      {/* File header */}
      <div class="flex items-center gap-3">
        <h2 class="text-lg font-semibold text-stone-700 dark:text-stone-200">
          {props.name}
        </h2>
        <ObsidianEditLink
          filePath={filePath()}
          class="text-stone-400 hover:text-accent-600 dark:hover:text-accent-400"
        />
      </div>

      {/* Tasks section */}
      <Show when={showTasks() && visibleTasks().length > 0}>
        <div data-testid="file-tasks-section">
          <h3 class="text-sm font-semibold text-stone-500 dark:text-stone-400 uppercase tracking-wide mb-3">
            Tasks
          </h3>
          <div class="flex flex-col">
            <For each={tree()}>
              {(node) => <TaskItem node={node} today={props.today} />}
            </For>
          </div>
        </div>
      </Show>

      {/* Note content (lazily fetched) */}
      <div data-testid="note-content">
        <h3 class="text-sm font-semibold text-stone-500 dark:text-stone-400 uppercase tracking-wide mb-3">
          Note
        </h3>
        <Show
          when={props.notesData && props.notesData.notePath === filePath()}
          fallback={
            <p class="text-sm text-stone-400 dark:text-stone-500">Loading note…</p>
          }
        >
          <AstRenderer ast={props.notesData!.noteAst as Pandoc} />
        </Show>
      </div>
    </div>
  );
};

export default VaultPage;
