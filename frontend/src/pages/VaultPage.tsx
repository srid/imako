import { Component, Show, createMemo, createEffect, For, onMount, onCleanup } from "solid-js";
import { useParams, useNavigate } from "@solidjs/router";
import { vaultData, notesData, vaultInfo } from "@/store";
import { sendQuery } from "@/sync/websocket";
import { FolderTree } from "@/components/FolderTree";
import { TaskItem } from "@/components/TaskItem";
import { ObsidianEditLink } from "@/components/ObsidianEditLink";
import { AstRenderer } from "@/components/markdown";
import type { Pandoc } from "@/components/markdown";
import type { FolderNode, Task, NotesData } from "@/types";
import { buildTaskTree } from "@/utils/taskTree";
import { isTaskVisible, showTasks, treeFilter, setTreeFilter } from "@/state/filters";
import { Icons } from "@/utils/icons";
import { JournalView } from "@/components/JournalView";

/**
 * Extract a subtree from a FolderNode by path.
 * Path format: "Notes/Tasks.md" or "Notes"
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
      const p = basePath ? `${basePath}/${filename}` : filename;
      result.push({ path: p, tasks });
    }
  }

  // Recurse into subfolders
  for (const [name, subfolder] of Object.entries(node.subfolders)) {
    const p = basePath ? `${basePath}/${name}` : name;
    result.push(...collectTasks(subfolder, p));
  }

  return result;
}

const VaultPage: Component = () => {
  const params = useParams<{ vaultPath?: string }>();
  const navigate = useNavigate();

  // Derive selectedPath from route params (null = root)
  const selectedPath = createMemo(() => {
    const raw = params.vaultPath;
    if (!raw) return null;
    return decodeURIComponent(raw);
  });

  // VaultQuery is sent automatically on ws.onopen (see websocket.ts).
  // The server accumulates all queries and pushes responses for each
  // on model change, so sending NotesQuery here won't overwrite VaultQuery.
  createEffect(() => {
    const path = selectedPath();
    if (!path) return;
    const tree = vaultData();
    if (!tree) return;
    const target = getSubtree(tree.folderTree, path);
    if (target?.type === "file") {
      sendQuery({ tag: "NotesQuery", contents: path });
    }
  });

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
        node: tree.folderTree,
        basePath: "",
        taskGroups: collectTasks(tree.folderTree, ""),
      };
    }

    const target = getSubtree(tree.folderTree, path);
    if (!target) {
      // Selected file/folder no longer exists (e.g. deleted from disk)
      // — redirect to root so the user isn't left on a stale URL
      navigate("/", { replace: true });
      return null;
    }

    if (target.type === "folder") {
      // Check if this is the daily notes folder
      const dnf = vaultInfo.dailyNotesFolder;
      if (dnf != null && path === dnf) {
        return {
          type: "dailyNotes" as const,
          name: target.name,
          dailyNoteDates: target.node.dailyNoteDates ?? {},
          folderPath: path,
        };
      }

      return {
        type: "folder" as const,
        name: target.name,
        node: target.node,
        basePath: path,
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

  /** Navigate to a vault path */
  const selectPath = (path: string | null) => {
    if (path) {
      navigate(`/p/${encodeURIComponent(path)}`);
    } else {
      navigate("/");
    }
  };

  return (
    <div class="grid grid-cols-1 md:grid-cols-[minmax(200px,1fr)_2fr] gap-6 min-h-[calc(100vh-12rem)]">
      {/* Left pane: Folder tree */}
      <aside class="border-r border-stone-200 dark:border-stone-700 pr-4 overflow-y-auto md:max-h-[calc(100vh-12rem)]">
        {/* Filter input — "/" to focus, Escape to clear */}
        {(() => {
          let filterRef!: HTMLInputElement;
          onMount(() => {
            const handler = (e: KeyboardEvent) => {
              if (e.key === "/" && document.activeElement?.tagName !== "INPUT") {
                e.preventDefault();
                filterRef.focus();
              }
            };
            document.addEventListener("keydown", handler);
            onCleanup(() => document.removeEventListener("keydown", handler));
          });
          return (
            <div class="relative mb-3">
              <input
                ref={filterRef}
                data-testid="tree-filter"
                type="text"
                placeholder='Filter files… (press "/")'
                value={treeFilter()}
                onInput={(e) => setTreeFilter(e.currentTarget.value)}
                onKeyDown={(e) => {
                  if (e.key === "Escape") {
                    setTreeFilter("");
                    filterRef.blur();
                  }
                }}
                class="w-full px-3 py-1.5 text-sm bg-stone-100 dark:bg-stone-800 text-stone-700 dark:text-stone-300 rounded-lg border border-stone-200 dark:border-stone-700 outline-none focus:ring-2 focus:ring-accent-500 focus:border-transparent placeholder:text-stone-400 dark:placeholder:text-stone-500"
              />
              <Show when={treeFilter()}>
                <button
                  onClick={() => setTreeFilter("")}
                  class="absolute right-2 top-1/2 -translate-y-1/2 text-stone-400 hover:text-stone-600 dark:hover:text-stone-300 text-xs"
                >
                  ✕
                </button>
              </Show>
            </div>
          );
        })()}
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
                onClick={() => selectPath(null)}
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
              <FolderTree node={data().folderTree} onSelect={selectPath} selectedPath={selectedPath()} />
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
                <Show
                  when={detail().type === "dailyNotes"}
                  fallback={
                    /* Folder / Root: scoped task hierarchy + contents listing */
                    <FolderTaskView
                      name={(detail() as any).name}
                      node={(detail() as any).node}
                      basePath={(detail() as any).basePath}
                      taskGroups={(detail() as any).taskGroups}
                      today={vaultInfo.today}
                      onSelect={selectPath}
                    />
                  }
                >
                  {/* Daily notes folder: calendar view */}
                  <JournalView
                    dailyNoteDates={(detail() as any).dailyNoteDates}
                    folderPath={(detail() as any).folderPath}
                    today={vaultInfo.today}
                    onSelectPath={selectPath}
                  />
                </Show>
              }
            >
              {/* File: tasks + note content */}
              <FileDetailView
                name={(detail() as any).name}
                path={(detail() as any).path}
                tasks={(detail() as any).tasks}
                today={vaultInfo.today}
                notesData={notesData()}
                onSelectPath={selectPath}
              />
            </Show>
          )}
        </Show>
      </main>
    </div>
  );
};

/**
 * Folder task view: shows folder contents listing + tasks grouped by files.
 */
const FolderTaskView: Component<{
  name: string;
  node: FolderNode;
  basePath: string;
  taskGroups: { path: string; tasks: Task[] }[];
  today: string;
  onSelect: (path: string | null) => void;
}> = (props) => {
  const subfolders = createMemo(() => Object.keys(props.node.subfolders).sort());
  const files = createMemo(() => Object.keys(props.node.files).sort());
  const hasChildren = createMemo(() => subfolders().length > 0 || files().length > 0);

  const childPath = (name: string) =>
    props.basePath ? `${props.basePath}/${name}` : name;

  return (
    <div class="space-y-6">
      {/* Folder contents listing */}
      <Show when={hasChildren()}>
        <div data-testid="folder-contents">
          <h3 class="text-sm font-semibold text-stone-500 dark:text-stone-400 uppercase tracking-wide mb-3">
            Contents
          </h3>
          <div class="grid gap-1">
            <For each={subfolders()}>
              {(name) => (
                <button
                  data-testid="folder-contents-folder"
                  onClick={() => props.onSelect(childPath(name))}
                  class="w-full text-left py-2 px-3 flex items-center gap-2.5 text-sm rounded-lg transition-colors hover:bg-accent-50 dark:hover:bg-accent-900/20 text-stone-700 dark:text-stone-200 hover:text-accent-600 dark:hover:text-accent-400"
                >
                  <span class="text-accent-500">{Icons.folder}</span>
                  <span class="truncate">{name}</span>
                </button>
              )}
            </For>
            <For each={files()}>
              {(name) => (
                <button
                  data-testid="folder-contents-file"
                  onClick={() => props.onSelect(childPath(name))}
                  class="w-full text-left py-2 px-3 flex items-center gap-2.5 text-sm rounded-lg transition-colors hover:bg-stone-100 dark:hover:bg-stone-800 text-stone-600 dark:text-stone-300 hover:text-accent-600 dark:hover:text-accent-400"
                >
                  <span class="text-stone-400 dark:text-stone-500">{Icons.file}</span>
                  <span class="truncate">{name}</span>
                </button>
              )}
            </For>
          </div>
        </div>
      </Show>

      {/* Tasks section */}
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
    </div>
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
  onSelectPath: (path: string) => void;
}> = (props) => {
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
          filePath={props.path}
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
      <div>
        <h3 class="text-sm font-semibold text-stone-500 dark:text-stone-400 uppercase tracking-wide mb-3">
          Note
        </h3>
        <Show
          when={props.notesData && props.notesData.notePath === props.path}
          fallback={
            <p class="text-sm text-stone-400 dark:text-stone-500">Loading note…</p>
          }
        >
          <div data-testid="note-content">
            <AstRenderer ast={props.notesData!.noteAst as Pandoc} />
          </div>
        </Show>
      </div>

      {/* Backlinks */}
      <Show when={props.notesData && props.notesData.notePath === props.path && props.notesData!.backlinks.length > 0}>
        <div data-testid="backlinks-section" class="border-t border-stone-200 dark:border-stone-700 pt-4">
          <h3 class="text-sm font-semibold text-stone-500 dark:text-stone-400 uppercase tracking-wide mb-3">
            Backlinks
          </h3>
          <div class="grid gap-1">
            <For each={props.notesData!.backlinks}>
              {(path) => (
                <button
                  data-testid="backlink-item"
                  onClick={() => props.onSelectPath(path)}
                  class="w-full text-left py-2 px-3 flex items-center gap-2.5 text-sm rounded-lg transition-colors hover:bg-accent-50 dark:hover:bg-accent-900/20 text-stone-600 dark:text-stone-300 hover:text-accent-600 dark:hover:text-accent-400"
                >
                  <span class="text-stone-400 dark:text-stone-500">{Icons.file}</span>
                  <span class="truncate">{path}</span>
                </button>
              )}
            </For>
          </div>
        </div>
      </Show>
    </div>
  );
};

export default VaultPage;
