import { Component, For, Show, createMemo } from "solid-js";
import type { FolderNode as FolderNodeType, Task } from "@/types";
import { Icons } from "@/utils/icons";
import { isCollapsed, toggleCollapse, showTasks, treeFilter } from "@/state/filters";

/**
 * Check if a folder/file name matches the tree filter (case-insensitive).
 */
const matchesFilter = (name: string, filter: string): boolean => {
  if (!filter) return true;
  return name.toLowerCase().includes(filter.toLowerCase());
};

/**
 * Check if a folder node has any matching children (recursively).
 */
const folderMatchesFilter = (node: FolderNodeType, filter: string): boolean => {
  if (!filter) return true;
  for (const filename of Object.keys(node.files)) {
    if (matchesFilter(filename, filter)) return true;
  }
  for (const [name, subfolder] of Object.entries(node.subfolders)) {
    if (matchesFilter(name, filter) || folderMatchesFilter(subfolder, filter)) return true;
  }
  return false;
};

/**
 * Check if a file has tasks.
 */
const hasTasks = (tasks: Task[]): boolean => tasks.length > 0;

interface FolderTreeProps {
  node: FolderNodeType;
  path?: string;
  onSelect: (path: string | null) => void;
  selectedPath: string | null;
}

export const FolderTree: Component<FolderTreeProps> = (props) => {
  const data = createMemo(() => ({
    currentPath: props.path ?? "",
    folders: Object.entries(props.node.subfolders),
    files: Object.entries(props.node.files),
  }));

  const filter = () => treeFilter();

  return (
    <div data-testid="folder-tree" class="flex flex-col gap-0.5">
      <For each={data().folders}>
        {([name, subnode]) => {
          const folderPath = () => {
            const base = data().currentPath;
            return base ? `${base}/${name}` : name;
          };
          const nodeId = () => `folder:${folderPath()}`;
          const isSelected = () => props.selectedPath === folderPath();

          return (
            <Show when={folderMatchesFilter(subnode, filter())}>
              <details
                data-testid="folder-node"
                open={!isCollapsed(nodeId())}
                onToggle={(e) => {
                  const isOpen = (e.target as HTMLDetailsElement).open;
                  if (isOpen && isCollapsed(nodeId())) toggleCollapse(nodeId());
                  else if (!isOpen && !isCollapsed(nodeId())) toggleCollapse(nodeId());
                }}
                class="group/folder"
              >
                <summary
                  class={`list-none cursor-pointer py-1.5 flex items-center gap-2 text-sm font-semibold select-none transition-colors ${
                    isSelected()
                      ? "text-accent-600 dark:text-accent-400 bg-accent-50 dark:bg-accent-900/20 rounded-md px-2 -mx-2"
                      : "text-stone-700 dark:text-stone-200 hover:text-accent-600 dark:hover:text-accent-400"
                  }`}
                  onClick={(e) => {
                    e.preventDefault();
                    e.stopPropagation();
                    props.onSelect(folderPath());
                    // Toggle open/close manually
                    const details = e.currentTarget.parentElement as HTMLDetailsElement;
                    details.open = !details.open;
                  }}
                >
                  <span class="w-4 h-4 flex items-center justify-center text-stone-400 dark:text-stone-500 transition-transform group-open/folder:rotate-90">
                    {Icons.chevronRight}
                  </span>
                  <span class="flex items-center gap-2">
                    <span class="text-accent-500">{Icons.folder}</span>
                    <span>{name}</span>
                  </span>
                </summary>
                <div class="pl-6 mt-0.5">
                  <FolderTree node={subnode} path={folderPath()} onSelect={props.onSelect} selectedPath={props.selectedPath} />
                </div>
              </details>
            </Show>
          );
        }}
      </For>
      <For each={data().files}>
        {([filename, tasks]) => {
          const isVisible = () => matchesFilter(filename, filter());
          const filePath = () => {
            const base = data().currentPath;
            return base ? `${base}/${filename}` : filename;
          };
          const isSelected = () => props.selectedPath === filePath();

          return (
            <Show when={isVisible()}>
              <button
                data-testid="file-node"
                onClick={() => props.onSelect(filePath())}
                class={`w-full text-left py-1.5 flex items-center gap-2 text-sm transition-colors rounded-md ${
                  isSelected()
                    ? "text-accent-600 dark:text-accent-400 bg-accent-50 dark:bg-accent-900/20 font-medium px-2 -mx-2"
                    : "text-stone-600 dark:text-stone-300 hover:text-accent-600 dark:hover:text-accent-400"
                }`}
              >
                <span class="w-4 h-4" /> {/* spacer to align with folder chevrons */}
                <span class="flex items-center gap-2 flex-1 min-w-0">
                  <span class="text-stone-400 dark:text-stone-500">{Icons.file}</span>
                  <span class="truncate">{filename}</span>

                  {/* Task indicator dot */}
                  <Show when={showTasks() && hasTasks(tasks)}>
                    <span
                      class="w-1.5 h-1.5 rounded-full bg-accent-500 flex-shrink-0"
                      title="Has tasks"
                    />
                  </Show>
                </span>
              </button>
            </Show>
          );
        }}
      </For>
    </div>
  );
};
