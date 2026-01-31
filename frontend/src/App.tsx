import { Component, onMount, For, Show, createSignal, createEffect } from "solid-js";
import { vault, connectVault, fetchVault } from "./store";
import type { FolderNode, Task, Filter } from "./types";

// Icon components using Unicode/Emoji (can be replaced with proper SVG icons)
const Icons = {
  folder: "📁",
  file: "📄",
  chevronRight: "›",
  checkSquare: "☑",
  cancelSquare: "☒",
  halfSquare: "◐",
  emptySquare: "☐",
  calendar: "📅",
  calendarStart: "▶",
  repeat: "🔁",
  tag: "🏷️",
};

// LocalStorage keys
const STORAGE_KEYS = {
  filters: "imako-filters",
  collapsed: "imako-collapsed",
};

// Filter state - tracks which filters are active (persisted)
const loadFilters = (): Set<string> => {
  try {
    const saved = localStorage.getItem(STORAGE_KEYS.filters);
    return saved ? new Set(JSON.parse(saved)) : new Set();
  } catch {
    return new Set();
  }
};

const [activeFilters, setActiveFilters] = createSignal<Set<string>>(loadFilters());

// Persist filters whenever they change
createEffect(() => {
  localStorage.setItem(STORAGE_KEYS.filters, JSON.stringify([...activeFilters()]));
});

const toggleFilter = (filterId: string) => {
  setActiveFilters((prev) => {
    const next = new Set(prev);
    if (next.has(filterId)) {
      next.delete(filterId);
    } else {
      next.add(filterId);
    }
    return next;
  });
};

// Collapse state - tracks which nodes are collapsed (persisted)
const loadCollapsed = (): Set<string> => {
  try {
    const saved = localStorage.getItem(STORAGE_KEYS.collapsed);
    return saved ? new Set(JSON.parse(saved)) : new Set();
  } catch {
    return new Set();
  }
};

const [collapsedNodes, setCollapsedNodes] = createSignal<Set<string>>(loadCollapsed());

// Persist collapsed state whenever it changes
createEffect(() => {
  localStorage.setItem(STORAGE_KEYS.collapsed, JSON.stringify([...collapsedNodes()]));
});

const isCollapsed = (nodeId: string): boolean => collapsedNodes().has(nodeId);

const toggleCollapse = (nodeId: string) => {
  setCollapsedNodes((prev) => {
    const next = new Set(prev);
    if (next.has(nodeId)) {
      next.delete(nodeId);
    } else {
      next.add(nodeId);
    }
    return next;
  });
};

// Check if a task should be hidden based on active filters
const isTaskVisible = (task: Task, today: string): boolean => {
  const isFuture = task.startDate && task.startDate > today;
  const isPast = task.status === "Completed" || task.status === "Cancelled";

  // If no filters are active, show non-future and non-past tasks
  const showFuture = activeFilters().has("showFuture");
  const showPast = activeFilters().has("showPast");

  if (isFuture && !showFuture) return false;
  if (isPast && !showPast) return false;
  return true;
};


const FilterBar: Component<{ filters: Filter[] }> = (props) => {
  return (
    <div class="mb-4 flex items-center gap-2">
      <For each={props.filters}>
        {(filter) => (
          <button
            onClick={() => toggleFilter(filter.filterId)}
            class={`px-3 py-1 text-xs font-medium rounded-full transition-colors ${
              activeFilters().has(filter.filterId)
                ? "bg-indigo-600 text-white hover:bg-indigo-700"
                : "bg-gray-100 dark:bg-gray-800 text-gray-500 dark:text-gray-400 hover:bg-gray-200 dark:hover:bg-gray-700"
            }`}
            aria-pressed={activeFilters().has(filter.filterId)}
          >
            {filter.filterLabel}
          </button>
        )}
      </For>
    </div>
  );
};

const TaskItem: Component<{ task: Task; today: string }> = (props) => {
  const statusIcon = () => {
    switch (props.task.status) {
      case "Completed":
        return Icons.checkSquare;
      case "Cancelled":
        return Icons.cancelSquare;
      case "InProgress":
        return Icons.halfSquare;
      default:
        return Icons.emptySquare;
    }
  };

  const statusColor = () => {
    switch (props.task.status) {
      case "Completed":
      case "Cancelled":
        return "text-gray-400";
      case "InProgress":
        return "text-amber-500";
      default:
        return "text-gray-400 hover:text-gray-600";
    }
  };

  const textStyle = () => {
    if (props.task.status === "Completed" || props.task.status === "Cancelled") {
      return "line-through text-gray-400 dark:text-gray-500";
    }
    return "text-gray-900 dark:text-gray-100";
  };

  const priorityStyle = () => {
    switch (props.task.priority) {
      case "Highest":
        return "text-red-600 dark:text-red-400";
      case "High":
        return "text-orange-600 dark:text-orange-400";
      case "Medium":
        return "text-amber-600 dark:text-amber-400";
      case "Low":
        return "text-blue-600 dark:text-blue-400";
      case "Lowest":
        return "text-slate-500 dark:text-slate-400";
      default:
        return "";
    }
  };

  const dateColor = (date: string) => {
    if (date < props.today) return "text-red-600 dark:text-red-400 font-medium"; // Overdue
    if (date === props.today) return "text-amber-600 dark:text-amber-400 font-medium"; // Today
    return "text-gray-500 dark:text-gray-400"; // Future
  };

  const formatDate = (dateStr: string) => {
    const date = new Date(dateStr);
    return date.toLocaleDateString("en-US", { month: "short", day: "numeric" });
  };

  const visible = () => isTaskVisible(props.task, props.today);

  return (
    <Show when={visible()}>
      <div class="group/task relative py-1 -mx-2 px-2 rounded hover:bg-gray-50 dark:hover:bg-gray-800/50 flex items-start gap-2 text-sm transition-colors">
        {/* Checkbox */}
        <span class={`w-5 h-5 flex-shrink-0 flex items-center justify-center ${statusColor()}`}>
          {statusIcon()}
        </span>

        {/* Content */}
        <div class="flex-1 min-w-0">
          <div class={`leading-snug ${textStyle()}`}>{props.task.description}</div>

          {/* Metadata row */}
          <Show when={props.task.priority !== "Normal" || props.task.dueDate || props.task.scheduledDate || props.task.startDate || props.task.tags.length > 0}>
            <div class="flex flex-wrap items-center gap-x-3 gap-y-1 mt-0.5 text-xs text-gray-400 dark:text-gray-500">
              {/* Priority */}
              <Show when={props.task.priority !== "Normal"}>
                <span class={`flex items-center gap-0.5 font-medium ${priorityStyle()}`}>
                  {props.task.priority}
                </span>
              </Show>

              {/* Due Date */}
              <Show when={props.task.dueDate}>
                <span class={`flex items-center gap-0.5 ${dateColor(props.task.dueDate!)}`} title="Due date">
                  {Icons.calendar} {formatDate(props.task.dueDate!)}
                </span>
              </Show>

              {/* Start Date */}
              <Show when={props.task.startDate}>
                <span class="flex items-center gap-0.5 text-purple-600 dark:text-purple-400" title="Start date">
                  {Icons.calendarStart} {formatDate(props.task.startDate!)}
                </span>
              </Show>

              {/* Tags */}
              <Show when={props.task.tags.length > 0}>
                <span class="flex items-center gap-0.5 text-indigo-500 dark:text-indigo-400">
                  {Icons.tag} {props.task.tags.join(", ")}
                </span>
              </Show>
            </div>
          </Show>
        </div>
      </div>
    </Show>
  );
};

const FileNode: Component<{ filename: string; tasks: Task[]; today: string; path: string }> = (props) => {
  const nodeId = () => `file:${props.path}/${props.filename}`;
  const visibleTasks = () => props.tasks.filter((t) => isTaskVisible(t, props.today));
  const completed = () => props.tasks.filter((t) => t.status === "Completed" || t.status === "Cancelled").length;
  const total = () => props.tasks.length;
  const progress = () => (total() === 0 ? 0 : (completed() / total()) * 100);
  const hasDue = () => props.tasks.some((t) => t.dueDate && t.dueDate <= props.today);

  return (
    <Show when={visibleTasks().length > 0}>
      <details open={!isCollapsed(nodeId())} onToggle={(e) => {
        const isOpen = (e.target as HTMLDetailsElement).open;
        if (isOpen && isCollapsed(nodeId())) toggleCollapse(nodeId());
        else if (!isOpen && !isCollapsed(nodeId())) toggleCollapse(nodeId());
      }} class="group/file">
        <summary class="list-none cursor-pointer -mx-2 px-3 py-1.5 rounded-md bg-slate-600 dark:bg-gray-700 hover:bg-slate-500 dark:hover:bg-gray-600 flex items-center gap-2 text-sm font-medium text-white select-none transition-colors mb-1">
          {/* Chevron */}
          <span class="w-4 h-4 flex items-center justify-center text-gray-400 transition-transform group-open/file:rotate-90">
            {Icons.chevronRight}
          </span>

          {/* Icon & Name */}
          <span class="flex items-center gap-2 flex-1 min-w-0">
            <span class="text-gray-300">{Icons.file}</span>
            <span class="truncate">{props.filename}</span>

            {/* Due indicator */}
            <Show when={hasDue()}>
              <span class="w-2 h-2 rounded-full bg-red-500 flex-shrink-0" title="Contains due tasks" />
            </Show>

            {/* Progress bar */}
            <Show when={total() > 0}>
              <span class="ml-2 w-16 h-1 bg-slate-700 rounded-full overflow-hidden">
                <span class="block h-full bg-slate-300" style={{ width: `${progress()}%` }} />
              </span>
            </Show>
          </span>

          {/* Count */}
          <span class="text-xs text-gray-400 font-normal">
            {completed()}/{total()}
          </span>
        </summary>

        {/* Tasks list */}
        <div class="pl-8 flex flex-col">
          <For each={props.tasks}>{(task) => <TaskItem task={task} today={props.today} />}</For>
        </div>
      </details>
    </Show>
  );
};


// Helper to check if a folder node has any visible tasks (recursively)
const folderHasVisibleTasks = (node: FolderNode, today: string): boolean => {
  // Check files in this folder
  for (const tasks of Object.values(node.files)) {
    if (tasks.some((t) => isTaskVisible(t, today))) {
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

const FolderTree: Component<{ node: FolderNode; path?: string; today: string }> = (props) => {
  const currentPath = () => props.path ?? "";
  const folders = () => Object.entries(props.node.subfolders);
  const files = () => Object.entries(props.node.files);

  return (
    <div class="flex flex-col gap-2">
      <For each={folders()}>
        {([name, subnode]) => {
          const folderPath = () => `${currentPath()}/${name}`;
          const nodeId = () => `folder:${folderPath()}`;
          return (
            <Show when={folderHasVisibleTasks(subnode, props.today)}>
              <details open={!isCollapsed(nodeId())} onToggle={(e) => {
                const isOpen = (e.target as HTMLDetailsElement).open;
                if (isOpen && isCollapsed(nodeId())) toggleCollapse(nodeId());
                else if (!isOpen && !isCollapsed(nodeId())) toggleCollapse(nodeId());
              }} class="group/folder">
                <summary class="list-none cursor-pointer -mx-2 px-2 py-1 rounded-md bg-slate-600 dark:bg-gray-700 hover:bg-slate-500 dark:hover:bg-gray-600 flex items-center gap-2 text-sm font-medium text-white select-none transition-colors">
                  <span class="w-4 h-4 flex items-center justify-center text-gray-300 transition-transform group-open/folder:rotate-90">
                    {Icons.chevronRight}
                  </span>
                  <span class="flex items-center gap-1.5">
                    <span class="text-gray-300">{Icons.folder}</span>
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
      <For each={files()}>
        {([filename, tasks]) => <FileNode filename={filename} tasks={tasks} today={props.today} path={currentPath()} />}
      </For>
    </div>
  );
};


const App: Component = () => {
  onMount(async () => {
    await fetchVault();
    connectVault();
  });

  return (
    <div class="min-h-screen bg-gray-50 dark:bg-gray-900 font-sans text-gray-900 dark:text-gray-100">
      <div class="max-w-4xl mx-auto my-6">
        {/* Vault path label */}
        <div class="text-center">
          <span class="inline-block px-3 py-1 text-xs font-mono bg-indigo-600 text-white rounded-t-lg">
            {vault.vaultPath}
          </span>
        </div>

        {/* Main content card */}
        <div class="bg-white dark:bg-gray-950 rounded-xl shadow-sm border border-indigo-600 p-6 sm:p-8 -mt-px">
          {/* Header */}
          <header class="mb-4">
            <h1 class="text-2xl font-bold text-gray-800 dark:text-gray-100">{vault.vaultName || "Imako"}</h1>
            <p class="text-gray-500 dark:text-gray-400 text-sm">Today: {vault.today}</p>
          </header>

          {/* Filter bar */}
          <FilterBar filters={vault.filters} />

          {/* Folder tree */}
          <FolderTree node={vault.folderTree} today={vault.today} />
        </div>
      </div>
    </div>
  );
};

export default App;
