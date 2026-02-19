import { createSignal } from "solid-js";
import { usePersistedSet } from "@/hooks/usePersistedSet";
import type { Task } from "@/types";

/** Filter type - client-side only */
export type Filter = "ShowFuture" | "ShowPast";

/** All available filters */
export const FILTERS: Filter[] = ["ShowFuture", "ShowPast"];

// LocalStorage keys
const STORAGE_KEYS = {
  filters: "imako-filters",
  collapsed: "imako-collapsed",
  showTasks: "imako-show-tasks",
};

// Filter state - tracks which filters are active (persisted)
export const [activeFilters, toggleFilter, isFilterActive] = usePersistedSet(
  STORAGE_KEYS.filters
);

// Collapse state - tracks which nodes are collapsed (persisted)
export const [collapsedNodes, toggleCollapse, isCollapsed] = usePersistedSet(
  STORAGE_KEYS.collapsed
);

// Show tasks toggle (persisted, default: on)
const storedShowTasks = localStorage.getItem(STORAGE_KEYS.showTasks);
export const [showTasks, setShowTasks] = createSignal(
  storedShowTasks === null ? true : storedShowTasks === "true"
);
export const toggleShowTasks = () => {
  const next = !showTasks();
  setShowTasks(next);
  localStorage.setItem(STORAGE_KEYS.showTasks, String(next));
};

// Tree filter for real-time search
export const [treeFilter, setTreeFilter] = createSignal("");

/**
 * Check if a task should be visible based on active filters.
 * Filters are client-side; backend provides all tasks.
 */
export const isTaskVisible = (task: Task, today: string): boolean => {
  const isFuture =
    (task.startDate && task.startDate > today) ||
    (task.parentStartDate && task.parentStartDate > today);
  const isPast = task.status === "Completed" || task.status === "Cancelled";

  const showFuture = isFilterActive("ShowFuture");
  const showPast = isFilterActive("ShowPast");

  if (isFuture && !showFuture) return false;
  if (isPast && !showPast) return false;
  return true;
};
