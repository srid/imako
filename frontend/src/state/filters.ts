import { usePersistedSet } from "@/hooks/usePersistedSet";
import type { Task } from "@/types";

// LocalStorage keys
const STORAGE_KEYS = {
  filters: "imako-filters",
  collapsed: "imako-collapsed",
};

// Filter state - tracks which filters are active (persisted)
export const [activeFilters, toggleFilter, isFilterActive] = usePersistedSet(
  STORAGE_KEYS.filters
);

// Collapse state - tracks which nodes are collapsed (persisted)
export const [collapsedNodes, toggleCollapse, isCollapsed] = usePersistedSet(
  STORAGE_KEYS.collapsed
);

/**
 * Check if a task should be visible based on active filters.
 * Filters are client-side; backend provides all tasks.
 */
export const isTaskVisible = (task: Task, today: string): boolean => {
  const isFuture = task.startDate && task.startDate > today;
  const isPast = task.status === "Completed" || task.status === "Cancelled";

  const showFuture = isFilterActive("showFuture");
  const showPast = isFilterActive("showPast");

  if (isFuture && !showFuture) return false;
  if (isPast && !showPast) return false;
  return true;
};
