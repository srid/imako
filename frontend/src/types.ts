/**
 * TypeScript types matching the Haskell backend.
 *
 * IMPORTANT: These types must be kept in sync with the Haskell ToJSON instances.
 *
 * Source files:
 *   - Task, TaskStatus: packages/ob/src/Ob/Task.hs
 *   - Priority, TaskProperties: packages/ob/src/Ob/Task/Properties.hs
 *   - FolderNode: packages/imako/src/Imako/Core/FolderTree.hs
 *   - Filter: packages/imako/src/Imako/Core/Filter.hs
 *   - DailyNote: packages/ob/src/Ob/DailyNotes.hs
 *   - AppView: packages/imako/src/Imako/Core.hs
 *
 * When adding/modifying types:
 *   1. Update the Haskell ToJSON instance
 *   2. Update this file to match
 *   3. Verify with: curl -s https://localhost:4009/api/view | jq
 *
 * TODO: Consider auto-generating with aeson-typescript or similar.
 */

export type TaskStatus = "Incomplete" | "InProgress" | "Completed" | "Cancelled";

export type Priority = "Highest" | "High" | "Medium" | "Normal" | "Low" | "Lowest";

export interface Task {
  description: string;
  sourceNote: string;
  status: TaskStatus;
  dueDate: string | null;
  scheduledDate: string | null;
  startDate: string | null;
  completedDate: string | null;
  priority: Priority;
  tags: string[];
  parentBreadcrumbs: string[];
}

export interface FolderNode {
  subfolders: Record<string, FolderNode>;
  files: Record<string, Task[]>;
}

export interface Filter {
  filterId: string;
  filterLabel: string;
}

export interface DailyNote {
  day: string;
  notePath: string;
}

export interface AppView {
  folderTree: FolderNode;
  filters: Filter[];
  today: string;
  vaultPath: string;
  vaultName: string;
  dailyNotes: DailyNote[];
}
