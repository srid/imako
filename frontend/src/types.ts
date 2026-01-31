/**
 * TypeScript types matching the Haskell backend.
 * These mirror the ToJSON instances in the Haskell code.
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
