/**
 * AUTO-GENERATED TypeScript types from Haskell.
 * DO NOT EDIT MANUALLY.
 *
 * Regenerate with: cabal run generate-types > frontend/src/types.ts
 * Or: just generate-types
 *
 * Source Haskell files:
 *   - Task, TaskStatus: packages/ob/src/Ob/Task.hs
 *   - Priority, TaskProperties: packages/ob/src/Ob/Task/Properties.hs
 *   - FolderNode: packages/imako/src/Imako/Core/FolderTree.hs
 *   - Filter: packages/imako/src/Imako/Core/Filter.hs
 *   - DailyNote: packages/ob/src/Ob/DailyNotes.hs
 *   - AppView: packages/imako/src/Imako/Core.hs
 */


export type TaskStatus = "Incomplete" | "InProgress" | "Cancelled" | "Completed";

export type Priority = "Highest" | "High" | "Medium" | "Normal" | "Low" | "Lowest";

export interface Task {
  description: string;
  sourceNote: string;
  status: TaskStatus;
  dueDate?: string;
  scheduledDate?: string;
  startDate?: string;
  completedDate?: string;
  priority: Priority;
  tags: string[];
  parentBreadcrumbs: string[];
}

export type Filter = IFilter;

export interface IFilter {
  filterId: string;
  filterLabel: string;
}

export type FolderNode = IFolderNode;

export interface IFolderNode {
  subfolders: {[k in string]: FolderNode};
  files: {[k in string]: Task[]};
}

export interface DailyNote {
  day: string;
  notePath: string;
}

export type AppView = IAppView;

export interface IAppView {
  folderTree: FolderNode;
  filters: Filter[];
  today: string;
  vaultPath: string;
  vaultName: string;
  dailyNotes: DailyNote[];
}
