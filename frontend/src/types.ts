/**
 * AUTO-GENERATED TypeScript types from Haskell.
 * DO NOT EDIT MANUALLY.
 *
 * Regenerate with: cabal run generate-types > frontend/src/types.ts
 * Or: just generate-types
 *
 * Source Haskell files:
 *   - Task, TaskStatus: packages/ob/src/Ob/Task.hs
 *   - Priority: packages/ob/src/Ob/Task/Properties.hs
 *   - FolderNode: packages/imako/src/Imako/Core/FolderTree.hs
 *   - Filter: packages/imako/src/Imako/Core/Filter.hs
 *   - Protocol types: packages/imako/src/Imako/API/Protocol.hs
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

export type Filter = "ShowFuture" | "ShowPast";

export type FolderNode = IFolderNode;

export interface IFolderNode {
  subfolders: {[k in string]: FolderNode};
  files: {[k in string]: Task[]};
}

export type Query = "TasksQuery" | "NotesQuery";

export type VaultInfo = IVaultInfo;

export interface IVaultInfo {
  vaultPath: string;
  vaultName: string;
}

export type TasksData = ITasksData;

export interface ITasksData {
  folderTree: FolderNode;
  filters: Filter[];
  today: string;
}

export type NotesData = INotesData;

export interface INotesData {
  noteCount: number;
}

export type ServerMessage = ITasksResultMsg | INotesResultMsg;

export interface ITasksResultMsg {
  tag: "TasksResultMsg";
  contents: [VaultInfo, TasksData];
}

export interface INotesResultMsg {
  tag: "NotesResultMsg";
  contents: [VaultInfo, NotesData];
}
