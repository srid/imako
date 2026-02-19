// AUTO-GENERATED - DO NOT EDIT. Regenerate with: just generate-types
import { Inline } from "@/components/markdown/types";

/**
 * AUTO-GENERATED TypeScript types from Haskell.
 * DO NOT EDIT MANUALLY.
 *
 * Regenerate with: just generate-types
 *
 * Source Haskell files:
 *   - Task, TaskStatus: packages/ob/src/Ob/Task.hs
 *   - Priority: packages/ob/src/Ob/Task/Properties.hs
 *   - FolderNode: packages/imako/src/Imako/Core/FolderTree.hs
 *   - Protocol types: packages/imako/src/Imako/API/Protocol.hs
 */

export type TaskStatus = "Incomplete" | "InProgress" | "Cancelled" | "Completed";

export interface Task {
  description: Inline[];
  sourceNote: string;
  status: TaskStatus;
  dueDate?: string;
  scheduledDate?: string;
  startDate?: string;
  completedDate?: string;
  priority: Priority;
  tags: string[];
  parentBreadcrumbs: string[];
  parentStartDate?: string;
  taskNum: number;
  parentTaskNum?: number;
}

export type Priority = "Highest" | "High" | "Medium" | "Normal" | "Low" | "Lowest";

export type FolderNode = IFolderNode;

export interface IFolderNode {
  subfolders: {[k in string]: FolderNode};
  files: {[k in string]: Task[]};
}

export type Query = IVaultQuery | INotesQuery;

export interface IVaultQuery {
  tag: "VaultQuery";
}

export interface INotesQuery {
  tag: "NotesQuery";
  contents: string;
}

export type VaultInfo = IVaultInfo;

export interface IVaultInfo {
  vaultPath: string;
  vaultName: string;
  today: string;
  notes: {[k in string]: string};
}

export type VaultData = IVaultData;

export interface IVaultData {
  folderTree: FolderNode;
}

export type NotesData = INotesData;

export interface INotesData {
  notePath: string;
  noteAst: any;
  backlinks: string[];
}

export type QueryResponse = IVaultResponse | INotesResponse;

export interface IVaultResponse {
  tag: "VaultResponse";
  contents: VaultData;
}

export interface INotesResponse {
  tag: "NotesResponse";
  contents: NotesData;
}

export type ServerMessage = IServerMessage;

export interface IServerMessage {
  vaultInfo: VaultInfo;
  response: QueryResponse;
}
