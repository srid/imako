/**
 * Server-side vault state.
 *
 * This store holds the authoritative data from the Haskell backend,
 * synced via WebSocket.
 *
 * - vaultInfo: Shared connection info (always available once connected)
 * - routeData: Route-specific data (sum type - one active at a time)
 */

import { createSignal } from "solid-js";
import { createStore } from "solid-js/store";
import type { VaultInfo, TasksData, NotesData } from "@/types";

const emptyVaultInfo: VaultInfo = {
  vaultName: "",
  vaultPath: "",
  today: "",
  notes: {},
  wikilinkResolutions: {},
};

/** Route-specific data - sum type (only one active at a time) */
export type RouteData =
  | { tag: "tasks"; data: TasksData }
  | { tag: "notes"; data: NotesData }
  | null;

// Shared vault info (extracted from any result)
export const [vaultInfo, setVaultInfo] = createStore<VaultInfo>(emptyVaultInfo);

// Route-specific data (sum type)
export const [routeData, setRouteData] = createSignal<RouteData>(null);

// Connection status
export const [isConnected, setIsConnected] = createSignal(false);
