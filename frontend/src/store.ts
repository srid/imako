/**
 * Server-side vault state.
 *
 * This store holds the authoritative data from the Haskell backend,
 * synced via WebSocket.
 *
 * - vaultInfo: Shared connection info (always available once connected)
 * - vaultData: Folder tree with tasks (always available)
 * - notesData: Currently loaded note content (lazy, one at a time)
 */

import { createSignal } from "solid-js";
import { createStore } from "solid-js/store";
import type { VaultInfo, FolderNode, NotesData } from "@/types";

const emptyVaultInfo: VaultInfo = {
  vaultName: "",
  vaultPath: "",
  today: "",
  dailyNotesFolder: null,
};

// Shared vault info (extracted from any result)
export const [vaultInfo, setVaultInfo] = createStore<VaultInfo>(emptyVaultInfo);

// Vault data (folder tree + tasks) — always available after connection
export const [vaultData, setVaultData] = createSignal<FolderNode | null>(null);

// Notes data (currently loaded note) — lazy, one at a time
export const [notesData, setNotesData] = createSignal<NotesData | null>(null);

// Connection status
export const [isConnected, setIsConnected] = createSignal(false);
