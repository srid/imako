/**
 * Server-side vault state.
 *
 * This store holds the authoritative data from the Haskell backend,
 * synced via WebSocket. For client-only UI state (filters, collapsed nodes),
 * see the `state/` directory.
 */

import { createSignal } from "solid-js";
import { createStore } from "solid-js/store";
import type { AppView } from "@/types";

const emptyAppView: AppView = {
  folderTree: { subfolders: {}, files: {} },
  filters: [],
  today: "",
  vaultPath: "",
  vaultName: "",
  dailyNotes: [],
};

export const [vault, setVault] = createStore<AppView>(emptyAppView);
export const [isConnected, setIsConnected] = createSignal(false);
