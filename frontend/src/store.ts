import { createStore, reconcile } from "solid-js/store";
import type { AppView } from "./types";

const emptyAppView: AppView = {
  folderTree: { subfolders: {}, files: {} },
  filters: [],
  today: "",
  vaultPath: "",
  vaultName: "",
  dailyNotes: [],
};

const [vault, setVault] = createStore<AppView>(emptyAppView);

export { vault };

/**
 * Connect to the backend WebSocket and sync vault state.
 * Uses reconcile() for efficient fine-grained updates.
 */
export function connectVault(): () => void {
  const protocol = location.protocol === "https:" ? "wss:" : "ws:";
  const ws = new WebSocket(`${protocol}//${location.host}/ws`);

  ws.onmessage = (event) => {
    const data: AppView = JSON.parse(event.data);
    setVault(reconcile(data));
  };

  ws.onerror = (error) => {
    console.error("WebSocket error:", error);
  };

  ws.onclose = () => {
    console.log("WebSocket closed, reconnecting in 3s...");
    setTimeout(connectVault, 3000);
  };

  return () => ws.close();
}

/**
 * Fetch initial vault state via REST API.
 */
export async function fetchVault(): Promise<void> {
  const response = await fetch("/api/view");
  const data: AppView = await response.json();
  setVault(reconcile(data));
}
