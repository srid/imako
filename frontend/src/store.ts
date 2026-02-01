import { createSignal } from "solid-js";
import { createStore, reconcile } from "solid-js/store";
import type { AppView } from "@/types";

const emptyAppView: AppView = {
  folderTree: { subfolders: {}, files: {} },
  filters: [],
  today: "",
  vaultPath: "",
  vaultName: "",
  dailyNotes: [],
};

const [vault, setVault] = createStore<AppView>(emptyAppView);
const [isConnected, setIsConnected] = createSignal(false);

export { vault, isConnected };

/**
 * Connect to the backend WebSocket and sync vault state.
 * The WebSocket sends initial state immediately upon connection.
 * Uses reconcile() for efficient fine-grained updates.
 */
export function connectVault(): () => void {
  const protocol = location.protocol === "https:" ? "wss:" : "ws:";
  const ws = new WebSocket(`${protocol}//${location.host}/ws`);

  ws.onmessage = (event) => {
    const data: AppView = JSON.parse(event.data);
    setVault(reconcile(data));
    setIsConnected(true);
  };

  ws.onerror = (error) => {
    console.error("WebSocket error:", error);
  };

  ws.onclose = () => {
    console.log("WebSocket closed, reconnecting in 3s...");
    setIsConnected(false);
    setTimeout(connectVault, 3000);
  };

  return () => ws.close();
}

