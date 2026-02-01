/**
 * WebSocket synchronization for vault data.
 *
 * Establishes a WebSocket connection to the backend and syncs vault state
 * in real-time. The backend pushes updates whenever files change.
 */

import { reconcile } from "solid-js/store";
import type { AppView } from "@/types";
import { setVault, setIsConnected } from "@/store";

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
