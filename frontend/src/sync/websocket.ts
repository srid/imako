/**
 * WebSocket synchronization for vault data.
 *
 * Establishes a WebSocket connection to the backend and syncs state
 * in real-time. Client sends Query, server responds with ServerMessage.
 */

import type { Query, ServerMessage } from "@/types";
import { setVaultInfo, setVaultData, setNotesData, setIsConnected } from "@/store";

let ws: WebSocket | null = null;

/**
 * Connect to the backend WebSocket.
 */
export function connectVault(): () => void {
  const protocol = location.protocol === "https:" ? "wss:" : "ws:";
  ws = new WebSocket(`${protocol}//${location.host}/ws`);

  ws.onopen = () => {
    setIsConnected(true);
    // Always subscribe to vault data on connect/reconnect
    sendQuery({ tag: "FolderTreeQuery" });
  };

  ws.onmessage = (event) => {
    const msg: ServerMessage = JSON.parse(event.data);
    handleServerMessage(msg);
  };

  ws.onerror = (error) => {
    console.error("WebSocket error:", error);
  };

  ws.onclose = () => {
    console.log("WebSocket closed, reconnecting in 3s...");
    setIsConnected(false);
    ws = null;
    setTimeout(connectVault, 3000);
  };

  // Gracefully close WebSocket before page unload to avoid EPIPE errors
  window.addEventListener("beforeunload", disconnectVault);
  window.addEventListener("pagehide", disconnectVault);

  return disconnectVault;
}

/**
 * Gracefully disconnect from the WebSocket.
 */
export function disconnectVault(): void {
  if (ws) {
    ws.close(1000, "Page unloading");
    ws = null;
  }
}

/**
 * Send a query to the backend to subscribe to specific data.
 */
export function sendQuery(query: Query): void {
  if (ws && ws.readyState === WebSocket.OPEN) {
    ws.send(JSON.stringify(query));
  }
}

/**
 * Handle incoming server messages and update stores.
 */
function handleServerMessage(msg: ServerMessage): void {
  setVaultInfo(msg.vaultInfo);

  const response = msg.response;
  if (response.tag === "FolderTreeResponse") {
    setVaultData(response.contents);
  } else if (response.tag === "NotesResponse") {
    setNotesData(response.contents);
  }
}
