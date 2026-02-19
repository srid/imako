/**
 * WebSocket synchronization for vault data.
 *
 * Establishes a WebSocket connection to the backend and syncs state
 * in real-time. Client sends Query, server responds with ServerMessage.
 */

import type { Query, ServerMessage } from "@/types";
import { setVaultInfo, setRouteData, setIsConnected } from "@/store";

let ws: WebSocket | null = null;

/**
 * Connect to the backend WebSocket.
 * Call sendQuery() after connection to subscribe to data.
 */
export function connectVault(): () => void {
  const protocol = location.protocol === "https:" ? "wss:" : "ws:";
  ws = new WebSocket(`${protocol}//${location.host}/ws`);

  ws.onopen = () => {
    setIsConnected(true);
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
  // Use both events for maximum compatibility
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
 * ServerMessage has { vaultInfo, response } structure.
 */
function handleServerMessage(msg: ServerMessage): void {
  // Update shared vault info
  setVaultInfo(msg.vaultInfo);

  // Update route-specific data based on response type
  const response = msg.response;
  if (response.tag === "VaultResponse") {
    setRouteData({ tag: "vault", data: response.contents });
  } else if (response.tag === "NotesResponse") {
    setRouteData({ tag: "notes", data: response.contents });
  }
}
