/**
 * WebSocket synchronization for vault data.
 *
 * Establishes a WebSocket connection to the backend and syncs state
 * in real-time. Client sends Query, server responds with ServerMessage.
 */

import type { Query, ServerMessage, VaultInfo, TasksData, NotesData } from "@/types";
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

  return () => {
    ws?.close();
    ws = null;
  };
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
 * Handle incoming server messages and update appropriate stores.
 * ServerMessage contents is a tuple: [VaultInfo, Data]
 */
function handleServerMessage(msg: ServerMessage): void {
  if (msg.tag === "TasksResultMsg") {
    const [info, data] = msg.contents as [VaultInfo, TasksData];
    setVaultInfo(info);
    setRouteData({ tag: "tasks", data });
  } else if (msg.tag === "NotesResultMsg") {
    const [info, data] = msg.contents as [VaultInfo, NotesData];
    setVaultInfo(info);
    setRouteData({ tag: "notes", data });
  }
}
