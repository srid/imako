import { Component, onMount, Show } from "solid-js";
import { vault, isConnected } from "@/store";
import { connectVault } from "@/sync/websocket";
import { FilterBar } from "@/components/FilterBar";
import { FolderTree } from "@/components/FolderTree";
import { VaultHeader } from "@/components/VaultHeader";

const App: Component = () => {
  onMount(() => {
    connectVault();
  });

  return (
    <div class="min-h-screen bg-gray-50 dark:bg-gray-900 font-sans text-gray-900 dark:text-gray-100">
      <Show
        when={isConnected()}
        fallback={
          <div class="flex items-center justify-center min-h-screen">
            <div class="text-center">
              <div class="inline-block w-8 h-8 border-4 border-indigo-600 border-t-transparent rounded-full animate-spin mb-4" />
              <p class="text-gray-500 dark:text-gray-400">Connecting to vault...</p>
            </div>
          </div>
        }
      >
        <div class="max-w-4xl mx-auto my-6">
          {/* Vault path label */}
          <div class="text-center">
            <span class="inline-block px-3 py-1 text-xs font-mono bg-indigo-600 text-white rounded-t-lg">
              {vault.vaultPath}
            </span>
          </div>

          {/* Main content card */}
          <div class="bg-white dark:bg-gray-950 rounded-xl shadow-sm border border-indigo-600 p-6 sm:p-8 -mt-px">
            {/* Header */}
            <header class="mb-4">
              <h1 class="text-2xl font-bold text-gray-800 dark:text-gray-100">{vault.vaultName || "Imako"}</h1>
              <p class="text-gray-500 dark:text-gray-400 text-sm">Today: {vault.today}</p>
            </header>

            {/* Filter bar */}
            <FilterBar filters={vault.filters} />

            {/* Folder tree */}
            <FolderTree node={vault.folderTree} today={vault.today} />
          </div>
        </div>
      </Show>
    </div>
  );
};

export default App;

