import { Component, onMount } from "solid-js";
import { vault, connectVault, fetchVault } from "./store";
import { FilterBar } from "./components/FilterBar";
import { FolderTree } from "./components/FolderTree";
import { VaultHeader } from "./components/VaultHeader";

const App: Component = () => {
  onMount(async () => {
    await fetchVault();
    connectVault();
  });

  return (
    <div class="min-h-screen bg-gray-50 dark:bg-gray-900 font-sans text-gray-900 dark:text-gray-100">
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
    </div>
  );
};

export default App;
