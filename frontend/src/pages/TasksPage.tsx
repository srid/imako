import { Component } from "solid-js";
import { vault } from "@/store";
import { FilterBar } from "@/components/FilterBar";
import { FolderTree } from "@/components/FolderTree";

const TasksPage: Component = () => {
  return (
    <>
      {/* Header */}
      <header class="mb-4">
        <h1 class="text-2xl font-bold text-gray-800 dark:text-gray-100">{vault.vaultName || "Imako"}</h1>
        <p class="text-gray-500 dark:text-gray-400 text-sm">Today: {vault.today}</p>
      </header>

      {/* Filter bar */}
      <FilterBar filters={vault.filters} />

      {/* Folder tree */}
      <FolderTree node={vault.folderTree} today={vault.today} />
    </>
  );
};

export default TasksPage;
