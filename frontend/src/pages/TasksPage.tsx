import { Component } from "solid-js";
import { vault } from "@/store";
import { FilterBar } from "@/components/FilterBar";
import { FolderTree } from "@/components/FolderTree";

const TasksPage: Component = () => {
  return (
    <>
      {/* Header */}
      <header class="mb-6">
        <h1 class="text-3xl font-bold tracking-tight text-stone-800 dark:text-stone-100">{vault.vaultName || "Imako"}</h1>
        <p class="text-stone-500 dark:text-stone-400 text-sm mt-1">Today: {vault.today}</p>
      </header>

      {/* Filter bar */}
      <FilterBar filters={vault.filters} />

      {/* Folder tree */}
      <FolderTree node={vault.folderTree} today={vault.today} />
    </>
  );
};

export default TasksPage;
