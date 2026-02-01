import { Component } from "solid-js";
import { vault } from "@/store";
import { FilterBar } from "@/components/FilterBar";
import { FolderTree } from "@/components/FolderTree";

const TasksPage: Component = () => {
  return (
    <>
      {/* Subheader: Date + Filters */}
      <div class="flex items-center justify-between gap-4 mb-6">
        <p class="text-sm text-stone-500 dark:text-stone-400">
          Today: <span class="font-medium text-stone-700 dark:text-stone-300">{vault.today}</span>
        </p>
        <FilterBar filters={vault.filters} />
      </div>

      {/* Folder tree */}
      <FolderTree node={vault.folderTree} today={vault.today} />
    </>
  );
};

export default TasksPage;
