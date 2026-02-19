import { Component, Show } from "solid-js";
import { vaultInfo } from "@/store";
import {
  showTasks,
  toggleShowTasks,
  toggleFilter,
  isFilterActive,
  treeFilter,
  setTreeFilter,
} from "@/state/filters";

export const Header: Component = () => {
  return (
    <header class="flex flex-col gap-3 pb-4 mb-4 border-b border-stone-200 dark:border-stone-700">
      {/* Top row: Vault name + path */}
      <div class="flex items-center justify-between gap-4">
        <h1 class="text-lg font-bold text-stone-800 dark:text-stone-100">
          {vaultInfo.vaultName || "Vault"}
        </h1>
        <span
          class="text-xs text-stone-400 dark:text-stone-500 truncate max-w-[200px] hidden sm:block"
          title={vaultInfo.vaultPath}
        >
          {vaultInfo.vaultPath}
        </span>
      </div>

      {/* Bottom row: Filters + Search */}
      <div class="flex items-center justify-between gap-3 flex-wrap">
        <div class="flex items-center gap-2">
          {/* Show tasks toggle (parent filter) */}
          <button
            onClick={toggleShowTasks}
            class={`px-3 py-1 text-sm font-medium rounded-full transition-all ${
              showTasks()
                ? "bg-accent-500 text-white shadow-sm hover:bg-accent-600"
                : "bg-stone-100 dark:bg-stone-800 text-stone-600 dark:text-stone-400 hover:bg-stone-200 dark:hover:bg-stone-700"
            }`}
            aria-pressed={showTasks()}
          >
            {showTasks() ? "☑" : "☐"} Tasks
          </button>

          {/* Future/Past filters - only visible when tasks are on */}
          <Show when={showTasks()}>
            <button
              onClick={() => toggleFilter("ShowFuture")}
              class={`px-3 py-1 text-sm font-medium rounded-full transition-all ${
                isFilterActive("ShowFuture")
                  ? "bg-accent-500 text-white shadow-sm hover:bg-accent-600"
                  : "bg-stone-100 dark:bg-stone-800 text-stone-600 dark:text-stone-400 hover:bg-stone-200 dark:hover:bg-stone-700"
              }`}
              aria-pressed={isFilterActive("ShowFuture")}
            >
              Future tasks
            </button>
            <button
              onClick={() => toggleFilter("ShowPast")}
              class={`px-3 py-1 text-sm font-medium rounded-full transition-all ${
                isFilterActive("ShowPast")
                  ? "bg-accent-500 text-white shadow-sm hover:bg-accent-600"
                  : "bg-stone-100 dark:bg-stone-800 text-stone-600 dark:text-stone-400 hover:bg-stone-200 dark:hover:bg-stone-700"
              }`}
              aria-pressed={isFilterActive("ShowPast")}
            >
              Past tasks
            </button>
          </Show>

          {/* Today badge */}
          <span class="text-xs text-stone-400 dark:text-stone-500 ml-2 hidden sm:inline">
            Today: <span class="font-medium text-stone-600 dark:text-stone-300">{vaultInfo.today}</span>
          </span>
        </div>

        {/* Inline search */}
        <div class="relative">
          <input
            type="text"
            placeholder="Filter files…"
            value={treeFilter()}
            onInput={(e) => setTreeFilter(e.currentTarget.value)}
            class="w-44 px-3 py-1 text-sm bg-stone-100 dark:bg-stone-800 text-stone-700 dark:text-stone-300 rounded-lg border border-stone-200 dark:border-stone-700 outline-none focus:ring-2 focus:ring-accent-500 focus:border-transparent placeholder:text-stone-400 dark:placeholder:text-stone-500"
          />
          <Show when={treeFilter()}>
            <button
              onClick={() => setTreeFilter("")}
              class="absolute right-2 top-1/2 -translate-y-1/2 text-stone-400 hover:text-stone-600 dark:hover:text-stone-300 text-xs"
            >
              ✕
            </button>
          </Show>
        </div>
      </div>
    </header>
  );
};
