import { Component, For } from "solid-js";
import { activeFilters, toggleFilter, FILTERS, type Filter } from "@/state/filters";

/** Human-readable labels for filters */
const FILTER_LABELS: Record<Filter, string> = {
  ShowFuture: "Future tasks",
  ShowPast: "Past tasks",
};

export const FilterBar: Component = () => {
  return (
    <div class="flex items-center gap-2 flex-wrap">
      <For each={FILTERS}>
        {(filter) => (
          <button
            onClick={() => toggleFilter(filter)}
            class={`px-4 py-1.5 text-sm font-medium rounded-full transition-all ${
              activeFilters().has(filter)
                ? "bg-accent-500 text-white shadow-sm hover:bg-accent-600"
                : "bg-stone-100 dark:bg-stone-800 text-stone-600 dark:text-stone-400 hover:bg-stone-200 dark:hover:bg-stone-700"
            }`}
            aria-pressed={activeFilters().has(filter)}
          >
            {FILTER_LABELS[filter]}
          </button>
        )}
      </For>
    </div>
  );
};
