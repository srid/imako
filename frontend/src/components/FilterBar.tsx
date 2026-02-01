import { Component, For } from "solid-js";
import type { Filter } from "@/types";
import { activeFilters, toggleFilter } from "@/state/filters";

export const FilterBar: Component<{ filters: Filter[] }> = (props) => {
  return (
    <div class="mb-6 flex items-center gap-2 flex-wrap">
      <For each={props.filters}>
        {(filter) => (
          <button
            onClick={() => toggleFilter(filter.filterId)}
            class={`px-4 py-1.5 text-sm font-medium rounded-full transition-all ${
              activeFilters().has(filter.filterId)
                ? "bg-accent-500 text-white shadow-sm hover:bg-accent-600"
                : "bg-stone-100 dark:bg-stone-800 text-stone-600 dark:text-stone-400 hover:bg-stone-200 dark:hover:bg-stone-700"
            }`}
            aria-pressed={activeFilters().has(filter.filterId)}
          >
            {filter.filterLabel}
          </button>
        )}
      </For>
    </div>
  );
};

