import { Component, For } from "solid-js";
import type { Filter } from "../types";
import { activeFilters, toggleFilter } from "../state/filters";

export const FilterBar: Component<{ filters: Filter[] }> = (props) => {
  return (
    <div class="mb-4 flex items-center gap-2">
      <For each={props.filters}>
        {(filter) => (
          <button
            onClick={() => toggleFilter(filter.filterId)}
            class={`px-3 py-1 text-xs font-medium rounded-full transition-colors ${
              activeFilters().has(filter.filterId)
                ? "bg-indigo-600 text-white hover:bg-indigo-700"
                : "bg-gray-100 dark:bg-gray-800 text-gray-500 dark:text-gray-400 hover:bg-gray-200 dark:hover:bg-gray-700"
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
