import { Component } from "solid-js";

export const VaultHeader: Component<{ path: string; name: string; today: string }> = (props) => {
  return (
    <>
      {/* Vault path label */}
      <div class="text-center">
        <span class="inline-block px-3 py-1 text-xs font-mono bg-indigo-600 text-white rounded-t-lg">
          {props.path}
        </span>
      </div>

      {/* Header */}
      <header class="mb-4">
        <h1 class="text-2xl font-bold text-gray-800 dark:text-gray-100">{props.name || "Imako"}</h1>
        <p class="text-gray-500 dark:text-gray-400 text-sm">Today: {props.today}</p>
      </header>
    </>
  );
};
