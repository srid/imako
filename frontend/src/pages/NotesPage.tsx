import { Component } from "solid-js";

const NotesPage: Component = () => {
  return (
    <>
      {/* Subheader */}
      <div class="flex items-center justify-between gap-4 mb-6">
        <h2 class="text-lg font-semibold text-stone-700 dark:text-stone-200">Notes</h2>
        <span class="text-sm text-stone-400 dark:text-stone-500">Search coming soon</span>
      </div>

      {/* Placeholder */}
      <p class="text-stone-500 dark:text-stone-400">Notes view coming soon...</p>
    </>
  );
};

export default NotesPage;
