import { A } from "@solidjs/router";

export const NavBar = () => {
  return (
    <nav class="flex gap-8 mb-8 border-b border-stone-200 dark:border-stone-700 pb-4">
      <A
        href="/tasks"
        activeClass="text-accent-700 dark:text-accent-400 font-semibold border-b-2 border-accent-500 -mb-[17px] pb-4"
        inactiveClass="text-stone-500 dark:text-stone-400 hover:text-accent-600 dark:hover:text-accent-400 transition-colors"
        class="text-base"
      >
        Tasks
      </A>
      <A
        href="/notes"
        activeClass="text-accent-700 dark:text-accent-400 font-semibold border-b-2 border-accent-500 -mb-[17px] pb-4"
        inactiveClass="text-stone-500 dark:text-stone-400 hover:text-accent-600 dark:hover:text-accent-400 transition-colors"
        class="text-base"
      >
        Notes
      </A>
    </nav>
  );
};

