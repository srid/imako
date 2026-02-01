import { A } from "@solidjs/router";

export const NavBar = () => {
  return (
    <nav class="flex gap-8 mb-8 border-b border-stone-200 dark:border-stone-700 pb-4">
      <A
        href="/tasks"
        activeClass="text-amber-700 dark:text-amber-400 font-semibold border-b-2 border-amber-500 -mb-[17px] pb-4"
        inactiveClass="text-stone-500 dark:text-stone-400 hover:text-amber-600 dark:hover:text-amber-400 transition-colors"
        class="text-base"
      >
        Tasks
      </A>
      <A
        href="/notes"
        activeClass="text-amber-700 dark:text-amber-400 font-semibold border-b-2 border-amber-500 -mb-[17px] pb-4"
        inactiveClass="text-stone-500 dark:text-stone-400 hover:text-amber-600 dark:hover:text-amber-400 transition-colors"
        class="text-base"
      >
        Notes
      </A>
    </nav>
  );
};

