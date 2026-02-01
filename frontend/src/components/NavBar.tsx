import { A } from "@solidjs/router";

export const NavBar = () => {
  return (
    <nav class="flex gap-6 mb-6">
      <A
        href="/tasks"
        activeClass="text-indigo-600 dark:text-indigo-400 font-semibold"
        inactiveClass="text-gray-600 dark:text-gray-400 hover:text-indigo-500 dark:hover:text-indigo-400"
      >
        Tasks
      </A>
      <A
        href="/notes"
        activeClass="text-indigo-600 dark:text-indigo-400 font-semibold"
        inactiveClass="text-gray-600 dark:text-gray-400 hover:text-indigo-500 dark:hover:text-indigo-400"
      >
        Notes
      </A>
    </nav>
  );
};
