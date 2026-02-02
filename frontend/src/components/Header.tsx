import { A } from "@solidjs/router";
import { vaultInfo } from "@/store";

export const Header = () => {
  return (
    <header class="flex items-center justify-between gap-4 pb-4 mb-6 border-b border-stone-200 dark:border-stone-700">
      {/* Left: Vault name + Navigation */}
      <div class="flex items-center gap-6">
        <h1 class="text-lg font-bold text-stone-800 dark:text-stone-100">
          {vaultInfo.vaultName || "Vault"}
        </h1>
        <nav class="flex gap-4">
          <A
            href="/tasks"
            activeClass="text-accent-600 dark:text-accent-400 font-semibold"
            inactiveClass="text-stone-500 dark:text-stone-400 hover:text-accent-600 dark:hover:text-accent-400 transition-colors"
            class="text-sm"
          >
            Tasks
          </A>
          <A
            href="/n"
            activeClass="text-accent-600 dark:text-accent-400 font-semibold"
            inactiveClass="text-stone-500 dark:text-stone-400 hover:text-accent-600 dark:hover:text-accent-400 transition-colors"
            class="text-sm"
          >
            Notes
          </A>
        </nav>
      </div>

      {/* Right: Vault path */}
      <span class="text-xs text-stone-400 dark:text-stone-500 truncate max-w-[200px] hidden sm:block" title={vaultInfo.vaultPath}>
        {vaultInfo.vaultPath}
      </span>
    </header>
  );
};
