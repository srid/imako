import { Component, For, Show, createMemo } from "solid-js";
import type { Task } from "@/types";
import { Icons } from "@/utils/icons";
import { obsidianOpenUrl } from "@/utils/obsidian";
import { isCollapsed, toggleCollapse, isTaskVisible } from "@/state/filters";
import { TaskItem } from "@/components/TaskItem";
import { vault } from "@/store";

export const FileNode: Component<{ filename: string; tasks: Task[]; today: string; path: string }> = (props) => {
  const nodeId = () => `file:${props.path}/${props.filename}`;

  // Computed task stats
  const stats = createMemo(() => {
    const visible = props.tasks.filter((t) => isTaskVisible(t, props.today));
    const completed = props.tasks.filter((t) => t.status === "Completed" || t.status === "Cancelled").length;
    const total = props.tasks.length;
    return {
      visible,
      completed,
      total,
      progress: total === 0 ? 0 : (completed / total) * 100,
      hasDue: props.tasks.some((t) => t.dueDate && t.dueDate <= props.today),
    };
  });

  return (
    <Show when={stats().visible.length > 0}>
      <details open={!isCollapsed(nodeId())} onToggle={(e) => {
        const isOpen = (e.target as HTMLDetailsElement).open;
        if (isOpen && isCollapsed(nodeId())) toggleCollapse(nodeId());
        else if (!isOpen && !isCollapsed(nodeId())) toggleCollapse(nodeId());
      }} class="group/file">
        <summary class="list-none cursor-pointer -mx-2 px-3 py-2 rounded-lg bg-amber-50 dark:bg-stone-800/50 hover:bg-amber-100 dark:hover:bg-stone-800 border border-amber-200 dark:border-stone-700 flex items-center gap-2 text-sm font-medium text-stone-700 dark:text-stone-300 select-none transition-colors mb-2">
          {/* Chevron */}
          <span class="w-4 h-4 flex items-center justify-center text-stone-400 dark:text-stone-500 transition-transform group-open/file:rotate-90">
            {Icons.chevronRight}
          </span>

          {/* Icon & Name */}
          <span class="flex items-center gap-2 flex-1 min-w-0">
            <span class="text-stone-400 dark:text-stone-500">{Icons.file}</span>
            <span class="truncate">{props.filename}</span>

            {/* Due indicator */}
            <Show when={stats().hasDue}>
              <span class="w-2 h-2 rounded-full bg-red-500 flex-shrink-0" title="Contains due tasks" />
            </Show>

            {/* Progress bar */}
            <Show when={stats().total > 0}>
              <span class="ml-2 w-16 h-1.5 bg-stone-200 dark:bg-stone-700 rounded-full overflow-hidden">
                <span class="block h-full bg-amber-500 rounded-full" style={{ width: `${stats().progress}%` }} />
              </span>
            </Show>
          </span>

          {/* Count */}
          <span class="text-xs text-stone-500 dark:text-stone-400 font-normal tabular-nums">
            {stats().completed}/{stats().total}
          </span>

          {/* Edit link */}
          <a
            href={obsidianOpenUrl(vault.vaultName, `${props.path}/${props.filename}`)}
            class="ml-2 opacity-0 group-hover/file:opacity-100 transition-opacity text-stone-400 hover:text-amber-600 dark:hover:text-amber-400"
            title="Open in Obsidian"
            onClick={(e) => e.stopPropagation()}
          >
            {Icons.edit}
          </a>
        </summary>

        {/* Tasks list */}
        <div class="pl-8 flex flex-col">
          <For each={props.tasks}>{(task) => <TaskItem task={task} today={props.today} />}</For>
        </div>
      </details>
    </Show>
  );
};
