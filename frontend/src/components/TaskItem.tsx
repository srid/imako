import { Component, Show, createMemo } from "solid-js";
import type { Task } from "@/types";
import { Icons } from "@/utils/icons";
import { formatDate, dateColor } from "@/utils/dates";
import { isTaskVisible } from "@/state/filters";
import { InlineRenderer } from "@/components/markdown/InlineRenderer";

export const TaskItem: Component<{ task: Task; today: string }> = (props) => {
  // Computed styles and visibility
  const computed = createMemo(() => {
    const { status, priority } = props.task;

    // Status icon
    const icon = (() => {
      switch (status) {
        case "Completed": return Icons.checkSquare;
        case "Cancelled": return Icons.cancelSquare;
        case "InProgress": return Icons.halfSquare;
        default: return Icons.emptySquare;
      }
    })();

    // Status color
    const statusColor = (() => {
      switch (status) {
        case "Completed":
        case "Cancelled": return "text-stone-400";
        case "InProgress": return "text-accent-500";
        default: return "text-stone-400 hover:text-stone-600 dark:hover:text-stone-300";
      }
    })();

    // Text styling
    const textStyle = (status === "Completed" || status === "Cancelled")
      ? "line-through text-stone-400 dark:text-stone-500"
      : "text-stone-800 dark:text-stone-100";

    // Priority styling
    const priorityStyle = (() => {
      switch (priority) {
        case "Highest": return "text-red-600 dark:text-red-400";
        case "High": return "text-orange-600 dark:text-orange-400";
        case "Medium": return "text-amber-600 dark:text-amber-400";
        case "Low": return "text-blue-600 dark:text-blue-400";
        case "Lowest": return "text-slate-500 dark:text-slate-400";
        default: return "";
      }
    })();

    return {
      icon,
      statusColor,
      textStyle,
      priorityStyle,
      visible: isTaskVisible(props.task, props.today),
    };
  });

  return (
    <Show when={computed().visible}>
      <div class="group/task relative py-1.5 -mx-2 px-2 rounded-lg hover:bg-accent-50/50 dark:hover:bg-accent-900/10 flex items-start gap-2 text-sm transition-colors">
        {/* Checkbox */}
        <span class={`w-5 h-5 flex-shrink-0 flex items-center justify-center ${computed().statusColor}`}>
          {computed().icon}
        </span>

        {/* Content */}
        <div class="flex-1 min-w-0">
          <div class={`leading-snug ${computed().textStyle}`}>
            <InlineRenderer inlines={props.task.description} />
          </div>

          {/* Metadata row */}
          <Show when={props.task.priority !== "Normal" || props.task.dueDate || props.task.scheduledDate || props.task.startDate || props.task.tags.length > 0}>
            <div class="flex flex-wrap items-center gap-x-3 gap-y-1 mt-1 text-xs text-stone-500 dark:text-stone-400">
              {/* Priority */}
              <Show when={props.task.priority !== "Normal"}>
                <span class={`flex items-center gap-0.5 font-medium ${computed().priorityStyle}`}>
                  {props.task.priority}
                </span>
              </Show>

              {/* Due Date */}
              <Show when={props.task.dueDate}>
                <span class={`flex items-center gap-0.5 ${dateColor(props.task.dueDate!, props.today)}`} title="Due date">
                  {Icons.calendar} {formatDate(props.task.dueDate!)}
                </span>
              </Show>

              {/* Start Date */}
              <Show when={props.task.startDate}>
                <span class="flex items-center gap-0.5 text-purple-600 dark:text-purple-400" title="Start date">
                  {Icons.calendarStart} {formatDate(props.task.startDate!)}
                </span>
              </Show>

              {/* Tags */}
              <Show when={props.task.tags.length > 0}>
                <span class="flex items-center gap-0.5 text-accent-600 dark:text-accent-400">
                  {Icons.tag} {props.task.tags.join(", ")}
                </span>
              </Show>
            </div>
          </Show>
        </div>
      </div>
    </Show>
  );
};
