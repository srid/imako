import { Component, Show } from "solid-js";
import type { Task } from "@/types";
import { Icons } from "@/utils/icons";
import { formatDate, dateColor } from "@/utils/dates";
import { isTaskVisible } from "@/state/filters";

export const TaskItem: Component<{ task: Task; today: string }> = (props) => {
  const statusIcon = () => {
    switch (props.task.status) {
      case "Completed":
        return Icons.checkSquare;
      case "Cancelled":
        return Icons.cancelSquare;
      case "InProgress":
        return Icons.halfSquare;
      default:
        return Icons.emptySquare;
    }
  };

  const statusColor = () => {
    switch (props.task.status) {
      case "Completed":
      case "Cancelled":
        return "text-gray-400";
      case "InProgress":
        return "text-amber-500";
      default:
        return "text-gray-400 hover:text-gray-600";
    }
  };

  const textStyle = () => {
    if (props.task.status === "Completed" || props.task.status === "Cancelled") {
      return "line-through text-gray-400 dark:text-gray-500";
    }
    return "text-gray-900 dark:text-gray-100";
  };

  const priorityStyle = () => {
    switch (props.task.priority) {
      case "Highest":
        return "text-red-600 dark:text-red-400";
      case "High":
        return "text-orange-600 dark:text-orange-400";
      case "Medium":
        return "text-amber-600 dark:text-amber-400";
      case "Low":
        return "text-blue-600 dark:text-blue-400";
      case "Lowest":
        return "text-slate-500 dark:text-slate-400";
      default:
        return "";
    }
  };

  const visible = () => isTaskVisible(props.task, props.today);

  return (
    <Show when={visible()}>
      <div class="group/task relative py-1 -mx-2 px-2 rounded hover:bg-gray-50 dark:hover:bg-gray-800/50 flex items-start gap-2 text-sm transition-colors">
        {/* Checkbox */}
        <span class={`w-5 h-5 flex-shrink-0 flex items-center justify-center ${statusColor()}`}>
          {statusIcon()}
        </span>

        {/* Content */}
        <div class="flex-1 min-w-0">
          <div class={`leading-snug ${textStyle()}`}>{props.task.description}</div>

          {/* Metadata row */}
          <Show when={props.task.priority !== "Normal" || props.task.dueDate || props.task.scheduledDate || props.task.startDate || props.task.tags.length > 0}>
            <div class="flex flex-wrap items-center gap-x-3 gap-y-1 mt-0.5 text-xs text-gray-400 dark:text-gray-500">
              {/* Priority */}
              <Show when={props.task.priority !== "Normal"}>
                <span class={`flex items-center gap-0.5 font-medium ${priorityStyle()}`}>
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
                <span class="flex items-center gap-0.5 text-indigo-500 dark:text-indigo-400">
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
