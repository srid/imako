//! Task item component.

use dioxus::prelude::*;
use ob::task::{Task, TaskStatus};

/// Renders a single task item.
#[component]
pub fn TaskItem(task: Task, today: String) -> Element {
    let status_class = match task.status {
        TaskStatus::Completed => "line-through text-stone-400 dark:text-stone-500",
        TaskStatus::InProgress => "text-amber-600 dark:text-amber-400",
        TaskStatus::Cancelled => "line-through text-stone-400 dark:text-stone-500",
        TaskStatus::Incomplete => "text-stone-700 dark:text-stone-200",
    };

    let checkbox = match task.status {
        TaskStatus::Incomplete => "☐",
        TaskStatus::InProgress => "◐",
        TaskStatus::Cancelled => "⊘",
        TaskStatus::Completed => "☑",
    };

    let has_due = task.due_date.is_some();
    let due_class = if has_due {
        let due = task.due_date.unwrap();
        let today_date = chrono::NaiveDate::parse_from_str(&today, "%Y-%m-%d").ok();
        if today_date.is_some_and(|t| due <= t) {
            "text-red-500 dark:text-red-400"
        } else {
            "text-stone-400 dark:text-stone-500"
        }
    } else {
        ""
    };

    rsx! {
        div {
            class: "py-1 flex items-start gap-2 text-sm {status_class}",
            "data-testid": "task-item",
            span { class: "flex-shrink-0 mt-0.5 text-stone-400 dark:text-stone-500",
                "{checkbox}"
            }
            span { class: "flex-1 min-w-0",
                span { "{task.description}" }
                // Breadcrumbs
                if !task.parent_breadcrumbs.is_empty() {
                    span { class: "text-xs text-stone-400 dark:text-stone-500 ml-2",
                        "({task.parent_breadcrumbs.join(\" › \")})"
                    }
                }
            }
            // Due date badge
            if let Some(due) = task.due_date {
                span { class: "flex-shrink-0 text-xs {due_class}",
                    "📅 {due}"
                }
            }
            // Priority badge
            {
                let priority_emoji = match task.priority {
                    ob::task::Priority::Highest => Some("🔺"),
                    ob::task::Priority::High => Some("⏫"),
                    ob::task::Priority::Medium => Some("🔼"),
                    ob::task::Priority::Low => Some("🔽"),
                    ob::task::Priority::Lowest => Some("⏬"),
                    ob::task::Priority::Normal => None,
                };
                if let Some(emoji) = priority_emoji {
                    rsx! {
                        span { class: "flex-shrink-0 text-xs", "{emoji}" }
                    }
                } else {
                    rsx! {}
                }
            }
        }
    }
}
