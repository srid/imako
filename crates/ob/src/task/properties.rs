//! Task properties parsing: dates, priority, tags from obsidian-tasks metadata.
//!
//! Parses emoji-prefixed metadata from task text:
//! - 📅 due date, ⏳ scheduled date, 🛫 start date, ✅ completed date
//! - Priority: 🔺 highest, ⏫ high, 🔼 medium, 🔽 low, ⏬ lowest
//! - Tags: #tag-name

use chrono::NaiveDate;

use crate::task::Priority;

/// Parsed task properties from inline metadata.
#[derive(Debug, Clone, Default)]
pub struct TaskProperties {
    pub description: String,
    pub start_date: Option<NaiveDate>,
    pub scheduled_date: Option<NaiveDate>,
    pub due_date: Option<NaiveDate>,
    pub completed_date: Option<NaiveDate>,
    pub priority: Priority,
    pub tags: Vec<String>,
}

/// Parse task metadata from the text following the checkbox marker.
///
/// Extracts dates (emoji-prefixed), priority, and tags.
/// Returns the clean description (with metadata stripped) and parsed properties.
pub fn parse_task_text(text: &str) -> TaskProperties {
    let mut props = TaskProperties::default();
    let mut clean_parts: Vec<&str> = Vec::new();
    let words: Vec<&str> = text.split_whitespace().collect();
    let mut i = 0;

    while i < words.len() {
        let word = words[i];

        // Check for date emoji + date pair
        if i + 1 < words.len() {
            if let Some(date) = try_parse_date_emoji(word, words[i + 1]) {
                match word {
                    "🛫" => props.start_date = Some(date),
                    "⏳" => props.scheduled_date = Some(date),
                    "📅" => props.due_date = Some(date),
                    "✅" => props.completed_date = Some(date),
                    _ => {}
                }
                i += 2;
                continue;
            }
        }

        // Check for recurrence (skip it and its text)
        if word == "🔁" {
            // Skip recurrence text until next date emoji or end
            i += 1;
            while i < words.len() && !is_date_emoji(words[i]) {
                i += 1;
            }
            continue;
        }

        // Check for priority emoji
        if let Some(p) = parse_priority(word) {
            props.priority = p;
            i += 1;
            continue;
        }

        // Check for tag
        if let Some(tag) = parse_tag(word) {
            props.tags.push(tag);
            i += 1;
            continue;
        }

        // Regular word — keep in description
        clean_parts.push(word);
        i += 1;
    }

    props.description = clean_parts.join(" ");
    props
}

fn is_date_emoji(s: &str) -> bool {
    matches!(s, "🛫" | "⏳" | "📅" | "✅")
}

fn try_parse_date_emoji(emoji: &str, date_str: &str) -> Option<NaiveDate> {
    if !is_date_emoji(emoji) {
        return None;
    }
    NaiveDate::parse_from_str(date_str, "%Y-%m-%d").ok()
}

/// Parse priority emoji.
pub fn parse_priority(s: &str) -> Option<Priority> {
    match s {
        "🔺" => Some(Priority::Highest),
        "⏫" => Some(Priority::High),
        "🔼" => Some(Priority::Medium),
        "🔽" => Some(Priority::Low),
        "⏬" => Some(Priority::Lowest),
        _ => None,
    }
}

/// Parse a hashtag (e.g. "#work" → "work").
pub fn parse_tag(s: &str) -> Option<String> {
    if s.starts_with('#') && s.len() > 1 {
        Some(s[1..].to_string())
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_task() {
        let props = parse_task_text("Buy groceries");
        assert_eq!(props.description, "Buy groceries");
        assert_eq!(props.priority, Priority::Normal);
        assert!(props.tags.is_empty());
    }

    #[test]
    fn test_parse_task_with_dates() {
        let props = parse_task_text("Fix bug 📅 2025-03-01 🛫 2025-02-15");
        assert_eq!(props.description, "Fix bug");
        assert_eq!(
            props.due_date,
            Some(NaiveDate::from_ymd_opt(2025, 3, 1).unwrap())
        );
        assert_eq!(
            props.start_date,
            Some(NaiveDate::from_ymd_opt(2025, 2, 15).unwrap())
        );
    }

    #[test]
    fn test_parse_task_with_priority() {
        let props = parse_task_text("Important task ⏫");
        assert_eq!(props.description, "Important task");
        assert_eq!(props.priority, Priority::High);
    }

    #[test]
    fn test_parse_task_with_tags() {
        let props = parse_task_text("Task #work #urgent");
        assert_eq!(props.description, "Task");
        assert_eq!(props.tags, vec!["work", "urgent"]);
    }

    #[test]
    fn test_parse_full_task() {
        let props =
            parse_task_text("Deploy release ⏫ #devops 📅 2025-03-15 🛫 2025-03-10 ⏳ 2025-03-12");
        assert_eq!(props.description, "Deploy release");
        assert_eq!(props.priority, Priority::High);
        assert_eq!(props.tags, vec!["devops"]);
        assert_eq!(
            props.due_date,
            Some(NaiveDate::from_ymd_opt(2025, 3, 15).unwrap())
        );
        assert_eq!(
            props.start_date,
            Some(NaiveDate::from_ymd_opt(2025, 3, 10).unwrap())
        );
        assert_eq!(
            props.scheduled_date,
            Some(NaiveDate::from_ymd_opt(2025, 3, 12).unwrap())
        );
    }
}
