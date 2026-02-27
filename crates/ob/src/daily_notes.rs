//! Daily notes configuration and matching.
//!
//! Reads configuration from `.obsidian/daily-notes.json` and matches
//! notes to dates based on folder/format config.

use std::path::Path;

use chrono::NaiveDate;
use serde::{Deserialize, Serialize};

/// Configuration for Obsidian's daily notes plugin.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DailyNotesConfig {
    /// Folder where daily notes are stored (relative to vault root). Defaults to ".".
    #[serde(default = "default_folder")]
    pub folder: String,
    /// Date format string (moment.js style, e.g., "YYYY-MM-DD"). Defaults to "YYYY-MM-DD".
    #[serde(default = "default_format")]
    pub format: String,
}

fn default_folder() -> String {
    ".".to_string()
}

fn default_format() -> String {
    "YYYY-MM-DD".to_string()
}

/// A daily note with its associated date.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DailyNote {
    pub day: NaiveDate,
    pub note_path: String,
}

/// Load daily notes configuration from the vault.
///
/// Returns None if the config file doesn't exist.
pub fn load_daily_notes_config(vault_path: &Path) -> Option<DailyNotesConfig> {
    let config_path = vault_path.join(".obsidian").join("daily-notes.json");
    if !config_path.exists() {
        return None;
    }

    let content = std::fs::read_to_string(&config_path).ok()?;
    serde_json::from_str(&content).ok()
}

/// Check if a file path matches the daily notes pattern.
pub fn is_daily_note(config: &DailyNotesConfig, path: &str) -> bool {
    let parent = path_parent(path);
    parent == config.folder && parse_daily_note_date(config, path).is_some()
}

/// Try to construct a DailyNote from a file path.
pub fn mk_daily_note(config: &DailyNotesConfig, path: &str) -> Option<DailyNote> {
    if !is_daily_note(config, path) {
        return None;
    }
    let day = parse_daily_note_date(config, path)?;
    Some(DailyNote {
        day,
        note_path: path.to_string(),
    })
}

/// Parse the date from a daily note filename.
pub fn parse_daily_note_date(config: &DailyNotesConfig, path: &str) -> Option<NaiveDate> {
    let basename = path_stem(path);
    let chrono_format = moment_to_chrono_format(&config.format);
    NaiveDate::parse_from_str(&basename, &chrono_format).ok()
}

/// Get the expected file path for a given day's daily note.
pub fn get_today_note_path(config: &DailyNotesConfig, day: NaiveDate) -> String {
    let chrono_format = moment_to_chrono_format(&config.format);
    let filename = day.format(&chrono_format).to_string() + ".md";
    if config.folder == "." {
        filename
    } else {
        format!("{}/{}", config.folder, filename)
    }
}

/// Convert moment.js date format to chrono's strftime format.
pub fn moment_to_chrono_format(moment_format: &str) -> String {
    let mut result = String::new();
    let chars: Vec<char> = moment_format.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        if i + 3 < chars.len() && &moment_format[i..i + 4] == "YYYY" {
            result.push_str("%Y");
            i += 4;
        } else if i + 1 < chars.len() && &moment_format[i..i + 2] == "YY" {
            result.push_str("%y");
            i += 2;
        } else if i + 3 < chars.len() && &moment_format[i..i + 4] == "MMMM" {
            result.push_str("%B");
            i += 4;
        } else if i + 2 < chars.len() && &moment_format[i..i + 3] == "MMM" {
            result.push_str("%b");
            i += 3;
        } else if i + 1 < chars.len() && &moment_format[i..i + 2] == "MM" {
            result.push_str("%m");
            i += 2;
        } else if i + 1 < chars.len() && &moment_format[i..i + 2] == "DD" {
            result.push_str("%d");
            i += 2;
        } else if chars[i] == 'D' {
            result.push_str("%-d");
            i += 1;
        } else {
            result.push(chars[i]);
            i += 1;
        }
    }

    result
}

/// Get the parent directory of a path string.
fn path_parent(path: &str) -> String {
    match path.rfind('/') {
        Some(idx) => path[..idx].to_string(),
        None => ".".to_string(),
    }
}

/// Get the file stem (name without extension) of a path string.
fn path_stem(path: &str) -> String {
    let filename = match path.rfind('/') {
        Some(idx) => &path[idx + 1..],
        None => path,
    };
    match filename.rfind('.') {
        Some(idx) => filename[..idx].to_string(),
        None => filename.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_moment_to_chrono_format() {
        assert_eq!(moment_to_chrono_format("YYYY-MM-DD"), "%Y-%m-%d");
        assert_eq!(moment_to_chrono_format("DD-MM-YYYY"), "%d-%m-%Y");
    }

    #[test]
    fn test_daily_note_matching() {
        let config = DailyNotesConfig {
            folder: "Daily".to_string(),
            format: "YYYY-MM-DD".to_string(),
        };
        assert!(is_daily_note(&config, "Daily/2025-02-27.md"));
        assert!(!is_daily_note(&config, "Notes/2025-02-27.md"));
        assert!(!is_daily_note(&config, "Daily/random-note.md"));
    }

    #[test]
    fn test_parse_daily_note_date() {
        let config = DailyNotesConfig {
            folder: "Daily".to_string(),
            format: "YYYY-MM-DD".to_string(),
        };
        let date = parse_daily_note_date(&config, "Daily/2025-02-27.md");
        assert_eq!(date, Some(NaiveDate::from_ymd_opt(2025, 2, 27).unwrap()));
    }

    #[test]
    fn test_get_today_note_path() {
        let config = DailyNotesConfig {
            folder: "Daily".to_string(),
            format: "YYYY-MM-DD".to_string(),
        };
        let day = NaiveDate::from_ymd_opt(2025, 2, 27).unwrap();
        assert_eq!(get_today_note_path(&config, day), "Daily/2025-02-27.md");
    }
}
