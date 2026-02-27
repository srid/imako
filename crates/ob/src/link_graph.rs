//! Directed graph of note references and backlink queries.
//!
//! Uses `petgraph` for efficient graph operations.
//! Edges represent resolved wikilinks: source → target.

use std::collections::{HashMap, HashSet};

use petgraph::graph::{DiGraph, NodeIndex};

use crate::note::Note;

/// A directed graph of wikilink references between notes.
#[derive(Debug, Clone)]
pub struct LinkGraph {
    graph: DiGraph<String, ()>,
    node_indices: HashMap<String, NodeIndex>,
}

impl LinkGraph {
    /// Build a link graph from a set of notes.
    pub fn build(notes: &[Note]) -> Self {
        let mut graph = DiGraph::new();
        let mut node_indices = HashMap::new();

        // Add all notes as nodes
        for note in notes {
            let idx = graph.add_node(note.path.clone());
            node_indices.insert(note.path.clone(), idx);
        }

        // Add edges for wikilinks
        for note in notes {
            let source_idx = node_indices[&note.path];
            let targets = extract_wikilink_targets(&note.content, notes);
            for target_path in targets {
                if let Some(&target_idx) = node_indices.get(&target_path) {
                    graph.add_edge(source_idx, target_idx, ());
                }
            }
        }

        LinkGraph {
            graph,
            node_indices,
        }
    }

    /// Get notes that link TO the given path (backlinks).
    pub fn backlinks_of(&self, path: &str) -> Vec<String> {
        let Some(&target_idx) = self.node_indices.get(path) else {
            return Vec::new();
        };

        let mut backlinks: Vec<String> = self
            .graph
            .neighbors_directed(target_idx, petgraph::Direction::Incoming)
            .map(|idx| self.graph[idx].clone())
            .collect();
        backlinks.sort();
        backlinks
    }

    /// Get notes that the given path links TO (forward links).
    pub fn forward_links_of(&self, path: &str) -> Vec<String> {
        let Some(&source_idx) = self.node_indices.get(path) else {
            return Vec::new();
        };

        self.graph
            .neighbors_directed(source_idx, petgraph::Direction::Outgoing)
            .map(|idx| self.graph[idx].clone())
            .collect()
    }
}

/// Extract wikilink targets from markdown content, resolved against the note index.
fn extract_wikilink_targets(content: &str, notes: &[Note]) -> Vec<String> {
    let mut targets = HashSet::new();

    // Simple regex-free wikilink extraction: find [[...]] patterns
    let mut i = 0;
    let bytes = content.as_bytes();
    while i + 1 < bytes.len() {
        if bytes[i] == b'[' && bytes[i + 1] == b'[' {
            // Found opening [[
            if let Some(end) = content[i + 2..].find("]]") {
                let inner = &content[i + 2..i + 2 + end];
                // Get target (before pipe if there is one)
                let target = inner.split('|').next().unwrap_or(inner).trim();
                if !target.is_empty() {
                    if let Some(resolved) = crate::link::resolve_wikilink(target, notes) {
                        targets.insert(resolved);
                    }
                }
                i = i + 2 + end + 2;
            } else {
                i += 2;
            }
        } else {
            i += 1;
        }
    }

    targets.into_iter().collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_link_graph() {
        let notes = vec![
            Note::from_string("A.md", "Links to [[B]] and [[C]]"),
            Note::from_string("B.md", "Links to [[A]]"),
            Note::from_string("C.md", "No links"),
        ];

        let graph = LinkGraph::build(&notes);

        // A links to B and C
        let a_links = graph.forward_links_of("A.md");
        assert!(a_links.contains(&"B.md".to_string()));
        assert!(a_links.contains(&"C.md".to_string()));

        // B's backlinks include A
        let b_backlinks = graph.backlinks_of("B.md");
        assert!(b_backlinks.contains(&"A.md".to_string()));

        // C's backlinks include A
        let c_backlinks = graph.backlinks_of("C.md");
        assert!(c_backlinks.contains(&"A.md".to_string()));

        // A's backlinks include B
        let a_backlinks = graph.backlinks_of("A.md");
        assert!(a_backlinks.contains(&"B.md".to_string()));
    }
}
