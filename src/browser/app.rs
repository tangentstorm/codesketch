use std::collections::HashMap;
use crate::types::Root;

/// TreeNode represents a node in our collapsible tree
#[derive(Debug, Clone)]
pub struct TreeNode {
    pub name: String,
    pub def_type: String,
    pub line: u32,
    pub line_end: Option<u32>,
    pub children: Vec<usize>,
    pub parent: Option<usize>,
    pub collapsed: bool,
    pub visible: bool,
    pub indent_level: usize,
    pub path: String,
    pub signature: Option<String>,
    pub visibility: String,
}

/// Application state
pub struct App {
    pub root: Root,
    pub nodes: Vec<TreeNode>,
    pub cursor_position: usize,
    pub scroll_offset: usize,
    pub height: usize,
    
    // Source code viewer
    pub current_file: Option<String>,
    pub file_contents: HashMap<String, Vec<String>>,
    pub highlighted_contents: HashMap<String, Vec<String>>,
}

impl App {
    /// Create a new app with the given code definitions
    pub fn new(root: Root) -> Self {
        let mut app = Self {
            root,
            nodes: Vec::new(),
            cursor_position: 0,
            scroll_offset: 0,
            height: 0,
            current_file: None,
            file_contents: HashMap::new(),
            highlighted_contents: HashMap::new(),
        };
        
        // Build the tree
        app.build_tree();
        
        app
    }
    
    /// Build the tree structure from the root definitions
    fn build_tree(&mut self) {
        // Convert definitions to tree nodes
        let mut node_index = 0;
        
        // Process each file
        for path_info in &self.root {
            let path = path_info.path.clone();
            
            // Sort definitions by line number
            let mut defs = path_info.defs.clone();
            defs.sort_by(|a, b| {
                let a_line = a.info.line_num.unwrap_or(0);
                let b_line = b.info.line_num.unwrap_or(0);
                a_line.cmp(&b_line)
            });
            
            // Create a map from definition name to node index
            let mut name_to_index = HashMap::new();
            
            // First pass: create all nodes
            for def in &defs {
                if let Some(line) = def.info.line_num {
                    let node = TreeNode {
                        name: def.iden.clone(),
                        def_type: format!("{:?}", def.def_type),
                        line,
                        line_end: def.info.line_end,
                        children: Vec::new(),
                        parent: None,
                        collapsed: false,
                        visible: true,
                        indent_level: 0,
                        path: path.clone(),
                        signature: def.info.signature.clone(),
                        visibility: format!("{:?}", def.vis),
                    };
                    
                    name_to_index.insert(def.iden.clone(), node_index);
                    self.nodes.push(node);
                    node_index += 1;
                }
            }
            
            // Second pass: set up parent-child relationships
            for def in &defs {
                if let Some(parent_name) = &def.info.parent {
                    if let Some(&parent_index) = name_to_index.get(parent_name) {
                        if let Some(&child_index) = name_to_index.get(&def.iden) {
                            // Set parent
                            self.nodes[child_index].parent = Some(parent_index);
                            // Add to parent's children
                            self.nodes[parent_index].children.push(child_index);
                        }
                    }
                }
            }
            
            // Load the file content - we'll do this at the end to avoid borrow issues
            if let Ok(content) = std::fs::read_to_string(&path) {
                let lines = content.lines().map(|s| s.to_string()).collect();
                self.file_contents.insert(path.clone(), lines);
            }
        }
        
        // Calculate indent levels
        self.calculate_indent_levels();
        self.update_visibility();
        
        // Set current file if we have any nodes
        if !self.nodes.is_empty() {
            self.current_file = Some(self.nodes[0].path.clone());
        }
        
        // Now preprocess syntax highlighting for all loaded files
        for path in self.file_contents.keys().cloned().collect::<Vec<_>>() {
            self.preprocess_syntax_highlighting(&path);
        }
    }
    
    /// Calculate indent levels for all nodes
    fn calculate_indent_levels(&mut self) {
        for i in 0..self.nodes.len() {
            let mut level = 0;
            let mut current = i;
            
            while let Some(parent) = self.nodes[current].parent {
                level += 1;
                current = parent;
            }
            
            self.nodes[i].indent_level = level;
        }
    }
    
    /// Update visibility based on collapsed state
    fn update_visibility(&mut self) {
        // First, mark all nodes as visible
        for node in &mut self.nodes {
            node.visible = true;
        }
        
        // Then hide children of collapsed nodes
        for i in 0..self.nodes.len() {
            if self.nodes[i].collapsed {
                self.hide_children(i);
            }
        }
    }
    
    /// Hide all children of the given node recursively
    fn hide_children(&mut self, index: usize) {
        for &child_index in self.nodes[index].children.clone().iter() {
            self.nodes[child_index].visible = false;
            self.hide_children(child_index);
        }
    }
    
    /// Move cursor to the next visible item
    pub fn next(&mut self) {
        let mut next_pos = self.cursor_position + 1;
        while next_pos < self.nodes.len() && !self.nodes[next_pos].visible {
            next_pos += 1;
        }
        
        if next_pos < self.nodes.len() {
            self.cursor_position = next_pos;
            self.adjust_scroll();
        }
    }
    
    /// Move cursor to the previous visible item
    pub fn previous(&mut self) {
        if self.cursor_position > 0 {
            let mut prev_pos = self.cursor_position - 1;
            while prev_pos > 0 && !self.nodes[prev_pos].visible {
                prev_pos -= 1;
            }
            
            if self.nodes[prev_pos].visible {
                self.cursor_position = prev_pos;
                self.adjust_scroll();
            }
        }
    }
    
    /// Toggle collapsed state of the current node
    pub fn toggle_collapsed(&mut self) {
        let index = self.cursor_position;
        if !self.nodes[index].children.is_empty() {
            self.nodes[index].collapsed = !self.nodes[index].collapsed;
            self.update_visibility();
        }
    }
    
    /// Select the current node (does nothing for now)
    pub fn select(&mut self) {
        // Just update the current file
        let current_path = self.nodes[self.cursor_position].path.clone();
        self.current_file = Some(current_path);
    }
    
    /// Go back in history (not implemented yet)
    pub fn back(&mut self) {
        // Not implemented
    }
    
    /// Go forward in history (not implemented yet)
    pub fn forward(&mut self) {
        // Not implemented
    }
    
    /// Tick function for animations (not used yet)
    pub fn tick(&mut self) {
        // Not implemented
    }
    
    /// Adjust scroll to keep cursor in view
    fn adjust_scroll(&mut self) {
        if self.cursor_position < self.scroll_offset {
            self.scroll_offset = self.cursor_position;
        } else if self.cursor_position >= self.scroll_offset + self.height {
            self.scroll_offset = self.cursor_position - self.height + 1;
        }
    }
    
    /// Set the UI height for scrolling calculations
    pub fn set_ui_height(&mut self, height: usize) {
        self.height = height;
        self.adjust_scroll();
    }
    
    /// Preprocess syntax highlighting for a file
    fn preprocess_syntax_highlighting(&mut self, path: &str) {
        // We'll implement this with syntect later
        // For now, just copy the content
        if let Some(content) = self.file_contents.get(path) {
            self.highlighted_contents.insert(path.to_string(), content.clone());
        }
    }
    
    /// Get source code for the currently selected node
    pub fn get_current_source(&self) -> Vec<String> {
        let node = &self.nodes[self.cursor_position];
        let path = &node.path;
        
        if let Some(content) = self.file_contents.get(path) {
            let start_line = node.line as usize - 1; // 0-indexed
            let end_line = node.line_end.map(|l| l as usize).unwrap_or_else(|| {
                // If no end line, show 10 lines or until end of file
                let mut end = start_line + 10;
                if end >= content.len() {
                    end = content.len();
                }
                end
            });
            
            // Extract the relevant lines
            content.iter()
                .skip(start_line)
                .take(end_line - start_line)
                .cloned()
                .collect()
        } else {
            vec!["File not loaded".to_string()]
        }
    }
}