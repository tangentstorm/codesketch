use ratatui::{
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span, Text},
    widgets::{Block, Borders, List, ListItem, Paragraph, Wrap},
    Frame,
};
use syntect::{
    highlighting::{Theme, ThemeSet},
    parsing::{SyntaxReference, SyntaxSet},
};
use std::sync::Once;

use super::App;

// Globals for syntax highlighting
static INIT: Once = Once::new();
static mut SYNTAX_SET: Option<SyntaxSet> = None;
static mut THEME_SET: Option<ThemeSet> = None;

/// Lazily initialize syntax highlighting
fn initialize_highlighting() {
    unsafe {
        INIT.call_once(|| {
            SYNTAX_SET = Some(SyntaxSet::load_defaults_newlines());
            THEME_SET = Some(ThemeSet::load_defaults());
        });
    }
}

/// Draw the UI layout
pub fn draw_ui(f: &mut Frame, app: &mut App) {
    initialize_highlighting();
    
    // Create a layout with two columns: left for tree view, right for source code
    let size = f.size();
    let chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Percentage(35),
            Constraint::Percentage(65),
        ])
        .split(size);
    
    // Draw the code outline tree on the left
    draw_tree(f, app, chunks[0]);
    
    // Draw the source code on the right
    draw_source_code(f, app, chunks[1]);
}

/// Draw the code outline tree widget
fn draw_tree(f: &mut Frame, app: &mut App, area: Rect) {
    // Create a block for the outline
    let block = Block::default()
        .title("Code Outline (↑↓: navigate, Tab: expand/collapse)")
        .borders(Borders::ALL);
    
    // Calculate available height for items
    let inner_area = block.inner(area);
    app.set_ui_height(inner_area.height as usize);
    
    // Create list items for visible nodes
    let items: Vec<ListItem> = app.nodes.iter()
        .enumerate()
        .filter(|(_, node)| node.visible)
        .skip(app.scroll_offset)
        .take(inner_area.height as usize)
        .map(|(i, node)| {
            // Create indent based on level
            let indent = "  ".repeat(node.indent_level);
            let indent_str = format!("{}", indent);
            
            // Create collapse indicator
            let collapse_indicator = if !node.children.is_empty() {
                if node.collapsed { "▶ " } else { "▼ " }
            } else {
                "  "
            };
            
            // Format name with optional signatures and types
            let display_name = match node.def_type.as_str() {
                "Function" | "Method" => {
                    if let Some(sig) = &node.signature {
                        format!("fn {} {}", node.name, sig)
                    } else {
                        format!("fn {}", node.name)
                    }
                },
                "Struct" => format!("struct {}", node.name),
                "Trait" => format!("trait {}", node.name),
                "Impl" => {
                    if let Some(sig) = &node.signature {
                        format!("impl {}", sig)
                    } else {
                        format!("impl {}", node.name)
                    }
                },
                "Field" => {
                    if let Some(sig) = &node.signature {
                        format!("{}: {}", node.name, sig)
                    } else {
                        node.name.clone()
                    }
                },
                _ => {
                    format!("{} {}", node.def_type.to_lowercase(), node.name)
                }
            };
            
            // Format line number
            let line_indicator = format!("{:4} ", node.line);
            
            // Combine all parts
            let mut spans = vec![
                Span::raw(line_indicator),
                Span::raw(indent_str),
                Span::raw(collapse_indicator),
            ];
            
            // Add visibility if present and not private
            if node.visibility == "Public" {
                spans.push(Span::styled(
                    "pub ".to_string(),
                    Style::default().fg(Color::Green)
                ));
            }
            
            // Add the name with appropriate styling based on type
            let name_style = match node.def_type.as_str() {
                "Function" | "Method" => Style::default().fg(Color::Yellow),
                "Struct" | "Field" => Style::default().fg(Color::Green),
                "Trait" => Style::default().fg(Color::Magenta),
                "Impl" => Style::default().fg(Color::Cyan),
                "Module" => Style::default().fg(Color::Blue),
                "Macro" => Style::default().fg(Color::Red),
                _ => Style::default(),
            };
            
            spans.push(Span::styled(display_name, name_style));
            
            // Highlight if this is the current position
            let style = if i == app.cursor_position {
                Style::default().bg(Color::DarkGray)
            } else {
                Style::default()
            };
            
            ListItem::new(Line::from(spans)).style(style)
        })
        .collect();
    
    // Create the list widget
    let list = List::new(items)
        .block(block)
        .highlight_style(Style::default().add_modifier(Modifier::BOLD));
    
    // Render the list
    f.render_widget(list, area);
}

/// Draw the source code viewer widget
fn draw_source_code(f: &mut Frame, app: &App, area: Rect) {
    // Create a block for the source code
    let title = match app.current_file {
        Some(ref path) => format!("Source Code: {} (q: quit)", path),
        None => "Source Code".to_string(),
    };
    
    let block = Block::default()
        .title(title)
        .borders(Borders::ALL);
    
    // Get source code for the current node
    let source = app.get_current_source();
    
    // Create text with line numbers
    let mut line_spans = Vec::new();
    let start_line = if app.cursor_position < app.nodes.len() {
        app.nodes[app.cursor_position].line as usize
    } else {
        1
    };
    
    for (i, line) in source.iter().enumerate() {
        let line_num = start_line + i;
        let line_prefix = format!("{:4} | ", line_num);
        
        // Apply simple syntax highlighting (full syntect implementation would be better)
        let highlighted_spans = simple_highlight_rust(line);
        
        // Add line number prefix
        let mut spans = vec![
            Span::styled(line_prefix, Style::default().fg(Color::DarkGray)),
        ];
        
        // Add highlighted content
        spans.extend(highlighted_spans);
        
        line_spans.push(Line::from(spans));
    }
    
    // Create a paragraph with line-wrapped text
    let paragraph = Paragraph::new(Text::from(line_spans))
        .block(block)
        .wrap(Wrap { trim: false });
    
    // Render the source code
    f.render_widget(paragraph, area);
}

/// Apply simple syntax highlighting for Rust code
fn simple_highlight_rust(line: &str) -> Vec<Span> {
    let mut spans = Vec::new();
    let mut current = String::new();
    
    // Simple tokenization for highlighting
    let mut in_string = false;
    let mut in_comment = false;
    
    for (i, c) in line.chars().enumerate() {
        // Check for comment start
        if !in_string && !in_comment && c == '/' && i + 1 < line.len() {
            if line.chars().nth(i + 1) == Some('/') {
                // If we have accumulated text, add it
                if !current.is_empty() {
                    spans.push(Span::raw(current.clone()));
                    current.clear();
                }
                in_comment = true;
                current.push(c);
                continue;
            }
        }
        
        // Check for string boundaries
        if !in_comment && c == '"' && (i == 0 || line.chars().nth(i - 1) != Some('\\')) {
            // End current span
            if !current.is_empty() {
                let style = if in_string {
                    Style::default().fg(Color::Green)
                } else {
                    Style::default()
                };
                spans.push(Span::styled(current.clone(), style));
                current.clear();
            }
            
            // Add the quote
            current.push(c);
            
            // Toggle string state
            in_string = !in_string;
            
            // If we just ended a string, add it as a span
            if !in_string {
                spans.push(Span::styled(current.clone(), Style::default().fg(Color::Green)));
                current.clear();
            }
            
            continue;
        }
        
        // Add character to current chunk
        current.push(c);
        
        // Handle keywords
        if !in_string && !in_comment {
            let keywords = ["fn", "pub", "struct", "enum", "impl", "trait", "mod", "use", 
                           "let", "mut", "const", "static", "if", "else", "match", "for", 
                           "while", "loop", "return", "self", "Self", "super", "where"];
                           
            for keyword in keywords {
                if current == keyword {
                    // Complete keyword found
                    spans.push(Span::styled(current.clone(), Style::default().fg(Color::Yellow)));
                    current.clear();
                    break;
                }
            }
        }
    }
    
    // Add any remaining text
    if !current.is_empty() {
        let style = if in_comment {
            Style::default().fg(Color::DarkGray)
        } else if in_string {
            Style::default().fg(Color::Green)
        } else {
            Style::default()
        };
        
        spans.push(Span::styled(current, style));
    }
    
    spans
}

/// Apply syntax highlighting to text (not implemented yet)
fn _syntax_highlight<'a>(text: &'a str, syntax: &SyntaxReference, theme: &Theme) -> Vec<Line<'a>> {
    // This would be implemented with syntect
    // For now, just return plain text
    text.lines()
        .map(|line| Line::from(Span::raw(line.to_string())))
        .collect()
}