use anyhow::Result;
use colored::*;

use crate::types::{Root, Definition, DefType, Visibility};

// Output definitions as JSON
pub fn output_json(root: &Root) -> Result<()> {
    let json = ::serde_json::to_string_pretty(root)?;
    println!("{}", json);
    Ok(())
}

// Output definitions as text with colorful formatting
pub fn output_text(root: &Root) -> Result<()> {
    for path_info in root {
        println!("{}", path_info.path.blue().bold());
        
        // Sort definitions by line number
        let mut defs = path_info.defs.clone();
        defs.sort_by(|a, b| {
            let a_line = a.info.line_num.unwrap_or(0);
            let b_line = b.info.line_num.unwrap_or(0);
            a_line.cmp(&b_line)
        });
        
        // Filter top-level definitions (no parent)
        // Also filter out functions that are methods of impl blocks
        let top_level_defs: Vec<_> = defs.iter()
            .filter(|d| {
                // Keep if no parent
                if d.info.parent.is_none() {
                    // For functions, skip those that belong to impl blocks
                    if d.def_type == DefType::Function {
                        // Check if this function is a method of any impl block
                        !defs.iter().any(|impl_def| 
                            impl_def.def_type == DefType::Impl && 
                            impl_def.info.children.contains(&d.iden)
                        )
                    } else {
                        true
                    }
                } else {
                    false
                }
            })
            .collect();
        
        // Generate all lines with their indentation level and then sort
        let mut lines = Vec::new();
        for def in top_level_defs {
            collect_definition_lines(def, &defs, 0, &mut lines);
        }
        
        // Sort by line number and remove duplicates
        lines.sort_by(|a, b| a.0.cmp(&b.0));
        lines.dedup();
        
        // Print all lines
        for (_, line) in lines {
            println!("{}", line);
        }
        
        println!();
    }
    
    Ok(())
}

// Collect all output lines for a definition
fn collect_definition_lines(def: &Definition, all_defs: &[Definition], depth: usize, lines: &mut Vec<(u32, String)>) {
    let indent = "  ".repeat(depth);
    
    // Format line number
    let line_num = match def.info.line_num {
        Some(num) => format!("{:4} ", num),
        None => "     ".to_string(),
    };
    
    // Format visibility
    let vis_str = match def.vis {
        Visibility::Public => "pub ".green(),
        Visibility::Protected => "protected ".yellow(),
        Visibility::Private => "".normal(),
    };
    
    // Format type and name
    let (type_str, name_str) = match def.def_type {
        DefType::Module => ("mod ".cyan(), def.iden.cyan().bold()),
        DefType::Struct => ("struct ".green(), def.iden.green().bold()),
        DefType::Enum => ("enum ".green(), def.iden.green().bold()),
        DefType::Function => ("fn ".yellow(), def.iden.normal().bold()),
        DefType::Trait => ("trait ".magenta(), def.iden.magenta().bold()),
        DefType::Impl => {
            // Special case for impl blocks
            if let Some(sig) = &def.info.signature {
                ("impl ".blue().bold(), sig.cyan())
            } else {
                ("impl ".blue().bold(), def.iden.cyan())
            }
        },
        DefType::Macro => ("macro ".red(), def.iden.red().bold()),
        DefType::Constant => ("const ".yellow(), def.iden.yellow().bold()),
        DefType::TypeAlias => ("type ".cyan(), def.iden.cyan().bold()),
        DefType::Field => {
            if let Some(sig) = &def.info.signature {
                ("".normal(), format!("{}: {}", def.iden, sig).normal())
            } else {
                ("".normal(), def.iden.normal())
            }
        },
        DefType::Method => ("fn ".yellow().dimmed(), def.iden.normal()),
        DefType::Other(ref s) => (format!("{} ", s).normal(), def.iden.normal().bold()),
    };
    
    // Format signature for various types
    let sig_str = match def.def_type {
        DefType::Function | DefType::Method => {
            if let Some(sig) = &def.info.signature {
                format!(" {}", sig).dimmed()
            } else {
                "".normal()
            }
        },
        DefType::TypeAlias | DefType::Constant => {
            if let Some(sig) = &def.info.signature {
                format!(" {}", sig).dimmed()
            } else {
                "".normal()
            }
        },
        _ => "".normal()
    };
    
    // Format full line
    let formatted_line = format!("{}{}{}{}{}{}", line_num, indent, vis_str, type_str, name_str, sig_str);
    
    // Add to lines collection with line number for sorting
    if let Some(line_number) = def.info.line_num {
        lines.push((line_number, formatted_line));
    }
    
    // Find and collect children
    let children: Vec<_> = all_defs.iter()
        .filter(|d| {
            // Include definitions that have this def as parent
            if let Some(parent) = &d.info.parent {
                // Basic parent check
                if parent == &def.iden {
                    // For impl blocks, also check line range
                    if def.def_type == DefType::Impl {
                        if let (Some(start), Some(end)) = (def.info.line_num, def.info.line_end) {
                            if let Some(child_line) = d.info.line_num {
                                // Only include methods that are within this impl block's line range
                                return child_line >= start && child_line <= end;
                            }
                        }
                    }
                    return true;
                }
            }
            false
        })
        .collect();
    
    // Sort children by line number
    let mut sorted_children = children.clone();
    sorted_children.sort_by(|a, b| {
        let a_line = a.info.line_num.unwrap_or(0);
        let b_line = b.info.line_num.unwrap_or(0);
        a_line.cmp(&b_line)
    });
    
    // Recursively collect children lines
    for child in sorted_children {
        collect_definition_lines(child, all_defs, depth + 1, lines);
    }
}

