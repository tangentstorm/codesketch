use anyhow::Result;
use colored::*;
use serde_json;

use crate::types::{Root, Definition, DefType, Visibility};

// Output definitions as JSON
pub fn output_json(root: &Root) -> Result<()> {
    let json = serde_json::to_string_pretty(root)?;
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
        
        // Output top-level definitions and their children
        for def in top_level_defs {
            print_definition(def, &defs, 0);
        }
        
        println!();
    }
    
    Ok(())
}

// Print a definition with proper indentation
fn print_definition(def: &Definition, all_defs: &[Definition], depth: usize) {
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
        DefType::Module => ("module ".cyan(), def.iden.cyan().bold()),
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
        DefType::Other(ref s) => (format!("{} ", s).normal(), def.iden.normal().bold()),
    };
    
    // Format function signature
    let sig_str = if def.def_type == DefType::Function {
        if let Some(sig) = &def.info.signature {
            format!(" {}", sig).dimmed()
        } else {
            "".normal()
        }
    } else {
        "".normal()
    };
    
    // Print the definition
    println!("{}{}{}{}{}{}", line_num, indent, vis_str, type_str, name_str, sig_str);
    
    // Find and print children
    let children: Vec<_> = all_defs.iter()
        .filter(|d| {
            // Include definitions that have this def as parent
            if let Some(parent) = &d.info.parent {
                parent == &def.iden
            } else {
                false
            }
        })
        .collect();
    
    // Sort children by line number
    let mut sorted_children = children.clone();
    sorted_children.sort_by(|a, b| {
        let a_line = a.info.line_num.unwrap_or(0);
        let b_line = b.info.line_num.unwrap_or(0);
        a_line.cmp(&b_line)
    });
    
    // Recursively print children
    for child in sorted_children {
        print_definition(child, all_defs, depth + 1);
    }
}