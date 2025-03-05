use anyhow::{Result, anyhow};
use std::path::{Path};
use std::fs;
use tree_sitter::{Parser, Query, QueryCursor, Node};
use walkdir::WalkDir;

use crate::types::{PathInfo, Definition, DefType, Visibility, DefInfo};

// Load Rust grammar
fn get_rust_parser() -> Result<Parser> {
    let mut parser = Parser::new();
    parser.set_language(tree_sitter_rust::language())?;
    Ok(parser)
}

// Parse a file and return its definitions
pub fn parse_file(path: &Path) -> Result<Option<PathInfo>> {
    let extension = path.extension().and_then(|e| e.to_str()).unwrap_or("");
    
    match extension {
        "rs" => parse_rust_file(path),
        _ => {
            println!("Unsupported file extension: {}", extension);
            Ok(None)
        }
    }
}

// Scan a directory for supported files
pub fn scan_directory(dir: &Path) -> Result<Vec<PathInfo>> {
    let mut results = Vec::new();
    
    for entry in WalkDir::new(dir).into_iter().filter_map(Result::ok) {
        let path = entry.path();
        if path.is_file() {
            if let Some(file_info) = parse_file(path)? {
                results.push(file_info);
            }
        }
    }
    
    Ok(results)
}

// Query Rust nodes matching pattern
fn query_nodes<'a>(node: &'a Node, source: &'a [u8], query_str: &str) -> Result<Vec<Node<'a>>> {
    let language = tree_sitter_rust::language();
    let query = Query::new(language, query_str)?;
    let mut cursor = QueryCursor::new();
    let matches = cursor.matches(&query, *node, source);
    
    let mut nodes = Vec::new();
    for m in matches {
        for capture in m.captures {
            nodes.push(capture.node);
        }
    }
    
    Ok(nodes)
}

// Get text from a node
fn node_text<'a>(node: &Node, source: &'a [u8]) -> &'a str {
    let start = node.start_byte();
    let end = node.end_byte();
    let text = &source[start..end];
    std::str::from_utf8(text).unwrap_or("")
}

// Get line number from a node (1-based)
fn node_line(node: &Node) -> u32 {
    let start = node.start_position();
    (start.row + 1) as u32
}

// Parse a Rust file
fn parse_rust_file(path: &Path) -> Result<Option<PathInfo>> {
    let source = fs::read(path)?;
    let mut parser = get_rust_parser()?;
    
    let tree = parser.parse(&source, None)
        .ok_or_else(|| anyhow!("Failed to parse {}", path.display()))?;
    
    let root_node = tree.root_node();
    
    // Extract all definitions
    let mut functions = find_rust_functions(&root_node, &source)?;
    let structs = find_rust_structs(&root_node, &source)?;
    let enums = find_rust_enums(&root_node, &source)?;
    let traits = find_rust_traits(&root_node, &source)?;
    let impls = find_rust_impls(&root_node, &source)?;
    let modules = find_rust_modules(&root_node, &source)?;
    let type_aliases = find_rust_type_aliases(&root_node, &source)?;
    let macros = find_rust_macros(&root_node, &source)?;
    let constants = find_rust_constants(&root_node, &source)?;
    
    // Filter out functions that are methods in impl blocks
    let impl_methods: Vec<String> = impls.iter()
        .flat_map(|impl_def| impl_def.info.children.clone())
        .collect();
    
    // Only keep functions that aren't methods of impls
    functions.retain(|func| !impl_methods.contains(&func.iden));
    
    // Combine all definitions
    let mut all_defs = Vec::new();
    all_defs.extend(functions);
    all_defs.extend(structs);
    all_defs.extend(enums);
    all_defs.extend(traits);
    all_defs.extend(impls);
    all_defs.extend(modules);
    all_defs.extend(type_aliases);
    all_defs.extend(macros);
    all_defs.extend(constants);
    
    // Build parent-child relationships
    let defs = build_definition_tree(all_defs);
    
    Ok(Some(PathInfo {
        path: path.to_string_lossy().to_string(),
        defs,
    }))
}

// Find all Rust functions
fn find_rust_functions(root: &Node, source: &[u8]) -> Result<Vec<Definition>> {
    let function_nodes = query_nodes(root, source, "(function_item) @function")?;
    
    let mut functions = Vec::new();
    for node in function_nodes {
        // Skip function declarations inside trait or impl blocks
        let parent = node.parent();
        if let Some(p) = parent {
            if p.kind() == "declaration_list" {
                let grandparent = p.parent();
                if let Some(gp) = grandparent {
                    if gp.kind() == "impl_item" || gp.kind() == "trait_item" {
                        continue;
                    }
                }
            }
        }
        
        // Debug the node kind and text
        // println!("Function node: kind={}, text={}", node.kind(), node_text(&node, source));
        
        // Get function name
        let name_query = "(function_item name: (identifier) @name)";
        let name_nodes = query_nodes(&node, source, name_query)?;
        
        let name = if !name_nodes.is_empty() {
            node_text(&name_nodes[0], source).to_string()
        } else {
            "unknown".to_string()
        };
        
        // Get visibility
        let vis_query = "(function_item (visibility_modifier) @vis)";
        let vis_nodes = query_nodes(&node, source, vis_query)?;
        
        let visibility = if !vis_nodes.is_empty() {
            let vis_text = node_text(&vis_nodes[0], source);
            if vis_text == "pub" { Visibility::Public } else { Visibility::Private }
        } else {
            Visibility::Private
        };
        
        // Get function signature (parameters and return type)
        let params_query = "(function_item parameters: (parameters) @params)";
        let params_nodes = query_nodes(&node, source, params_query)?;
        
        let mut signature = String::new();
        if !params_nodes.is_empty() {
            signature.push_str(node_text(&params_nodes[0], source));
            
            // Add return type if present
            let return_query = "(function_item return_type: (type_identifier) @return)";
            let return_nodes = query_nodes(&node, source, return_query)?;
            
            if !return_nodes.is_empty() {
                signature.push_str(" -> ");
                signature.push_str(node_text(&return_nodes[0], source));
            }
        }
        
        // Create function definition
        let line_num = node_line(&node);
        let def = Definition {
            iden: name,
            def_type: DefType::Function,
            vis: visibility,
            info: DefInfo {
                signature: Some(signature),
                parent: None,
                children: Vec::new(),
                line_num: Some(line_num),
                line_end: None,
            },
        };
        
        functions.push(def);
    }
    
    Ok(functions)
}

// Find all Rust structs
fn find_rust_structs(root: &Node, source: &[u8]) -> Result<Vec<Definition>> {
    let struct_nodes = query_nodes(root, source, "(struct_item) @struct")?;
    
    let mut structs = Vec::new();
    for node in struct_nodes {
        // Get struct name
        let name_query = "(struct_item name: (type_identifier) @name)";
        let name_nodes = query_nodes(&node, source, name_query)?;
        
        let name = if !name_nodes.is_empty() {
            node_text(&name_nodes[0], source).to_string()
        } else {
            "unknown".to_string()
        };
        
        // Get visibility
        let vis_query = "(struct_item (visibility_modifier) @vis)";
        let vis_nodes = query_nodes(&node, source, vis_query)?;
        
        let visibility = if !vis_nodes.is_empty() {
            let vis_text = node_text(&vis_nodes[0], source);
            if vis_text == "pub" { Visibility::Public } else { Visibility::Private }
        } else {
            Visibility::Private
        };
        
        // Create struct definition
        let line_num = node_line(&node);
        let def = Definition {
            iden: name,
            def_type: DefType::Struct,
            vis: visibility,
            info: DefInfo {
                signature: None,
                parent: None,
                children: Vec::new(),
                line_num: Some(line_num),
                line_end: None,
            },
        };
        
        structs.push(def);
    }
    
    Ok(structs)
}

// Find all Rust enums
fn find_rust_enums(root: &Node, source: &[u8]) -> Result<Vec<Definition>> {
    let enum_nodes = query_nodes(root, source, "(enum_item) @enum")?;
    
    let mut enums = Vec::new();
    for node in enum_nodes {
        // Get enum name
        let name_query = "(enum_item name: (type_identifier) @name)";
        let name_nodes = query_nodes(&node, source, name_query)?;
        
        let name = if !name_nodes.is_empty() {
            node_text(&name_nodes[0], source).to_string()
        } else {
            "unknown".to_string()
        };
        
        // Get visibility
        let vis_query = "(enum_item (visibility_modifier) @vis)";
        let vis_nodes = query_nodes(&node, source, vis_query)?;
        
        let visibility = if !vis_nodes.is_empty() {
            let vis_text = node_text(&vis_nodes[0], source);
            if vis_text == "pub" { Visibility::Public } else { Visibility::Private }
        } else {
            Visibility::Private
        };
        
        // Create enum definition
        let line_num = node_line(&node);
        let def = Definition {
            iden: name,
            def_type: DefType::Enum,
            vis: visibility,
            info: DefInfo {
                signature: None,
                parent: None,
                children: Vec::new(),
                line_num: Some(line_num),
                line_end: None,
            },
        };
        
        enums.push(def);
    }
    
    Ok(enums)
}

// Find all Rust traits
fn find_rust_traits(root: &Node, source: &[u8]) -> Result<Vec<Definition>> {
    let trait_nodes = query_nodes(root, source, "(trait_item) @trait")?;
    
    let mut traits = Vec::new();
    for node in trait_nodes {
        // Get trait name
        let name_query = "(trait_item name: (type_identifier) @name)";
        let name_nodes = query_nodes(&node, source, name_query)?;
        
        let name = if !name_nodes.is_empty() {
            node_text(&name_nodes[0], source).to_string()
        } else {
            "unknown".to_string()
        };
        
        // Get visibility
        let vis_query = "(trait_item (visibility_modifier) @vis)";
        let vis_nodes = query_nodes(&node, source, vis_query)?;
        
        let visibility = if !vis_nodes.is_empty() {
            let vis_text = node_text(&vis_nodes[0], source);
            if vis_text == "pub" { Visibility::Public } else { Visibility::Private }
        } else {
            Visibility::Private
        };
        
        // Find trait methods (using function_item instead of function_signature)
        let method_query = "(trait_item body: (declaration_list (function_item) @method))";
        let method_nodes = query_nodes(&node, source, method_query)?;
        
        let mut method_names = Vec::new();
        for method_node in &method_nodes {
            let method_name_query = "(function_item name: (identifier) @name)";
            let method_name_nodes = query_nodes(method_node, source, method_name_query)?;
            
            if !method_name_nodes.is_empty() {
                let method_name = node_text(&method_name_nodes[0], source).to_string();
                method_names.push(method_name);
            }
        }
        
        // Create trait definition
        let line_num = node_line(&node);
        let def = Definition {
            iden: name,
            def_type: DefType::Trait,
            vis: visibility,
            info: DefInfo {
                signature: None,
                parent: None,
                children: method_names,
                line_num: Some(line_num),
                line_end: None,
            },
        };
        
        traits.push(def);
    }
    
    Ok(traits)
}

// Find all Rust impl blocks
fn find_rust_impls(root: &Node, source: &[u8]) -> Result<Vec<Definition>> {
    let impl_nodes = query_nodes(root, source, "(impl_item) @impl")?;
    
    let mut impls = Vec::new();
    let mut methods = Vec::new();
    for node in impl_nodes {
        // Get type being implemented
        let type_query = "(impl_item type: (type_identifier) @type)";
        let type_nodes = query_nodes(&node, source, type_query)?;
        
        let type_name = if !type_nodes.is_empty() {
            node_text(&type_nodes[0], source).to_string()
        } else {
            // Try alternative pattern for generic types
            let generic_query = "(impl_item type: (generic_type) @type)";
            let generic_nodes = query_nodes(&node, source, generic_query)?;
            
            if !generic_nodes.is_empty() {
                node_text(&generic_nodes[0], source).to_string()
            } else {
                "unknown".to_string()
            }
        };
        
        // Check if it's a trait implementation
        let trait_query = "(impl_item trait: (type_identifier) @trait)";
        let trait_nodes = query_nodes(&node, source, trait_query)?;
        
        let trait_name = if !trait_nodes.is_empty() {
            Some(node_text(&trait_nodes[0], source).to_string())
        } else {
            None
        };
        
        // Generate a unique identifier for the impl block
        let unique_id = match &trait_name {
            Some(trait_name) => format!("{}:{}", type_name, trait_name),
            None => type_name.clone(),
        };
        
        // Create appropriate signature for the impl block
        let signature = trait_name.as_ref().map(|trait_name| format!("{} for {}", trait_name, type_name));
        
        // Get impl block start and end line numbers
        let impl_start_line = node_line(&node);
        let impl_end_line = node.end_position().row as u32 + 1;
        
        // Find methods within the impl block
        let method_query = "(impl_item body: (declaration_list (function_item) @method))";
        let method_nodes = query_nodes(&node, source, method_query)?;
        
        let mut method_names = Vec::new();
        for method_node in &method_nodes {
            let method_name_query = "(function_item name: (identifier) @name)";
            let method_name_nodes = query_nodes(method_node, source, method_name_query)?;
            
            if !method_name_nodes.is_empty() {
                let method_name = node_text(&method_name_nodes[0], source).to_string();
                method_names.push(method_name.clone());
                
                // Get method line number
                let method_line = node_line(method_node);
                
                // Check if the method is within this impl block's line range
                if method_line >= impl_start_line && method_line <= impl_end_line {
                    // Also create a function definition for each method 
                    // (we'll link it to its parent impl later)
                    let vis_query = "(function_item (visibility_modifier) @vis)";
                    let vis_nodes = query_nodes(method_node, source, vis_query)?;
                    
                    let visibility = if !vis_nodes.is_empty() {
                        let vis_text = node_text(&vis_nodes[0], source);
                        if vis_text == "pub" { Visibility::Public } else { Visibility::Private }
                    } else {
                        Visibility::Private
                    };
                    
                    // Get function signature
                    let params_query = "(function_item parameters: (parameters) @params)";
                    let params_nodes = query_nodes(method_node, source, params_query)?;
                    
                    let mut signature = String::new();
                    if !params_nodes.is_empty() {
                        signature.push_str(node_text(&params_nodes[0], source));
                        
                        // Add return type if present
                        let return_query = "(function_item return_type: (type_identifier) @return)";
                        let return_nodes = query_nodes(method_node, source, return_query)?;
                        
                        if !return_nodes.is_empty() {
                            signature.push_str(" -> ");
                            signature.push_str(node_text(&return_nodes[0], source));
                        }
                    }
                    
                    // Create method definition with a parent link to the impl block
                    let method_def = Definition {
                        iden: method_name.clone(),
                        def_type: DefType::Function,
                        vis: visibility,
                        info: DefInfo {
                            signature: if signature.is_empty() { None } else { Some(signature) },
                            parent: Some(unique_id.clone()),  // Link to parent impl
                            children: Vec::new(),
                            line_num: Some(method_line),
                            line_end: None,
                        },
                    };
                    
                    methods.push(method_def);
                }
            }
        }
        
        // Create impl definition with line range information
        let def = Definition {
            iden: unique_id,
            def_type: DefType::Impl,
            vis: Visibility::Private,
            info: DefInfo {
                signature,
                parent: None,
                children: method_names,
                line_num: Some(impl_start_line),
                line_end: Some(impl_end_line),
            },
        };
        
        impls.push(def);
    }
    
    // Combine impl blocks and their methods
    impls.extend(methods);
    
    Ok(impls)
}

// Find all Rust modules
fn find_rust_modules(root: &Node, source: &[u8]) -> Result<Vec<Definition>> {
    let mod_nodes = query_nodes(root, source, "(mod_item) @module")?;
    
    let mut modules = Vec::new();
    for node in mod_nodes {
        // Get module name
        let name_query = "(mod_item name: (identifier) @name)";
        let name_nodes = query_nodes(&node, source, name_query)?;
        
        let name = if !name_nodes.is_empty() {
            node_text(&name_nodes[0], source).to_string()
        } else {
            "unknown".to_string()
        };
        
        // Get visibility
        let vis_query = "(mod_item (visibility_modifier) @vis)";
        let vis_nodes = query_nodes(&node, source, vis_query)?;
        
        let visibility = if !vis_nodes.is_empty() {
            let vis_text = node_text(&vis_nodes[0], source);
            if vis_text == "pub" { Visibility::Public } else { Visibility::Private }
        } else {
            Visibility::Private
        };
        
        // Create module definition
        let line_num = node_line(&node);
        let def = Definition {
            iden: name,
            def_type: DefType::Module,
            vis: visibility,
            info: DefInfo {
                signature: None,
                parent: None,
                children: Vec::new(),
                line_num: Some(line_num),
                line_end: None,
            },
        };
        
        modules.push(def);
    }
    
    Ok(modules)
}

// Find all Rust type aliases
fn find_rust_type_aliases(root: &Node, source: &[u8]) -> Result<Vec<Definition>> {
    let type_nodes = query_nodes(root, source, "(type_item) @type")?;
    
    let mut types = Vec::new();
    for node in type_nodes {
        // Get type name
        let name_query = "(type_item name: (type_identifier) @name)";
        let name_nodes = query_nodes(&node, source, name_query)?;
        
        let name = if !name_nodes.is_empty() {
            node_text(&name_nodes[0], source).to_string()
        } else {
            "unknown".to_string()
        };
        
        // Get visibility
        let vis_query = "(type_item (visibility_modifier) @vis)";
        let vis_nodes = query_nodes(&node, source, vis_query)?;
        
        let visibility = if !vis_nodes.is_empty() {
            let vis_text = node_text(&vis_nodes[0], source);
            if vis_text == "pub" { Visibility::Public } else { Visibility::Private }
        } else {
            Visibility::Private
        };
        
        // Get aliased type
        let type_query = "(type_item type: (_) @aliased_type)";
        let type_nodes = query_nodes(&node, source, type_query)?;
        
        let signature = if !type_nodes.is_empty() {
            let aliased_type = node_text(&type_nodes[0], source);
            Some(format!("= {}", aliased_type))
        } else {
            None
        };
        
        // Create type definition
        let line_num = node_line(&node);
        let def = Definition {
            iden: name,
            def_type: DefType::TypeAlias,
            vis: visibility,
            info: DefInfo {
                signature,
                parent: None,
                children: Vec::new(),
                line_num: Some(line_num),
                line_end: None,
            },
        };
        
        types.push(def);
    }
    
    Ok(types)
}

// Find Rust macros
fn find_rust_macros(root: &Node, source: &[u8]) -> Result<Vec<Definition>> {
    let macro_nodes = query_nodes(root, source, "(macro_definition) @macro")?;
    
    let mut macros = Vec::new();
    for node in macro_nodes {
        // Get macro name
        let name_query = "(macro_definition name: (identifier) @name)";
        let name_nodes = query_nodes(&node, source, name_query)?;
        
        let name = if !name_nodes.is_empty() {
            node_text(&name_nodes[0], source).to_string()
        } else {
            "unknown".to_string()
        };
        
        // Create macro definition
        let line_num = node_line(&node);
        let def = Definition {
            iden: name,
            def_type: DefType::Macro,
            vis: Visibility::Public, // Assume macros are public
            info: DefInfo {
                signature: None,
                parent: None,
                children: Vec::new(),
                line_num: Some(line_num),
                line_end: None,
            },
        };
        
        macros.push(def);
    }
    
    Ok(macros)
}

// Find Rust constants
fn find_rust_constants(root: &Node, source: &[u8]) -> Result<Vec<Definition>> {
    let const_nodes = query_nodes(root, source, "(const_item) @const")?;
    
    let mut constants = Vec::new();
    for node in const_nodes {
        // Get constant name
        let name_query = "(const_item name: (identifier) @name)";
        let name_nodes = query_nodes(&node, source, name_query)?;
        
        let name = if !name_nodes.is_empty() {
            node_text(&name_nodes[0], source).to_string()
        } else {
            "unknown".to_string()
        };
        
        // Get visibility
        let vis_query = "(const_item (visibility_modifier) @vis)";
        let vis_nodes = query_nodes(&node, source, vis_query)?;
        
        let visibility = if !vis_nodes.is_empty() {
            let vis_text = node_text(&vis_nodes[0], source);
            if vis_text == "pub" { Visibility::Public } else { Visibility::Private }
        } else {
            Visibility::Private
        };
        
        // Get type
        let type_query = "(const_item type: (_) @type)";
        let type_nodes = query_nodes(&node, source, type_query)?;
        
        let signature = if !type_nodes.is_empty() {
            Some(format!(": {}", node_text(&type_nodes[0], source)))
        } else {
            None
        };
        
        // Create constant definition
        let line_num = node_line(&node);
        let def = Definition {
            iden: name,
            def_type: DefType::Constant,
            vis: visibility,
            info: DefInfo {
                signature,
                parent: None,
                children: Vec::new(),
                line_num: Some(line_num),
                line_end: None,
            },
        };
        
        constants.push(def);
    }
    
    Ok(constants)
}

// Build parent-child relationships between definitions
fn build_definition_tree(defs: Vec<Definition>) -> Vec<Definition> {
    // We're already setting the parent-child relationships when creating the definitions,
    // so we don't need to do additional processing here
    defs
}