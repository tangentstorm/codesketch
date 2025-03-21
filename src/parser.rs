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
    
    // First, find modules to build the module tree
    let modules = find_rust_modules(&root_node, &source)?;
    
    // Extract all other definitions
    let mut functions = find_rust_functions(&root_node, &source)?;
    let structs = find_rust_structs(&root_node, &source)?;
    let enums = find_rust_enums(&root_node, &source)?;
    let traits = find_rust_traits(&root_node, &source)?;
    let impls = find_rust_impls(&root_node, &source)?;
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
        
        // Find struct fields
        let mut field_children = Vec::new();
        
        // For normal structs with curly braces
        let field_query = "(struct_item body: (field_declaration_list (field_declaration name: (field_identifier) @field)))";
        let field_nodes = query_nodes(&node, source, field_query)?;
        
        for field_node in &field_nodes {
            let field_name = node_text(field_node, source);
            field_children.push(field_name.to_string());
        }
        
        // For tuple structs, we'll just detect if it's a tuple struct
        let tuple_query = "(struct_item body: (ordered_field_declaration_list))";
        let tuple_nodes = query_nodes(&node, source, tuple_query)?;
        
        if !tuple_nodes.is_empty() {
            // Just mark that it's a tuple struct with a special entry
            field_children.push("tuple_struct".to_string());
        }
        
        // Get struct start and end line numbers
        let struct_start_line = node_line(&node);
        let struct_end_line = node.end_position().row as u32 + 1;
        
        // Create struct definition
        let def = Definition {
            iden: name.clone(),
            def_type: DefType::Struct,
            vis: visibility,
            info: DefInfo {
                signature: None,
                parent: None,
                children: field_children,
                line_num: Some(struct_start_line),
                line_end: Some(struct_end_line),
            },
        };
        
        structs.push(def);
        
        // Find all field declarations and create field definitions
        
        // Map to track which fields we've already processed
        let mut processed_fields = std::collections::HashSet::new();
        
        // Process normal struct fields
        let all_fields_query = "(struct_item body: (field_declaration_list (field_declaration) @decl))";
        let all_field_decls = query_nodes(&node, source, all_fields_query)?;
        
        for field_decl in all_field_decls {
            // Get field name from declaration
            let field_name_query = "(field_declaration name: (field_identifier) @name)";
            let field_name_nodes = query_nodes(&field_decl, source, field_name_query)?;
            
            if field_name_nodes.is_empty() {
                continue;
            }
            
            let field_name = node_text(&field_name_nodes[0], source);
            
            // Skip if we've already processed this field
            if !processed_fields.insert(field_name.to_string()) {
                continue;
            }
            
            // Determine visibility
            let vis_query = "(field_declaration (visibility_modifier) @vis)";
            let vis_nodes = query_nodes(&field_decl, source, vis_query)?;
            
            let visibility = if !vis_nodes.is_empty() {
                let vis_text = node_text(&vis_nodes[0], source);
                if vis_text == "pub" { Visibility::Public } else { Visibility::Private }
            } else {
                Visibility::Private
            };
            
            // Get field type
            let type_query = "(field_declaration type: (_) @type)";
            let type_nodes = query_nodes(&field_decl, source, type_query)?;
            
            let signature = if !type_nodes.is_empty() {
                let type_text = node_text(&type_nodes[0], source);
                Some(type_text.to_string())
            } else {
                None
            };
            
            // Get field line number
            let field_line = node_line(&field_decl);
            
            // Create field definition
            let field_def = Definition {
                iden: field_name.to_string(),
                def_type: DefType::Field,
                vis: visibility,
                info: DefInfo {
                    signature,
                    parent: Some(name.clone()),
                    children: Vec::new(),
                    line_num: Some(field_line),
                    line_end: None,
                },
            };
            
            structs.push(field_def);
        }

        // Process tuple struct fields - add a special field showing the tuple struct signature
        let tuple_fields_query = "(struct_item body: (ordered_field_declaration_list) @list)";
        let tuple_lists = query_nodes(&node, source, tuple_fields_query)?;
        
        if !tuple_lists.is_empty() {
            // Get the tuple signature by extracting the whole ordered_field_declaration_list
            let tuple_list = &tuple_lists[0];
            let tuple_signature = node_text(tuple_list, source).trim().to_string();
            
            // Add just one special field showing the tuple signature
            let field_def = Definition {
                iden: "tuple".to_string(),
                def_type: DefType::Field,
                vis: Visibility::Private, // We'll use the struct's visibility
                info: DefInfo {
                    signature: Some(tuple_signature),
                    parent: Some(name.clone()),
                    children: Vec::new(),
                    line_num: Some(struct_start_line),
                    line_end: None,
                },
            };
            
            structs.push(field_def);
        }
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
        
        // Find trait methods (try both approaches as tree-sitter grammar might vary)
        // First, try with function_item
        let method_query = "(trait_item body: (declaration_list (function_item) @method))";
        let mut method_nodes = query_nodes(&node, source, method_query)?;
        
        // Alternate approaches if needed
        if method_nodes.is_empty() {
            // Try to get any children of the declaration list as a fallback
            let alt_query = "(trait_item body: (declaration_list (_) @item))";
            method_nodes = query_nodes(&node, source, alt_query)?;
        }
        
        let mut method_names = Vec::new();
        let mut trait_methods = Vec::new();
        
        for method_node in &method_nodes {
            // Get method name based on node type
            let kind = method_node.kind();
            let method_name_nodes = if kind == "function_item" {
                let method_name_query = "(function_item name: (identifier) @name)";
                query_nodes(method_node, source, method_name_query)?
            } else {
                // Try a generic approach for unknown node types
                Vec::new()
            };
            
            if !method_name_nodes.is_empty() {
                let method_name = node_text(&method_name_nodes[0], source).to_string();
                method_names.push(method_name.clone());
                
                // Get method signature based on node type
                let params_nodes = if kind == "function_item" {
                    let params_query = "(function_item parameters: (parameters) @params)";
                    query_nodes(method_node, source, params_query)?
                } else {
                    Vec::new()
                };
                
                let mut signature = String::new();
                if !params_nodes.is_empty() {
                    signature.push_str(node_text(&params_nodes[0], source));
                    
                    // Add return type if present based on node type
                    let return_nodes = if kind == "function_item" {
                        let return_query = "(function_item return_type: (_) @return)";
                        query_nodes(method_node, source, return_query)?
                    } else {
                        // Try a more generic approach for other node types
                        Vec::new()
                    };
                    
                    if !return_nodes.is_empty() {
                        signature.push_str(" -> ");
                        signature.push_str(node_text(&return_nodes[0], source));
                    }
                }
                
                // Get method line number
                let method_line = node_line(method_node);
                
                // Create method definition
                let method_def = Definition {
                    iden: method_name,
                    def_type: DefType::Method,
                    vis: Visibility::Public, // Methods in traits are implicitly public
                    info: DefInfo {
                        signature: if signature.is_empty() { None } else { Some(signature) },
                        parent: Some(name.clone()),
                        children: Vec::new(),
                        line_num: Some(method_line),
                        line_end: None,
                    },
                };
                
                trait_methods.push(method_def);
            }
        }
        
        // Get trait start and end line numbers
        let trait_start_line = node_line(&node);
        let trait_end_line = node.end_position().row as u32 + 1;
        
        // Create trait definition
        let def = Definition {
            iden: name.clone(),
            def_type: DefType::Trait,
            vis: visibility,
            info: DefInfo {
                signature: None,
                parent: None,
                children: method_names,
                line_num: Some(trait_start_line),
                line_end: Some(trait_end_line),
            },
        };
        
        traits.push(def);
        
        // Add all trait methods to the result
        traits.extend(trait_methods);
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
                        
                        // Add return type if present - try both specific and generic approach
                        let return_query = "(function_item return_type: (_) @return)";
                        let return_nodes = query_nodes(method_node, source, return_query)?;
                        
                        if !return_nodes.is_empty() {
                            signature.push_str(" -> ");
                            signature.push_str(node_text(&return_nodes[0], source));
                        }
                    }
                    
                    // Create method definition with a parent link to the impl block
                    let method_def = Definition {
                        iden: method_name.clone(),
                        def_type: DefType::Method,
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
        
        // Find module contents (if inline)
        let mut children = Vec::new();
        
        // First try to get module content nodes
        let body_query = "(mod_item body: (_)) @body";
        let body_nodes = query_nodes(&node, source, body_query)?;
        
        // Process declarations within the module body
        if !body_nodes.is_empty() {
            let body_node = &body_nodes[0];
            
            // Find all function declarations within the module
            let func_query = "(declaration_list (function_item name: (identifier) @name))";
            let func_nodes = query_nodes(body_node, source, func_query)?;
            for func_node in func_nodes {
                let func_name = node_text(&func_node, source);
                children.push(func_name.to_string());
            }
            
            // Find all struct declarations within the module
            let struct_query = "(declaration_list (struct_item name: (type_identifier) @name))";
            let struct_nodes = query_nodes(body_node, source, struct_query)?;
            for struct_node in struct_nodes {
                let struct_name = node_text(&struct_node, source);
                children.push(struct_name.to_string());
            }
            
            // Find all enum declarations within the module
            let enum_query = "(declaration_list (enum_item name: (type_identifier) @name))";
            let enum_nodes = query_nodes(body_node, source, enum_query)?;
            for enum_node in enum_nodes {
                let enum_name = node_text(&enum_node, source);
                children.push(enum_name.to_string());
            }
        }
        
        // Get module start and end line numbers
        let mod_start_line = node_line(&node);
        let mod_end_line = if !body_nodes.is_empty() {
            node.end_position().row as u32 + 1
        } else {
            mod_start_line
        };
        
        // Create module definition
        let def = Definition {
            iden: name,
            def_type: DefType::Module,
            vis: visibility,
            info: DefInfo {
                signature: None,
                parent: None,
                children,
                line_num: Some(mod_start_line),
                line_end: Some(mod_end_line),
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
    let mut result = defs.clone();
    
    // Find module ranges and assign definitions to their parent modules
    let modules: Vec<_> = defs.iter()
        .filter(|def| def.def_type == DefType::Module)
        .collect();
    
    // For each module, directly add children as referenced in module.info.children
    for module in &modules {
        for child_name in &module.info.children {
            // Find the definition with matching name and set its parent
            for def in result.iter_mut() {
                if &def.iden == child_name && def.info.parent.is_none() {
                    def.info.parent = Some(module.iden.clone());
                    break;
                }
            }
        }
    }
    
    // For remaining definitions, try to find their parent module by line range
    for def in result.iter_mut() {
        if def.def_type != DefType::Module && def.info.parent.is_none() {
            // Check if this definition falls within a module's line range
            if let Some(line_num) = def.info.line_num {
                for module in &modules {
                    if let (Some(start), Some(end)) = (module.info.line_num, module.info.line_end) {
                        if line_num > start && line_num < end {
                            // This definition is within this module's range
                            def.info.parent = Some(module.iden.clone());
                            break;
                        }
                    }
                }
            }
        }
    }
    
    result
}