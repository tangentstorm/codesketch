use clap::Parser;
use std::path::PathBuf;
use anyhow::Result;

mod parser;
mod types;
mod output;
mod browser;

/// CodeSketch - Tool for quickly getting a rough outline of the code in your files
#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Files or directories to analyze
    #[arg(required = true)]
    paths: Vec<PathBuf>,

    /// Output in JSON format
    #[arg(short = 'j', long, conflicts_with = "interactive")]
    json: bool,
    
    /// Start interactive browser mode
    #[arg(short = 'i', long, conflicts_with = "json")]
    interactive: bool,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    
    // Process all paths
    let mut all_definitions = Vec::new();
    
    for path in &cli.paths {
        if path.is_dir() {
            let defs = parser::scan_directory(path)?;
            all_definitions.extend(defs);
        } else {
            let defs = parser::parse_file(path)?;
            if let Some(file_defs) = defs {
                all_definitions.push(file_defs);
            }
        }
    }
    
    // Output the results
    if cli.json {
        output::output_json(&all_definitions)?;
    } else if cli.interactive {
        browser::run_browser(all_definitions)?;
    } else {
        output::output_text(&all_definitions)?;
    }

    Ok(())
}
