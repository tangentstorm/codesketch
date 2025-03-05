# CLAUDE.md - Assistance Guide

## Build & Test Commands
- Build: `cargo build` - Build the project
- Run: `cargo run -- /path/to/code` - Run on a directory/file
- Run (with format): `cargo run -- -f json /path/to/code` - Output in JSON format
- Install: `cargo install --path .` - Install the binary
- Clean: `cargo clean` - Clean build artifacts
- Sample: `cargo run -- samples/base.rs` - Run on a sample file
- Release Build: `cargo build --release` - Build optimized version

## Code Style Guidelines
- **Language**: Rust
- **Formatting**: Follow Rust conventions with 4-space indentation
- **Naming**: snake_case for functions/variables, PascalCase for types/structs/enums
- **Error Handling**: Use Result<T, E> for recoverable errors and Option<T> for nullable values
- **Imports**: Group by external crates then internal modules
- **Types**: Use strong typing with enums and structs
- **Comments**: Use standard Rust doc comments (///) for documentation
- **Testing**: Write tests using Rust's built-in testing framework

## Project Implementation
- Uses tree-sitter to parse and analyze Rust code
- Supports Rust files currently, with plans for Python and TS/JS
- Outputs structured JSON or human-readable text
- Extracts functions, structs, traits, impl blocks, modules and their relationships