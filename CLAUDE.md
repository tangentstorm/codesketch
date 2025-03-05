# CLAUDE.md - Assistance Guide

## Build & Test Commands
- Build: `cabal build` - Build the project
- Run: `cabal run codesketch -- /path/to/code` - Run on a directory/file
- Install: `cabal install` - Install the binary
- Test (all): `cabal test` - Run all tests
- Clean: `cabal clean` - Clean build artifacts
- Sample: `cabal run codesketch -- test/test_rust.rs` - Run on test file
- Make: `make build`, `make test`, etc. - Use Makefile shortcuts

## Code Style Guidelines
- **Languages**: Haskell project analyzing Rust, Python, TypeScript/JavaScript
- **Formatting**: Follow Haskell conventions with 2-space indentation
- **Naming**: camelCase for values/functions, PascalCase for types/modules
- **Error Handling**: Use Maybe/Either types and explicit error handling
- **Imports**: Group by external/standard/internal; qualified imports when needed
- **Types**: Use strong typing with algebraic data types; document types with Haddock
- **Comments**: Use -- for line comments, {- -} for block comments
- **Testing**: Write unit tests with Hspec

## Project Implementation
- Uses Haskell string processing to parse and analyze code
- Supports Rust files currently, with plans for Python and TS/JS
- Outputs structured JSON following the schema defined in README.md
- Extracts functions, classes/structs, traits, and modules