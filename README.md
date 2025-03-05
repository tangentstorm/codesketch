# codesketch

A tool for quickly getting a rough outline of the code in your files.

Uses tree-sitter to parse and analyze code files, extracting definitions like functions, structs, traits, and modules, showing their hierarchical relationships.

## Installation

```
# Clone the repository
git clone https://github.com/yourusername/codesketch.git
cd codesketch

# Build the project
cargo build

# Install the binary (optional)
cargo install --path .
```

## Usage

```
codesketch /path/to/code
```

Output is a human-readable text outline of all types and definitions in your code.

### Options

```
Options:
  -f, --format <FORMAT>  Output format (json, text, or interactive) [default: text]
  -h, --help             Print help
  -V, --version          Print version
```

### Interactive Mode

The interactive mode (`-f interactive`) provides a TUI browser to explore code:

- Navigate up/down with arrow keys
- Expand/collapse nodes with Tab
- View source code of selected items with syntax highlighting
- Search through code elements by pressing `/`
- Navigate between search results with up/down arrows
- Exit with 'q'

```
codesketch -f interactive /path/to/code
```

### Example Text Output

```
/path/to/file.rs:
  12 pub trait Base
  44   pub fn _eval_aux
  48   pub fn eval_all
  53   pub fn eval
  62   pub fn solution_set
  65   pub fn init_stats
  66   pub fn print_stats
  70 pub trait GraphViz
  74   pub fn save_dot
  80   pub fn show_named
  89   pub fn show
  92 impl GraphViz for T
  93   fn write_dot
 110 pub macro inherit
 127 pub struct Simplify
 127   pub field base
 129 impl Base for Simplify<T>
 131   fn and
```

The output shows:
- Line numbers
- Proper hierarchical structure with indentation
- Visibility (`pub`, private by default)
- Type information (`fn`, `struct`, `trait`, `impl`, `macro`, etc.)
- Struct fields and impl methods with correct indentation

## JSON Output Structure

When using the JSON output format (`-f json`), the output follows this schema:

```
root = [ files ];

file = {
  path: "./path/to/file.ext",
  defs: [ definition ]
};

definition = {
  name: string,             // Identifier name
  type: string,             // Type of definition (fn, struct, trait, etc.)
  visibility: string,       // pub, private, etc.
  line: number,             // Line number where definition starts
  line_end: number,         // Line number where definition ends
  children: [ definition ], // Nested definitions (methods, fields, etc.)
  parent: string            // For impl blocks, the type being implemented
}
```

## Example JSON Output

```json
[
  {
    "path": "/path/to/file.rs",
    "defs": [
      {
        "name": "Base",
        "type": "trait",
        "visibility": "pub",
        "line": 12,
        "line_end": 67,
        "children": [
          {
            "name": "_eval_aux",
            "type": "fn",
            "visibility": "pub",
            "line": 44,
            "line_end": 47
          },
          {
            "name": "eval_all",
            "type": "fn",
            "visibility": "pub",
            "line": 48,
            "line_end": 52
          }
        ]
      },
      {
        "name": "GraphViz for T",
        "type": "impl",
        "visibility": "",
        "line": 92,
        "line_end": 109,
        "parent": "T",
        "children": [
          {
            "name": "write_dot",
            "type": "fn",
            "visibility": "",
            "line": 93,
            "line_end": 109
          }
        ]
      }
    ]
  }
]
```

## Supported Languages

Currently supports:
- Rust (`.rs` files)

Planned support:
- Python
- TypeScript/JavaScript

## Implementation

The tool uses:
- Tree-sitter for parsing Rust code
- Rust's standard library for file and directory traversal
- Serde for JSON serialization
- Colored for terminal output formatting

The main components are:
- `parser.rs`: Core parsing logic using tree-sitter 
- `types.rs`: Data structures for representing code elements
- `output.rs`: Output formatting (text and JSON)
- `main.rs`: CLI interface and file handling