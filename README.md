# codesketch

A tool for quickly getting a rough outline of the code in your files.

Uses Haskell's string processing capabilities to parse and analyze code files, extracting definitions like functions, classes, and modules.

## Installation

```
# Clone the repository
git clone https://github.com/yourusername/codesketch.git
cd codesketch

# Build the project
cabal build

# Install the binary (optional)
cabal install
```

## Usage

```
codesketch /path/to/code
```

Output is a human-readable text outline of all types and definitions in your code.

### Options

- `--json` or `-j`: Output in JSON format
- `--debug` or `-d`: Enable debug output (to stderr)

### Example Text Output

```
/path/to/file.rs:
  [S] + MyStruct
  [F] - my_private_function
  [F] + my_public_function
  [T] + MyTrait
  [M] + my_module
```

Legend:
- Type indicators: `[S]` struct, `[E]` enum, `[F]` function, `[T]` trait, `[M]` module, `[I]` impl
- Visibility: `+` public, `~` protected, `-` private

## Structure

The output follows this schema:

```
root = [ paths ];

path = {
  path:"./path/to/file.ext",
  defs: [ def ]
};

def = {
  iden: string,
  type: "module" | "struct" | "enum" | "fn" | "trait" | "impl", // | etc,
  vis: "*" | "+" | "-",                      // for public/protected/private
}
```

Where:
- `*` indicates public visibility
- `+` indicates protected visibility
- `-` indicates private visibility

## Example Output

```json
[{
  "path": "/path/to/file.rs",
  "defs": [
    {"iden": "MyStruct", "type": "struct", "vis": "*"},
    {"iden": "MyEnum", "type": "enum", "vis": "*"},
    {"iden": "my_function", "type": "fn", "vis": "-"},
    {"iden": "MyTrait", "type": "trait", "vis": "*"}
  ]
}]
```

## Supported Languages

Currently supports:
- Rust (`.rs` files)

Planned support:
- Python
- TypeScript/JavaScript