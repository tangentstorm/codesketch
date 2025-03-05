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
  public struct MyStruct
  private fn my_private_function()
  public fn my_public_function(param: i32) -> bool
  public trait MyTrait
  public module my_module
```

The output shows:
- Full visibility (`public`, `private`, `protected`)
- Type information (`fn`, `struct`, `enum`, `trait`, `module`, etc.)
- Function signatures when available

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