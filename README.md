# codesketch

A tool for quickly getting a rough outline of the code in your files.

Uses treesitter.

usage:

```
codesketch /path/to/code
```

Output should be a json dump of all types and definitions in your code.

Structure looks something like this schema:

```
root = [ paths ];

path = {
  path:"./path/to/file.ext",
  defs: [ def ]
};

def = {
  iden: string,
  type: "module" | "class" | "fn" | "trait", // | etc,
  vis: "*" | "+" | "-",                      // for public/protected/private
}

```

Initial version should support rust, python, and typescript/javascript.
