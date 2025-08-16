## Milestones

### v0.1 - initial version
- [ ] MAYBE...operators shouldn't be methods, but top level functions that DO one thing, but allow for a lot of implicit casting of its operands to make that happen
- [ ] Mock out process subsystem for unit tests
- [ ] Make `Process.new` variadic
- [ ] $backtick function
- [ ] Context variables, with statements
- [ ] Standard Library
- [ ] Language versioning

### v0.2 - initial pre-release
- [ ] statically link readline
- [ ] Regular expressions
- [ ] raw string literals
- [ ] string escapes
- [ ] Optional function parameters
- [ ] List mutation methods
- [ ] Stack traces; depends on storing locs in runtime values
- [ ] return, break, continue keywords; error handling (`let result = mayError() catch (e) DEFAULT;`)

### v1.0
- [ ] Formatter
- [ ] LSP server
- [ ] Security policy

### Stretch goals
- [ ] Unicode strings
- [ ] Method syntax for hash maps
- [ ] Pattern matching

## Done

### pre-v0.1
- [x] Lexical scoping
- [x] Closures
- [x] Variable re-assignment
- [x] Function arguments
- [x] Invoking function expressions
- [x] Conditionals
- [x] Tooling to diagnose shift/reduce conflicts
- [x] Infix functions
- [x] Recursion (depends on conditionals)
- [x] Comments
- [x] Lists
- [x] Loops
- [x] Hashmaps
- [x] Updating lists
- [x] String interpolation
- [x] Implement all arithmetic operators
- [x] Automatic semicolon insertion
- [x] HashMap literals
- [x] Store locs in runtime values
- [x] Assertions
- [x] Classes
- [x] Comments
- [x] For-in loops
- [x] Do blocks
- [x] README.md
- [x] Process class
- [x] Class static methods
- [x] Class prototypes
- [x] Use readline for REPL
