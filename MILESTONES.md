# Milestones

## v0.1 - initially tagged version
- [ ] error handling (`let result = mayError() catch (e) DEFAULT;`)
- [ ] Stack traces
- [ ] Language versioning
- [ ] Standard Library
  - `List` methods
  - `String` methods
  - `$Process.allowNonzero : Bool`

## v0.2 - initial pre-release
- [ ] Regular expressions
- [ ] Test suite
- [ ] Security policy

## v1.0
- [ ] Formatter
- [ ] LSP server
- [ ] Binary builds
- [ ] JSON module
- [ ] statically link readline
- [ ] Optional function parameters

## Stretch goals
- [ ] Unicode strings
- [ ] Method syntax for hash maps
- [ ] Pattern matching
- [ ] Formatter
- [ ] Have optimizer check that:
    - `return` always occurs in a function definition
    - `break` and `continue` always occur in loops
- [ ] Yaml module

# Done

## pre-v0.1
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
- [x] operators are top level functions that DO one thing, but allow for implicit casting of its operands
- [x] raw string literals
- [x] Files
- [x] Return statements
- [x] Allow single statement blocks to omit semicolons
- [x] Migrate var declarations and reassignment to expressions
- [x] First class for loops
- [x] Break and continue statements
- [x] Context variables, with statements; $cwd
- [x] Mock out processes, files for unit tests
- [x] $script & $scriptDir
- [x] $env
- [x] Parser errors
- [x] Implement exit function
- [x] `&` and `&!` operators for spawning sub-processes
- [x] CLI args
- [x] $stdin, $stderr
