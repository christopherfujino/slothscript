# Milestones

## v0.1 - initially tagged version
- [ ] Language versioning

## v0.2 - initial pre-release
- [ ] Migrate AST to use actual Ast module
- [ ] Regular expressions
- [ ] Test suite
- [ ] Constraint system
    - language version
    - permissions
- [ ] JSON module
- [ ] Allow non-zero exit codes on sub-processes
- [ ] Bug: stacktrace printing is wrong in REPL

## v1.0
- [ ] Formatter
- [ ] LSP server
- [ ] Binary builds
- [ ] statically link readline
- [ ] optimize binary size
- [ ] Optional function parameters
- [ ] Network stack

## Stretch goals
- [ ] Bytecode VM (this should not happen until API becomes stable)
- [ ] Unicode strings
- [ ] Method syntax for hash maps
- [ ] Pattern matching
    - Error types
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
- [x] error handling (`let result = mayError() catch (e) DEFAULT;`)
- [x] `throw` keyword
- [x] `File::open()` -> `FileDescriptor`
    - `fd.writeAll("Hello, World!\n")`
    - `fd.close()
- [x] Reading from `$stdin`
- [x] Mock out standard I/O for tests
- [x] `String` methods
    - `.split(sep)`
    - `.contains(substring)`
- [x] Blank identifier does not bind
- [x] Add `FileDescriptor::write()`
- [x] Stack traces
- [x] `List` methods
    - `.contains(element)`
    - `.forEach(callback)`
    - `.map(callback)`
    - `.filter(callback)`
    - `.reduce(callback)`
    - `.push(element)`
    - `.pop(element)`
