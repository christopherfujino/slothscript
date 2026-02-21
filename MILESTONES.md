# Milestones

## v0.2
- [ ] Binary builds
- [ ] statically link readline

## v0.3 - initial pre-release
- [ ] Test suite
- [ ] JSON module
- [ ] Constraint system
    - language version
    - permissions
- [ ] Migrate AST to use actual Ast module
- [ ] Regular expressions (libpcre?)
- [ ] Allow non-zero exit codes on sub-processes

## v1.0
- [ ] Formatter
- [ ] LSP server
- [ ] optimize binary size
- [ ] Optional function parameters
- [ ] Network stack

## Stretch goals
- [ ] Bytecode VM (this should not happen until API becomes stable)
- [ ] From scratch regular expression engine
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

## v0.1 - initially tagged version
- [x] Language versioning

## pre-v0.1
- [x] Bug: $scriptDir and $cwd should be absolute, so they can be chained
- [x] `List` methods
    - `.pop(element)`
    - `.push(element)`
    - `.reduce(callback)`
    - `.filter(callback)`
    - `.map(callback)`
    - `.forEach(callback)`
    - `.contains(element)`
- [x] Stack traces
- [x] Add `FileDescriptor::write()`
- [x] Blank identifier does not bind
- [x] `String` methods
    - `.contains(substring)`
    - `.split(sep)`
- [x] Mock out standard I/O for tests
- [x] Reading from `$stdin`
- [x] `File::open()` -> `FileDescriptor`
    - `fd.close()
    - `fd.writeAll("Hello, World!\n")`
- [x] `throw` keyword
- [x] error handling (`let result = mayError() catch (e) DEFAULT;`)
- [x] $stdin, $stderr
- [x] CLI args
- [x] `&` and `&!` operators for spawning sub-processes
- [x] Implement exit function
- [x] Parser errors
- [x] $env
- [x] $script & $scriptDir
- [x] Mock out processes, files for unit tests
- [x] Context variables, with statements; $cwd
- [x] Break and continue statements
- [x] First class for loops
- [x] Migrate var declarations and reassignment to expressions
- [x] Allow single statement blocks to omit semicolons
- [x] Return statements
- [x] Files
- [x] raw string literals
- [x] operators are top level functions that DO one thing, but allow for implicit casting of its operands
- [x] Use readline for REPL
- [x] Class prototypes
- [x] Class static methods
- [x] Process class
- [x] README.md
- [x] Do blocks
- [x] For-in loops
- [x] Comments
- [x] Classes
- [x] Assertions
- [x] Store locs in runtime values
- [x] HashMap literals
- [x] Automatic semicolon insertion
- [x] Implement all arithmetic operators
- [x] String interpolation
- [x] Updating lists
- [x] Hashmaps
- [x] Loops
- [x] Lists
- [x] Comments
- [x] Recursion (depends on conditionals)
- [x] Infix functions
- [x] Tooling to diagnose shift/reduce conflicts
- [x] Conditionals
- [x] Invoking function expressions
- [x] Function arguments
- [x] Variable re-assignment
- [x] Closures
- [x] Lexical scoping

## Bugs
- [ ] bug: stacktrace printing is wrong in REPL
- [ ] bug: Do static methods need to take self as first arg?
