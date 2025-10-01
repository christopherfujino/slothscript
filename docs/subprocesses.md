There are three primary ways to spawn a subprocess:

```sloth
# stdin, stdout, & stderr is inherited from parent
# Execution is blocked until child exits with 0
Process.new("uname")! # -> null

# STDOUT & STDERR are buffered in memory, will be available in ProcessResult
let handle = Process.new([
    "curl",
    "-lO",
    "http://remote.path/file.tar.gz",
])& # -> ProcessHandle
handle.wait() # -> ProcessResult

# STDOUT & STDERR are buffered in memory and available in ProcessResult
# Execution is blocked until child exits with 0
Process.new("bash")&! # -> ProcessResult
```
