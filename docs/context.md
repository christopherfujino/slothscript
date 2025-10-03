|Variable | Type | Description |
|--|--|--|
|`$argv`| `List[String]` | The command line arguments, if any. Does not include the `sloth` interpreter binary|
|`$cwd` | `String` | The current working directory|
|`$env` | `HashMap[String]String` | The environment variables|
|`$script` | `String` | The path to the currently running Sloth script|
|`$scriptDir` | `String` | The path to the directory containing `$script`|
|`$stderr`|`FileDescriptor`|The STDERR file descriptor|
|`$stdin`|`FileDescriptor`|The STDIN file descriptor|
|`$stdout`|`FileDescriptor`|The STDOUT file descriptor|
