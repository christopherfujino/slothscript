```sloth
class Runtime {
  static property script : File
}
```

```sloth
class File {
  # Should this be a path instance?
  property path : string

  func parent() : Directory
}
```

```sloth
class Directory {

}
```
