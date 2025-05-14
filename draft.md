```bash
function add_to_path_if_not_present {
  local path="$1"
  [ ! -d "$path" ] && return
  # The second argument is optional, but will speed up this function
  local paths="${2:-$(echo "$PATH" | tr : '\n')}"
  echo "$paths" | grep --fixed-string --line-regexp "$dir" >/dev/null
  # Add $dir to $PATH if it does not yet appear
  [ "$?" -ne 0 ] && PATH="$dir:$PATH"
}

# add dirs to path, if they exist
dirs=(
  "$HOME/scripts"
  "$HOME/bin"
  "$HOME/go/bin"
  "$HOME/.node_modules/bin"
  "$HOME/.nvm"
  "$HOME/.pub-cache/bin"
)

paths=$(echo "$PATH" | tr : '\n')
for dir in "${dirs[@]}"; do
  add_to_path_if_not_present "$dir" "$paths"
done
```

```chrish
let add_to_path_if_not_present = func (dir, paths) =>
  if List.contains(paths, dir) then
    null
  else
    let newPath = String.join([paths], ":") in
    System.SetEnv("PATH", newPath)
  in

let home = System.GetEnv("HOME") in

let dirs = List.map(
  [
    [home, "scripts"],
    [home, "bin"],
    [home, "go", "bin"],
    [home, ".node_modules", "bin"],
    [home, ".nvm"],
    [home, ".pub-cache", "bin"],
  ],
  func(pathElements) => String.join(pathElements, "/"),
) in

let paths = String.split(System.GetEnv("PATH"), ":") in

List.each(
  dirs,
  func (path) => add_to_path_if_not_present(dir: path, paths: paths)
)
```

## Built In Functions

* `GetEnv(string name) -> string` - given an env variable name, return its value.
