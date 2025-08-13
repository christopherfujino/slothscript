# Slothscript

A slow scripting language.

```sloth
func fib(n) {
  if n <= 1 {
    n
  } else {
    fib(n - 1) + fib(n - 2)
  }
}

print(fib(20))
```

## Tour

User-written variables or functions must start with a lowercase letter or `_`.
Names starting with a capital letter are reserved for class names.

```sloth
let x = 1 + 1

print(x)
```

First-class functions:

```sloth
func makeCounter() {
  let x = 0
  func() {
    x = x + 1
    x
  }
}

let counter = makeCounter()
print(counter())
print(counter())
```
