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

Variables:

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
