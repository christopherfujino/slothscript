## Loops

Counting from 1 to 10:

Bash:
```bash
for i in {1..10}; do
    echo "$i"
done
```

Python:
```python
for i in range(1, 11):
    print(i)
```

Ruby:
```ruby
(1..10).each {|i| puts i}
```

Go:
```go
for i := 1; i <= 10; i++ {
    fmt.Println(i)
}
```

Slothscript:
```sloth
for i = 1; i <= 10; i += 1 {
    print(i);
}
```

Iterating over a list:

Bash:
```bash
primes=(2 3 5 7 11 13)
for el in "${primes[@]}"; do
    echo "$el"
done
```

Python:
```python
for el in [2, 3, 5, 7, 11, 13]:
    print(el)
```

Ruby:
```ruby
[2, 3, 5, 7, 11, 13].each {|el| puts el}
```

Go:
```go
for _, el := range []int{2, 3, 5, 7, 11, 13} {
    fmt.Println(el)
}
```

Slothscript:
for el in [2, 3, 5, 7, 11, 13] {
    print(el);
}

Loop until an expression evaluates to true:
```bash
while is-running(); do
    do-work()
done
```

```python
while isRunning():
    doWork()
```

```ruby
while is_running()
    do_work()
end
```

```go
for isRunning() {
    doWork()
}
```
