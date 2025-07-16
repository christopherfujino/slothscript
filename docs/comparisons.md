## Loops

Counting from 1 to 10:

```bash
for i in {1..10}; do
    echo "$i"
done
```

```python
for i in range(1, 11):
    print(i)
```

```ruby
(1..10).each {|i| puts i}
```

```go
for i := 1; i <= 10; i++ {
    fmt.Println(i)
}
```

Iterating over a list:

```bash
primes=(2 3 5 7 11 13)
for el in "${primes[@]}"; do
    echo "$el"
done
```

```python
for el in [2, 3, 5, 7, 11, 13]:
    print(el)
```

```ruby
[2, 3, 5, 7, 11, 13].each {|el| puts el}
```

```go
for _, el := range []int{2, 3, 5, 7, 11, 13} {
    fmt.Println(el)
}
```

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
