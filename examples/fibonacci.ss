# Note that the `fib` name is in scope in the right hand side of a `func`
# statement. This syntax is similar to Golang.
func fib (x) =>
  if x <= 1 then
    x
  else
    fib(x - 1) + fib(x - 2)

System.print("fib 7 is ${fib(7)}")
