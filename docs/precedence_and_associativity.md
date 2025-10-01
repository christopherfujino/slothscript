## Binary

### Go

From high to low precedence (left-associative):

`*`, `/`, `%`, `<<`, `>>`, `&`, `&^`
`+`, `-`, `|`, `^`
`==`, `!=`, `<`, `<=`, `>`, `>=`
`&&`
`||`

### C

`!` (right associative)
`*`, `/`, `%`
`+`, `-`
`<`, `<=`, `>`, `>=`
`==`, `!=`
`&&`
`||`

### Sloth

(Greediest first...)

- `not`
- `*`, `/`, `%`
- `+`, `-`, `|`
- `==`, `!=`, `<`, `<=`, `>`, `>=`
- `<-`, `->`
- `and`
- `or`, `!` (postfix), `&` (postfix)
