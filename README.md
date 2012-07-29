# "hatch" -- an implementation of programming language "egg" #

"hatch" is an implementation of programming language
"egg". Programming language "egg" is designed by Y. HAYAMIZU for the
exercise of an university class.

"hatch" consists of following three modules:

  * parser ... parse an egg program
  * translator ... compile an egg program into VM byte code
  * VM ... execute VM byte code

## Egg sample code ##

```
bind foo -> 1;
print(foo); # prints "foo"
```

```
bind fib -> lambda (n) { # lambda generates a function (or closure)
  if n <= 0
    0
  eles if n = 1
    1
  else
    fib(n - 1) + fib(n - 2)
};

puts(fib(10));
```

```
let Y -> lambda (f) { # You can construct Y-combinator! Yeah!
  (lambda (proc) {
    f(lambda (arg) { (proc(proc))(arg) })
  })(lambda (proc) {
    f(lambda (arg) { (proc(proc))(arg) })
  })
}
in
let fact0 -> lambda (f) {
  lambda (n) {
    if (n = 0)  1
    else        n * f(n - 1)
  }
}
in
let fact -> Y(fact0)
in
  puts(fact(10))
```
