# compiler-dodecahedron
Dodecahedron programming language compiler

OLD syntax
```

FORM $myvar === 343 + 34 
$myvar === @add(1,2)
$string === "my string" 
IF $myvar = 34 {
    @print(myvar)
}

```

let a+=3 : a+3

Convert AST into IR

(1 + a) * b
    |
    v
    1 + a
     |
     v
    1
       a
      +