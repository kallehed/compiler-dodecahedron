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

Program correctness thoughts:
 - A variable can only be used after it has been created. It's lifetime cascades to child blocks. Variable shadowing may not happen in descendant blocks. 
   Implement this by keeping track of created variables and making sure that used variables have been declared before. Have a 'global' hashset and
   let each block have a Vec of their declared variables, and let a block remove it's variables after it has run out.
- Should every AST node have a source reference, like, char range?