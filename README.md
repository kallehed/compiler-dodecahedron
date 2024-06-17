# compiler-dodecahedron
Dodecahedron programming language compiler

Convert AST into IR
```
(1 + a) * b
    |
    v
    1 + a
     |
     v
    1
       a
```

Program correctness thoughts:
 - A variable can only be used after it has been created. It's lifetime cascades to child blocks. Variable shadowing may not happen in descendant blocks. OBVIOUS
   Implement this by keeping track of created variables and making sure that used variables have been declared before. Have a 'global' hashset and
   let each block have a Vec of their declared variables, and let a block remove it's variables after it has run out.
- Should every AST node have a source reference, like, char range? Yes

TODO: Don't allow nested functions, right now: bad behaviour.
TODO: Better errors, be able to show 2 locations (use when bad delimiters, also when no return)
TOOD: maybe use Default more, but doesn't work when you can't default all the fields (can't default some iterator)

# Things to think about:
How should the Soken's be iterated? Should I create some sort of abstraction around it to make the verifyer and the c backend simpler? But I don't want to lose performance, also don't want to lose readability of code (sequentiallity), though readability already suffers a bit from reverse polish notation style expressions.

How should the c backend concatinate expressions to produce a string C can understand? I have made 3 implementations: Bad slow 1 million String impl | Put everything on stack -> (Put stuff on another stack and iterate | recurse over items, where dependencies have been written in) (the one without any recursion is most lines)

Should the parser be de-recursified? Maybe could put all actions to do on a stack, though this would maybe make it overall less readable AND increase line-count. Could try an implementation.
BAD IDEA - too hard to implement in shunting yard

Instead of doing a bunch of stuff with the Soken's, maybe I should generate an IR. It could be 4 item IR like: `a = b 'op' c`. Set register to op of two registers.
Problem: don't know how to call functions? Function call could specify at which register they begin, but seems weird, would also be weird if function call was variable length, though maybe could store that in separate buffer (which register values function is called with)
Something like f(1+2,3) would be converted to $0 = i64 1; $1 = i64 2; $2 = $0 + $1; $3 = i64 3; call f with $2,$3, could have table that says which f uses, or we could create 2 more registers: $4 = $2, $5 = $3, and do `call f start at $4`, and we would know to use $4 and %5. (This example is dumb because in the original the registers get next to each other). We could also delay the setting of the registers to not have to create 2 new registers. So we do: $0 = i64 1; $1 = i64 2; THEN: $2 = $0 + $1; $3 = i64 3; call f from $2. Though this seems harder to implement, because have to wait for everything to evaluate to next-to-last, before setting last to register. So would have to have a stack or something. IMPORTANT: Don't overthink with structs, complicated stuff THIS LANGUAGE ONLY HAS i64!

How should stuff like if statements be represented in Sokens?

alternatives:
- <EXPR> [if] <THEN_BLOCK> <ELSE_BLOCK>
good, because can put expr on stack and stuff, efficient, thougb get's very hairy when you get to the ELSE_BLOCK, because have to remember that we were in an if, and let's say you want to know if they returned from the THEN and ELSE block? would be hard to do logic, because would have to put stuff on a stack, which would somehow be popped after the ELSE block, which then did a bunch of [if] logic, which is stupid, because the code would be fragmented all over the place.
THOUGH: I could actually reach the if and then do recursion on THEN and ELSE. Good because we know what to do when done with them
- [if] <EXPR> <THEN_BLOCK> <ELSE_BLOCK>
good, because when you reach the [if] you can just first eval <EXPR>, and then recursively evaluate THEN and ELSE. Maybe introduces more complication though? Because have to add expression function call to if token, which could be cleaner.
- <EXPR> <THEN_BLOCK> <ELSE_BLOCK> [if]
good, because would have stack that not only contains expression items, but also block items, which would 'return' something useful to ANYTHING wanting anything to do with it. Bad because either the blocks would be generic, and therefore all blocks would be treated the same, or we would make the blocks special, and they would return special variants the if statement wants. GOOD: unfifies expression and statement parsing somewhat. BAD: When generating C code, each block would be a String or something, which would be returned and receieved by the [if], which would do something. But like I said, this gets you no choice in how to process the blocks. Though maybe there is no special cases for blocks, and if there were there could be special introducer Sokens for that?

Then question on the alternatives: On which one could we build an abstraction that let's you provide functions that parse special cases, and then just give e.g. an if statement, without you needing to verify the input or whatever.
leaning to do the second, because it's in the same order as in C or assembly would need it.
Though maybe if I do IR first it won't be neccessary?
Though then the question becomes how the IR should look like...

# How to run the things:
Compile when change:

    ls . src/* | entr -c cargo r

Run c program generated by c backend:

    gcc out.c && ./a.out


# run nasm on test.asm (generated by assembly backend):

    nasm -f elf64 -F dwarf -g -o hello.o test.asm

link the resulting object to executable:

    ld -o hello.out hello.o -lc -dynamic-linker /lib/ld-linux-x86-64.so.2

run:

    ./hello.out

or use shorthand:

    cargo r && nasm -f elf64 -F dwarf -g -o hello.o out.asm && ld -o hello.out hello.o -lc -dynamic-linker /lib/ld-linux-x86-64.so.2 && ./hello.out

# Parsing
Currently recursive descent that generates reverse polish notation type flat tree `Soken`'s
Correctness of balanced delimiters ([{}]) is controlled by lexer.

```
<PROGRAM> ::= <STATEMENT>*
<STATEMENT> ::= <FN_DEF> | <CREATE_VAR> | <IF> | <WHILE> | <RETURN> | <SCOPE> | <STATEMENT_EXPR>

<FN_DEF> ::= [fn] [ident] [(] ([ident] [,]))* [)] <SCOPE>
(except last comma)
<CREATE_VAR> ::= [let] [ident] <EXPR> [;]
<IF> ::= [if] <EXPR> <SCOPE>
<WHILE> ::= [while] <EXPR> <SCOPE>
<RETURN> ::= [ret] <EXPR> [;]
<SCOPE> ::= [{] <STATEMENT>* [}]
<STATEMENT_EXPR> ::= ([ident] | [int] | [string]) <EXPR> [;]

<EXPR> ::= (<PRIMARY> [binop])* ([;] | [}] | [)] | [{])
<PRIMARY> ::= [string] | [int] | ([(] <EXPR>) | <FUNC_CALL> | [ident] (no following [(])
<FUNC_CALL> ::= [ident] [(] (<EXPR> [,])* [)]
(except last comma)
```

# Flat syntax tree shenanigans
thought - let everything collapse right into operators like shunting yard algorithm

ex: [1][2][+] -> [3]

or: [2][3][*][1][+] -> [7]

SYNTAX FOR THIS SECTION: [XYZ] around means the XYZ is some kind of token stored in an array

[XYZ]* means 0 or more of XYZ

[XYZ]+ means 1 or more of XYZ

**FUNCTION_CALL**: (later could remove that calls hold nr of arguments, could be implicit)
```
f(1,2)
-> [1] [2] [call f 2args]
<FN_CALL> ::= <EXPR>* [FuncCall(ident, nr_args)]
```
(returns what the function got)

### ALL the following return NOTHING: (not even Unit)

**CREATE_VAR**:
```
let a = 2;
->  [create_var a] [a][2][=]           two statements
FORM: [CreateVar(ident: a)] <EXPR>
or actual:
let b;
-> [CreateVar(ident)]
<CREATE_VAR> ::= [CreateVar(ident)]
```
(problem with this is you can reference the variable in the definition: let x=x)


**SEMICOLON**:  (consumes value)
```
1+2;
-> [1][2][+][;]
FORM: <EXPR> [;]
```

**SCOPES**:
```
a=2;{a=3;}
-> [a][2][=] [{] [a][3][=] [}]
<SCOPE> ::= [{] <EXPR>* [}]
```

**RETURN**:
```
return 1+2*3;
->   [2][3][*][1][+][ret]       one statement
<RETURN> ::= [ret] [EXPR] [;]
```

**WHILE**:
```
while 1 {g(2);}
->  [1] [while] [2] [call g 1arg] [}]
<WHILE> ::= <EXPR> [while] <EXPR>* [}]
```
(condition first)

**IF**:
```
if 420 {return 2;}
->    [420] [if] [2] [ret] [}]
<IF> ::= [if] <EXPR> [{] <EXPR>* [}] ((optional:)) [else] <EXPR>* [}]
```
(good that if isn't succeded by {, as this would be unnecessary, also that block is different, as returns guaranteed)


**FN_DEF**: (get nbr of args from hashmap (functions))
```
fn f(a,b) {return 3} fn g(a,b) {f(1,2);return 4;}
->  [def f 2arg] [a] [b] [3] [ret] [}]  [a] [b] [def g 2arg] [1] [2] [call f 2arg] [4] [ret] [}]
<FN_DEF> ::=  [func_def_ident] [ident]* <EXPR>* [}]
```

Making an API to look at this: TODO

# Troubles:
On linux, when you run a program it is by default line buffered (input sent on \n),
but when I tried to pipe input (./prog > lefile) the file got nothing.
This was because piping changes the buffering so it only sends data when the buffer is full.
This was fixed by calling `fflush(stdout)` at the end of _start

# OLD syntax
```
FORM $myvar === 343 + 34
$myvar === @add(1,2)
$string === "my string"
IF $myvar = 34 {
    @print(myvar)
}
```
