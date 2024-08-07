# compiler-dodecahedron
Dodecahedron programming language compiler

# How to run the things:
Compile when change:

    ls . src/* | entr -c cargo r

Test c backend continuously:

    ls . src/* | entr -c -s 'cargo r && gcc -g out.c && ./a.out'

Run c program generated by c backend:

    gcc out.c && ./a.out


# run nasm on test.asm (generated by assembly backend) (not here anymore):

    nasm -f elf64 -F dwarf -g -o hello.o test.asm

link the resulting object to executable:

    ld -o hello.out hello.o -lc -dynamic-linker /lib/ld-linux-x86-64.so.2

run:

    ./hello.out

or use shorthand:

    cargo r && nasm -f elf64 -F dwarf -g -o hello.o out.asm && ld -o hello.out hello.o -lc -dynamic-linker /lib/ld-linux-x86-64.so.2 && ./hello.out

# turn llvm's test.bc into code to run

get test.s:

    llc test.bc

compile and link with standard library as well (print_int):

    clang test.s dode_std.c

run:
    ./a.out

shorthand:

    cargo r && llc test.bc && clang test.s dode_std.c && ./a.out

something else:
    llc -filetype=obj -relocation-model=pic test.bc -o test.o

    clang test.o -fPIE -pie -o a.out



## Convert AST into IR
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

# Structure and what things do:

### Lexer: lexer.rs
Takes source file and outputs tokens
Tokens could be identifiers (variable or function names) or keywords (if while { ( [ )
Checks that the delimiters (, [ and { are balanced.
### Parser: parser.rs
Takes tokens and outputs sokens (Semantic tokens)
Recursive descent operator precedence parser that makes sure all the tokens fit the grammar of the language. The sokens represent the program in reverse polish notation, not only expressions, but also scopes/blocks/{}
### Verification: ast_verify.rs
Takes Sokens and does type-checking, that-you-return checking, variable used before declaration.
Also, turns RValues into LValues if they are next to setters (=,+=,-=) (might remove this)
TODO: Remove things after a return statement. Or maybe do this in ir_gen? Bc have do to hard work in llvm_backend to manually remove that later
### Ir generation: ir_gen.rs, ir.rs
Generate IR (immediate representation) in the form of Vec<ByteCode>.
ir_gen.rs uses functions prefixed with mk_ in ir.rs to generate bytecode.
## alternative backends:

### c code generation: c_backend.rs
Iterates over IR using `InstrIterator` from `ir.rs` and generates c code using it
this c code contains a million goto's, because the IR is like assembly
### llvm ir gen: llvm_backend.rs
Iterates over IR using `InstrIterator`. Generates LLVM IR. We make our own `print_int` in LLVM IR, that calls printf with printf being declared.
TODO: make better llvm gener that looks at the Value and checks if it is a constant, ptr or register, and does better initial code generation based on that.

# language design thoughts:
## Mutability of variables
It's easier to reason about a program if we know certain variables are immutable. This should be done by default so you only make the variables that actually need it mutable.
    let mut a = 3; // rust
    let ~a = 3; // dodecahedron
    !~a = 3; // with ! declaration syntax
### parameter immutability
 we could make parameters mutable with:
     fn func(~a, b, ~c) {...}
but maybe just make them immutable?
## On ways to declare a variable: (declaration )
    let a = 0; // rusty way

    !a = 0; // HERE I AM! (conflicts with NOT operator)

    @a = 0; // I am AT here (no conflicts)
when a function returns multiple values:
    // rust, both immutable
    let (a, b) = func();
    // or one mutable
    let (mut a, b) = func();
Notice how in both scenarios both a and b are declared and initialized. While this may reduce reading overhead, it makes the following case longer:
    let mut a = 3;
    while a > 3 {
        let (new_a, b) = func();
        a = new_a; // have to set a here
        print(b);
    }
Could instead be written like:
    !~a = 3;
    while a > 3 {
      a, !b = func();
      print(b);
    }
Also reduces clutter, parenthesis

In an alternative rust syntax, where `let` was more of a prefix than a statement introducer, one could write:
   let mut a = 3;
   while a > 3 {
       (a, let b) = func();
       print(b);
   }
though that looks kind of confusing, and uses lots of space for `let`, when names are more important.

Alternative: var, const
   var a = 2;
   const b = 3;
less good, because longer to type. Also, how does this work with multiple return values?
    var a, b = func(); // makes both mutable BAD
so only alternative is:
    var a, const b = func();
but that gets very verbose and unnecessarily long so:
    !~a, !b = func(); // is better
though, it's worse for the normal case, where you just want to get the returned values with new names and use them, which rust does well: `let (a,b,c,d,e,f) = func();`
so I could do:
    let ~a, b = func();
But might as well test the ! syntax, maybe it's good IDK.

## on tuples:
you can have a single value, though in actuality that is the same as having a tuple with a single value. (i32) = i32, in wheA.

rust way of dealing with tuples:
    let (x, y) = my_tuple_returning_func();
    // or
    let xy = my_tuple_returning_func(); // then xy.0 = x, xy.1 = y
If you have a tuple of (a, b) and a function is: fn(a,b) you have to do:
    my_non_tuple_taking_func(xy.0, xy.1);
    // it would be nicer with just
    my_non_tuple_taking_func(xy)
so xy is some sort of object holding the structure required for 2 arguments.



# Program correctness thoughts:
 - Variable shadowing not allowed.
 TODO: Don't allow nested functions, right now: bad behaviour.
 TODO: Better errors, be able to show 2 locations (use when bad delimiters, also when no return)

# Design principles
### On how errors are shown to user:
Compilation errors are a special case, instead of storing where a token came from inside the token, have a separate vec of (usize,usize) to store locations. When lexing, you just have to write to this Vec. When compilation error -> can do slow things like look up stuff in this big Vec, but that's fine.
Then the Soken's have a parallel array that references the Token idx that it came from. When failing on a Soken, we take it's idx -> we look up origins to get the token idx, then use that to index into source code locations array


# Things to think about:
How should the Soken's be iterated? Should I create some sort of abstraction around it to make the verifyer and the c backend simpler? But I don't want to lose performance, also don't want to lose readability of code (sequentiality), though readability already suffers a bit from reverse polish notation style expressions, bc have to express stuff backwards.

How should the c backend concatinate expressions to produce a string C can understand? I have made 3 implementations: Bad slow 1 million String impl | Put everything on stack -> (Put stuff on another stack and iterate | recurse over items, where dependencies have been written in) (the one without any recursion is most lines) CURRENTLY: Create a million strings, this was deemed most maintanable, and it's the last step of the compiler, so it doesn't matter as much if it is slow?
NEW VERSION: Instead generate C from IR, which solves all of this (Though IR generation is Vec heavy)

Should the parser be de-recursified? Maybe could put all actions to do on a stack, though this would maybe make it overall less readable AND increase line-count. Could try an implementation.
BAD IDEA - too hard to implement in shunting yard. Better to specify the correct behaviour imperatively than try to prohibit all bad behaviours. Recursive descent operator precedence parsing FTW.

Instead of doing a bunch of stuff with the Soken's, maybe I should generate an IR. It could be 4 item IR like: `a = b 'op' c`. Set register to op of two registers.
Problem: don't know how to call functions? Function call could specify at which register they begin, but seems weird, would also be weird if function call was variable length, though maybe could store that in separate buffer (which register values function is called with)
Something like f(1+2,3) would be converted to $0 = i64 1; $1 = i64 2; $2 = $0 + $1; $3 = i64 3; call f with $2,$3, could have table that says which f uses, or we could create 2 more registers: $4 = $2, $5 = $3, and do `call f start at $4`, and we would know to use $4 and %5. (This example is dumb because in the original the registers get next to each other). We could also delay the setting of the registers to not have to create 2 new registers. So we do: $0 = i64 1; $1 = i64 2; THEN: $2 = $0 + $1; $3 = i64 3; call f from $2. Though this seems harder to implement, because have to wait for everything to evaluate to next-to-last, before setting last to register. So would have to have a stack or something. IMPORTANT: Don't overthink with structs, complicated stuff THIS LANGUAGE ONLY HAS i64!
Other solution: Instead of specifying which register they start at, the function call could be followed by the registers used. You would look up how many args the function call takes, then take that many things of the IR. Could be implemented with the InstrIterator, so it returns some kind of slice containing the registers.

How should stuff like if statements be represented in Sokens?

alternatives:
- <EXPR> [if] <THEN_BLOCK> <ELSE_BLOCK>
good, because can put expr on stack and stuff, efficient, though get's very hairy when you get to the ELSE_BLOCK, because have to remember that we were in an if, and let's say you want to know if they returned from the THEN and ELSE block? would be hard to do logic, because would have to put stuff on a stack, which would somehow be popped after the ELSE block, which then did a bunch of [if] logic, which is stupid, because the code would be fragmented all over the place.
THOUGH: I could actually reach the if and then do recursion on THEN and ELSE. Good because we know what to do when done with them
- [if] <EXPR> <THEN_BLOCK> <ELSE_BLOCK>
good, because when you reach the [if] you can just first eval <EXPR>, and then recursively evaluate THEN and ELSE. Maybe introduces more complication though? Because have to add expression function call to if token, which could be cleaner. ALSO: Maybe there is just ONE obvious way we always want to look through an if statement, and EVEN if there was some absurd case where you want to generate the code for the ELSE_BLOCK first, you COULD just walk over the EXPR and THEN_BLOCK, and then do them later. It's a little bit slower, but much better than anything else, like allocating them beforehand.
- <EXPR> <THEN_BLOCK> <ELSE_BLOCK> [if]
good, because would have stack that not only contains expression items, but also block items, which would 'return' something useful to ANYTHING wanting anything to do with it. Bad because either the blocks would be generic, and therefore all blocks would be treated the same, or we would make the blocks special, and they would return special variants the if statement wants. GOOD: unfifies expression and statement parsing somewhat. BAD: When generating C code, each block would be a String or something, which would be returned and receieved by the [if], which would do something. But like I said, this gets you no choice in how to process the blocks. Though maybe there is no special cases for blocks, and if there were there could be special introducer Sokens for that?

Then question on the alternatives: On which one could we build an abstraction that let's you provide functions that parse special cases, and then just give e.g. an if statement, without you needing to verify the input or whatever.
leaning to do the second, because it's in the same order as in C or assembly would need it.
Though maybe if I do IR first it won't be neccessary?
Though then the question becomes how the IR should look like...

Why generate an IR (intermediate representation)? So you have the Sokens, which have the property that they can be errorchecked and whatever. They contain names. Not good for optimization. We need an IR so we can get something that is good at being lowered down to C/assembly/llvmIR without having to do weird nonsense like we gotta do with the Sokens.
How should the IR look? It currently just has an infinite amount of "registers" which therefore hold both variables and inbetween expressions. Make it like Risc V idk.

Also on IR: IR has it's own indexing for functions, this is so you can index an array to get info about functions (params, regs_used). If I still used IdentIdx this would have to be a HashMap

How should the rust code be written? Currently I have `State` structs that hold 'global' information
about the process of a pass (lexer, parser, verifyer, ir_gen) so I get a bunch of stuff from a func call
put it all into my struct. Thus I have to write the types of the input THREE times. Also, when
I am doing logic, I call a bunch of methods like: `self.get_expr` or `self.pop_stack` to do something
in my `State` context. This always passes around a `State` pointer, and I usually hold EVERYTHING
in this struct to remove friction of all the context needed as arguments. Though this doesn't feel very efficient. Also, because 99% of methods are like `fn XXX(&mut self, ...)` (they take mutable to Self), you can't do things like: self.XXX(self.YYY()), which is SUPER-annoying and destroys code readability somewhat.
SOLUTION (bad/maybe good): make all the methods macros. The macros are defined in the function.
This means they can refer to all the args of the function (some of the context), and ALSO variables (if the macro is defined AFTER the variable). This is very nice. Though all of them will be inlined... which is also probably fine unless I want to to something complicated or weird assertion. THOUGH I could put function call into a macro if I want to. (You can also do {} in macro to make block that returns thing, rust is very nice in that way, with returning blocks, like holy shit)
Problem: Can't do recursion anymore.
Problem: Macros are less typed

Why are the lexer tokens so big? They are 4 bytes, which means simple keywords like { or ( take 4 bytes (instead of the 1 byte they would take in utf8). How to fix? Solution: Store less things in the Tokens: Instead have separate array of things like identifiers, keywords... Though this does not mesh well with rust. Especially in our current case: Everything we hold in the Tokens are less than 2 bytes, this means we can have ONE parallel array, and use the same index into Token for the auxiliary array. Won't implement this now though, freezes the code.

# Parsing and grammar of language
Currently recursive descent that generates reverse polish notation type flat tree `Soken`'s
Correctness of balanced delimiters ([{}]) is controlled by lexer.
grammar of language from the perspective of the parser:
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

# Flat syntax tree shenanigans + grammar of Soken's (Semantic tokens)
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
->  [a] [init_var `a` to prev]           two statements
FORM:  <EXPR> [InitVar(ident: a)] [;]
or actual:
let b;
-> [InitVar(ident)]
<CREATE_VAR> ::= [CreateVar(ident)] [;]
```
// (problem with this is you can reference the variable in the definition: let x=x)
(try to fix this by postfixing the InitVar thing instead of prefixing it)


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
<SCOPE> ::= [{] <EXPR>* [}] [drop_scope]
```

**RETURN**:
```
return 1+2*3;
->   [2][3][*][1][+][ret]       one statement
<RETURN> ::=  <EXPR> [ret]
```

**WHILE**:
```
while 1 {g(2);}
->  [1] [while] [2] [call g 1arg] [}]
<WHILE> ::= <EXPR> <BLOCK> [while]
```
(condition first)

**IF**:
```
if 420 {return 2;}
->    [420]  [{] [ret] [}] [{] [}] [if] [2]
<IF> ::=  <EXPR> <BLOCK> <BLOCK> [if]
```
(good that if isn't succeded by {, as this would be unnecessary, also that block is different, as returns guaranteed)


**FN_DEF**: (get nbr of args from hashmap (functions))
```
fn f(a,b) {return 3} fn g(a,b) {f(1,2);return 4;}
->  [def f 2arg] [a] [b] [3] [ret] [}]  [a] [b] [def g 2arg] [1] [2] [call f 2arg] [4] [ret] [}]
<FN_DEF> ::=  [func_def_ident] [ident]* <EXPR>* [}] [end_func_def]
```

Making an API to look at this: TODO

# IR
2 byte alignment for all the things
variable length encoding for different instructions
Following format says how many 2bytes follow and and what they represent after each header 2byte

```
    2
LOAD_REG: reg reg
 -> .0 = .1

    2
LOAD_INT: reg int
 -> .0 = .1

    3
OP: reg op reg reg
 -> .0 = .2 `.1` .3

    1
JUMP: label
 -> jump to .0

    2
JUMP_REG_ZERO: reg label
 -> if .0==0: jump to .1

    1
LABEL: label
 -> .0:

    1
RETURN: reg
 -> return .0

    1
FUNC_DEF: fnidx
 -> fn_def with idx .0

    0
END_FUNC:
 -> end_func

    ?
CALL: reg fnidx reg*
 -> call fn .1 with X many regs starting at .2, place result into .0
  (get nbr of arguments though auxiliary ir_functions)
```

# Troubles:
On linux, when you run a program it is by default line buffered (input sent on \n),
but when I tried to pipe input (./prog > lefile) the file got nothing.
This was because piping changes the buffering so it only sends data when the buffer is full.
This was fixed by calling `fflush(stdout)` at the end of _start

Accidentally read the bytecode at a 1-index offset, leading to EVERYTHING getting misinterpreted , lead to un-debuggable segfault for some reason >:(

Incredibly STUPID and ANNOYING error, that seemed to replicate everywhere without giving a trace of itself, only infecting other pieces of code, making me debug something COMPLETELY different for hours because I THOUGHT something was wrong with my c ffi. AHHHH
I got an i1 from a comparison, then I loaded that into a alloca memory, not realizing that it would only set ONE bit of my 64 bit register memory. SO later when I tried to use the alloca memory, everything failed because it already had bits somewhere else.

# OLD syntax
```
FORM $myvar === 343 + 34
$myvar === @add(1,2)
$string === "my string"
IF $myvar = 34 {
    @print(myvar)
}
```
