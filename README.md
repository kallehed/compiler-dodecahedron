# compiler-dodecahedron
Dodecahedron programming language compiler

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


Program correctness thoughts:
 - A variable can only be used after it has been created. It's lifetime cascades to child blocks. Variable shadowing may not happen in descendant blocks. OBVIOUS
   Implement this by keeping track of created variables and making sure that used variables have been declared before. Have a 'global' hashset and
   let each block have a Vec of their declared variables, and let a block remove it's variables after it has run out.
- Should every AST node have a source reference, like, char range? Yes

TODO: Don't allow nested functions, right now: bad behaviour.

# Notes on running things:
run when change: `ls . src/* | entr cargo r`

run program after c backend done:
    `gcc out.c && ./a.out`

run nasm on test.asm:
    `nasm -f elf64 -F dwarf -g -o hello.o test.asm`
link the resulting object to executable:
    `ld -o hello.out hello.o -lc -dynamic-linker /lib/ld-linux-x86-64.so.2`
run: `./hello.out`

or faster version:
cargo r
nasm -f elf64 -F dwarf -g -o hello.o out.asm && ld -o hello.out hello.o -lc -dynamic-linker /lib/ld-linux-x86-64.so.2 && ./hello.out
./hello.out

# Flat syntax tree shenanigans
thought - let everything collapse right into operators like shunting yard algorithm
ex: [1][2][+] -> [3]
or: [2][3][*][1][+] -> [7]

CREATE_VAR:
let a = 2;
 ->  [crtvar a] [a][2][=]           two statements
FORM: [CREATE_VAR] [EXPR]

SCOPES:
a=2;{a=3;}
[a][2][=] [{] [a][3][=] [}]
FORM: [{] [STAT]... [}]

RETURN:
return 1+2*3;
->   [2][3][*][1][+][ret]       one statment
FORM: [Int] [RETURN]

WHILE:
while 1 {g(2);}
->  [1] [while] [{] [2] [call g 1arg] [}]
FORM: [CONDEXPR] [WHILE] [{] [EXPR] [}]

IF:
if 420 {return 2;} else {return 3;}
->    [420] [if] [{] [2] [ret] [}] [{] [3] [ret] [}]
FORM: [COND] [IF] [{] [TRUE_STATEMENTS] [}] [{] [FALSE_STATEMENTS] [}]

FN_DEF:
fn f(a,b) {return 3} fn g(a,b) {f(1,2);return 4;}
-> [a] [b] [3] [ret] [def f 2arg ] [a] [b] [1] [2] [call f 2arg] [4] [ret] [def g 2arg]
FORM: [ARG]... [FUNCNAME: NR_ARGS] [{] [STATEMENT] [}]


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
