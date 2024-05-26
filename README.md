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

# Notes on running things:

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
