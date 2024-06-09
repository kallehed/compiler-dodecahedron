
section .data

    hello_message db "%ld", 10, 0

        section .text

        default rel
     ; important so we don't have to write rel everywhere
        global _start

        extern printf

        extern fflush

        extern stdout

        _start:

        call main
         ; call main
        mov rdi, [stdout]
       ; call fflush
        call fflush

        mov rax, 60
      ; exit program
        mov edi, 0

    	syscall

        print_int:

        mov rdi, hello_message 

        mov rax, 0

        mov rsi, [rsp - 8]

        call printf

        ret

        main:
mov qword [rsp - 8], 6
mov rax, [rsp - 8]
mov qword [rsp - 16], rax
sub rsp, 0
call print_int
add rsp, 0
mov qword [rsp - 8], rax
mov qword [rsp - 16], 1
mov rax, [rsp - 16]
mov qword [rsp - 8], rax
mov qword [rsp - 24], 2
mov rax, [rsp - 24]
mov rbx, [rsp - 8]
add rax, rbx
mov qword [rsp - 24], rax
mov qword [rsp - 32], 12
mov rax, [rsp - 24]
mov rbx, [rsp - 32]
add rax, rbx
mov qword [rsp - 24], rax
mov qword [rsp - 32], 2
mov rax, [rsp - 24]
mov rbx, [rsp - 32]
add rax, rbx
mov qword [rsp - 24], rax
mov qword [rsp - 32], 2
mov rax, [rsp - 24]
mov rbx, [rsp - 32]
cmp rax, rbx
setg al
movzx rax, al
mov qword [rsp - 24], rax
mov qword [rsp - 32], 10
mov rax, [rsp - 24]
mov rbx, [rsp - 32]
cmp rax, rbx
setl al
movzx rax, al
mov qword [rsp - 24], rax
mov rax, [rsp - 24]
mov qword [rsp - 16], rax
mov qword [rsp - 32], 3
mov rax, [rsp - 16]
mov rbx, [rsp - 8]
imul rax, rbx
mov qword [rsp - 40], rax
mov qword [rsp - 48], 2
mov rax, [rsp - 40]
mov rbx, [rsp - 48]
imul rax, rbx
mov qword [rsp - 40], rax
mov rax, [rsp - 16]
mov rbx, [rsp - 40]
sub rax, rbx
mov qword [rsp - 40], rax
mov rax, [rsp - 32]
mov rbx, [rsp - 40]
add rax, rbx
mov qword [rsp - 32], rax
mov rax, [rsp - 32]
mov qword [rsp - 24], rax
mov rax, [rsp - 8]
mov rbx, [rsp - 16]
add rax, rbx
mov qword [rsp - 32], rax
mov rax, [rsp - 32]
mov rbx, [rsp - 24]
add rax, rbx
mov qword [rsp - 32], rax
mov rax, [rsp - 32]
mov qword [rsp - 40], rax
sub rsp, 24
call print_int
add rsp, 24
mov qword [rsp - 32], rax
mov qword [rsp - 8], 0
mov rax, [rsp - 8]
cmp rax, 0
je .L0
mov qword [rsp - 8], 69
mov rax, [rsp - 8]
mov qword [rsp - 16], rax
sub rsp, 0
call print_int
add rsp, 0
mov qword [rsp - 8], rax
jmp .L1
.L0:
mov qword [rsp - 8], 420
mov rax, [rsp - 8]
mov qword [rsp - 16], rax
sub rsp, 0
call print_int
add rsp, 0
mov qword [rsp - 8], rax
.L1:
mov qword [rsp - 8], 1000000
mov qword [rsp - 16], 0
mov qword [rsp - 24], 1
mov rax, [rsp - 24]
mov qword [rsp - 32], rax
mov rax, [rsp - 16]
mov qword [rsp - 24], rax
mov rax, [rsp - 8]
mov qword [rsp - 16], rax
sub rsp, 0
call func5
add rsp, 0
mov qword [rsp - 8], rax
mov qword [rsp - 8], 5
mov rax, [rsp - 8]
mov qword [rsp - 16], rax
sub rsp, 0
call func6
add rsp, 0
mov qword [rsp - 8], rax
mov qword [rsp - 16], 1
mov rax, [rsp - 16]
mov qword [rsp - 8], rax
.L2:
mov qword [rsp - 16], 10
mov rax, [rsp - 8]
mov rbx, [rsp - 16]
cmp rax, rbx
setl al
movzx rax, al
mov qword [rsp - 16], rax
mov rax, [rsp - 16]
cmp rax, 0
je .L3
mov rax, [rsp - 8]
mov qword [rsp - 24], rax
sub rsp, 8
call func7
add rsp, 8
mov qword [rsp - 16], rax
mov rax, [rsp - 16]
mov qword [rsp - 24], rax
sub rsp, 8
call print_int
add rsp, 8
mov qword [rsp - 16], rax
mov qword [rsp - 16], 1
mov rax, [rsp - 16]
add rax, [rsp - 8]
mov qword [rsp - 8], rax
jmp .L2
.L3:
mov qword [rsp - 16], 3
mov rax, [rsp - 16]
mov qword [rsp - 24], rax
sub rsp, 8
call func6
add rsp, 8
mov qword [rsp - 16], rax
mov qword [rsp - 16], 0
mov rax, [rsp - 16]
ret
func8:
mov qword [rsp - 8], 45
mov rax, [rsp - 8]
ret
mov qword [rsp - 16], 34
mov rax, [rsp - 16]
mov qword [rsp - 8], rax
mov qword [rsp - 16], 1
mov rax, [rsp - 16]
cmp rax, 0
je .L4
mov qword [rsp - 16], 3
mov rax, [rsp - 16]
ret
jmp .L5
.L4:
.L5:
.L6:
mov qword [rsp - 16], 1
mov rax, [rsp - 16]
cmp rax, 0
je .L7
mov qword [rsp - 16], 4
mov rax, [rsp - 16]
ret
jmp .L6
.L7:
func6:
mov qword [rsp - 24], 0
mov rax, [rsp - 24]
mov qword [rsp - 16], rax
.L8:
mov rax, [rsp - 16]
mov rbx, [rsp - 8]
cmp rax, rbx
setl al
movzx rax, al
mov qword [rsp - 24], rax
mov rax, [rsp - 24]
cmp rax, 0
je .L9
mov qword [rsp - 24], 1
mov rax, [rsp - 24]
add rax, [rsp - 16]
mov qword [rsp - 16], rax
mov qword [rsp - 24], 0
mov rax, [rsp - 24]
mov qword [rsp - 32], rax
sub rsp, 16
call print_int
add rsp, 16
mov qword [rsp - 24], rax
jmp .L8
.L9:
mov qword [rsp - 24], 0
mov rax, [rsp - 24]
ret
func5:
mov rax, [rsp - 16]
mov qword [rsp - 32], rax
mov rax, [rsp - 24]
mov qword [rsp - 40], rax
.L10:
mov rax, [rsp - 32]
mov rbx, [rsp - 8]
cmp rax, rbx
setl al
movzx rax, al
mov qword [rsp - 48], rax
mov rax, [rsp - 48]
cmp rax, 0
je .L11
mov rax, [rsp - 32]
mov qword [rsp - 56], rax
sub rsp, 40
call print_int
add rsp, 40
mov qword [rsp - 48], rax
mov rax, [rsp - 32]
mov rbx, [rsp - 40]
add rax, rbx
mov qword [rsp - 56], rax
mov rax, [rsp - 56]
mov qword [rsp - 48], rax
mov rax, [rsp - 40]
mov qword [rsp - 32], rax
mov rax, [rsp - 48]
mov qword [rsp - 40], rax
jmp .L10
.L11:
mov qword [rsp - 48], 0
mov rax, [rsp - 48]
ret
func7:
mov qword [rsp - 16], 2
mov rax, [rsp - 8]
mov rbx, [rsp - 16]
cmp rax, rbx
setl al
movzx rax, al
mov qword [rsp - 16], rax
mov rax, [rsp - 16]
cmp rax, 0
je .L12
mov qword [rsp - 16], 1
mov rax, [rsp - 16]
ret
jmp .L13
.L12:
.L13:
mov qword [rsp - 16], 1
mov rax, [rsp - 8]
mov rbx, [rsp - 16]
sub rax, rbx
mov qword [rsp - 16], rax
mov rax, [rsp - 16]
mov qword [rsp - 24], rax
sub rsp, 8
call func7
add rsp, 8
mov qword [rsp - 16], rax
mov rax, [rsp - 16]
mov rbx, [rsp - 8]
imul rax, rbx
mov qword [rsp - 16], rax
mov rax, [rsp - 16]
ret
