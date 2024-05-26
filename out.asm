
section .data
    hello_message db "Hello %ld!", 10, 0

        section .text

        default rel

        global _start

        extern printf

        _start:

        call main

        mov rax, 60

        mov edi, 0

    	syscall

        print_int:

        mov rdi, hello_message 

        mov rax, 0

        mov rsi, [rsp - 8]

        call printf

        ret

        main:
mov qword [rsp - 8], 1000000
mov rax, [rsp - 8]
mov qword [rsp - 16], rax
sub rsp, 0
call func1
add rsp, 0
mov qword [rsp - 8], rax
mov qword [rsp - 8], 5
mov rax, [rsp - 8]
mov qword [rsp - 16], rax
sub rsp, 0
call func2
add rsp, 0
mov qword [rsp - 8], rax
mov qword [rsp - 16], 1
mov rax, [rsp - 16]
mov qword [rsp - 8], rax
.L0:
mov qword [rsp - 16], 10
mov rax, [rsp - 8]
mov rbx, [rsp - 16]
cmp rax, rbx
setl al
movzx rax, al
mov qword [rsp - 16], rax
mov rax, [rsp - 16]
cmp rax, 0
je .L1
mov rax, [rsp - 8]
mov qword [rsp - 24], rax
sub rsp, 8
call func5
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
jmp .L0
.L1:
mov qword [rsp - 16], 3
mov rax, [rsp - 16]
mov qword [rsp - 24], rax
sub rsp, 8
call func2
add rsp, 8
mov qword [rsp - 16], rax
mov qword [rsp - 16], 0
mov rax, [rsp - 16]
ret
func2:
mov qword [rsp - 24], 0
mov rax, [rsp - 24]
mov qword [rsp - 16], rax
.L2:
mov rax, [rsp - 16]
mov rbx, [rsp - 8]
cmp rax, rbx
setl al
movzx rax, al
mov qword [rsp - 24], rax
mov rax, [rsp - 24]
cmp rax, 0
je .L3
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
jmp .L2
.L3:
mov qword [rsp - 24], 0
mov rax, [rsp - 24]
ret
func1:
mov qword [rsp - 24], 0
mov rax, [rsp - 24]
mov qword [rsp - 16], rax
mov qword [rsp - 32], 1
mov rax, [rsp - 32]
mov qword [rsp - 24], rax
.L4:
mov rax, [rsp - 16]
mov rbx, [rsp - 8]
cmp rax, rbx
setl al
movzx rax, al
mov qword [rsp - 32], rax
mov rax, [rsp - 32]
cmp rax, 0
je .L5
mov rax, [rsp - 16]
mov qword [rsp - 40], rax
sub rsp, 24
call print_int
add rsp, 24
mov qword [rsp - 32], rax
mov rax, [rsp - 16]
mov rbx, [rsp - 24]
add rax, rbx
mov qword [rsp - 40], rax
mov rax, [rsp - 40]
mov qword [rsp - 32], rax
mov rax, [rsp - 24]
mov qword [rsp - 16], rax
mov rax, [rsp - 32]
mov qword [rsp - 24], rax
jmp .L4
.L5:
mov qword [rsp - 32], 0
mov rax, [rsp - 32]
ret
func5:
mov qword [rsp - 16], 2
mov rax, [rsp - 8]
mov rbx, [rsp - 16]
cmp rax, rbx
setl al
movzx rax, al
mov qword [rsp - 16], rax
mov rax, [rsp - 16]
cmp rax, 0
je .L6
mov qword [rsp - 16], 1
mov rax, [rsp - 16]
ret
.L6:
mov qword [rsp - 16], 1
mov rax, [rsp - 8]
mov rbx, [rsp - 16]
sub rax, rbx
mov qword [rsp - 16], rax
mov rax, [rsp - 16]
mov qword [rsp - 24], rax
sub rsp, 8
call func5
add rsp, 8
mov qword [rsp - 16], rax
mov rax, [rsp - 16]
mov rbx, [rsp - 8]
imul rax, rbx
mov qword [rsp - 16], rax
mov rax, [rsp - 16]
ret
