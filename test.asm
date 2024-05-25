section .text
	default rel
	global _start

	extern printf
	extern puts

_start:
	sub rsp, 16
    ; syscall number for write
    mov rax, 1

    ; file descriptor (stdout)
    mov rdi, 1

    ; pointer to the message
    lea rsi, [hello_message]

    ; message length
    mov rdx, hello_message_len

    ; invoke syscall
    syscall

    mov rax, 1
    mov rdi, 1
    lea rsi, [hello_message]
    mov rdx, hello_message_len
    syscall

    ;;; tests
    mov rax, [rsp - 0]
    mov qword [rsp - 8], rsi
    cmp rax, rbx 
    sete al
    setl al
    movzx rax, al
    
    xor rbx,rbx	;Clearing out ebx [Ebx contains error code 0 for normal exit]
 	xor rax,rax	;Clearing out eax 
	mov rax, 60	;exit sys call no. in eax
    mov edi, 0 ; return nbr
	syscall	



section .data
    hello_message db "Hello, %lu!", 10, 0
    hello_message_len equ $ - hello_message
