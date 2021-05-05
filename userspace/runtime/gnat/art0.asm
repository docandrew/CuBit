;
; Ada Runtime equivalent to crt0.S
;
; Assume program loader put *argv and *envp on the stack
; rdi = argc
; rsi = argv
; rdx = envc
; rcx = envp
; rbp = 0 for stack frame pointers

BITS 64

global _start
extern main

SYSCALL_EXIT    equ 0

section .text
align 4

_start:
    ; End of stack frame pointer list
    mov rbp, 0
    push rbp        ; rip = 0
    push rbp        ; rbp = 0
    mov rbp, rsp    ; rbp = rsp

    ; main calls adainit and then _ada_main in turn.
    call main

    ; terminate process w/ exit code
    mov rdi, rax
    call _exit

_exit:
    mov rax, SYSCALL_EXIT
    syscall
    jmp _exit
