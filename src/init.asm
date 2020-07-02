;------------------------------------------------------------------------------
; CuBitOS
; Copyright (C) 2020 Jon Andrew
;
; First user process
;------------------------------------------------------------------------------
%include "cubit.inc"

BITS 64

startInit:
mov rdi, 1              ; fd - STDOUT
mov rsi, hello          ; buf
mov rdx, helloLen       ; count
mov rax, SYSCALL_WRITE
syscall

mov r13, 0xAC

exit:
    mov rax, SYSCALL_EXIT
    syscall
    jmp exit

hello: db 'Hello from Userland!',10
helloLen: equ $-hello
