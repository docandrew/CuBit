;------------------------------------------------------------------------------
; CuBitOS
; Copyright (C) 2020 Jon Andrew
;
; First user process
;------------------------------------------------------------------------------
%include "cubit.inc"

BITS 64

startInit:
mov rdi, STDOUT         ; fd
mov rsi, hello          ; buf
mov rdx, helloLen       ; count
mov rax, SYSCALL_WRITE
syscall

exit:
    mov rax, SYSCALL_EXIT
    syscall
    jmp exit

hello: db 'Hello from Userland!',10,0
helloLen: equ $-hello
