;------------------------------------------------------------------------------
; CuBitOS
; Copyright (C) 2020 Jon Andrew
;
; First user process
;------------------------------------------------------------------------------
%include "cubit.inc"

BITS 64

startInit:
mov rdi, 0xA770
mov rsi, 0xA771
mov rdx, 0xA772
mov r10, 0xA773
mov r8, 0xA774
mov r9, 0xA775
mov rax, SYSCALL_EXECVE
syscall

mov r13, 0xAC

exit:
    mov rax, SYSCALL_EXIT
    syscall
    jmp exit