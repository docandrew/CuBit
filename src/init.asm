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

testOpen:
mov rdi, initexe        ; file
mov rsi, initflag       ; file flags
mov rdx, initmode       ; file mode
mov rax, SYSCALL_OPEN
syscall

;mov r13, 0xAC

exit:
    mov rax, SYSCALL_EXIT
    syscall
    jmp exit

hello: db 'Hello from Userland!',10
helloLen: equ $-hello

initflag: equ 0xBEEFCACE
initmode: equ 0
initexe: db '/boot/init.exe',10,0
initexeLen: equ $-initexe