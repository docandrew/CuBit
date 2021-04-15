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
mov rdi, initexeLen     ; filename len
mov rsi, initexe        ; filename
mov rdx, initflag       ; open flags
mov rcx, initmode       ; file mode
mov rax, SYSCALL_OPEN
syscall

exit:
    mov rax, SYSCALL_EXIT
    syscall
    jmp exit

hello: db 'Hello from Userland!',0
helloLen: equ $-hello

initflag: equ 0xBEEFCACE
initmode: equ 0
initexe: db 'A:/asdf.txt',0        ;'A:/boot/init.exe',0
initexeLen: equ $-initexe