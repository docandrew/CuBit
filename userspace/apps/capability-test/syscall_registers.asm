; Exercise the real syscall boundary independently of compiler asm constraints.
; C ABI: (syscall number, first syscall argument) -> 1 if registers survived.
; Only use calls that ignore arguments 1..5: GETPID, SLEEP, unknown, denied SPAWN.
bits 64
section .text
global syscall_registers_preserved
syscall_registers_preserved:
    push rbx
    push rbp
    push r12
    push r13
    push r14
    push r15

    mov rax, rdi
    mov rdi, rsi
    mov rsi, 0x12345671
    mov rdx, 0x12345672
    mov r10, 0x12345673
    mov r8,  0x12345674
    mov r9,  0x12345675
    mov rbx, 0x12345676
    mov rbp, 0x12345677
    mov r12, 0x12345678
    mov r13, 0x12345679
    mov r14, 0x1234567a
    mov r15, 0x1234567b
    push rdi

    syscall

    cmp rdi, [rsp]
    jne .failed
%macro check_register 2
    cmp %1, %2
    jne .failed
%endmacro
    check_register rsi, 0x12345671
    check_register rdx, 0x12345672
    check_register r10, 0x12345673
    check_register r8,  0x12345674
    check_register r9,  0x12345675
    check_register rbx, 0x12345676
    check_register rbp, 0x12345677
    check_register r12, 0x12345678
    check_register r13, 0x12345679
    check_register r14, 0x1234567a
    check_register r15, 0x1234567b
    mov eax, 1
    jmp .done
.failed:
    xor eax, eax
.done:
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbp
    pop rbx
    ret

section .note.GNU-stack noalloc noexec nowrite progbits
