/*
 * CuBit OS - C Runtime Startup
 * Copyright (C) 2026 Jon Andrew
 *
 * Entry point for C programs running on CuBit. Zeroes BSS, calls main(),
 * then exits with main's return value.
 */

.section .text
.global _start
.global _exit

_start:
    /* End of stack frame pointer list (for debuggers/stack traces) */
    xorq    %rbp, %rbp
    pushq   %rbp            /* rip = 0 */
    pushq   %rbp            /* rbp = 0 */
    movq    %rsp, %rbp

    /* Zero BSS section */
    leaq    __bss_start(%rip), %rdi
    leaq    __bss_end(%rip), %rcx
    subq    %rdi, %rcx
    jz      .bss_done       /* skip if BSS is empty */
    shrq    $3, %rcx        /* count in qwords */
    xorq    %rax, %rax
    rep     stosq
.bss_done:

    /* Run C constructors (libnsfb surface registration uses these). */
    leaq    __init_array_start(%rip), %r12
    leaq    __init_array_end(%rip), %r13
.init_next:
    cmpq    %r13, %r12
    je      .init_done
    movq    (%r12), %rax
    addq    $8, %r12
    testq   %rax, %rax
    je      .init_next
    call    *%rax
    jmp     .init_next
.init_done:

    /* Call main(argc=0, argv=NULL) */
    xorq    %rdi, %rdi      /* argc = 0 */
    xorq    %rsi, %rsi      /* argv = NULL */
    call    main

    /* Exit with return value from main (in %eax) */
    movq    %rax, %rdi
    call    _exit

_exit:
    movq    $0, %rax        /* SYSCALL_EXIT = 0 */
    syscall
    jmp     _exit           /* should never return */

.section .note.GNU-stack,"",@progbits
