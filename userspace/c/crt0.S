/*
 * CuBit OS - C Runtime Startup
 * Copyright (C) 2026 Jon Andrew
 *
 * Start code of the Ada services and freestanding C programs: _start is
 * the shared one (userspace/runtime/start/cubit_start.S), which keeps the
 * launch block's lengths and calls __cubit_language_start below. That
 * zeroes BSS, installs the main thread's thread area, runs the C
 * constructors, calls main(), then exits with main's return value. An Ada main is a procedure: when Ada.Command_Line
 * set an exit status, that is the exit code instead of main's RAX.
 */

    .intel_syntax noprefix
    .include "cubit_start.S"
    .att_syntax prefix

.section .bss
.balign 8
.global __cubit_exit_status
.global __cubit_exit_status_set
__cubit_exit_status:             .quad 0
__cubit_exit_status_set:         .quad 0

.section .text
.global __cubit_language_start
.weak __cubit_thread_start
.global _exit

__cubit_language_start:
    /* End of the frame pointer list (for debuggers/stack traces): the
     * shared _start's call pushed the return address, so one zero frame
     * pointer keeps the stack aligned for the calls below (RBP is 0). */
    pushq   %rbp
    movq    %rsp, %rbp

    /* Zero BSS section (the launch lengths are initialized data). */
    leaq    __bss_start(%rip), %rdi
    leaq    __bss_end(%rip), %rcx
    subq    %rdi, %rcx
    jz      .bss_done       /* skip if BSS is empty */
    shrq    $3, %rcx        /* count in qwords */
    xorq    %rax, %rax
    rep     stosq
.bss_done:

    /* The main thread's thread pointer (static TLS) and secondary stack
     * (CuBit.Thread_Areas.Start_Main), before any constructor or Ada code
     * can use them. Weak: a program that never uses the secondary stack
     * links none, and its FS base stays zero. */
    leaq    __cubit_thread_start(%rip), %rax
    testq   %rax, %rax
    jz      .thread_done
    call    *%rax
.thread_done:

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

    /* Exit with return value from main (in %eax), or the recorded status */
    movq    %rax, %rdi
    cmpq    $0, __cubit_exit_status_set(%rip)
    je      .exit_now
    movq    __cubit_exit_status(%rip), %rdi
.exit_now:
    call    _exit

_exit:
    movq    $0, %rax        /* SYSCALL_EXIT = 0 */
    syscall
    jmp     _exit           /* should never return */

.section .note.GNU-stack,"",@progbits
