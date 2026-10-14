/*
 * CuBit OS - C Runtime Startup
 * Copyright (C) 2026 Jon Andrew
 *
 * Entry point for C programs running on CuBit. Zeroes BSS, calls main(),
 * then exits with main's return value.
 *
 * The kernel starts the main thread with RDI = the length of its launch
 * argument block (0: none), mapped read-only at a fixed address
 * (docs/process-arguments.md). Its whole length is kept in
 * __cubit_launch_block_length (for the startup table, CuBit.Launch_Block)
 * and its payload's in __cubit_launch_arguments_length (for
 * Ada.Command_Line and the launch-configuration readers). An Ada main is a procedure: when Ada.Command_Line
 * set an exit status, that is the exit code instead of main's RAX.
 */

.section .bss
.balign 8
.global __cubit_launch_arguments_length
.global __cubit_launch_block_length
.global __cubit_exit_status
.global __cubit_exit_status_set
__cubit_launch_arguments_length: .quad 0
__cubit_launch_block_length:     .quad 0
__cubit_exit_status:             .quad 0
__cubit_exit_status_set:         .quad 0

.section .text
.global _start
.global _exit

_start:
    /* Keep the launch length across the BSS clear (RBX is callee-saved). */
    movq    %rdi, %rbx

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
    movq    %rbx, __cubit_launch_block_length(%rip)
    /* CAP-002: the launch block may end with the startup table and a
     * trailer (u64 table bytes, u64 "CUBITLB1"; CuBit.Launch_Envelopes).
     * Its payload readers see only the payload's length. */
    movq    %rbx, %rax            /* payload length, unless a trailer says less */
    cmpq    $16, %rbx
    jb      .launch_split_done
    movabsq $0x00005A0000000000, %rcx  /* Launch_Envelopes block address */
    addq    %rbx, %rcx            /* the block's end */
    movabsq $0x31424C5449425543, %rdx
    cmpq    -8(%rcx), %rdx
    jne     .launch_split_done
    movq    -16(%rcx), %rdx         /* table bytes */
    cmpq    $3216, %rdx             /* Startup_Tables.Maximum_Table_Bytes */
    ja      .launch_split_done
    addq    $7, %rdx
    andq    $-8, %rdx               /* padded to a word */
    addq    $16, %rdx               /* and the trailer */
    cmpq    %rbx, %rdx
    ja      .launch_split_done
    movq    %rbx, %rax
    subq    %rdx, %rax
.launch_split_done:
    movq    %rax, __cubit_launch_arguments_length(%rip)

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
