# For the console demo: as reports two errors on its unix.stderr port.
        .text
        .globl _start
_start:
        movq $60, %rax
        frobnicate %rax
        xorl %edi, %edi
        syscall
        jmp nowhere_at_all
