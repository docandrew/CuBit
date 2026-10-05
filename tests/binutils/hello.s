# A minimal x86-64 program for the binutils test: assembled and linked on
# CuBit, then compared byte for byte with the same tools' output on Linux.
        .text
        .globl _start
_start:
        movl    $60, %eax       # exit
        xorl    %edi, %edi
        syscall
        .data
message:
        .ascii  "assembled on CuBit\n"
