/*
 * CuBit OS - Userspace C Linker Script
 * Copyright (C) 2026 Jon Andrew
 *
 * Places text at 0x400000 (standard for x86-64 executables).
 * Kernel ELF loader reads PT_LOAD segments and maps pages accordingly.
 * Stack is set up by the kernel at PROCESS_STACK_TOP_VIRT.
 */

ENTRY(_start)

SECTIONS
{
    . = 0x400000;

    .text : ALIGN(4096)
    {
        *(.text .text.*)
    }

    .rodata : ALIGN(4096)
    {
        *(.rodata .rodata.*)
    }

    .data : ALIGN(4096)
    {
        *(.data .data.*)
    }

    __bss_start = .;
    .bss : ALIGN(4096)
    {
        *(.bss .bss.*)
        *(COMMON)
    }
    __bss_end = .;

    /DISCARD/ :
    {
        *(.eh_frame)
        *(.comment)
        *(.note.*)
        *(.note.gnu.build-id)
    }
}
