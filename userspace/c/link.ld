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

    .init_array : ALIGN(8)
    {
        __init_array_start = .;
        KEEP(*(.init_array .init_array.*))
        __init_array_end = .;
    }

    __bss_start = .;
    .bss : ALIGN(4096)
    {
        *(.bss .bss.*)
        *(COMMON)
    }
    __bss_end = .;

    .cubit.caps (INFO) : ALIGN(4)
    {
        KEEP(*(.cubit.caps))
    }

    .cubit.access (INFO) : ALIGN(4)
    {
        KEEP(*(.cubit.access))
    }

    .cubit.id (INFO) : ALIGN(4)
    {
        KEEP(*(.cubit.id))
    }

    .cubit.streams (INFO) : ALIGN(4)
    {
        KEEP(*(.cubit.streams))
    }

    /DISCARD/ :
    {
        *(.eh_frame)
        *(.comment)
        *(.note.*)
        *(.note.gnu.build-id)
    }
}
