/*
 * CuBit OS - Userspace C Linker Script
 * Copyright (C) 2026 Jon Andrew
 *
 * Places text at 0x400000 (standard for x86-64 executables).
 * Kernel ELF loader reads PT_LOAD segments and maps pages accordingly.
 * The final link must supply -z stack-size=SIZE.  CuBit treats the resulting
 * PT_GNU_STACK p_memsz as a required launch contract and does not invent a
 * default when the header or size is absent.
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
