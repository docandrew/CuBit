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
    /* The ELF and program headers are loaded, read-only, on their own
     * page (the loader takes page-aligned segments; text stays apart from
     * them): the exception unwinder finds the program's .eh_frame_hdr
     * through them (CuBit.Unwinder_Support's dl_iterate_phdr, from
     * __ehdr_start). */
    . = 0x400000 + SIZEOF_HEADERS;

    .text : ALIGN(4096)
    {
        *(.text .text.*)
    }

    .rodata : ALIGN(4096)
    {
        *(.rodata .rodata.*)
    }

    /* GNU build-id (every link passes --build-id): names the program's
     * debug file, which image staging keeps on the host
     * (docs/image-debug-info.md). Placed before /DISCARD/ drops .note.*. */
    .note.gnu.build-id : ALIGN(4)
    {
        KEEP(*(.note.gnu.build-id))
    }

    /* Unwind tables for zero-cost Ada exceptions and tracebacks (EXC-001):
     * the call-frame information, the binary-search table over it that
     * --eh-frame-hdr builds (PT_GNU_EH_FRAME), and the handlers' tables. */
    .eh_frame_hdr : { *(.eh_frame_hdr) }
    .eh_frame : { KEEP(*(.eh_frame)) }
    .gcc_except_table : { *(.gcc_except_table .gcc_except_table.*) }

    /* Thread-local data: the static TLS segment (PT_TLS), its image then
     * its zero-initialized rest. CuBit.Thread_Areas (the user runtime's
     * per-thread secondary-stack slot) and Rust std's thread blocks copy
     * it below each thread pointer they make, from the symbols after it. */
    . = ALIGN(4096);
    .tdata : ALIGN(8)
    {
        *(.tdata .tdata.*)
    }
    .tbss : ALIGN(8)
    {
        *(.tbss .tbss.*)
    }
    __cubit_tls_start = ADDR(.tdata);
    __cubit_tls_image_bytes = SIZEOF(.tdata);
    __cubit_tls_bytes = ADDR(.tbss) + SIZEOF(.tbss) - ADDR(.tdata);
    __cubit_tls_align = MAX(ALIGNOF(.tdata), ALIGNOF(.tbss));

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

    /* The rest of the ELF manifest (ccl-manifest), kept under
     * --gc-sections like the sections above. */
    .cubit.startup (INFO) : ALIGN(4)
    {
        KEEP(*(.cubit.startup))
    }
    .cubit.channels (INFO) : ALIGN(4)
    {
        KEEP(*(.cubit.channels))
    }

    .cubit.resources (INFO) : ALIGN(4)
    {
        KEEP(*(.cubit.resources))
    }

    .cubit.devices (INFO) : ALIGN(4)
    {
        KEEP(*(.cubit.devices))
    }

    .cubit.launch (INFO) : ALIGN(4)
    {
        KEEP(*(.cubit.launch))
    }

    .cubit.description (INFO) : ALIGN(4)
    {
        KEEP(*(.cubit.description))
    }

    /DISCARD/ :
    {
        *(.comment)
        *(.note.*)
        *(.note.gnu.build-id)
    }
}
