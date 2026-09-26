/*
 * CuBit libc programs (userspace/libc). As userspace/c/link.ld, plus:
 * the ELF and program headers are loaded (the start code finds the TLS
 * template through them), thread-local sections, fini/preinit arrays, and
 * .eh_frame kept for C++ unwinding.
 */
ENTRY(_start)

SECTIONS
{
    . = 0x400000 + SIZEOF_HEADERS;

    .text : ALIGN(16)
    {
        /* KEEP: crti/crtn split _init/_fini across objects; the epilogue
         * has no symbol, and --gc-sections (Rust links) would drop it,
         * leaving _init to fall through into the next function. */
        KEEP(*(SORT_NONE(.init)))
        *(.text .text.*)
        KEEP(*(SORT_NONE(.fini)))
    }

    .rodata : ALIGN(4096)
    {
        *(.rodata .rodata.*)
    }

    .eh_frame_hdr : { *(.eh_frame_hdr) }
    .eh_frame : { KEEP(*(.eh_frame)) }
    .gcc_except_table : { *(.gcc_except_table .gcc_except_table.*) }

    /* Writable data starts on its own page even when .tdata is empty, so
     * no page is both executable and writable. */
    . = ALIGN(4096);
    .tdata : ALIGN(64)
    {
        *(.tdata .tdata.*)
    }
    .tbss :
    {
        *(.tbss .tbss.*)
    }

    .data : ALIGN(4096)
    {
        *(.data .data.*)
    }

    .preinit_array : ALIGN(8)
    {
        __preinit_array_start = .;
        KEEP(*(.preinit_array))
        __preinit_array_end = .;
    }
    .init_array : ALIGN(8)
    {
        __init_array_start = .;
        KEEP(*(SORT_BY_INIT_PRIORITY(.init_array.*) SORT_BY_INIT_PRIORITY(.ctors.*)))
        KEEP(*(.init_array .ctors))
        __init_array_end = .;
    }
    .fini_array : ALIGN(8)
    {
        __fini_array_start = .;
        KEEP(*(SORT_BY_INIT_PRIORITY(.fini_array.*) SORT_BY_INIT_PRIORITY(.dtors.*)))
        KEEP(*(.fini_array .dtors))
        __fini_array_end = .;
    }

    __bss_start = .;
    .bss : ALIGN(4096)
    {
        *(.bss .bss.*)
        *(COMMON)
    }
    __bss_end = .;

    .cubit.caps (INFO) : ALIGN(4) { KEEP(*(.cubit.caps)) }
    .cubit.access (INFO) : ALIGN(4) { KEEP(*(.cubit.access)) }
    .cubit.id (INFO) : ALIGN(4) { KEEP(*(.cubit.id)) }
    .cubit.streams (INFO) : ALIGN(4) { KEEP(*(.cubit.streams)) }

    /DISCARD/ :
    {
        *(.comment)
        *(.note.*)
        *(.note.gnu.build-id)
    }
}
