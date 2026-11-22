/* Augment the CuBit static layout before its wildcard note discard. This
 * section must remain allocated and be covered by a readable PT_LOAD. */
SECTIONS
{
    .note.gnu.build-id : ALIGN(4)
    {
        __cubit_mesa_build_id_begin = .;
        KEEP(*(.note.gnu.build-id))
        __cubit_mesa_build_id_end = .;
    }
    __cubit_mesa_code_begin = ADDR(.text);
    __cubit_mesa_code_end = ADDR(.text) + SIZEOF(.text);
}
INSERT BEFORE .rodata;
