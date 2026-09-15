; Match Multiboot_Entry's transport constants. The hosted entry test assembles
; this exact gate and compares it against the production SPARK admission core.
; Input: EDI = loader magic, ESI = 32-bit physical header address.
; Does not read loader memory or clobber either input register.
%macro ADMIT_MULTIBOOT_ENTRY 1
    cmp edi, 0x2BADB002
    jne %1
    test esi, esi
    jz %1
    cmp esi, (0x40000000 - 118)
    ja %1
%endmacro
