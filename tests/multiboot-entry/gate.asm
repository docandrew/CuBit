BITS 64
%include "multiboot_entry.inc"
section .text
global test_boot_entry_gate
test_boot_entry_gate:
    ADMIT_MULTIBOOT_ENTRY .reject
    mov eax, 1
    ret
.reject:
    xor eax, eax
    ret
section .note.GNU-stack noalloc noexec nowrite progbits
