;------------------------------------------------------------------------------
; CuBit OS
; Copyright (C) 2020 Jon Andrew
;
; AP processors will start here once sent the IPI sequence, in real-mode,
; however the A20 line does not need to be enabled. We enter protected mode
; here and then jump into boot.asm (ap_entry) to finish setting up long mode.
;------------------------------------------------------------------------------

%include "cubit.inc"

global ap_start

extern p4_table
extern boot_ap_entry

BITS 16

; we'll link this section down low, since it has to be in first
; 65535 bytes for real mode.
section .text_ap_entry

ap_real:
    cli

    ; zero out the segment registers
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax

    ; load our gdt defined below
    lgdt [ap_gdt_pointer]

    ; turn on protection
    mov eax, cr0
    or eax, 0x0000_00001            ; bit 0 = protection enabled
    mov cr0, eax

    ;mov ecx, 0x134f                 ; test code, remove me
    ; make the jump to protected mode
    jmp ap_gdt_code:ap_protected

BITS 32

align 4
ap_protected:
    ; set our data segment in segment registers
    mov ax, ap_gdt_data
    mov ds, ax
    mov es, ax
    mov ss, ax

    ; zeroize fs, gs
    mov ax, 0
    mov fs, ax
    mov gs, ax

    ; need a small stack here so our call can push to it
    mov esp, ap_stack_top

    ; set ebx to 1 so boot.asm knows we're an AP
    mov ebx, 1
    ;mov edx, 0x5000_1337                 ; test code, remove me
    ; paging not enabled yet on this CPU, so we'll call the function
    ; at the physical memory here.
    call (boot_ap_entry - KERNEL_BASE)

hang:
    hlt
    jmp hang

    ; just a test to make sure this code got loaded at the right place
    ; dq  0x70BACC09

align 16
ap_stack_bottom:
    times 32 db 0
ap_stack_top:

align 16
ap_gdt:
ap_gdt_nulldesc: equ $ - ap_gdt
    dq 0
ap_gdt_code: equ $ - ap_gdt         ; base 0, limit 0xFFFF_FFFF
    dw 0xFFFF                       ; lower 16 bits of segment
    dw 0x0000                       ; lower 16 bits of base
    db 0x00                         ; bits 16:23 of base
    db 0x9A                         ; non-system segment, readable, 
                                    ; executable, DPL=0
    db 0xCF                         ; 4k scaled, 32-bit default, bits 16:19 of limit
    db 0x00                         ; bits 24:31 of base
ap_gdt_data: equ $ - ap_gdt       
    dw 0xFFFF
    dw 0x0000
    db 0x00
    db 0x92                         ; writable
    db 0xCF
    db 0x00

ap_gdt_pointer:
    dw $ - ap_gdt - 1
    dd ap_gdt