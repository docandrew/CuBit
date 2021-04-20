;------------------------------------------------------------------------------
; CuBitOS
; Copyright (C) 2019 Jon Andrew
;
; CuBit boot procedure, execution of the kernel starts here. This is expected
; to have been loaded by a Multiboot-compliant bootloader such as GRUB.
;
; TODO: Some of the code here expects fairly modern CPUs for things like the
;  FSGSBASE instructions, and it would be helpful to add checks here to ensure
;  that they are present before continuing to boot.
;------------------------------------------------------------------------------

%include "cubit.inc"

global MultiBootHeader
global bootstrap_stack_top
global trampoline
global p4_table
global boot_ap_entry

; Phys addresses calculated from our virtually-addressed labels by the linker
extern stext_phys
extern edata_phys
extern ebss_phys
extern start_phys

; Ada initialization routines (elaboration)
extern adainit

; Entry into our Ada code for the first CPU
extern kmain

; Entry into our Ada code for subsequent CPUs
extern apEnter

; Ada code will set this variable so we know what CPU is entering here when
; booting up the APs.
extern startingCPU

; Declare constants used for creating a multiboot header.
; note that we do not set bit 16 in FLAGS here, so the boot loader will use
; the entry address specified by our linker script.
MBALIGN     equ  0x1                    ; align loaded modules on page boundaries
MEMINFO     equ  0x2                    ; provide memory map
VIDEOMODE   equ  0x4                    ; tell GRUB to set video mode
MEMLOC      equ  0x10000                ; tell GRUB to load our kernel at a specific location

; Uncomment this line to tell GRUB to use the video mode set in the header
; Multiboot 'flag' field
FLAGS       equ  MBALIGN | MEMINFO | VIDEOMODE
;FLAGS       equ MBALIGN | MEMINFO | MEMLOC
;FLAGS       equ MBALIGN | MEMINFO

MAGIC       equ  0x1BADB002             ; 'magic number' lets bootloader find the header
CHECKSUM    equ -(MAGIC + FLAGS)        ; checksum of above, to prove we are multiboot

; Multiboot header
section .text
align 4
MultiBootHeader:
    dd MAGIC
    dd FLAGS
    dd CHECKSUM

; Memory Load Locations (Physical)
    dd stext_phys               ; mboot header address (start of .text)
    dd stext_phys               ; beginning of text segment
    dd edata_phys               ; end of the data segment
    dd ebss_phys                ; end of the BSS segment
    dd start_phys               ; entry address
 
; For setting video mode
    dd 0x0          ; mode type (0-linear, 1-text)
    dd 1024         ; width
    dd 768          ; height
    dd 32           ; bpp depth

; Stack setup (see end for location)
STACKSIZE equ 0x1000

; The linker script specifies start as the entry point to the kernel and the
; bootloader will jump to this position once the kernel has been loaded.
global start

; still 32-bit at this point
BITS 32
start:
    cli
    ; Set up the physical address of our stack
    mov esp, (bootstrap_stack_top - KERNEL_BASE)

    ; Before we start running checks, store multiboot header and magic # that was passed from GRUB
    mov edi, eax 		                    ; argument 1 (magic #) to kmain2 in boot64.asm (64-bit calling convention)
    mov esi, ebx 		                    ; arg 2 (multiboot header struct address)

    ; Zero out ebx for the initial processor. AP cores will come in with a
    ; non-zero ebx for their CPU number.
    xor ebx, ebx

; Where APs will enter.
boot_ap_entry:
    mov eax, (p4_table - KERNEL_BASE)		; load page table
    mov cr3, eax

enable_pae:
    ; enable PAE, set CR4.PAE (bit 5) = 1
    mov eax, cr4
    ;or eax, 1 << 2     ; disable RDTSC calls from Ring 3 - this is touted
                        ;  as a Spectre mitigation, but other methods exist to
                        ;  get precise timing data in app code
    or eax, 1 << 5      ; enable PAE
    or eax, 1 << 16     ; enable FSGSBASE instructions
    mov cr4, eax

enable_long_mode:
    ; set bits in EFER (extended feature) register
    mov ecx, 0xC0000080
    rdmsr
    or eax, 1 << 0      ; set SYSCALL active
    or eax, 1 << 8      ; set long mode bit
    or eax, 1 << 11     ; set NXE bit
    wrmsr

enable_paging:
    ; enable paging in CR0 register
    mov eax, cr0
    or eax, 1 << 31
    mov cr0, eax

;    mov ecx, (gdt64_pointer - KERNEL_BASE)
    lgdt [dword (gdt64_pointer - KERNEL_BASE)]

    ; jump to long mode
    ; trampoline to 64-bit

    jmp gdt64_code:(trampoline - KERNEL_BASE)

; error handler
;  error:
;  	cli
;  	mov dword [0xb8000], 0x4f524f45
;  	mov dword [0xb8004], 0x4f3a4f52
;  	mov dword [0xb8008], 0x4f204f20
;  	mov byte [0xb800a], al
;  .errhang	
;  	hlt
;  	jmp [dword (.errhang - KERNEL_BASE)]

; Set up our initial GDT for jump to long mode. Kernel segments only.
; see section 4.8 in AMD manual volume 2
; We reload the GDT later with usermode code+data segments and a TSS.
align 16
gdt64:
gdt64_nulldesc: equ $ - gdt64					; null descriptor
    dq 0
gdt64_code: equ $ - gdt64
    dq (1<<43) | (1<<44) | (1<<47) | (1<<53)	; code segment, (43=1, 44=1, 47=present, 53=long mode)
gdt64_data: equ $ - gdt64
    dq (1<<44) | (1<<47)						; data segment, (44=1, 47=present)

gdt64_pointer:
    dw $ - gdt64 - 1                            ; size of the GDT (minus 1)
    dq (gdt64 - KERNEL_BASE)                    ; need physical address

; location of our early bootstrap stack
align 16
bootstrap_stack_bottom:
    times STACKSIZE db 0
bootstrap_stack_top:
;p5_table					; future Intel spec (56-bit virtual addresses)

; Since the page tables are page-aligned, we can just set each table entry
; to their respective address, since the bottom 12 bits are already 0.
;
; in this bootstrap code, we map all physical memory to both the lower-half
; as well as the higher-half, in 2 places. The kernel must be linked in
; the top 2GiB of virtual memory, so we need 0xFFFF_FFFF_8000_0000 to be
; mapped to the first 1GiB of memory as well.
;
; We'll use huge (2MiB) pages here to start, then later we will switch
; to a 4k paging system.
;
; All of these ranges map to the same first 1GiB of physical memory:
; 0x0000_0000_0000_0000 - 0x0000_0000_4000_0000 (identity-mapped range)
; 0xFFFF_8000_0000_0000 - 0xFFFF_8000_4000_0000 (linear-mapped range)
; 0xFFFF_FFFF_8000_0000 - 0xFFFF_FFFF_C000_0000 (kernel .text)
;
; Also see memory limits in virtmem.ads
;
align 4096
p4_table:				        ; PML4E
    dq ((boot_p3_table - KERNEL_BASE) + 0x3)	; p4[0] = p3[0], present | writable
    times 255 dq 0			 
    dq ((boot_p3_table - KERNEL_BASE) + 0x3)	; p4[256] = p3[0], present | writable
    times 254 dq 0
    dq ((boot_p3_table - KERNEL_BASE) + 0x3)    ; p4[511] = p3[0], present | writable

align 4096
boot_p3_table:				    ; PDPE
    dq ((boot_p2_table - KERNEL_BASE) + 0x3)	; p3[0] = p2[0], present | writable
    times 509 dq 0			                    ; non-present entries
    dq ((boot_p2_table - KERNEL_BASE) + 0x3)    ; p3[510] = p2[0], present | writable
    dq 0                                        ; p3[511] = non-present.

align 4096
boot_p2_table:                  ; PDE, set PDE.PS (bit 7) = 1 for 2MB pages
    %assign i 0
    %rep 512
    dq (i + 0x83)               ; huge, present, writable
    %assign i (i + 0x200000)
    %endrep
;align 4096
;boot_p1_table:				; PD. 512 2MB tables here, identity-mapped first 1GB
;	%rep 512*25
;	dq (i << 12) | 0x03
;	%assign i i+1
;	%endrep
BITS 64
align 16

trampoline:
    mov rax, qword higher_half_start
    jmp rax

;section .text
higher_half_start:
    ; add null descriptors, long mode doesn't care about these.
    xor ax, ax
    mov ss, ax
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax

    ; If rbx is 0, then the BSP is executing this, otherwise an AP.
    test rbx, rbx
    jnz setup_ap

setup_bsp:
    ; Setup our kernel stack.
    mov rsp, qword (STACK_TOP)

    ; Add a stack canary to bottom of primary stack for CPU #0
    mov rax, qword (STACK_TOP - PER_CPU_STACK_SIZE + SEC_STACK_SIZE)
    mov rbx, 0xBAD_CA11_D37EC7ED
    mov [rax], rbx

    ; Save rdi, rsi so adainit doesn't clobber them
    push rdi
    push rsi

    ; Initialize with adainit for elaboration prior to entering Ada.
    mov rax, qword adainit
    call rax

    ; Restore arguments to kmain
    pop rsi
    pop rdi

    ; call into Ada code
    mov rax, qword kmain
    call rax

setup_ap:
    ; see exactly what CPU was just booted
    mov rbx, qword [(startingCPU - KERNEL_BASE)]

    ; Subsequent per-CPU stacks will be STACK_TOP - (2 pages * the CPU number in rbx)
    ; Make sure the max number of CPUs booted * the PER_CPU_STACK_SIZE doesn't exceed
    ; STACK_TOP - STACK_BOTTOM!
    mov rax, PER_CPU_STACK_SIZE     ; rax = PER_CPU_STACK_SIZE
    mul rbx                         ; rdx:rax = PER_CPU_STACK_SIZE * startingCPU
    mov rcx, rax                    ; rcx = PER_CPU_STACK_SIZE * startingCPU
    mov rax, qword (STACK_TOP)      ; rax = STACK_TOP
    sub rax, rcx                    ; rax = STACK_TOP - (PER_CPU_STACK_SIZE * startingCPU)
    mov rsp, rax                    ; rsp = STACK_TOP - (PER_CPU_STACK_SIZE * startingCPU)
    sub rax, (PER_CPU_STACK_SIZE - SEC_STACK_SIZE)     ; rax = cpu primary stack bottom
    mov rcx, 0xBAD_CA11_D37EC7ED    ; add stack canary at bottom of primary stack.
    mov [rax], rcx

    ; put CPU number as argument to the apEnter function
    mov rdi, rbx
    call apEnter

hang:
    hlt
    jmp hang