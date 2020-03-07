/* 
 * The bootloader will look at this image and start execution at the symbol
 *  designated as the entry point.
 */
OUTPUT_FORMAT("elf64-x86-64")
OUTPUT_ARCH(i386:x86-64)
ENTRY(start)

/* AP starting point */
AP_START        = 0x7000;

/* kernel load and link locations */
KERNEL_PHYS     = 0x00100000;
KERNEL_BASE     = 0xFFFFFFFF80000000;

/* 
 * Kernel is linked at 0xFFFF_FFFF_8000_0000. We want the various sections
 * aligned on page boundaries so we can assign different read/write/execute
 * permissions for them.
 *
 * Note that the AP real-mode startup code is linked down at 0x7000, we'll
 * tell the other cores to start executing there.
 *
 * Note, aligning each section at 4K keeps things pretty for debug builds,
 * and also let's us map each page type differently, at the cost of some
 * memory wasted from extra internal fragmentation.
 */
SECTIONS
{
    . = AP_START;

    .text_ap : AT(AP_START) {
        stext_ap = .;
        *(.text_ap_entry)
        etext_ap = .;
    }

	. = KERNEL_PHYS + KERNEL_BASE;

    KERNEL_START_VIRT = .;

    .text : AT(KERNEL_PHYS)
    {
        stext = .;
        build/boot.o (.text .text.*)    /* need this at the front */
        *( EXCLUDE_FILE(build/init.o) .text .text.*)
    }
    
    . = ALIGN(4K);
    etext = .;

    .rodata :
    {
        srodata = .;
        *(.rodata .rodata.*)
    }

    . = ALIGN(4K);
    erodata = .;
    
    .data :
    {
        sdata = .;
        *(.data .data.*)
    }

    . = ALIGN(4K);
    edata = .;
    
    .bss :
    {
        sbss = .;
        *(COMMON)
        *(.bss .bss.*)
        ebss = .;
    }

    /* 
     * We build the init process into the main kernel image here, but it will
     * get re-mapped as a user-mode process with it's own page tables.
     *
     */
     /*
     . = ALIGN(4K);
     .init :
     {
         sinit = .;
         build/init.o (.text .text.*)
         einit = .;
     }*/

    . = ALIGN(4K);          /* Round up to next page for neatness */
    KERNEL_END_VIRT = .;

    /DISCARD/ :
    {
        *(.eh_frame)
    }
}

/* 
 * Define physical addresses for these sections that we will put in our
 * Multiboot Header (see boot.asm). The linker will take our symbols
 * from boot.asm, link it in the higher-half, but then we do the math here
 * to get a physical address back out, this physical address is what the
 * asm instructions will work with.
 */
stext_phys                  = stext - KERNEL_BASE;
edata_phys                  = edata - KERNEL_BASE;
ebss_phys                   = ebss - KERNEL_BASE;
start_phys                  = start - KERNEL_BASE;
/*
bootstrap_stack_top_phys    = bootstrap_stack_top - KERNEL_BASE;
trampoline_phys             = trampoline - KERNEL_BASE;
gdt64_phys                  = gdt64 - KERNEL_BASE;
p4_table_phys               = p4_table - KERNEL_BASE;
gdt64_pointer_phys          = gdt64_pointer - KERNEL_BASE;
*/
