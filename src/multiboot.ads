-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- Multiboot
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

with elf; use elf;
with MemoryAreas;

package Multiboot with
    SPARK_Mode => On
is
    
    MULTIBOOT_MAGIC : constant := 16#2BADB002#;
    
    -- "unused" member of the MultibootFlags record
    type UnusedFlags is new Integer range 0..65535;

    type MultibootFlags is
        record
            hasMemoryInfo       : Boolean;  -- is there basic upper/lower memory info?
            hasBootDevice       : Boolean;  -- is a boot device set?
            hasCmdLine          : Boolean;  -- is a command line defined?
            hasModules          : Boolean;  -- are there modules available?
            
            -- next two flags are mutually exclusive
            hasAoutSyms         : Boolean;  -- is a a.out symbol table loaded?
            hasELFSectionHeader : Boolean;  -- is there an ELF section header?

            hasMemoryMap        : Boolean;  -- is there a full memory map?
            hasDriveTable       : Boolean;  -- is there drive info?
            hasConfigTable      : Boolean;  -- is there a config table?
            hasBootLoaderName   : Boolean;  -- is the boot loader name available?
            hasAPMTable         : Boolean;  -- is the APM table available?
            hasVideoInfo        : Boolean;  -- is VBE info available?
            hasFramebuffer      : Boolean;  -- is framebuffer info available?
            unused              : UnusedFlags;  -- unused data, here to prevent warning.
        end record with 
            Size => 32, Convention => C;
    
    --for MultibootFlags'Size use 32;         -- 16 bits for the Multiboot flags

    for MultibootFlags use
        record
            hasMemoryInfo       at 0 range 0..0;
            hasBootDevice       at 0 range 1..1;
            hasCmdLine          at 0 range 2..2;
            hasModules          at 0 range 3..3;
            hasAoutSyms         at 0 range 4..4;
            hasELFSectionHeader at 0 range 5..5;
            hasMemoryMap        at 0 range 6..6;
            hasDriveTable       at 0 range 7..7;
            hasConfigTable      at 0 range 8..8;
            hasBootLoaderName   at 0 range 9..9;
            hasAPMTable         at 0 range 10..10;
            hasVideoInfo        at 0 range 11..11;
            hasFramebuffer      at 0 range 12..12;
            unused              at 0 range 13..31;
        end record;

    -- Pointer to this struct is loaded in ebx by Multiboot. Since it's lower-half, we
    -- can get it from rbx, but boot.asm copies it into rsi (parameter 2 to kmain).
    -- All of the addresses below that are Unsigned_32 are going to be in lower memory.
    -- The framebuffer is an Unsigned_64 since it can be somewhere in upper memory.
    type MultibootInfo is
        record                                          --bit | description
                                                        --=======================================================
            flags               : MultibootFlags;       --   0 bitfield describing which fields below are present
            mem_lower           : Unsigned_32;          --   4 amount of lower memory
            mem_upper           : Unsigned_32;          --   8 amount of upper memory
            boot_device         : Unsigned_32;          --  12 partition numbers
            cmdline             : Unsigned_32;          --  16 char* command-line passed by GRUB
            mods_count          : Unsigned_32;          --  20 number of kernel modules
            mods_addr           : Unsigned_32;          --  24 ptr to kernel modules
            ELFsec              : ELFSectionHeader;     --  28-40 ELF section headers
            -- num      : Unsigned_32
            -- size     : Unsigned_32
            -- addr     : Unsigned_32
            -- shndx    : Unsigned_32
            mmap_length         : Unsigned_32;          -- 44 length of buffer w/ memory map
            mmap_addr           : Unsigned_32;          -- 48 ptr to memory map buffer
            drives_length       : Unsigned_32;          -- 52 length of drive buffer
            drives_addr         : Unsigned_32;          -- 56 ptr to drive buffer
            config_table        : Unsigned_32;          -- 60 ROM config table 
            boot_loader_name    : Unsigned_32;          -- 64 null-terminated string with boot loader name
            apm_table           : Unsigned_32;          -- 68 ptr to APM table
            vbe_control_info    : Unsigned_32;          -- 72 phys addr of control info returned by VBE function 00h
            vbe_mode_info       : Unsigned_32;          -- 76 phys addr of control info returned by VBE function 01h

            vbe_mode            : Unsigned_16;          -- 80 VBE mode in VBE 3.0 format
            vbe_interface_seg   : Unsigned_16;          -- 82 protected-mode interface
            vbe_inferface_off   : Unsigned_16;          -- 84 protected-mode interface
            vbe_interface_len   : Unsigned_16;          -- 86 protected-mode interface

            framebuffer_addr    : Integer_Address;      -- 88 address to linear framebuffer
            framebuffer_pitch   : Unsigned_32;          -- 96 linear framebuffer pitch
            framebuffer_width   : Unsigned_32;          --100 linear framebuffer width/horiz. resolution
            framebuffer_height  : Unsigned_32;          --104 linear framebuffer height/vert. resolution
            framebuffer_bpp     : Unsigned_8;           --108 linear framebuffer bits-per pixel
            framebuffer_type    : Unsigned_8;           --109 if 0, indexed color is used. if 1, direct RGB color.
            
            -- If direct RGB color is used, then the next fields will be set:
            framebuffer_red_field_position      : Unsigned_8;   -- 110
            framebuffer_red_mask_size           : Unsigned_8;   -- 111
            framebuffer_green_field_position    : Unsigned_8;   -- 112
            framebuffer_green_mask_size         : Unsigned_8;   -- 113
            framebuffer_blue_field_position     : Unsigned_8;   -- 114
            framebuffer_blue_mask_size          : Unsigned_8;   -- 115
        end record with 
            Size => 116*8,
            Convention => C;
    
    for MultibootInfo use
        record
            flags               at 0    range 0 .. 31;
            mem_lower           at 4    range 0 .. 31;
            mem_upper           at 8    range 0 .. 31;
            boot_device         at 12   range 0 .. 31;
            cmdline             at 16   range 0 .. 31;
            mods_count          at 20   range 0 .. 31;
            mods_addr           at 24   range 0 .. 31;
            ELFsec              at 28   range 0 .. 127;
            mmap_length         at 44   range 0 .. 31;
            mmap_addr           at 48   range 0 .. 31;
            drives_length       at 52   range 0 .. 31;
            drives_addr         at 56   range 0 .. 31;
            config_table        at 60   range 0 .. 31;
            boot_loader_name    at 64   range 0 .. 31;
            apm_table           at 68   range 0 .. 31;
            vbe_control_info    at 72   range 0 .. 31;
            vbe_mode_info       at 76   range 0 .. 31;

            vbe_mode            at 80   range 0 .. 15;
            vbe_interface_seg   at 82   range 0 .. 15;
            vbe_inferface_off   at 84   range 0 .. 15;
            vbe_interface_len   at 86   range 0 .. 15;

            framebuffer_addr    at 88   range 0 .. 63;
            framebuffer_pitch   at 96   range 0 .. 31;
            framebuffer_width   at 100  range 0 .. 31;
            framebuffer_height  at 104  range 0 .. 31;
            framebuffer_bpp     at 108  range 0 .. 7;
            framebuffer_type    at 109  range 0 .. 7;
            
            -- If direct RGB color is used, then the next fields will be set:
            framebuffer_red_field_position      at 110  range 0 .. 7;
            framebuffer_red_mask_size           at 111  range 0 .. 7;
            framebuffer_green_field_position    at 112  range 0 .. 7;
            framebuffer_green_mask_size         at 113  range 0 .. 7;
            framebuffer_blue_field_position     at 114  range 0 .. 7;
            framebuffer_blue_mask_size          at 115  range 0 .. 7;
        end record;
    
    MEMORY_USABLE       : constant := 1;
    MEMORY_RESERVED     : constant := 2;
    MEMORY_ACPI         : constant := 3;
    MEMORY_HIBER        : constant := 4;
    MEMORY_DEFECTIVE    : constant := 5;

    type MBMemoryArea is
        record 
            size : Unsigned_32;
            addr : Unsigned_64;
            length : Unsigned_64;
            kind : Unsigned_32;
        end record with 
            Size => 24*8,
            Convention => C;
    
    for MBMemoryArea use
        record
            size    at 0 range 0 .. 31;
            addr    at 4 range 0 .. 63;
            length  at 12 range 0 .. 63;
            kind    at 20 range 0 .. 31;
        end record;


    ---------------------------------------------------------------------------
    -- getMemoryAreas - using the memory map provided by Multiboot, identify
    --  areas of usable memory and location of ACPI tables.
    -- Note: this function returns an extra memory area for the framebuffer,
    --  to ensure we map it into memory even when it does not show up as part
    --  of the other memory areas.
    ---------------------------------------------------------------------------
    function getMemoryAreas(mbinfo : in MultibootInfo)
        return MemoryAreas.MemoryAreaArray with
        Pre => mbinfo.flags.hasMemoryMap;
    

    ---------------------------------------------------------------------------
    -- numAreas - 
    --  Return the number of memory areas described in the Multiboot info
    --  structure. This does not add any extra areas that aren't explicitly in
    --  the 
    ---------------------------------------------------------------------------
    function numAreas(mbinfo : in MultibootInfo) 
        return Natural with
        Pre => mbinfo.flags.hasMemoryMap;

end Multiboot;