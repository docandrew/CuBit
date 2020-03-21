-------------------------------------------------------------------------------
-- Cubit OS
-- Copyright (C) 2019 Jon Andrew
--
-- @summary Virtual Memory Manager
--
-------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;

with MemoryAreas;
with Virtmem;

Pragma Elaborate_All (Virtmem);

package Mem_mgr with
    SPARK_Mode => On
is

    subtype kernelTextPages is Virtmem.PFN range
        Virtmem.addrToPFN(Virtmem.K2P(To_Integer(Virtmem.stext'Address))) .. 
        Virtmem.addrToPFN(Virtmem.K2P(To_Integer(Virtmem.etext'Address) - 1));

    subtype kernelROPages is Virtmem.PFN range
        Virtmem.addrToPFN(Virtmem.K2P(To_Integer(Virtmem.srodata'Address))) .. 
        Virtmem.addrToPFN(Virtmem.K2P(To_Integer(Virtmem.erodata'Address) - 1));

    subtype kernelRWPages is Virtmem.PFN range
        Virtmem.addrToPFN(Virtmem.K2P(To_Integer(Virtmem.sdata'Address))) .. 
        Virtmem.addrToPFN(Virtmem.K2P(To_Integer(Virtmem.ebss'Address) - 1));

    subtype kernelStackPages is Virtmem.PFN range
        Virtmem.addrToPFN(Virtmem.V2P(Virtmem.STACK_BOTTOM)) ..
        Virtmem.addrToPFN(Virtmem.V2P(Virtmem.STACK_TOP-1));

    -- Kernel section physical addresses
    -- stextPhys       : constant Virtmem.PhysAddress := 
            -- To_Integer(Virtmem.stext'Address) - Virtmem.KERNEL_BASE;
-- 
    -- etextPhys       : constant Virtmem.PhysAddress := 
            -- To_Integer(Virtmem.etext'Address) - Virtmem.KERNEL_BASE;
-- 
    -- srodataPhys     : constant Virtmem.PhysAddress := 
            -- To_Integer(Virtmem.srodata'Address) - Virtmem.KERNEL_BASE;
    -- 
    -- erodataPhys     : constant Virtmem.PhysAddress := 
            -- To_Integer(Virtmem.erodata'Address) - Virtmem.KERNEL_BASE;
    -- 
    -- sdataPhys       : constant Virtmem.PhysAddress := 
            -- To_Integer(Virtmem.sdata'Address) - Virtmem.KERNEL_BASE;
    -- 
    -- edataPhys       : constant Virtmem.PhysAddress := 
            -- To_Integer(Virtmem.edata'Address) - Virtmem.KERNEL_BASE;
    -- 
    -- sbssPhys        : constant Virtmem.PhysAddress := 
            -- To_Integer(Virtmem.sbss'Address) - Virtmem.KERNEL_BASE;
    -- 
    -- ebssPhys        : constant Virtmem.PhysAddress := 
            -- To_Integer(Virtmem.ebss'Address) - virtmem.KERNEL_BASE;
    -- 
    --Stack addresses
    -- stackBottomPhys : constant Virtmem.PhysAddress :=
            -- Virtmem.STACK_BOTTOM - Virtmem.LINEAR_BASE;
    -- 
    -- stackTopPhys    : constant Virtmem.PhysAddress :=
            -- Virtmem.STACK_TOP - Virtmem.LINEAR_BASE;

    ---------------------------------------------------------------------------
    -- setupLinearMapping
    -- @description In boot.asm we have both identity-mapped and linear-mapped
    -- the first 1GiB of physical memory. Here, we unmap the identity-mapping
    -- and linear-map all addressable physical memory (even if it refers to
    -- stuff like the ACPI tables) in the system to the higher-half 
    -- linear-mapped range.
    --
    -- Kernel .text, .data, .bss and .rodata sections are mapped to _both_
    -- the linear range and back to the kernel range.
    --
    -- Uses the boot allocator for allocating the page tables.
    --
    -- @WARNING: This function maps all "reserved" memory areas as normal
    -- read-write areas. They should be used with care, and likely need to
    -- be remapped with the appropriate I/O flags before use. An exception to
    -- this is the video framebuffer area, which this function will map as an
    -- I/O region.
    --
    -- @NOTE: This function also maps everything under physical 0x100000 as
    -- R/W, even if it does not show up in the list of memory areas provided.
    -- It does this prior to mapping the given areas, so if an area in the
    -- areas argument occupies memory below 0x100000 it will be remapped with
    -- the appropriate flags. Note that the AP bootup code is placed by the
    -- bootloader at 0x7000, but since the CPUs executing it start in Real
    -- Mode, that the memory there doesn't need to be mapped a particular way
    -- (or at all).
    --
    ---------------------------------------------------------------------------
    procedure setupLinearMapping(areas : in MemoryAreas.MemoryAreaArray);

    ---------------------------------------------------------------------------
    -- mapIOFrame
    -- Maps a physical frame of memory-mapped I/O to the Kernel's
    --  virtual address space using a linear mapping.
    -- @param addr - base address of the physical frame
    -- @return True if successful, False otherwise.
    --
    -- generic parameters:
    -- @param allocate, a procedure that can be used to allocate
    --  a single physical frame at a time.
    ---------------------------------------------------------------------------
    generic
        with procedure allocate(newFrame : out Virtmem.PhysAddress);
    function mapIOFrame(addr : in Virtmem.PhysAddress) return Boolean;

    ---------------------------------------------------------------------------
    -- switchAddressSpace
    -- switch to the top-level set of page tables in Mem_mgr (kernelP4).
    ---------------------------------------------------------------------------
    procedure switchAddressSpace;

private
    ---------------------------------------------------------------------------
    -- determineFlagsAndMapFrame
    -- Given a physical address, 
    ---------------------------------------------------------------------------
    generic
        with procedure allocate(newFrame : out Virtmem.PhysAddress);
    procedure determineFlagsAndMapFrame(frame : in Virtmem.PFN);
    
    generic
        with procedure allocate(newFrame : out Virtmem.PhysAddress);
    procedure mapBigFrame(bigFrame : in Virtmem.BigPFN);

    generic
        with procedure allocate(newFrame : out Virtmem.PhysAddress);
    procedure mapBigFrameAsSmallFrames(bigFrame : in Virtmem.BigPFN);

    generic
        with procedure allocate(newFrame : out Virtmem.PhysAddress);
    procedure mapIOArea(area : in MemoryAreas.MemoryArea);

    generic
        with procedure allocate(newFrame : out Virtmem.PhysAddress);
    procedure mapArea(area : in MemoryAreas.MemoryArea);
end Mem_mgr;