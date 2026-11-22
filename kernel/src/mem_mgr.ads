-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2019 Jon Andrew
--
-- @summary Virtual Memory Manager
--
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

with MemoryAreas; use MemoryAreas;
with Virtmem;

Pragma Elaborate_All (Virtmem);

-- Linker-symbol bounds and live page-table mappings are Ada boundaries.
package Mem_mgr is

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
    -- setup
    --
    -- @summary Setup linear mapping of system memory
    --
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
    procedure setup (areas : in MemoryAreas.MemoryAreaArray);

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
        with procedure allocate (newFrame : out Virtmem.PhysAddress);
    function mapIOFrame (addr  : in Virtmem.PhysAddress;
                         flags : in Unsigned_64 := Virtmem.PG_IO) return Boolean;

    ---------------------------------------------------------------------------
    -- switchAddressSpace
    -- switch to the top-level set of page tables in Mem_mgr (kernelP4),
    -- keeping cached translations where CPUs tag them (TLB_Context).
    ---------------------------------------------------------------------------
    procedure switchAddressSpace;

    -- The kernel's top-level page table (physical), as CR3 holds it.
    function kernelRoot return Virtmem.PhysAddress;

    ---------------------------------------------------------------------------
    -- mapIOArea
    -- Given a memory area with type MemoryAreas.IO, map all the frames within
    -- it into the kernel's page tables.
    ---------------------------------------------------------------------------
    generic
        with procedure allocate (newFrame : out Virtmem.PhysAddress);
    procedure mapIOArea (area  : in MemoryAreas.MemoryArea;
                         flags : in Unsigned_64 := Virtmem.PG_IO) with
        Pre => area.kind = MemoryAreas.IO;

    ---------------------------------------------------------------------------
    -- mapKernelMemIntoProcess - copy the kernel's higher-half P4 entries
    -- into a new process's page tables. No TLB flush: no CPU has loaded
    -- them yet, and the slot's address-space tag begins a new life
    -- (TLB_Context.Begin_Life).
    --
    -- @param procP4 - The new process's P4 table.
    ---------------------------------------------------------------------------
    procedure mapKernelMemIntoProcess (procP4 : in out Virtmem.P4);

    ---------------------------------------------------------------------------
    -- unmapKernelMemFromProcess - before a process's page tables are freed,
    -- clear their kernel half so only the process's own tables are deleted.
    -- No TLB flush: no CPU has them loaded any more.
    --
    -- @param procP4 - The ended process's P4 table.
    ---------------------------------------------------------------------------
    procedure unmapKernelMemFromProcess (procP4 : in out Virtmem.P4);

    ---------------------------------------------------------------------------
    -- createGuardPage - unmap a single 4KB page from the kernel's
    -- linear-mapped region so that accesses fault. If the page falls within
    -- a 2MB big page, the big page is split into 512 individual 4KB
    -- mappings first. Used to create guard pages below kernel stacks.
    --
    -- @param physAddr - physical address of the page to unmap
    ---------------------------------------------------------------------------
    procedure createGuardPage (physAddr : in Virtmem.PhysAddress);

    -- Physical exhaustion leaves the mapping unchanged and returns False.
    procedure tryCreateGuardPage
      (physAddr : Virtmem.PhysAddress; success : out Boolean);

    ---------------------------------------------------------------------------
    -- removeGuardPage - restore a previously unmapped guard page so the
    -- buddy allocator can reuse the frame. Must be called before freeing
    -- the underlying frame.
    --
    -- @param physAddr - physical address of the page to remap
    ---------------------------------------------------------------------------
    procedure removeGuardPage (physAddr : in Virtmem.PhysAddress);

    ---------------------------------------------------------------------------
    -- retireKernelImagePage - hand one page of the kernel image (a page of
    -- the reclaimable boot section) to the buddy allocator's view: its
    -- linear mapping becomes writable data, as for any free frame, and its
    -- kernel-image alias is removed, so a stale access through the old
    -- symbol faults instead of reading or writing reused memory. Only during
    -- single-CPU boot (no shootdown); the caller frees the frame afterwards.
    ---------------------------------------------------------------------------
    procedure retireKernelImagePage (physAddr : in Virtmem.PhysAddress);

    ---------------------------------------------------------------------------
    -- detachLinearRegion / attachLinearRegion (KERN-014 slice 4): the 2 MiB
    -- region at Base (2 MiB aligned RAM) leaves the kernel's linear map, or
    -- returns to it. Detach clears the region's one P2 entry (a 2 MiB page or
    -- the P1 table under it) and hands it back as Saved; the CALLER runs the
    -- TLB round before any frame of the region is used. Attach writes Saved
    -- back: a not-present entry is never cached, so it needs no round.
    ---------------------------------------------------------------------------
    procedure detachLinearRegion
      (Base : Virtmem.PhysAddress; Saved : out Virtmem.PageTableEntry);
    procedure attachLinearRegion
      (Base : Virtmem.PhysAddress; Saved : Virtmem.PageTableEntry);

    ---------------------------------------------------------------------------
    -- installKernelTable - a new kernel-half area (KERN-014: the per-CPU
    -- window): point the empty kernel P4 entry Index at Table, a zeroed
    -- kernel-owned P3. Boot only, before any process copies the kernel half
    -- (mapKernelMemIntoProcess), so no address space lacks it.
    ---------------------------------------------------------------------------
    procedure installKernelTable
      (Index : Virtmem.PageTableIndex; Table : Virtmem.PhysAddress)
      with Pre => Virtmem."<=" (256, Index);

private
    ---------------------------------------------------------------------------
    -- determineFlagsAndMapFrame
    -- Given a physical address, see if it falls into one of our kernel ranges
    -- (.text, .bss, etc.) and map appropriately.
    ---------------------------------------------------------------------------
    generic
        with procedure allocate (newFrame : out Virtmem.PhysAddress);
    procedure determineFlagsAndMapFrame (frame : in Virtmem.PFN);
    
    ---------------------------------------------------------------------------
    -- mapBigFrame
    ---------------------------------------------------------------------------
    generic
        with procedure allocate (newFrame : out Virtmem.PhysAddress);
    procedure mapBigFrame (bigFrame : in Virtmem.BigPFN;
                           flags    : in Unsigned_64 := Virtmem.PG_KERNELDATA);

    ---------------------------------------------------------------------------
    -- mapBigFrameAsSmallFrames
    ---------------------------------------------------------------------------
    generic
        with procedure allocate (newFrame : out Virtmem.PhysAddress);
    procedure mapBigFrameAsSmallFrames (bigFrame : in Virtmem.BigPFN);

    ---------------------------------------------------------------------------
    -- mapArea
    ---------------------------------------------------------------------------
    generic
        with procedure allocate (newFrame : out Virtmem.PhysAddress);
    procedure mapArea (area : in MemoryAreas.MemoryArea);
end Mem_mgr;
