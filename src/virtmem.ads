-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2019 Jon Andrew
--
-- Virtual Memory / Paging Routines
--
-- We call pages that are mapped with all 4 levels of page tables (4k on 
-- x86-64) "pages". Mappings requiring only 3 levels of page tables 
-- (2MiB on x86-64) "big pages". Mappings using just 2 levels of page tables
-- (1GiB on x86-64) are called "huge pages".
-------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;

with Config;
with Util; use Util;

package Virtmem 
    with SPARK_Mode => On
is
    -- 4K page translations
    FRAME_SHIFT         : constant := 12;
    PAGE_SHIFT          : constant := FRAME_SHIFT;
    FRAME_SIZE          : constant := 2**FRAME_SHIFT;
    PAGE_SIZE           : constant := FRAME_SIZE;

    -- 2-MByte page translations
    BIG_FRAME_SHIFT     : constant := 21;
    BIG_FRAME_SIZE      : constant := 2**BIG_FRAME_SHIFT;
    BIG_PAGE_SIZE       : constant := BIG_FRAME_SIZE;

    -- 1-GByte page translations (not used currently)
    HUGE_FRAME_SHIFT    : constant := 30;
    HUGE_FRAME_SIZE     : constant := 2**HUGE_FRAME_SHIFT;
    HUGE_PAGE_SIZE      : constant := HUGE_FRAME_SIZE;

    -- Number of entries in a page table. 512 * 64bits/entry = 4096
    NUM_PAGE_TABLE_ENTRIES : constant := 512;

    -- Index into a page table
    type PageTableIndex is new Integer range 0 .. NUM_PAGE_TABLE_ENTRIES - 1;

    -- Offset into a page
    type PageOffset     is new Integer range 0 .. PAGE_SIZE - 1;
    type BigPageOffset  is new Integer range 0 .. BIG_PAGE_SIZE - 1;
    type HugePageOffset is new Integer range 0 .. HUGE_PAGE_SIZE - 1;

    -- Flags for page table entries
    PG_EMPTY            : constant := 0;
    PG_UNUSED           : constant := 0;
    PG_PRESENT          : constant := 2**0;
    PG_WRITABLE         : constant := 2**1;
    PG_USER             : constant := 2**2;
    PG_WRITETHROUGH     : constant := 2**3;
    PG_CACHEDISABLED    : constant := 2**4;
    PG_ACCESSED         : constant := 2**5;
    PG_DIRTY            : constant := 2**6;
    PG_SIZE             : constant := 2**7;
    PG_GLOBAL           : constant := 2**8;
    PG_NXE              : constant := 2**63;

    -- Predefined flags
    PG_KERNELCODE       : constant := PG_PRESENT;
    PG_KERNELDATA       : constant := PG_PRESENT + PG_WRITABLE + PG_NXE;
    PG_KERNELDATARO     : constant := PG_PRESENT + PG_NXE;
    PG_USERCODE         : constant := PG_PRESENT + PG_USER;
    PG_USERDATA         : constant := PG_PRESENT + PG_WRITABLE + PG_USER + PG_NXE;
    PG_USERDATARO       : constant := PG_PRESENT + PG_USER + PG_NXE;
    PG_IO               : constant := PG_PRESENT + PG_WRITABLE + PG_NXE + 
                                        PG_WRITETHROUGH + PG_CACHEDISABLED;

    -- The upper 12-bits are sign-extended, so the max physical
    -- address we can have is 52-bits. Our limit is lower though, since
    -- we offset-map all physical memory into the higher-half.
    --
    -- This is the theoretical limit to how much physical memory our OS
    -- can support, at least with 48-bit canonical virtual addresses. (128TiB)
    MAX_PHYS_ADDRESS : constant := 2**48 - 1;
    --MAX_PHYS_ADDRESS : constant := 16#0000_FFFF_FFFF_FFFF#;

    subtype PhysAddress is Integer_Address; -- range 0 .. MAX_PHYS_ADDRESS;

    -- Max physical address that we have on _our_ system. May not be
    -- all usable RAM, but ACPI tables or something else.
    -- initialized in BootAllocator
    MAX_PHYS_ADDRESSABLE    : PhysAddress := 0;

    -- Max physical RAM address on our system.
    -- initialized in BootAllocator.
    MAX_PHYS_USABLE         : PhysAddress := 0;

    subtype VirtAddress is Integer_Address;

    -- Page Frame Number is a sign-extended 52-bit address / PAGE_SIZE.
    -- Note: if Intel brings out a 56-bit phys addressable CPU,
    -- we'll need to make this selectable.
    type PFN        is new Unsigned_64 
        range 0 .. Unsigned_64(PhysAddress'Last) / PAGE_SIZE; --16#FF_FFFF_FFFF#;

    type BigPFN  is new PFN
        range 0 .. PFN(PhysAddress'Last / BIG_PAGE_SIZE) with Size => 64;

    type HugePFN is new PFN
        range 0 .. PFN(PhysAddress'Last / HUGE_PAGE_SIZE) with Size => 64;

    -- Types for the undefined spaces in a PageTableEntry
    type UndefinedAType is new Integer range 0..7;
    type UndefinedBType is new Integer range 0..16#7FF#;

    -- Structure of an x86-64 Page Table Entry
    -- @field size - Really means, "this is the last level of page table"
    type PageTableEntry is
    record
        present         : Boolean           := False;
        writable        : Boolean           := False;
        user            : Boolean           := False;
        writeThrough    : Boolean           := False;
        cacheDisabled   : Boolean           := False;
        accessed        : Boolean           := False;
        dirty           : Boolean           := False;
        size            : Boolean           := False;
        global          : Boolean           := False;
        undefinedA      : UndefinedAType    := 0;
        pgNum           : PFN               := 0;
        undefinedB      : UndefinedBType    := 0;
        NX              : Boolean           := False;
    end record with Size => 64, Convention => C;
    
    for PageTableEntry use
    record
        present         at 0 range 0  .. 0;
        writable        at 0 range 1  .. 1;
        user            at 0 range 2  .. 2;
        writeThrough    at 0 range 3  .. 3;
        cacheDisabled   at 0 range 4  .. 4;
        accessed        at 0 range 5  .. 5;
        dirty           at 0 range 6  .. 6;
        size            at 0 range 7  .. 7;
        global          at 0 range 8  .. 8;
        undefinedA      at 0 range 9  .. 11;
        pgNum           at 0 range 12 .. 51;
        undefinedB      at 0 range 52 .. 62;
        NX              at 0 range 63 .. 63;
    end record;

    function u64ToPTE is new Ada.Unchecked_Conversion(Unsigned_64, PageTableEntry);

    -- Types for the page tables
    type P4 is array (PageTableIndex) of PageTableEntry
        with Alignment => PAGE_SIZE, Size => 512*64, Convention => C;
   
    type P3 is array (PageTableIndex) of PageTableEntry
        with Alignment => PAGE_SIZE, Size => 512*64, Convention => C;
   
    type P2 is array (PageTableIndex) of PageTableEntry
        with Alignment => PAGE_SIZE, Size => 512*64, Convention => C;
   
    type P1 is array (PageTableIndex) of PageTableEntry
        with Alignment => PAGE_SIZE, Size => 512*64, Convention => C;

    -- If attempting to get or create a next-level table where it's not
    -- valid, i.e. get a P1 table when the P2 table is for big pages, this will
    -- be raised.
    PageTableLevelException : exception;

    ---------------------------------------------------------------------------
    -- getP4Index()
    -- getP3Index()
    -- getP2Index()
    -- getP1Index()
    --
    -- Returns the index into the N-level page table specified by this
    --  virtual address. Used for walking page tables.
    ---------------------------------------------------------------------------
    function getP4Index(virtAddr : in VirtAddress) return PageTableIndex;
    function getP3Index(virtAddr : in VirtAddress) return PageTableIndex;
    function getP2Index(virtAddr : in VirtAddress) return PageTableIndex;
    function getP1Index(virtAddr : in VirtAddress) return PageTableIndex;

    ---------------------------------------------------------------------------
    -- getOffset - Get the index into the page specified by this address
    ---------------------------------------------------------------------------
    function getOffset(virtAddr : in VirtAddress) return PageOffset;

    ---------------------------------------------------------------------------
    -- isCanonical - Determine if a virtual address is canonical or not
    -- based on the sign-extended virtual address bits in x86-64.
    ---------------------------------------------------------------------------
    function isCanonical(virtAddr : in VirtAddress) return Boolean;

    ---------------------------------------------------------------------------
    -- getP4Table - Returns the address of the currently-used P4 table as
    -- stored in CR3.
    ---------------------------------------------------------------------------
    function getActiveP4 return PhysAddress;

    ---------------------------------------------------------------------------
    -- setActiveP4 - load the physical address of a top-level page table into
    -- the CR3 register, causing a new virtual address space to be used.
    -- @param p4addr - physical address of the new top-level page table.
    ---------------------------------------------------------------------------
    procedure setActiveP4(p4addr : in PhysAddress);

    ---------------------------------------------------------------------------
    -- flushTLB - reload CR3 and cause TLB to be flushed.
    ---------------------------------------------------------------------------
    procedure flushTLB;

    ---------------------------------------------------------------------------
    -- PFNToAddr - Return the address of a given Page Frame Number (PFN)
    --
    -- @param page - page frame number
    -- @return the base address (PAGE_SIZE-aligned) of the page.
    ---------------------------------------------------------------------------
    function PFNToAddr(page : in PFN) return PhysAddress;

    ---------------------------------------------------------------------------
    -- bigPFNToAddr - Return the address of a given Page Frame Number (PFN) for
    -- big pages.
    --
    -- @param page - big page frame number
    -- @return the base address (BIG_PAGE_SIZE-aligned) of the page.
    ---------------------------------------------------------------------------
    function BigPFNToAddr(page : in BigPFN) return PhysAddress;

    ---------------------------------------------------------------------------
    -- addrToPFN - Return the Page Frame Number (PFN) for a given address.
    --
    -- @param addr - physical address
    -- @return PFN containing the physical address
    ---------------------------------------------------------------------------
    function addrToPFN(addr : in PhysAddress) return PFN;

    ---------------------------------------------------------------------------
    -- addrToBigPFN - Return the Big Page Frame Number (PFN) for a given
    -- address.
    --
    -- @param addr - physical address, does NOT need to be BIG_FRAME_SIZE-
    --  aligned, but if it isn't, you're probably doing something weird.
    -- @return Big PFN containing the physical address
    ---------------------------------------------------------------------------
    function addrToBigPFN(addr : in PhysAddress) return BigPFN;

    ---------------------------------------------------------------------------
    -- bigPFNToPFN - Convert the given Big PFN to a PFN starting at the
    --  same physical address if big pages were used. i.e. address 0x1000000
    --  would be the start of PFN 0x1000, or Big PFN 0x8.
    ---------------------------------------------------------------------------
    function bigPFNToPFN(bigPage : in BigPFN) return PFN;

    ---------------------------------------------------------------------------
    -- PFNToBigPFN - Convert the given PFN to a big PFN containing the given 
    --  PFN if big pages were used. i.e. address 0x1001000
    --  would be the start of PFN 0x1001, or be inside Big PFN 0x8.
    --
    -- @param page - does NOT need to be aligned with a BigPFN start address.
    -- @return - The result will be BIG_FRAME_SIZE-aligned, so many different
    --  PFNs will map to the same Big PFN returned from this function.
    ---------------------------------------------------------------------------
    function PFNToBigPFN(page : in PFN) return BigPFN;

    ---------------------------------------------------------------------------
    -- zeroize - set all entries of a given page table to all zeroes. Note that
    -- this does NOT free any page tables that those entries may have pointed
    -- to.
    ---------------------------------------------------------------------------
    generic
        type PN is array (PageTableIndex) of PageTableEntry;
    procedure zeroize(table : in out PN);

    ---------------------------------------------------------------------------
    -- getNextTable - Given a table of level N and index, Returns the 
    -- physical address of the next level-M table, or 0 if it does not exist.
    --
    -- @param table - table of level N.
    -- @param index - index to level N-1 table.
    -- @return - physical address of table level N-1.
    ---------------------------------------------------------------------------
    generic
        type PN is array (PageTableIndex) of PageTableEntry;
    function getNextTable(table : in PN; index : PageTableIndex)
        return PhysAddress;

    ---------------------------------------------------------------------------
    -- createNextTable - Given a table of level N and index, returns the 
    -- address of the next level N-1 table. If the table does not already exist,
    -- it will be created using the allocator provided as a generic parameter
    -- during the function instantiation. If unable to create it, this function
    -- will output tableAddr of 0.
    --
    -- If attempting to mix incompatible page table sizes, i.e. a 4k P1 table
    -- beneath a 2MiB P2 table, this procedure will raise an exception.
    --
    -- @param table - table of level N
    -- @param index - page table index to get or create. 
    -- @param tableAddr - output physical address of table level N-1, or 0 if
    --  it does not exist and then this function was unable to create it.
    --
    -- generic discriminants:
    -- @param PN - Page Table, P4-P2.
    -- @param allocate - function to allocate a new physical 4096 byte frame
    --  for the page table if required.
    -- @param flags - flags for the page table entry pointing to the next 
    --  level table.
    ---------------------------------------------------------------------------
    generic
        type PN is array (PageTableIndex) of PageTableEntry;
        with procedure allocate(newframe : out PhysAddress);
        flags : Unsigned_64;
    procedure createNextTable(table     : in out PN; 
                             index      : in PageTableIndex;
                             tableAddr  : out PhysAddress);

    ---------------------------------------------------------------------------
    -- mapPage - map a frame of physical memory into the virtual address space
    --  set by the designated P4 page table.
    -- @param PhysAddress - 4KiB-aligned physical memory address of frame
    -- @param VirtAddress - virtual memory address of page.
    -- @param flags - bitwise-ORed values of the flags for the PTE
    -- @param myP4 - the top-level page table for this mapping.
    -- @param success - True if successful, False otherwise.
    --
    -- TODO: add alignment-checking precondition here.
    ---------------------------------------------------------------------------
    generic
        with procedure allocate(newframe : out PhysAddress);
    procedure mapPage(  phys    : in PhysAddress; 
                        virt    : in VirtAddress;
                        flags   : in Unsigned_64; 
                        myP4    : in out P4;
                        success : out Boolean);

    ---------------------------------------------------------------------------
    -- mapBigPage - map a big frame of physical memory into the virtual address
    --  space set by the designated P4 page table. 
    -- 
    -- @param PhysAddress - 2MiB-aligned physical memory address of frame
    -- @param VirtAddress - virtual memory address of page.
    -- @param flags - bitwise-ORed values of the flags for the PDE. This function
    --  will set the PG_HUGE flag automatically.
    -- @param myP4 - the top-level page table for this mapping.
    -- @param success - True if successful, False otherwise.
    --
    -- TODO: add alignment-checking precondition here.
    ---------------------------------------------------------------------------
    generic
        with procedure allocate(newframe : out PhysAddress);
    procedure mapBigPage(   phys    : in PhysAddress;
                            virt    : in VirtAddress;
                            flags   : in Unsigned_64;
                            myP4    : in out P4;
                            success : out Boolean);

    ---------------------------------------------------------------------------
    -- unmapPage - mark the page containing the virtual address given as not
    -- present.
    -- @param virt - virtual address to unmap
    -- @param myP4 - top level page table 
    -- @param success - True if this address was mapped to physical memory, or
    --  False if it (or any intermediate page tables) was not previously mapped
    --
    -- CAUTION: If the virtual address points to the middle of a big page, the
    -- entire big page will be marked as non-present.
    ---------------------------------------------------------------------------
    procedure unmapPage(virt    : in VirtAddress;
                        myP4    : in P4;
                        success : out Boolean);

    ---------------------------------------------------------------------------
    -- tableWalk
    -- Walk page tables to see what physical address is mapped to this
    -- virtual address. (NOTE: this is for arbitrarily-mapped physical frames.
    -- For linear-mapped physical memory, see V2P(). If the virtual address
    -- does not have a corresponding mapped physical address, then this returns
    -- 0.
    -- @param virt - virtual address
    -- @param myP4 - access to the top-level page table that maps the virtual
    --  address to a physical one.
    -- @return - physical address that the virtual address maps to or 0 if not
    --  mapped.
    ---------------------------------------------------------------------------
    function tableWalk(virt : in VirtAddress; myP4 : in P4)
        return PhysAddress;

    ---------------------------------------------------------------------------
    -- deleteP4
    -- deleteP3
    -- deleteP2
    -- Walk page tables, deleting ALL entries and free-ing the heap-allocated
    -- memory for their sub-tables.
    ---------------------------------------------------------------------------
    generic
        with procedure free(frame : in PhysAddress);
    procedure deleteP4(myP4 : in out P4);

    generic
        with procedure free(frame : in PhysAddress);
    procedure deleteP3(myP3 : in out P3);

    generic
        with procedure free(frame : in PhysAddress);
    procedure deleteP2(myP2 : in out P2);

    ---------------------------------------------------------------------------
    -- mapHugePage - map a 2MB physical area into the virtual address space
    --  set by the designated P4 page table.
    -- Param PhysAddress - physical memory address of frame
    --  (needs to be 2MiB-aligned)
    -- Param VirtAddress - virtual memory address of page.
    -- Param flags - bitwise-ORed values of the flags for the PTE
    ---------------------------------------------------------------------------
    --function mapHuge(phys : in PhysAddress; virt : in VirtAddress;
    --                     flags : in Unsigned_64; myP4 : access P4) return Boolean;


    ---------------------------------------------------------------------------
    -- makePTE - convenience function for setting the PFN and each of the bool 
    --  flags in a PageTableEntry.
    ---------------------------------------------------------------------------
    function makePTE(frame : in PFN;
                     flags : in Unsigned_64) return PageTableEntry;
    
    ---------------------------------------------------------------------------
    -- makeBigPTE - convenience function for setting a Big PFN and each of the
    --  bool flags in a PageTableEntry. This function will automatically set
    --  the "size" flag (for big or huge pages). Should be used to get a PTE
    --  for insertion into a P2 table.
    ---------------------------------------------------------------------------
    function makeBigPTE(bigFrame : in BigPFN;
                     flags : in Unsigned_64) return PageTableEntry;
    

    ---------------------------------------------------------------------------
    -- V2P - Virtual to Physical conversion for physical mem linearly mapped to
    -- higher-half. Note this is NOT the same as walking page tables for
    -- arbitrarily-mapped memory for user space (see tableWalk()).
    --
    -- WARNING: Do NOT use this function for getting the physical address of
    -- static kernel structures or stack variables. They aren't in the linear
    -- region! Use K2P.
    --
    -- @param virtAddr : A virtual address in the linear-mapped region.
    ---------------------------------------------------------------------------
    function V2P(virtAddr : in VirtAddress) return PhysAddress with
        Inline,
        Pre => virtAddr >= LINEAR_BASE;

    ---------------------------------------------------------------------------
    -- P2V - Physical to Virtual address conversion for physical mem linearly
    -- mapped to higher-half.
    --
    -- @param physAddr is any physical address on the system.
    -- @return the virtual address in the linear-mapped region 
    --  0xFFFF_8000_0000_0000 to 0xFFFF_????_????_???? (TBD)
    ---------------------------------------------------------------------------
    function P2V(physAddr : in PhysAddress) return VirtAddress with
        Inline,
        Post => P2V'Result >= LINEAR_BASE;

    ---------------------------------------------------------------------------
    -- K2P - Kernel to Virtual address conversion for physical mem mapped into
    -- the kernel's text/data/bss area.
    --
    -- @param kernAddr : A virtual address in the kernel-mapped region.
    ---------------------------------------------------------------------------
    function K2P(kernAddr : in VirtAddress) return PhysAddress with
        Inline,
        Pre => kernAddr >= KERNEL_BASE;

    ---------------------------------------------------------------------------
    -- Virtual mem constants and addresses
    ---------------------------------------------------------------------------

    -- Kernel stacks, make sure this matches boot.asm. 
    -- @NOTE: since RSP is decremented first during PUSHes, the actual top
    --  address used is < STACK_TOP, so [STACK_TOP] itself can be used as the
    --  base for other storage.
    STACK_TOP           : constant Integer_Address := 16#FFFF_8000_0100_0000#;
    STACK_BOTTOM        : constant Integer_Address := 16#FFFF_8000_0080_0000#;

    -- Higher-half linear-mapped base address
    LINEAR_BASE         : constant Integer_Address := 16#FFFF_8000_0000_0000#;

    -- Kernel text area
    KERNEL_BASE         : constant Integer_Address := 16#FFFF_FFFF_8000_0000#;

    -- Memory set aside for DMA (will increase as we add drivers)
    DMA_REGION_START    : constant Integer_Address := 16#FFFF_8000_0100_0000#;
    DMA_REGION_END      : constant Integer_Address := 16#FFFF_8000_0400_0000#;

    -- In boot.asm, we only map the first 1GB of memory
    --BOOTSTRAP_LIMIT_VIRT : constant := 16#FFFF_FFFF_4000_0000#;
    --BOOTSTRAP_LIMIT_PHYS : constant := 16#0000_0000_4000_0000#;

    -- The linear region has all physical memory in the system linearly-mapped
    -- to the higher-half. We will use this during page faults to determine whether
    -- to demand-load a page into memory.
    LINEAR_REGION_PHYS : Integer_Address range 0 .. 16#0FFF_FFFF_FFFF#;
    LINEAR_REGION_VIRT : Integer_Address range LINEAR_BASE .. (LINEAR_BASE + 16#0FFF_FFFF_FFFF#);

    --DEVICE_REGION_VIRT : Unsigned_64 range ?? .. ??

    -- Virtual kernel addresses, set in linker.ld
    -- smboot : Symbol
    --     with Import => True, Convention => C, External_Name => "smboot";

    -- emboot : Symbol
    --     with Import => True, Convention => C, External_Name => "emboot";

    ---------------------------------------------------------------------------
    -- start/end of our AP boot code
    ---------------------------------------------------------------------------
    stextAP : Symbol
        with Import => True, Convention => C, External_Name => "stext_ap";

    etextAP : Symbol
        with Import => True, Convention => C, External_Name => "etext_ap";

    stext : Symbol
        with Import => True, Convention => C, External_Name => "stext";

    etext : Symbol
        with Import => True, Convention => C, External_Name => "etext";
    
    srodata : Symbol
        with Import => True, Convention => C, External_Name => "srodata";

    erodata : Symbol
        with Import => True, Convention => C, External_Name => "erodata";

    -- srodata32 : Symbol
    --     with Import => True, Convention => C, External_Name => "srodata32";

    -- erodata32 : Symbol
    --     with Import => True, Convention => C, External_Name => "erodata32";

    sdata : Symbol
        with Import => True, Convention => C, External_Name => "sdata";

    edata : Symbol
        with Import => True, Convention => C, External_Name => "edata";

    sbss : Symbol
        with Import => True, Convention => C, External_Name => "sbss";

    ebss : Symbol
        with Import => True, Convention => C, External_Name => "ebss";

    -- sbss32 : Symbol
    --     with Import => True, Convention => C, External_Name => "sbss32";

    -- ebss32 : Symbol
    --     with Import => True, Convention => C, External_Name => "ebss32";

    KERNEL_START_VIRT : Symbol
        with Import => True, Convention => C, External_Name => "KERNEL_START_VIRT";
    
    KERNEL_END_VIRT : Symbol
        with Import => True, Convention => C, External_Name => "KERNEL_END_VIRT";

end Virtmem;