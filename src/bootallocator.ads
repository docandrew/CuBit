-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- Early Boot Physical Memory Allocator
--
-- Note that MAX_PHYS_MEMORY and PhysPFN type defined here refer to the max
-- physical memory supported by CuBit - NOT the max addressable physical
-- memory, as used in the virtmem package. The values here are limited by the
-- amount of physical memory we can track with this boot physical allocator.
--
-- Not fully thread-safe, but should only be used from the boot thread once we
-- get the buddy allocator complete.
--
-- TODO: Give this allocator it's own limit, smaller than the max memory CuBit
--  can support. Remove PhysPFN type.
-------------------------------------------------------------------------------

with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

with Config;
with Spinlock;
with MemoryAreas;
with Virtmem; use Virtmem;
with x86;

Pragma Elaborate_All (Spinlock);

package BootAllocator with
    Abstract_State  => BitmapState,
    Initializes     => (BitmapState, initialized),
    SPARK_Mode      => On
is
    -- 2**18 = 2**12 bytes/page * 2**6 pages/u64;
    MAX_BITMAP_BLOCKS : constant := config.MAX_BOOT_ALLOC / 2**18;
    
    type FrameBitmapType is array (0 .. MAX_BITMAP_BLOCKS-1) of Unsigned_64 
        with Default_Component_Value => 16#FFFF_FFFF_FFFF_FFFF#,
             Component_Size  => 64;

    -- Maximum PFN we can allocate with the BootAllocator. Each page
    --  is represented by a single bitmap bit, and there's 64 bits per
    --  bitmap block.
    MAX_BOOT_PFN : constant := MAX_BITMAP_BLOCKS * 64;

    subtype AllocSize is Positive range 1 .. MAX_BOOT_PFN;

    -- Page Frame Number (addr / 4096) for physical memory.
    -- Works like a regular PFN but restricted to the physical memory limit
    --  we define above.
    --subtype PhysPFN is Virtmem.PFN range 0 .. MAX_PHYS_PFN;

    -- For static analysis, ensure this allocator is initialized before use.
    initialized : Boolean := False with Ghost;

    -- Keep track of highest PFN we allocated.
    highestPFNAllocated : Virtmem.PFN;
    
    mutex : aliased Spinlock.Spinlock;
   
    OutOfMemoryException : exception;
    OutOfBoundsException : exception;

    ---------------------------------------------------------------------------
    -- setup
    -- Given an array of memory areas from the boot loader, initialize this
    -- bootstrap memory allocator. This creates the bitmap describing all
    -- memory available in the system.
    -- @param areas - array of memory areas
    -- @param freeBytes - output, amount of free memory in the system.
    ---------------------------------------------------------------------------
    procedure setup(areas : in MemoryAreas.MemoryAreaArray;
                    freeBytes : out Unsigned_64) with
        Global  => (Output => ( BitmapState, 
                                BootAllocator.initialized,
                                Virtmem.MAX_PHYS_ADDRESSABLE,
                                Virtmem.MAX_PHYS_USABLE)),
        Post    => BootAllocator.initialized;

    ---------------------------------------------------------------------------
    -- isFree - return True if this frame is free (0), False otherwise.
    --
    -- TODO: always return True if the frame is outside of the boot allocator's
    -- memory limit.
    ---------------------------------------------------------------------------
    function isFree(frame : in Virtmem.PFN) return Boolean with
        Global  => (Input       => BitmapState, 
                    Proof_In    => BootAllocator.initialized),
        Pre     => (BootAllocator.initialized and then frame <= MAX_BOOT_PFN);

    ---------------------------------------------------------------------------
    -- allocFrame - finds a free frame and marks it as in use
    --
    -- @param: out addr : base address of the allocated frame.
    ---------------------------------------------------------------------------
    procedure allocFrame(addr : out Virtmem.PhysAddress) with
        Global  => (
            In_Out      => (BitmapState,
                            mutex,
                            x86.interruptsEnabled),
            Proof_In    => (BootAllocator.initialized)),
        
        Pre     => BootAllocator.initialized and not Spinlock.isLocked(mutex),
        Post    => addr <= (MAX_BOOT_PFN * Virtmem.FRAME_SIZE) and
                    not Spinlock.isLocked(mutex);

    ---------------------------------------------------------------------------
    -- allocFrames - find a contiguous number of frames in physical memory,
    --  marks them in use. Uses a linear first-fit algorithm.
    --
    -- @param num - number of frames to allocate
    -- @param addr - base address of the allocated memory
    ---------------------------------------------------------------------------
    procedure allocFrames(num : in AllocSize; addr : out Virtmem.PhysAddress) with
        Global  => (
            In_Out      => (BitmapState,
                            mutex,
                            x86.interruptsEnabled),
            Proof_In    => (BootAllocator.initialized)),

        Pre     => num <= MAX_BOOT_PFN and 
                   BootAllocator.initialized and 
                   not Spinlock.isLocked(mutex),
        Post    => addr <= (MAX_BOOT_PFN * Virtmem.FRAME_SIZE) and
                   not Spinlock.isLocked(mutex);

    ---------------------------------------------------------------------------
    -- allocSmall - perform a persistent (non-freeable), small memory
    --  allocation. Use for boot-time allocation of small data structures.
    --
    -- @exception - throws out of memory exception if allocation fails.
    ---------------------------------------------------------------------------
    --procedure allocSmall(addr : out Virtmem.PhysAddress)

    ---------------------------------------------------------------------------
    -- Free a physical frame allocation at a certain address
    ---------------------------------------------------------------------------
    procedure free(addr : in Virtmem.PhysAddress) with
        Global  => (
            In_Out      => BitmapState, 
            Proof_In    => BootAllocator.initialized),

        Pre     => (BootAllocator.initialized and then 
                    addr <= (MAX_BOOT_PFN * Virtmem.FRAME_SIZE));

    ---------------------------------------------------------------------------
    -- getFreeFrameCount - return number of free physical frames in the system.
    ---------------------------------------------------------------------------
    function getFreeFrameCount return Unsigned_64 with
        Global  => (Input => BitmapState);

private
    -- mark all as used initially, since there probably be less free pages
    -- than used ones.
    bitmap : FrameBitmapType := 
        (others => 16#FFFF_FFFF_FFFF_FFFF#) with Part_Of => BitmapState;

    -- Amount of free memory in the system
    freePhysicalFrames  : Unsigned_64 with Part_Of => BitmapState;

    ---------------------------------------------------------------------------
    -- findFreeFrame - finds a single page of free physical memory.
    --
    -- Return: PFN of the free frame
    ---------------------------------------------------------------------------
    function findFreeFrame return Virtmem.PFN with
        Global  => (Input       => BitmapState, 
                    Proof_In    => BootAllocator.initialized),
        Pre     => BootAllocator.initialized,
        Post    => findFreeFrame'Result < MAX_BOOT_PFN;

    ---------------------------------------------------------------------------
    -- findFreeFrames - finds group of contigous pages of free physical memory.
    --
    -- Return: PFN of the first free frame in the block
    ---------------------------------------------------------------------------
    function findFreeFrames(num : in AllocSize) return Virtmem.PFN with
        Global  => (Input       => BitmapState, 
                    Proof_In    => BootAllocator.initialized),
        Pre     => BootAllocator.initialized and then
                   num <= MAX_BOOT_PFN,
        Post    => findFreeFrames'Result <= Virtmem.PFN(MAX_BOOT_PFN - num);

    ---------------------------------------------------------------------------
    -- markUsed - set a page's bit to 1
    ---------------------------------------------------------------------------
    procedure markUsed(frame : in Virtmem.PFN) with
        Global  => (In_Out      => BitmapState, 
                    Proof_In    => BootAllocator.initialized),
        Pre     => (BootAllocator.initialized and then frame <= MAX_BOOT_PFN);

    ---------------------------------------------------------------------------
    -- markFree - set a page's bit to 0
    ---------------------------------------------------------------------------
    procedure markFree(frame : in Virtmem.PFN) with
        Global  => (In_Out => BitmapState, 
                    Proof_In => BootAllocator.initialized),
        Pre     => (BootAllocator.initialized and then frame <= MAX_BOOT_PFN);

    ---------------------------------------------------------------------------
    -- getBlock - return the index into bitmap array in which the frame
    --  resides.
    ---------------------------------------------------------------------------
    function getBlock(frame : in Virtmem.PFN) return Natural with
        Pre     => frame <= MAX_BOOT_PFN,
        Post    => getBlock'Result < MAX_BITMAP_BLOCKS;

    ---------------------------------------------------------------------------
    -- getOffset - return the bit within a Unsigned_64 block that 
    --  represents this single frame.
    ---------------------------------------------------------------------------
    function getOffset(frame : in Virtmem.PFN) return Natural with
        Pre     => frame <= MAX_BOOT_PFN,
        Post    => getOffset'Result < 64;


    ---------------------------------------------------------------------------
    -- For allocations smaller than a page, the boot allocator sees if there is
    -- enough space left in the current page, and if not, it will ask for a new
    -- one. Note that the boot allocator can only free entire pages, so we
    -- maintain a list of the used pages to be freed later, when the storage
    -- pool goes out of scope.
    ---------------------------------------------------------------------------
    --subtype BootAllocSize is Natural range 1..Virtmem.FRAME_SIZE;

    --procedure alloc(size : in BootAllocSize; obj : out System.Address);

    ---------------------------------------------------------------------------
    -- Boot allocator storage pool. Does not allow deallocation of individual
    -- objects.
    ---------------------------------------------------------------------------
    -- package StoragePool is
    --     type BootAllocPool is new Root_Storage_Pool with private;
        
    --     AllocationSizeExceeded : exception;

    -- private
    --     type BootAllocPool is new Root_Storage_Pool with null record;

    --     overriding procedure Allocate
    --        (Self        : in out BootAllocPool;
    --         Addr        : out System.Address;
    --         Size        : in Storage_Count;
    --         Alignment   : in Storage_Count);
        
    --     overriding procedure Deallocate
    --        (Self        : in out BootAllocPool;
    --         Addr        : in System.Address;
    --         Size        : in Storage_Count;
    --         Alignment   : in Storage_Count);

    --     overriding function Storage_Size(Self : in BootAllocPool)
    --         return Storage_Count is (Storage_Count'Last)
    --             with Inline => True;
    -- end StoragePool;


    -- package body StoragePool
    -- is
    --     type StorageElementAccess is access Storage_Element;

    --     -- The space we've used up in the current allocated page. If the
    --     -- next allocation will exceed the max level, go get a new page
    --     -- and put the allocation there.
    --     maxLevel : Virtmem.PAGE_SIZE;
    --     curLevel : Natural range 0..(Virtmem.PAGE_SIZE - 1);

    --     -- Base address of the current page we're storing in.
    --     curPage  : System.Address;

    --     function Convert is new
    --         Ada.Unchecked_Conversion(System.Address, StorageElementAccess);

    --     -----------------------------------------------------------------------
    --     -- Allocate
    --     -----------------------------------------------------------------------

        
    --     -----------------------------------------------------------------------
    --     -- Deallocate - no-op.
    --     -----------------------------------------------------------------------
    --     overriding procedure Deallocate
    --        (Self        : in out BootAllocPool;
    --         Addr        : in System.Address;
    --         Size        : in Storage_Count;
    --         Alignment   : in Storage_Count)
    --     is
    --         pragma Unreferenced (Self);
    --         pragma Unreferenced (Addr);
    --         pragma Unreferenced (Size);
    --         pragma Unreferenced (Alignment);
    --     begin
    --         null;
    --     end Deallocate;    
    -- end StoragePool;

end BootAllocator;
