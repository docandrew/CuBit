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
-- Not thread-safe, but should only be used from the boot thread once we
-- get the buddy allocator complete.
--
-- TODO: Give this allocator it's own limit, smaller than the max memory CuBit
--  can support. Remove PhysPFN type.
-------------------------------------------------------------------------------

with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

with Config;
-- with Spinlocks;
with MemoryAreas;
with Virtmem; use Virtmem;
with x86;

-- Pragma Elaborate_All (Spinlocks);

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

    -- For static analysis, ensure this allocator is initialized before use.
    initialized : Boolean := False with Ghost;

    -- Keep track of highest PFN we allocated.
    highestPFNAllocated : Virtmem.PFN;
    
    OutOfMemoryException : exception;
    OutOfBoundsException : exception;

    ---------------------------------------------------------------------------
    -- setup
    -- Given an array of memory areas from the boot loader, initialize this
    -- bootstrap memory allocator. This creates the bitmap describing all
    -- memory available in the system.
    -- @param areas - array of memory areas
    ---------------------------------------------------------------------------
    procedure setup (areas : in MemoryAreas.MemoryAreaArray) with
        Global  => (Output => ( BitmapState, 
                                BootAllocator.initialized,
                                Virtmem.MAX_PHYS_ADDRESSABLE,
                                Virtmem.MAX_PHYS_USABLE)),
        Post    => BootAllocator.initialized;

    ---------------------------------------------------------------------------
    -- isFree
    -- @return True if this frame is free, False otherwise.
    ---------------------------------------------------------------------------
    function isFree (frame : in Virtmem.PFN) return Boolean with
        Global  => (Input       => BitmapState, 
                    Proof_In    => BootAllocator.initialized),
        Pre     => (BootAllocator.initialized and then frame <= MAX_BOOT_PFN);

    ---------------------------------------------------------------------------
    -- allocFrame - finds a free frame and marks it as in use
    --
    -- @param out addr : base address of the allocated frame, will be 0 if no
    --  frame is available.
    ---------------------------------------------------------------------------
    procedure allocFrame (addr : out Virtmem.PhysAddress) with
        Global  => (
            In_Out      => (BitmapState, x86.interruptsEnabled),
            Proof_In    => (BootAllocator.initialized)),
        
        Pre     => BootAllocator.initialized,
        Post    => addr <= (MAX_BOOT_PFN * Virtmem.FRAME_SIZE);

    ---------------------------------------------------------------------------
    -- allocFrames - find a contiguous number of frames in physical memory,
    --  marks them in use. Uses a linear first-fit algorithm.
    --
    -- @param num - number of frames to allocate
    -- @param addr - base address of the allocated memory
    ---------------------------------------------------------------------------
    procedure allocFrames (num : in AllocSize; addr : out Virtmem.PhysAddress) with
        Global  => (
            In_Out      => (BitmapState, x86.interruptsEnabled),
            Proof_In    => (BootAllocator.initialized)),

        Pre     => num <= MAX_BOOT_PFN and 
                   BootAllocator.initialized,
        Post    => addr <= (MAX_BOOT_PFN * Virtmem.FRAME_SIZE);

    ---------------------------------------------------------------------------
    -- free
    -- Free a physical frame allocation at a certain address
    ---------------------------------------------------------------------------
    procedure free (addr : in Virtmem.PhysAddress) with
        Global  => (
            In_Out      => BitmapState, 
            Proof_In    => BootAllocator.initialized),

        Pre     => (BootAllocator.initialized and then 
                    addr <= (MAX_BOOT_PFN * Virtmem.FRAME_SIZE));

    ---------------------------------------------------------------------------
    -- numFreeFrames
    -- @return number of free physical frames available in the boot allocator
    ---------------------------------------------------------------------------
    function numFreeFrames return Unsigned_64 with
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
    -- @return PFN of the free frame
    ---------------------------------------------------------------------------
    function findFreeFrame return Virtmem.PFN with
        Global  => (Input       => BitmapState, 
                    Proof_In    => BootAllocator.initialized),
        Pre     => BootAllocator.initialized,
        Post    => findFreeFrame'Result < MAX_BOOT_PFN;

    ---------------------------------------------------------------------------
    -- findFreeFrames - finds group of contigous pages of free physical memory.
    --
    -- @return PFN of the first free frame in the block
    ---------------------------------------------------------------------------
    function findFreeFrames(num : in AllocSize) return Virtmem.PFN with
        Global  => (Input       => BitmapState, 
                    Proof_In    => BootAllocator.initialized),
        Pre     => BootAllocator.initialized and then
                   num <= MAX_BOOT_PFN,
        Post    => findFreeFrames'Result <= Virtmem.PFN(MAX_BOOT_PFN - num);

    ---------------------------------------------------------------------------
    -- markUsed
    -- set a page's bit to 1
    ---------------------------------------------------------------------------
    procedure markUsed(frame : in Virtmem.PFN) with
        Global  => (In_Out      => BitmapState, 
                    Proof_In    => BootAllocator.initialized),
        Pre     => (BootAllocator.initialized and then frame <= MAX_BOOT_PFN);

    ---------------------------------------------------------------------------
    -- markFree
    -- set a page's bit to 0
    ---------------------------------------------------------------------------
    procedure markFree(frame : in Virtmem.PFN) with
        Global  => (In_Out => BitmapState, 
                    Proof_In => BootAllocator.initialized),
        Pre     => (BootAllocator.initialized and then frame <= MAX_BOOT_PFN);

    ---------------------------------------------------------------------------
    -- getBlock
    -- @return the index into bitmap array in which the frame resides.
    ---------------------------------------------------------------------------
    function getBlock(frame : in Virtmem.PFN) return Natural with
        Pre     => frame <= MAX_BOOT_PFN,
        Post    => getBlock'Result < MAX_BITMAP_BLOCKS;

    ---------------------------------------------------------------------------
    -- getOffset
    -- @return the bit within a Unsigned_64 block that 
    --  represents this single frame.
    ---------------------------------------------------------------------------
    function getOffset(frame : in Virtmem.PFN) return Natural with
        Pre     => frame <= MAX_BOOT_PFN,
        Post    => getOffset'Result < 64;

end BootAllocator;
