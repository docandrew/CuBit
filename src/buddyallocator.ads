-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary Physical Memory Manager
--
-- @description Implementation of a Buddy Allocator using free lists organized
-- by block order, where block size = FRAME_SIZE * (2 ^ Order).
-- 
-- The CuBit physical memory manager works on blocks strictly
-- aligned by their size. This has some tradeoffs. We lose some memory to
-- external fragmentation if a usable area does not start at a multiple of our
-- largest block size, i.e. if we're using a MAX_BUDDY_ORDER of 10, our largest
-- block is 4MiB. If a memory area starts at 1MiB, we'd lose 3MiB prior to the
-- 4MiB boundary before we can allocate memory starting at the 4MiB mark. The
-- worst-case scenario would be a memory area starting at <4MiB and ending at
-- <8MiB. This allocator will not use this memory.
--
-- The advantages to this scheme are two-fold. One, we can use power-of-2
-- addresses with only a single shift and xor to do buddy/parent calculations,
-- which is faster than doing arithmetic on page frame numbers. Two, all
-- allocations yield pages which are strictly aligned by their block size, with
-- no additional accounting required when aligned blocks are needed.
--
-- If on a memory-constrained system, it benefits the user to use a smaller
-- value for Config.MAX_BUDDY_ORDER to minimize memory losses outside of these
-- aligned boundaries.
--
-- NOTE: Explicitly storing addresses here is probably not the most
-- Ada-standard way of doing things, but it makes the arithmetic easier without
-- a bunch of Address_to_Access conversions, etc. Using explicit placement
-- with Import is much more straightforward.
--
-- CAUTION: Theoretically, a malicious user program could put the address
--  of our buddy in the buddy offset of a page that's mapped, and when the
--  buddy is freed, it would coalesce with a block that's owned by the
--  user. It's unlikely that a user would be able to guess the physical
--  page he's on, nor what size block was allocated, but it is still a
--  risk. This is mitigated in other systems by maintaining a separate
--  bitmap for the frame usage, but the bitmaps grow quite large.
--
--  One technique to mitigate the risk is to always give user processes
--  two smaller buddies, rather than a single larger block. 
--
--  CuBit wouldn't try and free either buddy until they are
--  both no longer mapped to the process. The only time it would use
--  extra memory is for MAX_BUDDY_ORDER or order 0 allocations.
--
--  Obviously there comes a point where the excess memory given to
--  processes may outweigh the memory used by the bitmaps. Likely, it's
--  probably irrelevant, as the greedy process just ends up having to ask
--  us for memory less frequently.
-- 
--  Another option is to check whether the buddy being freed is mapped to a
--  process, but this could be expensive. Or, we just add bitmaps.
--
-- NOTE: This package makes no use of dynamic memory allocation at this time,
--  and could theoretically be used as the boot allocator, however that may
--  change later if we add bitmaps or other lists representing the state of
--  physical frames.
--
-- CAUTION: This allocator builds its free lists in the virtual, linear-mapped
--  memory range. All the internal addresses it uses are going to be
--  higher-half. The addresses it returns are using a conversion to
--  a physical address.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

with BootAllocator;
with Config;
with MemoryAreas;
with Virtmem;

package BuddyAllocator with
    SPARK_Mode => On
is

    NO_BLOCK_AVAILABLE  : constant Virtmem.PhysAddress := Integer_Address'Last;
    
    -- Track whether setup has been called on this package
    initialized         : Boolean := False with Ghost;

    ---------------------------------------------------------------------------
    -- Order is the number of frames per buddy block
    -- i.e. block size = FRAME_SIZE * (2^ORDER)
    ---------------------------------------------------------------------------
    type Order is new Natural range 0..Config.MAX_BUDDY_ORDER;

    ---------------------------------------------------------------------------
    -- FreeBlock - this record is created _inside_ the free memory block,
    --  except for the list heads in freeLists, which are kept in the
    --  freeLists array.
    --
    -- @param prevNode - the previous node in the free list, NOT necessarily
    --  this node's buddy.
    -- @param nextNode - the next node in the free list, again, NOT necessarily
    --  this node's buddy.
    -- @param buddy - contains the address of our buddy when we're on the free
    --  list, used by that buddy when he is freed to see if he can coalesce
    --  with us.
    -- @param numFreeBlocks - number of free blocks in this list (only 
    --  applies to head of list)
    ---------------------------------------------------------------------------
    type FreeBlock is
    record
        prevBlock       : System.Address;
        nextBlock       : System.Address;
        buddy           : System.Address;
        numFreeBlocks   : Unsigned_64;
    end record;

    ---------------------------------------------------------------------------
    -- Circularly-linked list. Not SPARK-blessed, but we're not using access
    -- types so it's sort of a loophole.
    ---------------------------------------------------------------------------
    type FreeListArray is array (Order) of FreeBlock;

    ---------------------------------------------------------------------------
    -- Array of free lists for each order/block size
    ---------------------------------------------------------------------------
    freeLists : FreeListArray;

    ---------------------------------------------------------------------------
    -- blockSize - Return the size (in bytes) of a given block order
    ---------------------------------------------------------------------------
    function blockSize(ord : in Order) return Unsigned_64 with
        Depends => (blockSize'Result => ord),
        Post    => blockSize'Result > Virtmem.FRAME_SIZE;

    ---------------------------------------------------------------------------
    -- isValidBlock - given an address and an order, return True if this
    --  address represents a potentially valid block starting address, False
    --  otherwise.
    ---------------------------------------------------------------------------
    function isValidBlock(ord : in Order; addr : in Virtmem.PhysAddress)
        return Boolean with
        Post => isValidBlock'Result = 
            ((addr mod Integer_Address(blockSize(ord))) = 0);

    ---------------------------------------------------------------------------
    -- getListAddress
    -- Returns the head address of a freeLists list for a given order.
    --  This is a bit of a backdoor for SPARK_Mode with the setup procedure, to
    --  give us the 'Address attribute for the list heads.
    --
    --  Frankly, I'm not sure if this is a good idea or not. I'd like SPARK in
    --  the setup procedure since we're doing fairly complicated bounds-checks,
    --  etc., but sneaking around SPARK mode with tricks like this function
    --  seems like it could get down a bad road.
    ---------------------------------------------------------------------------
    function getListAddress(ord : in Order) return System.Address with
        Global  => (Input => freeLists),
        Depends => (getListAddress'Result => (ord, freeLists));

    ---------------------------------------------------------------------------
    -- setup
    -- Add frames of memory to be managed by this allocator.
    -- As mentioned in the package comments, memory not strictly within the
    -- boundaries of max-order sized blocks will not be allocatable using this
    -- package. In doing so, this means that this allocator ignores the first
    -- FRAME_SIZE * 2^MAX_BUDDY_ORDER bytes of memory.
    -- Regardless of Config.MAX_BUDDY_ORDER, this procedure will not attempt to
    -- use any memory < Config.MIN_PHYS_ALLOC.
    ---------------------------------------------------------------------------
    procedure setup(areas : in MemoryAreas.MemoryAreaArray) with
        Global  => (In_Out      => (freeLists, BuddyAllocator.initialized),
                    Input       => (BootAllocator.BitmapState,
                                    Virtmem.MAX_PHYS_USABLE),
                    Proof_In    => BootAllocator.initialized),
        Depends => (freeLists   => (areas,
                                    BootAllocator.BitmapState,
                                    Virtmem.MAX_PHYS_USABLE),
                    BuddyAllocator.initialized => null,
                    null        => (freeLists, BuddyAllocator.initialized)),
        Pre     => BootAllocator.initialized,
        Post    => BuddyAllocator.initialized;

    ---------------------------------------------------------------------------
    -- alloc
    -- Allocate a block of memory with the given order
    -- @param ord - order required
    -- @param addr - output address of the block if successful, or
    --  NO_BLOCK_AVAILABLE if no blocks of suitable size were found.
    ---------------------------------------------------------------------------
    procedure alloc(ord : in Order; addr : out Virtmem.PhysAddress) with
        Global  => (In_Out      => freeLists,
                    Proof_In    => BuddyAllocator.initialized),
        Depends => (addr        => (ord, freeLists),
                    freeLists   => (ord, freeLists)),
        Pre     => BuddyAllocator.initialized,
        Post    => isValidBlock(ord, addr);

    ---------------------------------------------------------------------------
    -- allocFrame
    -- Convenience procedure for getting a single frame, shorthand for
    --  alloc(0, addr)
    -- @param addr - output address returned by this function, 0 if
    --  unsuccessful.
    ---------------------------------------------------------------------------
    procedure allocFrame(addr : out Virtmem.PhysAddress) with
        Global  => (In_Out      => freeLists,
                    Proof_In    => BuddyAllocator.initialized),
        Depends => (addr        => (freeLists),
                    freeLists   => (freeLists)),
        Pre     => BuddyAllocator.initialized,
        Post    => isValidBlock(0, addr);

    ---------------------------------------------------------------------------
    -- free
    -- Free a block of memory of specific order
    -- @param ord - order of the _original allocation_
    -- @param addr - address of the _original allocation_
    ---------------------------------------------------------------------------
    procedure free(ord : in Order; addr : in Virtmem.PhysAddress) with
        Global  => (In_Out      => freeLists,
                    Proof_In    => BuddyAllocator.initialized),
        Depends => (freeLists   => (ord, addr, freeLists)),
        Pre     => isValidBlock(ord, addr) and BuddyAllocator.initialized;

    ---------------------------------------------------------------------------
    -- freeFrame
    -- Convenience procedure for freeing a single frame, shorthand for
    --  free(0, addr)
    -- @param addr - address of the _original allocation_
    ---------------------------------------------------------------------------
    procedure freeFrame(addr : in Virtmem.PhysAddress) with
        Global  => (In_Out      => freeLists,
                    Proof_In    => BuddyAllocator.initialized),
        Depends => (freeLists   => (addr, freeLists)),
        Pre     => isValidBlock(0, addr) and BuddyAllocator.initialized;

    ---------------------------------------------------------------------------
    -- getFreeBytes
    -- Returns the amount of memory available (in bytes) from this allocator
    ---------------------------------------------------------------------------
    function getFreeBytes return Unsigned_64 with
        Global  => (Input       => freeLists,
                    Proof_In    => BuddyAllocator.initialized),
        Depends => (getFreeBytes'Result => freeLists),
        Pre     => BuddyAllocator.initialized;

    ---------------------------------------------------------------------------
    -- getFreeFrames
    -- Returns the amount of frames available from this allocator
    ---------------------------------------------------------------------------
    function getFreeFrames return Unsigned_64 with
        Global  => (Input       => freeLists,
                    Proof_In    => BuddyAllocator.initialized),
        Depends => (getFreeFrames'Result => freeLists),
        Pre     => BuddyAllocator.initialized;


    ---------------------------------------------------------------------------
    -- print
    -- Prints a nice table with the buddy allocator information.
    ---------------------------------------------------------------------------
    procedure print;
    
end BuddyAllocator;