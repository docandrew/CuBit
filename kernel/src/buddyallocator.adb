-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary Physical Memory Allocator
-------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;

with Spinlocks;
with TextIO; use TextIO;
with Util;

package body BuddyAllocator
    with SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- Buddy-pair XOR bitmap for safe coalesce checks.
    -- One bit per buddy pair per order, toggled on each alloc/free
    -- transition. Replaces unsafe in-place metadata check that read from
    -- allocated blocks (whose content could spoof the buddy address).
    -- Dynamically allocated from the boot allocator during setup.
    ---------------------------------------------------------------------------
    bitmapBase     : System.Address := System.Null_Address;
    maxBitmapPFN   : Unsigned_64 := 0;
    orderBitOffset : array (Order) of Unsigned_64 := (others => 0);

    ---------------------------------------------------------------------------
    -- Address Arithmetic (don't tell!)
    ---------------------------------------------------------------------------
    function "<" (Left : in System.Address; Right : System.Address) return Boolean
    is
    begin
        return To_Integer(Left) < To_Integer(Right);
    end "<";

    function "not" (arg : Storage_Count) return Storage_Count
    is
    begin
        -- Need to convert to modular type for this intrinsic.
        return Storage_Count(not Unsigned_64(arg));
    end "not";

    ---------------------------------------------------------------------------
    -- getBuddy
    ---------------------------------------------------------------------------
    function getBuddy (ord  : in Order;
                       addr : in System.Address) return System.Address with
        SPARK_Mode => On
    is
        mask : constant Integer_Address := Integer_Address(blockSize (ord));
    begin
        return To_Address(To_Integer(addr) xor mask);
    end getBuddy;

    ---------------------------------------------------------------------------
    -- blockStart
    -- One of our design decisions is to ensure all the buddies in our
    -- allocator are power-of-2 aligned. When we setup the initial set of free
    -- lists, we want to ensure that we only free frames that are within an
    -- aligned block of MAX_BUDDY_ORDER size. This ensures all blocks within
    -- the buddy structure are going to stay block size-aligned.
    ---------------------------------------------------------------------------
    function blockStart (ord : in Order; addr : in System.Address) return System.Address
        with SPARK_Mode => On,
        Post => blockStart'Result < addr
    is
        roundDownMask : constant Integer_Address := 
            Integer_Address(not (blockSize (ord) - 1));
    begin
        -- discard the lowest (FRAME_SHIFT + MAX_BUDDY_ORDER) bits
        --pragma Assert(addr and roundDownMask < addr);
        return To_Address(To_Integer(addr) and roundDownMask);

    end blockStart;

    ---------------------------------------------------------------------------
    -- blockEnd
    ---------------------------------------------------------------------------
    function blockEnd (ord : in Order; addr : in System.Address)
        return System.Address
        with SPARK_Mode => On
    is
    begin
        return blockStart (ord, addr) + blockSize (ord) - 1;
    end blockEnd;

    ---------------------------------------------------------------------------
    -- allocBitmap - Allocate and initialize the buddy-pair XOR bitmap
    -- from the boot allocator. Must be called before any free/alloc ops.
    ---------------------------------------------------------------------------
    procedure allocBitmap with
        SPARK_Mode => Off
    is
        maxPFN     : constant Unsigned_64 :=
            Unsigned_64(Virtmem.MAX_PHYS_USABLE) /
            Unsigned_64(Virtmem.FRAME_SIZE);
        totalBits  : Unsigned_64 := 0;
        totalBytes : Storage_Count;
        numFrames  : Positive;
        physAddr   : Virtmem.PhysAddress;
    begin
        for ord in Order range 0 .. Order'Last - 1 loop
            orderBitOffset(ord) := totalBits;
            totalBits := totalBits +
                Shift_Right(maxPFN, Natural(ord) + 1);
        end loop;

        totalBytes := Storage_Count(Shift_Right(totalBits + 63, 6) * 8);

        if totalBytes < Virtmem.FRAME_SIZE then
            numFrames := 1;
        else
            numFrames := Natural(
                (totalBytes + Virtmem.FRAME_SIZE - 1) / Virtmem.FRAME_SIZE);
        end if;

        BootAllocator.allocFrames(numFrames, physAddr);

        bitmapBase   := Virtmem.P2Va(physAddr);
        maxBitmapPFN := maxPFN;

        declare
            ignore : System.Address;
        begin
            ignore := Util.memset(bitmapBase, 0, totalBytes);
        end;

        print("Buddy bitmap: ");
        print(Natural(totalBytes));
        println(" bytes");
    end allocBitmap;

    ---------------------------------------------------------------------------
    -- toggleBit - Toggle the XOR bit for the buddy pair containing addr
    -- at the given order. Called on each alloc/free state transition.
    ---------------------------------------------------------------------------
    procedure toggleBit (ord : in Order; addr : in System.Address) with
        SPARK_Mode => Off
    is
        pfn     : constant Unsigned_64 :=
            Unsigned_64(Virtmem.vaddrToPFN(addr));
        pi      : constant Unsigned_64 :=
            Shift_Right(pfn, Natural(ord) + 1);
        bitPos  : constant Unsigned_64 := orderBitOffset(ord) + pi;
        wordIdx : constant Unsigned_64 := Shift_Right(bitPos, 6);
        bitIdx  : constant Natural := Natural(bitPos and 63);

        word : aliased Unsigned_64 with
            Import, Address => bitmapBase +
                Storage_Offset(wordIdx * 8);
    begin
        word := word xor Shift_Left(Unsigned_64(1), bitIdx);
    end toggleBit;

    ---------------------------------------------------------------------------
    -- testBit - Return True if the buddy-pair bit is set (one buddy free,
    -- one allocated). Return False if clear (both in same state).
    ---------------------------------------------------------------------------
    function testBit (ord : in Order; addr : in System.Address)
        return Boolean with
        SPARK_Mode => Off
    is
        pfn     : constant Unsigned_64 :=
            Unsigned_64(Virtmem.vaddrToPFN(addr));
        pi      : constant Unsigned_64 :=
            Shift_Right(pfn, Natural(ord) + 1);
        bitPos  : constant Unsigned_64 := orderBitOffset(ord) + pi;
        wordIdx : constant Unsigned_64 := Shift_Right(bitPos, 6);
        bitIdx  : constant Natural := Natural(bitPos and 63);

        word : aliased Unsigned_64 with
            Import, Address => bitmapBase +
                Storage_Offset(wordIdx * 8);
    begin
        return (word and Shift_Left(Unsigned_64(1), bitIdx)) /= 0;
    end testBit;

    ---------------------------------------------------------------------------
    -- popFromFreeList
    ---------------------------------------------------------------------------
    procedure popFromFreeList (ord  : in Order;
                               addr : out System.Address) with
        SPARK_Mode => On,
        Pre     => freeLists(ord).numFreeBlocks > 0,
        Post    => freeLists(ord).numFreeBlocks =
                   freeLists(ord).numFreeBlocks - 1
    is
        retBlock : aliased FreeBlock
            with Import, Volatile, Address => freeLists(ord).nextBlock;
    begin
        -- set output
        addr := freeLists(ord).nextBlock;

        linkNext:
        declare
            nextBlock : aliased FreeBlock
                with Import, Volatile, Address => retBlock.nextBlock;
        begin
            -- fwd link to next block in list (may be the head)
            freeLists(ord).nextBlock := retBlock.nextBlock;

            -- link next block in list back to head
            nextBlock.prevBlock := retBlock.prevBlock;
        end linkNext;

        freeLists(ord).numFreeBlocks := freeLists(ord).numFreeBlocks - 1;

        -- Toggle bitmap: this block transitions from free to allocated.
        if ord < Order'Last then
            toggleBit (ord, addr);
        end if;
    end popFromFreeList;

    ---------------------------------------------------------------------------
    -- addToFreeList - perform an insertion at the front of the free list for
    -- order ord
    ---------------------------------------------------------------------------
    procedure addToFreeList (ord : Order;
                             newBlockAddr : in System.Address) with
        SPARK_Mode => On
    is
        newBlock  : aliased FreeBlock with
            Import, Volatile, Address => newBlockAddr;

        nextBlock : aliased FreeBlock with
            Import, Volatile, Address => freeLists(ord).nextBlock;
    begin
        -- point us to the next block in the line
        newBlock.prevBlock          := nextBlock.prevBlock;
        newBlock.nextBlock          := freeLists(ord).nextBlock;
        newBlock.buddy              := getBuddy (ord, newBlockAddr);

        -- point list head fwd to us
        freeLists(ord).nextBlock    := newBlockAddr;

        -- point next block in line back to us
        nextBlock.prevBlock         := newBlockAddr;

        -- increase block count
        freeLists(ord).numFreeBlocks := freeLists(ord).numFreeBlocks + 1;
    end addToFreeList;

    ---------------------------------------------------------------------------
    -- splitBlock
    --
    -- Adds unused half (the upper half) of a block with address addr and order
    -- ord to freeLists(N-1).
    ---------------------------------------------------------------------------
    procedure splitBlock (ord : in Order; addr : in System.Address) with
        SPARK_Mode => On,
        Pre     => ord > 0,
        Post    => freeLists(ord - 1).numFreeBlocks =
                   freeLists(ord - 1).numFreeBlocks'Old + 1
    is
        rightHalfAddr : constant System.Address := getBuddy((ord - 1), addr);
    begin
        addToFreeList (ord - 1, rightHalfAddr);

        -- Toggle bitmap: right half transitions to free at ord-1.
        toggleBit (ord - 1, rightHalfAddr);
    end splitBlock;

    ---------------------------------------------------------------------------
    -- isBuddyFree - given an order and a block address, determine whether that
    -- block's buddy is free.
    ---------------------------------------------------------------------------
    ---------------------------------------------------------------------------
    -- isBuddyFree - uses the XOR bitmap. The caller must have already
    -- toggled the bit for this pair (see free procedure). Bit = 0 after
    -- toggle means both buddies are in the same state; since we just freed
    -- ours, the buddy must also be free.
    ---------------------------------------------------------------------------
    function isBuddyFree (ord : in Order; addr : in System.Address) return Boolean
    with
        SPARK_Mode => Off
    is
    begin
        return not testBit (ord, addr);
    end isBuddyFree;

    ---------------------------------------------------------------------------
    -- unlink
    -- Given a particular address with a block in a free list, unlink
    -- it from its neighbors. Make the neighbors point to each other instead of
    -- us, removing it from it's free list.
    -- @param ord - order of the block to remove from free list
    -- @param addr - address of the block to remove from the free list
    ---------------------------------------------------------------------------
    procedure unlink (ord : in Order; addr : in System.Address) with
        SPARK_Mode => On,
        Pre  => freeLists(ord).numFreeBlocks > 0,
        Post => freeLists(ord).numFreeBlocks =
                freeLists(ord).numFreeBlocks'Old - 1
    is
        block : aliased FreeBlock with
            Import, Volatile, Address => addr;

        prevAddr : constant System.Address := block.prevBlock;
        nextAddr : constant System.Address := block.nextBlock;
    begin

        linkNeighbors:
        declare
            prevBlock : aliased FreeBlock with
                Import, Volatile, Address => prevAddr;

            nextBlock : aliased FreeBlock with
                Import, Volatile, Address => nextAddr;
        begin
            prevBlock.nextBlock := nextAddr;
            nextBlock.prevBlock := prevAddr;
        end linkNeighbors;

        -- decrement the free list count when we unlink somebody
        freeLists(ord).numFreeBlocks := freeLists(ord).numFreeBlocks - 1;
    end unlink;

    ---------------------------------------------------------------------------
    -- blockSize
    ---------------------------------------------------------------------------
    function blockSize (ord : in Order) return Storage_Count with
        SPARK_Mode => On
    is
    begin
        return Storage_Count(Shift_Left (Value  => Unsigned_64(1),
                                         Amount => Integer(Virtmem.FRAME_SHIFT + ord)));
    end blockSize;

    ---------------------------------------------------------------------------
    -- getOrder
    ---------------------------------------------------------------------------
    function getOrder (allocSize : in Storage_Count) return Order with
        SPARK_Mode => On
    is
    begin
        if allocSize = 0 then
            raise AllocatorException with "getOrder with argument 0";
        else
            for ord in Order'Range loop
                if blockSize (ord) >= allocSize then
                    return ord;
                end if;
            end loop;

            raise AllocatorException with "getOrder - allocation size exceeds BuddyAllocator maximum block size";
        end if;
    end getOrder;

    ---------------------------------------------------------------------------
    -- isValidBlock
    ---------------------------------------------------------------------------
    function isValidBlock (ord : in Order; addr : in System.Address)
        return Boolean with
        SPARK_Mode => On
    is
    begin
        return (addr mod blockSize(ord)) = 0;
    end isValidBlock;

    ---------------------------------------------------------------------------
    -- getListAddress
    ---------------------------------------------------------------------------
    function getListAddress (ord : in Order) return System.Address with
        SPARK_Mode => Off
    is
    begin
        return freeLists(ord)'Address;
    end getListAddress;

    ---------------------------------------------------------------------------
    -- getAlignedStart
    -- Given the start of a _physical_ memory region, round up to the nearest max 
    -- block-aligned _virtual_ (linear-mapped) address.
    ---------------------------------------------------------------------------
    function getAlignedStart (startPhys : Virtmem.PhysAddress) return System.Address
    is
    begin
        return blockStart (Order'Last, Virtmem.P2Va(startPhys))
             + blockSize (Order'Last);
    end getAlignedStart;

    ---------------------------------------------------------------------------
    -- getAlignedEnd
    -- Given the end of a physical memory region, round down to the nearest max
    -- block-aligned _virtual_ (linear-mapped) address.
    ---------------------------------------------------------------------------
    function getAlignedEnd (endPhys : Virtmem.PhysAddress) return System.Address
    is
    begin
        return blockStart (Order'last, Virtmem.P2Va(endPhys)) - 1;
    end getAlignedEnd;

    ---------------------------------------------------------------------------
    -- setup
    ---------------------------------------------------------------------------
    procedure setup (areas : in MemoryAreas.MemoryAreaArray) with
        SPARK_Mode => On
    is
        use type MemoryAreas.MemoryAreaType;
        use type Virtmem.PFN;

        alignedStart          : System.Address;
        alignedEnd            : System.Address;

        -- For performance, we always want to free the largest block we can.
        -- If inside the area controlled by the boot allocator, if we're below
        -- the boot allocated high-water mark, then we have to go page-by-page.
        -- Past the next max order-aligned frame, we can free max order-sized
        -- blocks.
        topLevelBlockStart    : System.Address;
        topLevelBlockEnd      : System.Address;
        startPFN              : Virtmem.PFN;
        endPFN                : Virtmem.PFN;
        numTopLevelBlocksHere : Storage_Count;
    begin
        -- make freeLists self-referential and empty to start
        for ord in Order'Range loop
            freeLists(ord).prevBlock := getListAddress (ord);
            freeLists(ord).nextBlock := getListAddress (ord);
            freeLists(ord).buddy     := System.Null_Address;
        end loop;

        -- Allocate XOR bitmap for safe buddy-pair coalesce checks.
        allocBitmap;

        eachArea:
        for area of areas loop
            if area.kind /= MemoryAreas.USABLE or 
               area.endAddr < Config.MIN_PHYS_ALLOC then
                null;
            else           
                -- Determine max-block aligned memory boundaries.
                alignedStart := getAlignedStart (area.startAddr);
                alignedEnd   := getAlignedEnd (area.endAddr);

                numTopLevelBlocksHere := (alignedEnd - alignedStart) / blockSize (Order'Last);

                -- If this memory area was too small to fit a top-level block,
                -- then the "round up" and "round down" will be flip-flopped.
                if alignedEnd < alignedStart then
                    -- This memory area is too small. Skip it.
                    null;
                else
                    pragma Assert (alignedStart mod blockSize(Order'Last) = 0);
                    pragma Assert (alignedEnd mod blockSize(Order'Last) = 0);
                    pragma Assert (alignedEnd - alignedStart >= blockSize(Order'Last));

                    -- if this top level block is beyond the area owned by the
                    -- boot allocator, we can free the entire top-level block.
                    -- Otherwise, we go page-by-page based on what's
                    -- not owned by the boot allocator.
                    for i in 0..numTopLevelBlocksHere - 1 loop

                        topLevelBlockStart := alignedStart + (i * blockSize (Order'Last));

                        topLevelBlockEnd := topLevelBlockStart + (blockSize (Order'Last) - 1);

                        startPFN := Virtmem.vaddrToPFN (topLevelBlockStart);
                        endPFN   := Virtmem.vaddrToPFN (topLevelBlockEnd);

                        if BootAllocator.highestPFNAllocated > startPFN then

                            -- go page by page in this block
                            eachPFN: for pfn in startPFN..endPFN loop

                                if BootAllocator.isFree (pfn) then
                                    free (ord  => Order'First,
                                          addr => Virtmem.P2Va (Virtmem.pfnToAddr (pfn)));
                                end if;

                            end loop eachPFN;
                        else
                            free (Order'Last, topLevelBlockStart);

                        end if;
                    end loop;
                end if;
            end if;
        end loop eachArea;

        -- @TODO free memory used by the boot allocator. This will probably
        -- take a little effort, since it's buried in the midst of the kernel's
        -- .bss, and we consider everything under ebss to be off-limits. We can
        -- play some games with the linker script to put the bitmaps in their
        -- own section, then we'll have symbols here that we can use to reclaim
        -- that memory.

        initialized := True;        -- Ghost assignment
    end setup;

    ---------------------------------------------------------------------------
    -- alloc
    ---------------------------------------------------------------------------
    procedure alloc (ord : in Order; addr : out System.Address) with
        SPARK_Mode => Off  -- lock calls change Global contract
    is
        use System;

        retBlock : System.Address;
        curOrd   : Order := ord;
    begin
        Spinlocks.enterCriticalSection (lock);

        -- find a list order big enough to satisfy our request
        findLoop: loop

            if freeLists(curOrd).nextBlock /= getListAddress (curOrd) then
                -- found free space in order i
                -- remove the block from the list

                popFromFreeList (curOrd, retBlock);

                -- assign output
                addr := retBlock;

                -- if we got a block that was too big for our request, continue
                -- to split it until it is the size we need.
                while curOrd > ord loop
                    -- prove no splits of order 0
                    pragma Assert (curOrd > 0);

                    splitBlock (curOrd, retBlock);
                    curOrd := curOrd - 1;
                end loop;

                Spinlocks.exitCriticalSection (lock);

                -- Zero the allocated block outside the lock to prevent
                -- information leakage between processes. Only zero the
                -- requested size, not the full popped block.
                declare
                    ignore : System.Address;
                begin
                    ignore := Util.memset (addr, 0, blockSize (ord));
                end;
                return;
            end if;

            exit findLoop when curOrd = Order'Last;
            curOrd := curOrd + 1;
        end loop findLoop;

        -- no blocks found that can satisfy the request
        addr := NO_BLOCK_AVAILABLE;
        Spinlocks.exitCriticalSection (lock);
    end alloc;

    ---------------------------------------------------------------------------
    -- allocFrame
    ---------------------------------------------------------------------------
    procedure allocFrame (addr : out Virtmem.PhysAddress) with
        SPARK_Mode => Off
    is
        vaddr : System.Address;
    begin
        alloc (0, vaddr);
        addr := Virtmem.V2P (vaddr);
    end allocFrame;

    ---------------------------------------------------------------------------
    -- getOrderNum
    ---------------------------------------------------------------------------
    function getOrderNum (ord : in Order) return Natural
    is
        function toNat is new Ada.Unchecked_Conversion(Source => Order, Target => Natural);
    begin
        return toNat (ord);
    end getOrderNum;

    ---------------------------------------------------------------------------
    -- free
    ---------------------------------------------------------------------------
    procedure free (ord : in Order; addr : in System.Address) with
        SPARK_Mode => Off  -- lock calls change Global contract
    is
        curOrd   : Order := ord;
        freeAddr : Integer_Address := To_Integer(addr);
    begin
        Spinlocks.enterCriticalSection (lock);

        -- "bubble up" free blocks as long as each order's buddy is free
        -- and we aren't at max order
        coalesce: while curOrd < Order'Last loop

            -- Toggle bitmap for this buddy pair, then test.
            toggleBit (curOrd, To_Address(freeAddr));

            if isBuddyFree (curOrd, To_Address(freeAddr)) then
                -- buddy is free, coalesce

                -- remove buddy from its current free list
                unlink (curOrd, getBuddy (curOrd, To_Address(freeAddr)));

                -- combined us+buddy address, whether we were left or right
                freeAddr := freeAddr and Integer_Address(not blockSize (curOrd));

                -- see if our coalesced block can be combined with the next level up
                curOrd := curOrd + 1;
            else
                -- buddy not free
                exit coalesce;
            end if;
        end loop coalesce;

        -- add us to front of the respective free list
        addToFreeList (curOrd, To_Address(freeAddr));

        Spinlocks.exitCriticalSection (lock);
    end free;

    ---------------------------------------------------------------------------
    -- freeFrame
    ---------------------------------------------------------------------------
    procedure freeFrame (addr : in Virtmem.PhysAddress) with
        SPARK_Mode => Off
    is
    begin
        free (0, Virtmem.P2Va (addr));
    end freeFrame;

    ---------------------------------------------------------------------------
    -- getFreeBytes
    ---------------------------------------------------------------------------
    function getFreeBytes return Storage_Count with
        SPARK_Mode => On
    is
        ret : Storage_Count := 0;
    begin
        for ord in Order'Range loop
            ret := ret + (Storage_Count(freeLists(ord).numFreeBlocks) * blockSize (ord));
        end loop;

        return ret;
    end getFreeBytes;


    ---------------------------------------------------------------------------
    -- getFreeFrames
    ---------------------------------------------------------------------------
    function getFreeFrames return Natural with
        SPARK_Mode => On
    is
    begin
        return Natural(getFreeBytes / Virtmem.FRAME_SIZE);
    end getFreeFrames;

    ---------------------------------------------------------------------------
    -- print
    ---------------------------------------------------------------------------
    procedure print with
        SPARK_Mode => On
    is
    begin
        println ("-----------------------------------------------------");
        println ("                  Buddy Allocator                    ", LT_BLUE, BLACK);
        println ("-----------------------------------------------------");
        
        for ord in Order'Range loop
            print ("Order: ");        print (Integer(ord));
            print (" Block Size: ");  print (Natural(blockSize(ord)));
            print (" Free Blocks: "); println (freeLists(ord).numFreeBlocks);
        end loop;
        
        print ("Total free: "); print (Natural(getFreeBytes / 16#100000#));
        println (" MiB");

    end print;
end BuddyAllocator;