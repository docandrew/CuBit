-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary Physical Memory Allocator
-------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;

with TextIO; use TextIO;
with Util;

package body BuddyAllocator
    with SPARK_Mode => On
is

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
            with Import, Address => freeLists(ord).nextBlock;
    begin
        -- set output
        addr := freeLists(ord).nextBlock;
        
        linkNext:
        declare
            nextBlock : aliased FreeBlock
                with Import, Address => retBlock.nextBlock;
        begin
            -- fwd link to next block in list (may be the head)
            freeLists(ord).nextBlock := retBlock.nextBlock;

            -- link next block in list back to head
            nextBlock.prevBlock := retBlock.prevBlock;
        end linkNext;

        -- clear the buddy flag so when our buddy checks to see
        -- if we're free, he knows we aren't. Zeroize the whole block for
        -- safety.
        Util.memset (addr, 0, blockSize (ord));
        -- retBlock.buddy      := System.Null_Address;
        -- retBlock.prevBlock  := System.Null_Address;
        -- retBlock.nextBlock  := System.Null_Address;

        freeLists(ord).numFreeBlocks := freeLists(ord).numFreeBlocks - 1;
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
            Import, Address => newBlockAddr;

        nextBlock : aliased FreeBlock with
            Import, Address => freeLists(ord).nextBlock;
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
    end splitBlock;

    ---------------------------------------------------------------------------
    -- isBuddyFree - given an order and a block address, determine whether that
    -- block's buddy is free.
    ---------------------------------------------------------------------------
    function isBuddyFree (ord : in Order; addr : in System.Address) return Boolean
    with
        SPARK_Mode => On
    is
        use System; -- for "=" operator

        buddy : aliased FreeBlock with
            Import, Address => getBuddy (ord, addr);
    begin
        -- Later on, we can replace this function with a bitmap lookup
        return buddy.buddy = addr;
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
            Import, Address => addr;

        prevAddr : constant System.Address := block.prevBlock;
        nextAddr : constant System.Address := block.nextBlock;
    begin

        linkNeighbors:
        declare
            prevBlock : aliased FreeBlock with
                Import, Address => prevAddr;

            nextBlock : aliased FreeBlock with
                Import, Address => nextAddr;
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
        SPARK_Mode => On
    is
        use System;

        retBlock : System.Address;
        curOrd   : Order := ord;
    begin
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

                return;
            end if;

            exit findLoop when curOrd = Order'Last;
            curOrd := curOrd + 1;
        end loop findLoop;

        -- no blocks found that can satisfy the request
        addr := NO_BLOCK_AVAILABLE;
    end alloc;

    ---------------------------------------------------------------------------
    -- allocFrame
    ---------------------------------------------------------------------------
    procedure allocFrame (addr : out Virtmem.PhysAddress) with
        SPARK_Mode => On
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
        SPARK_Mode => On
    is
        curOrd   : Order := ord;
        freeAddr : Integer_Address := To_Integer(addr);
    begin
        -- "bubble up" free blocks as long as each order's buddy is free
        -- and we aren't at max order
        coalesce: while curOrd < Order'Last loop

            if isBuddyFree (curOrd, To_Address(freeAddr)) then
                -- buddy indicates free (see CAUTION), coalesce
                
                -- remove buddy from its current free list
                unlink (curOrd, getBuddy (curOrd, To_Address(freeAddr)));

                -- combined us+buddy address, whether we were left or right
                freeAddr := freeAddr and Integer_Address(not blockSize (curOrd));

                -- see if our coalesced block can be combined with the next level up
                curOrd := curOrd + 1;
            else
                -- buddy not free, annotate ourselves with his address so when
                -- he's freed later he can coalesce with us.
                markBuddy:
                declare
                    block: aliased FreeBlock with
                        Import, Address => To_Address(freeAddr);
                begin
                    block.buddy := getBuddy (curOrd, To_Address(freeAddr));
                end markBuddy;

                exit coalesce;
            end if;
        end loop coalesce;

        -- add us to front of the respective free list
        addToFreeList (curOrd, To_Address(freeAddr));
    end free;

    ---------------------------------------------------------------------------
    -- freeFrame
    ---------------------------------------------------------------------------
    procedure freeFrame (addr : in Virtmem.PhysAddress) with
        SPARK_Mode => On
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