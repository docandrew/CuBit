-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary Physical Memory Allocator
-------------------------------------------------------------------------------
with Textmode; use Textmode;

package body BuddyAllocator
    with SPARK_Mode => On
is

    function getBuddy(ord : in Order;
                      addr : Virtmem.PhysAddress) 
        return Virtmem.PhysAddress with
        SPARK_Mode => On
    is
        mask : constant Unsigned_64 := blockSize(ord);
    begin
        return Virtmem.PhysAddress(Unsigned_64(addr) xor mask);
    end getBuddy;


    ---------------------------------------------------------------------------
    -- One of our design decisions is to ensure all the buddies in our
    -- allocator are power-of-2 aligned. When we setup the initial set of free
    -- lists, we want to ensure that we only free frames that are within an
    -- aligned block of MAX_BUDDY_ORDER size. This ensures all blocks within
    -- the buddy structure are going to stay block size-aligned.
    ---------------------------------------------------------------------------
    function blockStart(ord : in Order; addr : in Virtmem.PhysAddress)
        return Integer_Address
        with SPARK_Mode => On,
        Post => blockStart'Result < addr
    is
        roundDownMask   : constant Integer_Address := 
            Integer_Address(not (blockSize(ord) - 1));
    begin
        -- discard the lowest (FRAME_SHIFT + MAX_BUDDY_ORDER) bits
        --pragma Assert(addr and roundDownMask < addr);
        return addr and roundDownMask;

    end blockStart;

    function blockEnd(ord : in Order; addr : in Virtmem.PhysAddress)
        return Integer_Address
        with SPARK_Mode => On
    is
    begin
        return blockStart(ord, addr) + Virtmem.PhysAddress(blockSize(ord)) - 1;
    end blockEnd;


    ---------------------------------------------------------------------------
    -- popFromFreeList
    ---------------------------------------------------------------------------
    procedure popFromFreeList(ord : in Order;
                                addr : out Virtmem.PhysAddress) with
        SPARK_Mode => On,
        Pre     => freeLists(ord).numFreeBlocks > 0,
        Post    => freeLists(ord).numFreeBlocks =
                   freeLists(ord).numFreeBlocks - 1
    is
        retBlock : aliased FreeBlock
            with Import, Address => freeLists(ord).nextBlock;
    begin
        -- set output
        addr := To_Integer(freeLists(ord).nextBlock);

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
        -- if we're free, he knows we aren't.
        -- TODO: always zero the whole block?
        retBlock.buddy      := System.Null_Address;
        retBlock.prevBlock  := System.Null_Address;
        retBlock.nextBlock  := System.Null_Address;

        freeLists(ord).numFreeBlocks := freeLists(ord).numFreeBlocks - 1;
    end popFromFreeList;


    ---------------------------------------------------------------------------
    -- addToFreeList - perform an insertion at the front of the free list for
    -- order ord
    ---------------------------------------------------------------------------
    procedure addToFreeList(ord : Order;
                            newBlockAddr : in Virtmem.PhysAddress) with
        SPARK_Mode => On
    is
        newBlock  : aliased FreeBlock with
            Import, Address => To_Address(newBlockAddr);

        nextBlock : aliased FreeBlock with
            Import, Address => freeLists(ord).nextBlock;
    begin
        -- point us to the next block in the line
        newBlock.prevBlock          := nextBlock.prevBlock;
        newBlock.nextBlock          := freeLists(ord).nextBlock;
        newBlock.buddy              := To_Address(getBuddy(ord, newBlockAddr));

        -- point list head fwd to us
        freeLists(ord).nextBlock    := To_Address(newBlockAddr);

        -- point next block in line back to us
        nextBlock.prevBlock         := To_Address(newBlockAddr);

        -- increase block count
        freeLists(ord).numFreeBlocks := freeLists(ord).numFreeBlocks + 1;
    end addToFreeList;


    ---------------------------------------------------------------------------
    -- splitBlock
    --
    -- Adds unused half (the upper half) of a block with address addr and order
    -- ord to freeLists(N-1).
    ---------------------------------------------------------------------------
    procedure splitBlock(ord : in Order; addr : in Virtmem.PhysAddress) with
        SPARK_Mode => On,
        Pre     => ord > 0,
        Post    => freeLists(ord - 1).numFreeBlocks =
                   freeLists(ord - 1).numFreeBlocks'Old + 1
    is
        rightHalfAddr : constant Integer_Address := getBuddy((ord - 1), addr);
    begin
        addToFreeList(ord - 1, rightHalfAddr);
    end splitBlock;


    ---------------------------------------------------------------------------
    -- isBuddyFree - given an order and a block address, determine whether that
    -- block's buddy is free.
    ---------------------------------------------------------------------------
    function isBuddyFree(ord : in Order; addr : in Virtmem.PhysAddress) return Boolean
    with
        SPARK_Mode => On
    is
        use System; -- for "=" operator

        buddy : aliased FreeBlock with
            Import, Address => To_Address(getBuddy(ord, addr));
    begin
        -- Later on, we can replace this function with a bitmap lookup
        return buddy.buddy = To_Address(addr);
    end isBuddyFree;


    ---------------------------------------------------------------------------
    -- unlink
    -- Given a particular address with a block in a free list, unlink
    -- it from its neighbors. Make the neighbors point to each other instead of
    -- us, removing it from it's free list.
    -- @param ord - order of the block to remove from free list
    -- @param addr - address of the block to remove from the free list
    ---------------------------------------------------------------------------
    procedure unlink(ord : in Order; addr : in Virtmem.PhysAddress) with
        SPARK_Mode => On,
        Pre  => freeLists(ord).numFreeBlocks > 0,
        Post => freeLists(ord).numFreeBlocks =
                freeLists(ord).numFreeBlocks'Old - 1
    is
        block : aliased FreeBlock with
            Import, Address => To_Address(addr);

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
        freeLists(ord).numFreeBlocks :=
            freeLists(ord).numFreeBlocks - 1;
    end unlink;


    ---------------------------------------------------------------------------
    -- blockSize
    ---------------------------------------------------------------------------
    function blockSize(ord : in Order) return Unsigned_64 with
        SPARK_Mode => On
    is
    begin
        return Shift_Left(Unsigned_64(1), Natural(Virtmem.FRAME_SHIFT + ord));
    end blockSize;


    ---------------------------------------------------------------------------
    -- isValidBlock
    ---------------------------------------------------------------------------
    function isValidBlock(ord : in Order; addr : in Virtmem.PhysAddress)
        return Boolean with
        SPARK_Mode => On
    is
    begin
        return (addr mod Virtmem.PhysAddress(blockSize(ord))) = 0;
    end isValidBlock;


    ---------------------------------------------------------------------------
    -- getListAddress
    ---------------------------------------------------------------------------
    function getListAddress(ord : in Order) return System.Address with
        SPARK_Mode => Off
    is
    begin
        return freeLists(ord)'Address;
    end getListAddress;


    ---------------------------------------------------------------------------
    -- setup
    ---------------------------------------------------------------------------
    procedure setup(areas : in MemoryAreas.MemoryAreaArray) with
        SPARK_Mode => On
    is
        use type MemoryAreas.MemoryAreaType;
        use type Virtmem.PFN;

        alignedStart                : Virtmem.PhysAddress;
        alignedEnd                  : Virtmem.PhysAddress;

        -- For performance, we always want to free the largest block we can.
        -- If inside the area controlled by the boot allocator, if we're below
        -- the boot allocated high-water mark, then we have to go page-by-page.
        -- Past the next max order-aligned frame, we can free max order-sized
        -- blocks.
        topLevelBlockStart          : Virtmem.PhysAddress;
        topLevelBlockEnd            : Virtmem.PhysAddress;
        numTopLevelBlocksHere       : Unsigned_64;
    begin
        -- make freeLists self-referential and empty to start
        for ord in Order'Range loop
            freeLists(ord).prevBlock := getListAddress(ord);
            freeLists(ord).nextBlock := getListAddress(ord);
            freeLists(ord).buddy     := System.Null_Address;
        end loop;

        eachArea:
        for area of areas loop
            if  area.kind /= MemoryAreas.USABLE or
                area.endAddr < Config.MIN_PHYS_ALLOC then

                null;
            else           
                -- Round up to next max order block
                alignedStart := blockStart(Order'Last, area.startAddr) +
                                Integer_Address(blockSize(Order'Last));

                numTopLevelBlocksHere := Unsigned_64(
                    (blockStart(Order'Last, area.endAddr) - alignedStart) / 
                    Integer_Address(blockSize(Order'Last)));

                -- Round down to end previous max order block.
                alignedEnd := blockStart(Order'Last, area.endAddr) - 1;

                -- print("Memory area start: "); print(area.startAddr);
                -- print(" aligned: "); println(alignedStart);

                -- print("Memory area end:   "); print(area.endAddr);
                -- print(" aligned: "); println(alignedEnd);

                -- print("Top level blocks here: "); printdln(numTopLevelBlocksHere);
                -- print("Max boot allocator:    "); println(Virtmem.pfnToAddr(BootAllocator.highestPFNAllocated));
                -- If this memory area was too small to fit a top-level block,
                -- then the "round up" and "round down" will be flipped
                if alignedEnd < alignedStart then
                    --println("Skipping memory area (too small)");
                    null;
                else
                    pragma Assert(alignedStart mod 
                            Integer_Address(blockSize(Order'Last)) = 0);
                    pragma Assert(alignedEnd mod 
                            Integer_Address(blockSize(Order'Last)) = 0);
                    pragma Assert((alignedEnd - alignedStart) 
                            >= Integer_Address(blockSize(Order'Last)));

                    -- if this top level block is beyond the area owned by the
                    -- boot allocator, we can free the entire top-level block.
                    -- Otherwise, we go page-by-page based on what's
                    -- owned by the boot allocator.
                    for i in 0..numTopLevelBlocksHere - 1 loop

                        topLevelBlockStart := alignedStart + 
                            Integer_Address(i * blockSize(Order'Last));

                        topLevelBlockEnd := topLevelBlockStart +
                            Integer_Address(blockSize(Order'Last) - 1);

                        if BootAllocator.highestPFNAllocated >
                            Virtmem.addrToPFN(topLevelBlockStart) then

                            --print("Freeing frames in region ");
                            --print(topLevelBlockStart); print(" to "); println(topLevelBlockEnd);
                            -- go page by page in this block
                            eachPFN:
                            for pfn in 
                                Virtmem.addrToPFN(topLevelBlockStart)..
                                Virtmem.addrToPFN(topLevelBlockEnd)
                            loop
                                if BootAllocator.isFree(pfn) then
                                    free(Order'First, Virtmem.pfnToAddr(pfn));
                                    --print(" free pfn at addr: "); println(Virtmem.P2V(Virtmem.pfnToAddr(pfn)));
                                end if;
                            end loop eachPFN;
                            println;
                        else
                            --print("Freeing top-level block at ");
                            --println(topLevelBlockStart);
                            free(Order'Last, topLevelBlockStart);

                        end if;
                    end loop;
                end if;
            end if;
        end loop eachArea;

        -- TODO: free memory used by the boot allocator. This will probably
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
    procedure alloc(ord : in Order; addr : out Virtmem.PhysAddress) with
        SPARK_Mode => On
    is
        use System;

        retBlock    : Integer_Address;
        curOrd      : Order := ord;
    begin
        -- find a list order big enough to satisfy our request
        findLoop: loop
        --while curOrd <= Order'Last loop

            if freeLists(curOrd).nextBlock /= getListAddress(curOrd) then
                -- found free space in order i

                -- remove the block from the list
                popFromFreeList(curOrd, retBlock);

                -- assign output
                addr := Virtmem.V2P(retBlock);

                -- if we got a block that was too big for our request, continue
                -- to split it until it is the size we need.
                while curOrd > ord loop
                    
                    -- prove no splits of order 0
                    pragma Assert (curOrd > 0);

                    splitBlock(curOrd, retBlock);
                    curOrd := curOrd - 1;
                end loop;

                return;
            end if;

            -- necessary to avoid exceeding Order constraints in the next line
            exit findLoop when curOrd = Order'Last;
            curOrd := curOrd + 1;
        end loop findLoop;

        -- no blocks found that can satisfy the request
        addr := NO_BLOCK_AVAILABLE;
    end alloc;


    ---------------------------------------------------------------------------
    -- allocFrame
    ---------------------------------------------------------------------------
    procedure allocFrame(addr : out Virtmem.PhysAddress) with
        SPARK_Mode => On
    is
    begin
        alloc(0, addr);
    end allocFrame;


    ---------------------------------------------------------------------------
    -- free
    ---------------------------------------------------------------------------
    procedure free(ord : in Order; addr : in Virtmem.PhysAddress) with
        SPARK_Mode => On
    is
        freeAddr : Integer_Address := Virtmem.P2V(addr);

        curOrd : Order := ord;
    begin
        -- "bubble up" free blocks as long as each order's buddy is free
        -- and we aren't at max order
        coalesce: while curOrd < Order'Last loop
            if isBuddyFree(curOrd, freeAddr) then
                -- buddy indicates free (see CAUTION), coalesce
                
                -- remove buddy from its current free list
                unlink(curOrd, getBuddy(curOrd, freeAddr));

                -- combined us+buddy address, whether we were left or right
                freeAddr := freeAddr and 
                    Integer_Address(not blockSize(curOrd));
                
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
                    block.buddy := To_Address(getBuddy(curOrd, freeAddr));
                end markBuddy;

                exit coalesce;
            end if;
        end loop coalesce;

        -- add us to top free list
        addToFreeList(curOrd, freeAddr);
    end free;


    ---------------------------------------------------------------------------
    -- freeFrame
    ---------------------------------------------------------------------------
    procedure freeFrame(addr : in Virtmem.PhysAddress) with
        SPARK_Mode => On
    is
    begin
        free(0, addr);
    end freeFrame;


    ---------------------------------------------------------------------------
    -- getFreeBytes
    ---------------------------------------------------------------------------
    function getFreeBytes return Unsigned_64 with
        SPARK_Mode => On
    is
        ret : Unsigned_64 := 0;
    begin
        for ord in Order'Range loop
            ret := ret + (freeLists(ord).numFreeBlocks * blockSize(ord));
        end loop;

        return ret;
    end getFreeBytes;


    ---------------------------------------------------------------------------
    -- getFreeFrames
    ---------------------------------------------------------------------------
    function getFreeFrames return Unsigned_64 with
        SPARK_Mode => On
    is
        ret : Unsigned_64 := 0;
    begin
        for ord in Order'Range loop
            ret := ret + (freeLists(ord).numFreeBlocks * 
                          Shift_Left(1, Natural(ord)));
        end loop;

        return ret;
    end getFreeFrames;


    ---------------------------------------------------------------------------
    -- print
    ---------------------------------------------------------------------------
    procedure print with
        SPARK_Mode => On
    is
    begin
        println("-----------------------------------------------------");
        println("                  Buddy Allocator                    ");
        println("-----------------------------------------------------");
        for ord in Order'Range loop
            print("Order: "); print(Integer(ord));
            print(" Block Size: "); printd(blockSize(ord));
            print(" Free Blocks: "); printdln(freeLists(ord).numFreeBlocks);
        end loop;
        print("Total free: "); printd(getFreeBytes / 16#100000#);
        println(" MiB");
    end print;
end BuddyAllocator;