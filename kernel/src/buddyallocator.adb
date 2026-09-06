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
    use type System.Address;

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

    -- One byte per physical frame.  Bits 0..6 are the pin count and bit 7
    -- records a freeFrame deferred until the final pin is returned.
    pinStateBase    : System.Address := System.Null_Address;
    frameOwnerBase  : System.Address := System.Null_Address;
    maxPinPFN       : Unsigned_64 := 0;
    PIN_COUNT_MASK  : constant Unsigned_8 := 16#7F#;
    PIN_DEFERRED    : constant Unsigned_8 := 16#80#;

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

    procedure allocPinState with
        SPARK_Mode => Off
    is
        stateBytes : constant Storage_Count :=
          Storage_Count (Unsigned_64 (Virtmem.MAX_PHYS_USABLE) /
                         Unsigned_64 (Virtmem.FRAME_SIZE) + 1);
        numFrames  : constant Positive := Positive
          ((stateBytes + Virtmem.FRAME_SIZE - 1) / Virtmem.FRAME_SIZE);
        pinPhys    : Virtmem.PhysAddress;
        ownerPhys  : Virtmem.PhysAddress;
        ignore     : System.Address;
    begin
        BootAllocator.allocFrames (numFrames, pinPhys);
        BootAllocator.allocFrames (numFrames, ownerPhys);
        pinStateBase := Virtmem.P2Va (pinPhys);
        frameOwnerBase := Virtmem.P2Va (ownerPhys);
        maxPinPFN := Unsigned_64 (Virtmem.MAX_PHYS_USABLE) /
          Unsigned_64 (Virtmem.FRAME_SIZE);
        ignore := Util.memset (pinStateBase, 0, stateBytes);
        ignore := Util.memset (frameOwnerBase, 0, stateBytes);

        print ("Frame pin state: ");
        print (Natural (stateBytes));
        println (" bytes");
    end allocPinState;

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
    -- Given the start of a _physical_ memory region, round up to the nearest
    -- block-aligned _virtual_ (linear-mapped) address at the given order.
    ---------------------------------------------------------------------------
    function getAlignedStart (startPhys : Virtmem.PhysAddress;
                              ord       : Order) return System.Address
    is
    begin
        return blockStart (ord, Virtmem.P2Va(startPhys))
             + blockSize (ord);
    end getAlignedStart;

    ---------------------------------------------------------------------------
    -- getAlignedEnd
    -- Given the end of a physical memory region, round down to the nearest
    -- block-aligned _virtual_ (linear-mapped) address at the given order.
    ---------------------------------------------------------------------------
    function getAlignedEnd (endPhys : Virtmem.PhysAddress;
                            ord     : Order) return System.Address
    is
    begin
        return blockStart (ord, Virtmem.P2Va(endPhys)) - 1;
    end getAlignedEnd;

    ---------------------------------------------------------------------------
    -- effectiveOrder
    -- Choose the largest block order where alignment waste stays reasonable
    -- for the given area size.  Target: blockSize(order) <= areaSize / 32,
    -- so we lose at most ~6% to boundary alignment.
    ---------------------------------------------------------------------------
    function effectiveOrder (areaSize : Storage_Count) return Order
    is
        effOrd : Order := Order'Last;
    begin
        while effOrd > 0 and then blockSize (effOrd) > areaSize / 32 loop
            effOrd := effOrd - 1;
        end loop;
        return effOrd;
    end effectiveOrder;

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
        -- Allocate lifetime metadata before releasing boot-managed frames to
        -- the buddy lists, so the metadata can never itself be allocated.
        allocPinState;

        eachArea:
        for area of areas loop
            if area.kind /= MemoryAreas.USABLE or
               area.endAddr < Config.MIN_PHYS_ALLOC then
                null;
            else
                -- Pick the largest block order that keeps alignment waste
                -- reasonable for this area's size.
                declare
                    areaSize : constant Storage_Count :=
                        Storage_Count (area.endAddr - area.startAddr);
                    effOrd   : constant Order := effectiveOrder (areaSize);
                begin
                    alignedStart := getAlignedStart (area.startAddr, effOrd);
                    alignedEnd   := getAlignedEnd (area.endAddr, effOrd);

                    numTopLevelBlocksHere :=
                        (alignedEnd - alignedStart) / blockSize (effOrd);

                    -- If this memory area was too small to fit a block at
                    -- effOrd, the round-up and round-down will be flipped.
                    if alignedEnd < alignedStart then
                        null;
                    else
                        for i in 0 .. numTopLevelBlocksHere - 1 loop

                            topLevelBlockStart :=
                                alignedStart + (i * blockSize (effOrd));

                            topLevelBlockEnd :=
                                topLevelBlockStart + (blockSize (effOrd) - 1);

                            startPFN := Virtmem.vaddrToPFN (topLevelBlockStart);
                            endPFN   := Virtmem.vaddrToPFN (topLevelBlockEnd);

                            if BootAllocator.highestPFNAllocated > startPFN then
                                -- Within boot allocator range: page by page
                                eachPFN:
                                for pfn in startPFN .. endPFN loop
                                    if BootAllocator.isFree (pfn) then
                                        free (ord  => Order'First,
                                              addr => Virtmem.P2Va (
                                                  Virtmem.pfnToAddr (pfn)));
                                    end if;
                                end loop eachPFN;
                            else
                                free (effOrd, topLevelBlockStart);
                            end if;
                        end loop;
                    end if;
                end;
            end if;
        end loop eachArea;

        -- @TODO free memory used by the boot allocator. This will probably
        -- take a little effort, since it's buried in the midst of the kernel's
        -- .bss, and we consider everything under ebss to be off-limits. We can
        -- play some games with the linker script to put the bitmaps in their
        -- own section, then we'll have symbols here that we can use to reclaim
        -- that memory.

        --  Record total usable memory before anything is allocated
        declare
            total : Storage_Count := 0;
        begin
            for ord in Order'Range loop
                total := total +
                    Storage_Count (freeLists (ord).numFreeBlocks) *
                    blockSize (ord);
            end loop;
            totalManagedBytes := total;
        end;

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
    procedure freeLocked (ord : in Order; addr : in System.Address) with
        SPARK_Mode => Off
    is
        curOrd   : Order := ord;
        freeAddr : Integer_Address := To_Integer(addr);
    begin
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
    end freeLocked;

    procedure free (ord : in Order; addr : in System.Address) with
        SPARK_Mode => Off  -- lock calls change Global contract
    is
    begin
        Spinlocks.enterCriticalSection (lock);
        freeLocked (ord, addr);

        Spinlocks.exitCriticalSection (lock);
    end free;

    ---------------------------------------------------------------------------
    -- freeFrame
    ---------------------------------------------------------------------------
    procedure freeFrame (addr : in Virtmem.PhysAddress) with
        SPARK_Mode => Off
    is
        pfn : constant Unsigned_64 := Unsigned_64 (Virtmem.addrToPFN (addr));
    begin
        Spinlocks.enterCriticalSection (lock);
        if pinStateBase /= System.Null_Address and then pfn <= maxPinPFN then
            declare
                state : Unsigned_8 with
                    Import,
                    Volatile,
                    Address => pinStateBase + Storage_Offset (pfn);
                owner : Unsigned_8 with
                    Import,
                    Volatile,
                    Address => frameOwnerBase + Storage_Offset (pfn);
            begin
                owner := 0;
                if (state and PIN_COUNT_MASK) /= 0 then
                    state := state or PIN_DEFERRED;
                    Spinlocks.exitCriticalSection (lock);
                    return;
                end if;
            end;
        end if;

        freeLocked (0, Virtmem.P2Va (addr));
        Spinlocks.exitCriticalSection (lock);
    end freeFrame;

    procedure pinFrame
      (addr    : in Virtmem.PhysAddress;
       success : out Boolean) with
        SPARK_Mode => Off
    is
        pfn : constant Unsigned_64 := Unsigned_64 (Virtmem.addrToPFN (addr));
    begin
        success := False;
        if pinStateBase = System.Null_Address or else pfn > maxPinPFN or else
           addr mod Virtmem.FRAME_SIZE /= 0
        then
            return;
        end if;

        Spinlocks.enterCriticalSection (lock);
        declare
            state : Unsigned_8 with
                Import,
                Volatile,
                Address => pinStateBase + Storage_Offset (pfn);
            count : constant Unsigned_8 := state and PIN_COUNT_MASK;
        begin
            if (state and PIN_DEFERRED) = 0 and then count < PIN_COUNT_MASK then
                state := state + 1;
                success := True;
            end if;
        end;
        Spinlocks.exitCriticalSection (lock);
    end pinFrame;

    procedure unpinFrame
      (addr    : in Virtmem.PhysAddress;
       success : out Boolean) with
        SPARK_Mode => Off
    is
        pfn : constant Unsigned_64 := Unsigned_64 (Virtmem.addrToPFN (addr));
    begin
        success := False;
        if pinStateBase = System.Null_Address or else pfn > maxPinPFN or else
           addr mod Virtmem.FRAME_SIZE /= 0
        then
            return;
        end if;

        Spinlocks.enterCriticalSection (lock);
        declare
            state : Unsigned_8 with
                Import,
                Volatile,
                Address => pinStateBase + Storage_Offset (pfn);
            count : constant Unsigned_8 := state and PIN_COUNT_MASK;
        begin
            if count /= 0 then
                state := state - 1;
                success := True;
                if count = 1 and then (state and PIN_DEFERRED) /= 0 then
                    state := 0;
                    freeLocked (0, Virtmem.P2Va (addr));
                end if;
            end if;
        end;
        Spinlocks.exitCriticalSection (lock);
    end unpinFrame;

    procedure claimUserFrame
      (addr    : in Virtmem.PhysAddress;
       owner   : in Unsigned_8;
       success : out Boolean) with
        SPARK_Mode => Off
    is
        pfn : constant Unsigned_64 := Unsigned_64 (Virtmem.addrToPFN (addr));
    begin
        success := False;
        if owner = 0 or else frameOwnerBase = System.Null_Address or else
           pfn > maxPinPFN or else addr mod Virtmem.FRAME_SIZE /= 0
        then
            return;
        end if;

        Spinlocks.enterCriticalSection (lock);
        declare
            currentOwner : Unsigned_8 with
                Import,
                Volatile,
                Address => frameOwnerBase + Storage_Offset (pfn);
        begin
            if currentOwner = 0 then
                currentOwner := owner;
                success := True;
            end if;
        end;
        Spinlocks.exitCriticalSection (lock);
    end claimUserFrame;

    procedure releaseUserFrame
      (addr  : in Virtmem.PhysAddress;
       owner : in Unsigned_8) with
        SPARK_Mode => Off
    is
        pfn : constant Unsigned_64 := Unsigned_64 (Virtmem.addrToPFN (addr));
    begin
        if owner = 0 or else frameOwnerBase = System.Null_Address or else
           pfn > maxPinPFN or else addr mod Virtmem.FRAME_SIZE /= 0
        then
            return;
        end if;

        Spinlocks.enterCriticalSection (lock);
        declare
            currentOwner : Unsigned_8 with
                Import,
                Volatile,
                Address => frameOwnerBase + Storage_Offset (pfn);
        begin
            if currentOwner = owner then
                currentOwner := 0;
            end if;
        end;
        Spinlocks.exitCriticalSection (lock);
    end releaseUserFrame;

    function isUserFrameOwnedBy
      (addr  : Virtmem.PhysAddress;
       owner : Unsigned_8) return Boolean with
        SPARK_Mode => Off
    is
        pfn : constant Unsigned_64 := Unsigned_64 (Virtmem.addrToPFN (addr));
        result : Boolean := False;
    begin
        if owner = 0 or else frameOwnerBase = System.Null_Address or else
           pfn > maxPinPFN or else addr mod Virtmem.FRAME_SIZE /= 0
        then
            return False;
        end if;

        Spinlocks.enterCriticalSection (lock);
        declare
            currentOwner : Unsigned_8 with
                Import,
                Volatile,
                Address => frameOwnerBase + Storage_Offset (pfn);
        begin
            result := currentOwner = owner;
        end;
        Spinlocks.exitCriticalSection (lock);
        return result;
    end isUserFrameOwnedBy;

    ---------------------------------------------------------------------------
    -- getFreeBytes
    ---------------------------------------------------------------------------
    function getTotalBytes return Storage_Count with
        SPARK_Mode => On
    is
    begin
        return totalManagedBytes;
    end getTotalBytes;

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
