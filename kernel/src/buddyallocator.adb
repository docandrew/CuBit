-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary Physical Memory Allocator
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with Spinlocks;
with Frame_Pins;
with Buddy_Bitmap;
with Buddy_Blocks;
with Buddy_Geometry;
with Buddy_Boot_Admission;
with Buddy_Metadata;
with Firmware_Frames;
with Intrusive_List_Splices;
with TextIO; use TextIO;
with Util;

package body BuddyAllocator
    with SPARK_Mode => On
is
    use type System.Address;
    use type Buddy_Geometry.Count;
    package List_Splices is new Intrusive_List_Splices (System.Address);

    ---------------------------------------------------------------------------
    -- Buddy-pair XOR bitmap for safe coalesce checks.
    -- One bit per buddy pair per order, toggled on each alloc/free
    -- transition. Replaces unsafe in-place metadata check that read from
    -- allocated blocks (whose content could spoof the buddy address).
    -- Dynamically allocated from the boot allocator during setup.
    ---------------------------------------------------------------------------
    bitmapBase     : System.Address := System.Null_Address;
    bitmapLayout : Buddy_Bitmap.Layout
      (Buddy_Bitmap.Order (Natural (Order'Last) - 1));

    -- One byte per physical frame.  Bits 0..6 are the pin count and bit 7
    -- records a freeFrame deferred until the final pin is returned.
    pinStateBase    : System.Address := System.Null_Address;
    frameOwnerBase  : System.Address := System.Null_Address;
    maxPinPFN       : Unsigned_64 := 0;
    blockStateBase : System.Address := System.Null_Address;

    -- Only this adapter converts addresses into metadata locations. All
    -- callers hold the allocator lock (or are in single-threaded setup).
    function descriptorAddress (ord : Order; addr : System.Address)
      return System.Address with SPARK_Mode => Off
    is
        phys : Integer_Address;
        pfn : Unsigned_64;
        frames : constant Unsigned_64 := 2 ** Natural (ord);
    begin
        if To_Integer (addr) < Virtmem.LINEAR_BASE or else
           addr mod Virtmem.FRAME_SIZE /= 0
        then
            raise AllocatorException with "Invalid buddy block address";
        end if;
        phys := To_Integer (addr) - Virtmem.LINEAR_BASE;
        pfn := Unsigned_64 (phys / Integer_Address (Virtmem.FRAME_SIZE));
        if blockStateBase = System.Null_Address or else pfn > maxPinPFN
          or else not Buddy_Geometry.Fits
            (Buddy_Geometry.Frame (pfn), Buddy_Geometry.Frame_Count (frames),
             Buddy_Geometry.Frame (maxPinPFN))
        then
            raise AllocatorException with "Block outside allocator metadata";
        end if;
        return To_Address (Integer_Address (Buddy_Metadata.Address_Of
          (Unsigned_64 (To_Integer (blockStateBase)), Buddy_Metadata.Frame (pfn),
           Buddy_Metadata.Block_State)));
    end descriptorAddress;

    function geometryBlock (ord : Order; addr : System.Address)
      return Buddy_Geometry.Block with SPARK_Mode => Off
    is
        checked : constant System.Address := descriptorAddress (ord, addr);
        pragma Unreferenced (checked);
    begin
        -- The single metadata admission boundary checked alignment and extent
        -- with the same Fits predicate required by the proved constructor.
        return Buddy_Geometry.Make
          (Buddy_Geometry.Frame (Virtmem.vaddrToPFN (addr)), 2 ** Natural (ord));
    end geometryBlock;

    function blockAddress (item : Buddy_Geometry.Block) return System.Address
      with SPARK_Mode => Off
    is
    begin
        return Virtmem.P2Va (Virtmem.PhysAddress (Buddy_Geometry.First (item)) *
          Virtmem.FRAME_SIZE);
    end blockAddress;

    function blockState (ord : Order; addr : System.Address)
      return Buddy_Blocks.Descriptor with SPARK_Mode => Off
    is
        item : Buddy_Blocks.Descriptor with Import,
          Address => descriptorAddress (ord, addr);
    begin
        return item;
    end blockState;

    procedure moveBlock (ord : Order; addr : System.Address;
                         action : Buddy_Blocks.Transition)
      with SPARK_Mode => Off
    is
        item : Buddy_Blocks.Descriptor with Import,
          Address => descriptorAddress (ord, addr);
        success : Boolean;
    begin
        Buddy_Blocks.Move (item, Buddy_Blocks.Order (ord), action, success);
        if not success then
            raise AllocatorException with "Invalid buddy block transition";
        end if;
    end moveBlock;

    function containsAllocatedFrame (addr : Virtmem.PhysAddress) return Boolean
      with SPARK_Mode => Off
    is
        frame : constant System.Address := Virtmem.P2Va (addr);
        head : System.Address;
    begin
        -- At most MAX_BUDDY_ORDER + 1 metadata reads; supports constituent
        -- frames of a DMA allocation without treating them as allocation heads.
        for size in Order loop
            head := To_Address (To_Integer (frame) and
              not (Integer_Address (blockSize (size)) - 1));
            if Buddy_Blocks.Matches (blockState (0, head),
                                     Buddy_Blocks.Allocated,
                                     Buddy_Blocks.Order (size))
            then
                return True;
            end if;
        end loop;
        return False;
    end containsAllocatedFrame;

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
        Post => To_Integer (blockStart'Result) <= To_Integer (addr)
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
        totalBytes : Storage_Count;
        numFrames  : Positive;
        physAddr   : Virtmem.PhysAddress;
    begin
        -- Firmware supplies an inclusive maximum, not a count of frames.
        if maxPFN > Unsigned_64 (Buddy_Bitmap.Frame_Number'Last) then
            raise AllocatorException with "Physical frame number exceeds x86-64 format";
        end if;
        bitmapLayout := Buddy_Bitmap.Make
          (Buddy_Bitmap.Frame_Number (maxPFN), bitmapLayout.Last_Order);
        totalBytes := Storage_Count (Buddy_Bitmap.Word_Count (bitmapLayout)) * 8;

        if totalBytes < Virtmem.FRAME_SIZE then
            numFrames := 1;
        else
            numFrames := Natural(
                (totalBytes + Virtmem.FRAME_SIZE - 1) / Virtmem.FRAME_SIZE);
        end if;

        BootAllocator.allocFrames(numFrames, physAddr);

        bitmapBase   := Virtmem.P2Va(physAddr);

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
        use type Buddy_Metadata.Byte_Count;
        -- allocBitmap already admitted the firmware maximum into a bounded
        -- frame type. All metadata tables must use that same inclusive extent.
        highest : constant Buddy_Metadata.Frame := Buddy_Metadata.Frame
          (Buddy_Bitmap.Highest_Frame (bitmapLayout));
        stateBytes : constant Storage_Count :=
          Storage_Count (Buddy_Metadata.Bytes (highest, Buddy_Metadata.Pin_State));
        ownerBytes : constant Storage_Count :=
          Storage_Count (Buddy_Metadata.Bytes (highest, Buddy_Metadata.Frame_Owners));
        blockBytes : constant Storage_Count :=
          Storage_Count (Buddy_Metadata.Bytes (highest, Buddy_Metadata.Block_State));
        statePages : constant Buddy_Metadata.Table_Size := Buddy_Metadata.Pages
          (highest, Buddy_Metadata.Pin_State, Buddy_Metadata.Page_Size (Virtmem.FRAME_SIZE));
        ownerPages : constant Buddy_Metadata.Table_Size := Buddy_Metadata.Pages
          (highest, Buddy_Metadata.Frame_Owners, Buddy_Metadata.Page_Size (Virtmem.FRAME_SIZE));
        blockPages : constant Buddy_Metadata.Table_Size := Buddy_Metadata.Pages
          (highest, Buddy_Metadata.Block_State, Buddy_Metadata.Page_Size (Virtmem.FRAME_SIZE));
        pinPhys    : Virtmem.PhysAddress;
        ownerPhys  : Virtmem.PhysAddress;
        blockPhys  : Virtmem.PhysAddress;
        ignore     : System.Address;
    begin
        -- Firmware determines the metadata extent. Validate before narrowing
        -- into AllocSize: kernel builds intentionally omit subtype checks.
        if statePages > Buddy_Metadata.Byte_Count (BootAllocator.AllocSize'Last)
          or else ownerPages > Buddy_Metadata.Byte_Count (BootAllocator.AllocSize'Last)
          or else blockPages > Buddy_Metadata.Byte_Count (BootAllocator.AllocSize'Last)
        then
            raise BootAllocator.OutOfMemoryException with "Buddy metadata exceeds boot allocation capacity";
        end if;
        BootAllocator.allocFrames (BootAllocator.AllocSize (statePages), pinPhys);
        BootAllocator.allocFrames (BootAllocator.AllocSize (ownerPages), ownerPhys);
        BootAllocator.allocFrames (BootAllocator.AllocSize (blockPages), blockPhys);
        pinStateBase := Virtmem.P2Va (pinPhys);
        frameOwnerBase := Virtmem.P2Va (ownerPhys);
        blockStateBase := Virtmem.P2Va (blockPhys);
        maxPinPFN := Unsigned_64 (highest);
        ignore := Util.memset (pinStateBase, 0, stateBytes);
        ignore := Util.memset (frameOwnerBase, 0, ownerBytes);
        -- Descriptor's all-zero representation is Reserved, order zero.
        ignore := Util.memset (blockStateBase, 0, blockBytes);

        print ("Frame pin state: ");
        print (Natural (stateBytes));
        println (" bytes");
    end allocPinState;

    -- The only raw-address admission boundary for bitmap operations. Layout
    -- geometry and indexing live in the same pure core used by host proofs.
    function bitmapBit (ord : Order; addr : System.Address) return Buddy_Bitmap.Count
      with SPARK_Mode => Off
    is
        pfn : constant Unsigned_64 := Unsigned_64 (Virtmem.vaddrToPFN (addr));
    begin
        if pfn > Unsigned_64 (Buddy_Bitmap.Highest_Frame (bitmapLayout)) then
            raise AllocatorException with "Frame outside buddy bitmap";
        end if;
        return Buddy_Bitmap.Locate
          (bitmapLayout, Buddy_Bitmap.Order (ord), Buddy_Bitmap.Frame_Number (pfn));
    end bitmapBit;

    ---------------------------------------------------------------------------
    -- toggleBit - Toggle the XOR bit for the buddy pair containing addr
    -- at the given order. Called on each alloc/free state transition.
    ---------------------------------------------------------------------------
    procedure toggleBit (ord : in Order; addr : in System.Address) with
        SPARK_Mode => Off
    is
        bitPos  : constant Buddy_Bitmap.Count := bitmapBit (ord, addr);
        wordIdx : constant Storage_Offset := Storage_Offset (Buddy_Bitmap.Word_Index (bitPos));
        bitIdx  : constant Natural := Buddy_Bitmap.Within_Word (bitPos);

        word : aliased Unsigned_64 with
            Import, Address => bitmapBase +
                wordIdx * 8;
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
        bitPos  : constant Buddy_Bitmap.Count := bitmapBit (ord, addr);
        wordIdx : constant Storage_Offset := Storage_Offset (Buddy_Bitmap.Word_Index (bitPos));
        bitIdx  : constant Natural := Buddy_Bitmap.Within_Word (bitPos);

        word : aliased Unsigned_64 with
            Import, Address => bitmapBase +
                wordIdx * 8;
    begin
        return (word and Shift_Left(Unsigned_64(1), bitIdx)) /= 0;
    end testBit;

    ---------------------------------------------------------------------------
    -- popFromFreeList
    ---------------------------------------------------------------------------
    procedure popFromFreeList (ord  : in Order;
                               addr : out System.Address) with
        SPARK_Mode => Off, -- physical-memory free-list overlays
        Pre     => freeLists(ord).numFreeBlocks > 0,
        Post    => freeLists(ord).numFreeBlocks =
                   freeLists(ord).numFreeBlocks'Old - 1
    is
        retBlock : aliased FreeBlock
            with Import, Volatile, Address => freeLists(ord).nextBlock;
    begin
        -- set output
        addr := freeLists(ord).nextBlock;
        moveBlock (ord, addr, Buddy_Blocks.Remove);

        linkNext:
        declare
            nextBlock : aliased FreeBlock
                with Import, Volatile, Address => retBlock.nextBlock;
        begin
            -- fwd link to next block in list (may be the head)
            List_Splices.Remove
              (freeLists (ord).nextBlock, nextBlock.prevBlock,
               getListAddress (ord), retBlock.nextBlock);
        end linkNext;

        freeLists (ord).numFreeBlocks := List_Splices.Removed
          (freeLists (ord).numFreeBlocks);

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
        SPARK_Mode => Off -- physical-memory free-list overlays
    is
        newBlock  : aliased FreeBlock with
            Import, Volatile, Address => newBlockAddr;

        nextBlock : aliased FreeBlock with
            Import, Volatile, Address => freeLists(ord).nextBlock;
    begin
        moveBlock (ord, newBlockAddr, Buddy_Blocks.Publish);
        List_Splices.Insert_Front
          (freeLists (ord).nextBlock, nextBlock.prevBlock,
           newBlock.prevBlock, newBlock.nextBlock,
           getListAddress (ord), newBlockAddr);
        newBlock.buddy              := getBuddy (ord, newBlockAddr);
        freeLists (ord).numFreeBlocks := List_Splices.Added
          (freeLists (ord).numFreeBlocks);
    end addToFreeList;

    ---------------------------------------------------------------------------
    -- splitBlock
    --
    -- Adds unused half (the upper half) of a block with address addr and order
    -- ord to freeLists(N-1).
    ---------------------------------------------------------------------------
    procedure splitBlock (ord : in Order; addr : in System.Address) with
        SPARK_Mode => Off, -- free-list and bitmap mutation
        Pre     => ord > 0,
        Post    => freeLists(ord - 1).numFreeBlocks =
                   freeLists(ord - 1).numFreeBlocks'Old + 1
    is
        parent : constant Buddy_Geometry.Block := geometryBlock (ord, addr);
        leftRange, rightRange : Buddy_Geometry.Block;
    begin
        Buddy_Geometry.Split (parent, leftRange, rightRange);
        declare
            rightHalfAddr : constant System.Address := blockAddress (rightRange);
            left : Buddy_Blocks.Descriptor with Import,
              Address => descriptorAddress (ord - 1, blockAddress (leftRange));
            right : Buddy_Blocks.Descriptor with Import,
              Address => descriptorAddress (ord - 1, rightHalfAddr);
            success : Boolean;
        begin
            Buddy_Blocks.Split (left, right, Buddy_Blocks.Order (ord), success);
            if not success then
                raise AllocatorException with "Invalid buddy split";
            end if;
            addToFreeList (ord - 1, rightHalfAddr);

            -- Toggle bitmap: right half transitions to free at ord-1.
            toggleBit (ord - 1, rightHalfAddr);
        end;
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
        SPARK_Mode => Off, -- physical-memory free-list overlays
        Pre  => freeLists(ord).numFreeBlocks > 0,
        Post => freeLists(ord).numFreeBlocks =
                freeLists(ord).numFreeBlocks'Old - 1
    is
        block : aliased FreeBlock with
            Import, Volatile, Address => addr;

    begin
        -- Validate membership before reading potentially live payload as links.
        moveBlock (ord, addr, Buddy_Blocks.Remove);
        linkNeighbors:
        declare
            prevAddr : constant System.Address := block.prevBlock;
            nextAddr : constant System.Address := block.nextBlock;
            prevBlock : aliased FreeBlock with
                Import, Volatile, Address => prevAddr;

            nextBlock : aliased FreeBlock with
                Import, Volatile, Address => nextAddr;
        begin
            List_Splices.Remove
              (prevBlock.nextBlock, nextBlock.prevBlock, prevAddr, nextAddr);
        end linkNeighbors;

        freeLists (ord).numFreeBlocks := List_Splices.Removed
          (freeLists (ord).numFreeBlocks);

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


    procedure freeLocked (ord : Order; addr : System.Address)
      with SPARK_Mode => Off;

    procedure admitBootBlock (ord : Order; addr : System.Address)
      with SPARK_Mode => Off
    is
        use type Buddy_Blocks.Block_Kind;
        frames : constant Natural := 2 ** Natural (ord);
        base : constant System.Address := descriptorAddress (ord, addr);
        type Descriptors is array (Natural range <>) of Buddy_Blocks.Descriptor
          with Component_Size => Buddy_Blocks.Descriptor_Bits;
        items : Descriptors (0 .. frames - 1) with Import, Address => base;
        success : Boolean;
    begin
        -- Validate the entire range before admitting any part of it. Overlap
        -- in firmware regions must never seed the same memory twice.
        for item of items loop
            if Buddy_Blocks.Kind (item) /= Buddy_Blocks.Reserved then
                raise AllocatorException with "Overlapping buddy boot admission";
            end if;
        end loop;
        for i in items'Range loop
            Buddy_Blocks.Admit (items (i), Buddy_Blocks.Order (ord),
                                i = items'First, success);
            if not success then
                raise AllocatorException with "Invalid buddy boot admission";
            end if;
        end loop;
        freeLocked (ord, addr);
    end admitBootBlock;

    ---------------------------------------------------------------------------
    -- setup
    ---------------------------------------------------------------------------
    procedure setup (Map : Firmware_Frames.Region_Array) with
        SPARK_Mode => Off -- allocation and physical-memory initialization
    is
        package FF renames Firmware_Frames;
        use type FF.Count;
        use type FF.Region_Kind;
        use type FF.Decision;
        use type Buddy_Boot_Admission.Admission_Source;
    begin
        Spinlocks.Initialize (lock, lockName'Access);
        -- make freeLists self-referential and empty to start
        for ord in Order'Range loop
            freeLists(ord).prevBlock := getListAddress (ord);
            freeLists(ord).nextBlock := getListAddress (ord);
            freeLists(ord).buddy     := System.Null_Address;
            freeLists(ord).numFreeBlocks := 0;
        end loop;

        -- Allocate XOR bitmap for safe buddy-pair coalesce checks.
        allocBitmap;
        -- Allocate lifetime metadata before releasing boot-managed frames to
        -- the buddy lists, so the metadata can never itself be allocated.
        allocPinState;

        -- Tile each usable span with aligned blocks; split only where a
        -- reservation/earlier owner or the boot bitmap requires finer detail.
        -- No discarded aligned edges and no second admission of duplicate RAM.
        for Owner in Map'Range loop
            if Map (Owner).Kind = FF.Usable then
                declare
                    Cursor : FF.Boundary := FF.Boundary'Max
                      (FF.First (Map (Owner).Pages),
                       FF.Count (Config.MIN_PHYS_ALLOC / Virtmem.FRAME_SIZE));
                    Limit : constant FF.Boundary := FF.Limit (Map (Owner).Pages);
                begin
                    while Cursor < Limit loop
                        declare
                            Ord : Order := Order (FF.Largest_Block
                              (Cursor, Limit - 1, FF.Block_Order (Order'Last)));
                            Pages : FF.Count := FF.Block_Pages (FF.Block_Order (Ord));
                            Action : FF.Decision;
                            Boot_Checked : constant Boolean :=
                              Buddy_Boot_Admission.Source_Of
                                (Buddy_Boot_Admission.Frame (Cursor),
                                 Buddy_Boot_Admission.Frame
                                   (BootAllocator.highestPFNAllocated)) =
                                Buddy_Boot_Admission.Boot_Bitmap;
                        begin
                            loop
                                Action := FF.Classify
                                  (Map, Owner, Cursor, Cursor + Pages - 1);
                                if Action = FF.Reject then
                                    exit;
                                elsif Action = FF.Split or else
                                  (Ord > 0 and then Boot_Checked)
                                then
                                    -- Split is impossible for a singleton by
                                    -- Classify's proved contract.
                                    Ord := Ord - 1;
                                    Pages := Pages / 2;
                                else
                                    if not Boot_Checked
                                      or else BootAllocator.isFree (Virtmem.PFN (Cursor))
                                    then
                                        admitBootBlock (Ord, Virtmem.P2Va
                                          (Virtmem.PFNToAddr (Virtmem.PFN (Cursor))));
                                    end if;
                                    exit;
                                end if;
                            end loop;
                            Cursor := Cursor + Pages;
                        end;
                    end loop;
                end;
            end if;
        end loop;

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

                moveBlock (ord, retBlock, Buddy_Blocks.Commit);
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
        -- Null is the allocation-failure sentinel, not a linear-map address.
        addr := (if vaddr = NO_BLOCK_AVAILABLE then 0 else Virtmem.V2P (vaddr));
    end allocFrame;

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
                -- XOR proposes a candidate; geometry validates the complete
                -- pair before list removal, and computes the merged address.
                declare
                    candidate : constant System.Address :=
                      getBuddy (curOrd, To_Address (freeAddr));
                    leftAddr : constant System.Address :=
                      (if candidate < To_Address (freeAddr) then candidate
                       else To_Address (freeAddr));
                    rightAddr : constant System.Address :=
                      (if candidate < To_Address (freeAddr) then To_Address (freeAddr)
                       else candidate);
                    leftRange : constant Buddy_Geometry.Block :=
                      geometryBlock (curOrd, leftAddr);
                    rightRange : constant Buddy_Geometry.Block :=
                      geometryBlock (curOrd, rightAddr);
                begin
                    if not Buddy_Geometry.Can_Merge (leftRange, rightRange) then
                        raise AllocatorException with "Invalid buddy merge geometry";
                    end if;
                    unlink (curOrd, candidate);
                    declare
                        left : Buddy_Blocks.Descriptor with Import,
                          Address => descriptorAddress (curOrd, leftAddr);
                        right : Buddy_Blocks.Descriptor with Import,
                          Address => descriptorAddress (curOrd, rightAddr);
                        success : Boolean;
                    begin
                        Buddy_Blocks.Merge (left, right,
                          Buddy_Blocks.Order (curOrd), success);
                        if not success then
                            raise AllocatorException with "Invalid buddy merge";
                        end if;
                    end;
                    freeAddr := To_Integer (blockAddress
                      (Buddy_Geometry.Merge (leftRange, rightRange)));
                end;

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
        if ord = 0 then
            -- Use the same pin-aware lifetime path for both public free APIs.
            declare
                checked : constant System.Address := descriptorAddress (ord, addr);
                pragma Unreferenced (checked);
            begin
                freeFrame (Virtmem.V2P (addr));
            end;
            return;
        end if;
        Spinlocks.enterCriticalSection (lock);
        -- Multi-frame DMA teardown must return every pin before freeing the
        -- containing allocation. Do not silently bypass per-frame lifetimes.
        declare
            base : constant System.Address := descriptorAddress (ord, addr);
            pragma Unreferenced (base);
            pfn : constant Storage_Offset := Storage_Offset (Virtmem.vaddrToPFN (addr));
            type Bytes is array (Natural range <>) of Unsigned_8;
            pins : Bytes (0 .. 2 ** Natural (ord) - 1) with Import,
              Address => pinStateBase + pfn;
            owners : Bytes (pins'Range) with Import,
              Address => frameOwnerBase + pfn;
        begin
            for i in pins'Range loop
                if pins (i) /= 0 or else owners (i) /= 0 then
                    raise AllocatorException with "Free of owned or pinned buddy block";
                end if;
            end loop;
        end;
        moveBlock (ord, addr, Buddy_Blocks.Release_Block);
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
        use type Frame_Pins.Release_Action;
    begin
        Spinlocks.enterCriticalSection (lock);
        -- Admission occurs before owner/pin mutations as well as list writes.
        moveBlock (0, Virtmem.P2Va (addr), Buddy_Blocks.Defer);
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
                lifetime : Frame_Pins.State;
                action : Frame_Pins.Release_Action;
            begin
                owner := 0;
                lifetime := Frame_Pins.Decode (state);
                Frame_Pins.Request_Free (lifetime, action);
                state := Frame_Pins.Encode (lifetime);
                if action = Frame_Pins.Keep_Frame then
                    Spinlocks.exitCriticalSection (lock);
                    return;
                end if;
            end;
        end if;

        moveBlock (0, Virtmem.P2Va (addr), Buddy_Blocks.Reclaim);
        freeLocked (0, Virtmem.P2Va (addr));
        Spinlocks.exitCriticalSection (lock);
    end freeFrame;


    procedure pinOwnedFrame
      (addr : Virtmem.PhysAddress; owner : Unsigned_8; success : out Boolean)
      with SPARK_Mode => Off
    is
        pfn : constant Unsigned_64 := Unsigned_64 (Virtmem.addrToPFN (addr));
    begin
        success := False;
        if owner = 0 or else pinStateBase = System.Null_Address or else
           frameOwnerBase = System.Null_Address or else pfn > maxPinPFN or else
           addr mod Virtmem.FRAME_SIZE /= 0
        then
            return;
        end if;
        Spinlocks.enterCriticalSection (lock);
        declare
            currentOwner : Unsigned_8 with Import, Volatile,
                Address => frameOwnerBase + Storage_Offset (pfn);
            raw : Unsigned_8 with Import, Volatile,
                Address => pinStateBase + Storage_Offset (pfn);
            lifetime : Frame_Pins.State := Frame_Pins.Decode (raw);
        begin
            if currentOwner = owner then
                Frame_Pins.Pin (lifetime, success);
                raw := Frame_Pins.Encode (lifetime);
            end if;
        end;
        Spinlocks.exitCriticalSection (lock);
    end pinOwnedFrame;

    procedure unpinFrame
      (addr    : in Virtmem.PhysAddress;
       success : out Boolean) with
        SPARK_Mode => Off
    is
        pfn : constant Unsigned_64 := Unsigned_64 (Virtmem.addrToPFN (addr));
        use type Frame_Pins.Release_Action;
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
            lifetime : Frame_Pins.State := Frame_Pins.Decode (state);
            action : Frame_Pins.Release_Action;
        begin
            Frame_Pins.Unpin (lifetime, success, action);
            state := Frame_Pins.Encode (lifetime);
            if action = Frame_Pins.Reclaim_Frame then
                moveBlock (0, Virtmem.P2Va (addr), Buddy_Blocks.Reclaim);
                freeLocked (0, Virtmem.P2Va (addr));
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
            if currentOwner = 0 and then containsAllocatedFrame (addr) then
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
