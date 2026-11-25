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
with Free_Link_Codes;
with Pool_Region_Maps;
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
    -- Frame pfn's entry in the owner table (Buddy_Metadata.Frame_Owners).
    function ownerAddress (pfn : Unsigned_64) return System.Address is
      (frameOwnerBase + Storage_Offset (pfn) * Buddy_Metadata.Owner_Bytes)
      with Inline, Pre => pfn <= Unsigned_64 (Virtmem.PFN'Last);
    -- Five 9-bit levels cover 45 PFN bits (more than the physical address
    -- geometry). Only the root is static; descendants are allocated on demand.
    -- Nodes never move or disappear, so a prepared slot survives lock drops.
    type Charge_Node is array (Natural range 0 .. 511) of Unsigned_64;
    chargeRoot : aliased Charge_Node := (others => 0);
    chargeRefund : Charge_Refund_Handler := null;
    -- A dense leaf table costs about 1/512 of represented RAM; leave room for
    -- directory levels while bounding adversarial sparse growth separately.
    Charge_Metadata_Share_Divisor : constant Unsigned_64 := 256;
    chargeMetadataPages : Unsigned_64 := 0;
    maxPinPFN       : Unsigned_64 := 0;
    blockStateBase : System.Address := System.Null_Address;
    -- Free-list links, one Free_Link_Codes.Links per frame (KERN-014): a
    -- free block's next and previous live here, never in the block, so a
    -- frame a process gave back carries no kernel addresses and a frame
    -- outside the kernel's map can still be listed.
    linkBase : System.Address := System.Null_Address;
    ---------------------------------------------------------------------------
    -- The process pool (KERN-014 slice 4): 2 MiB regions taken from the
    -- kernel's lists as order-9 blocks and detached from the kernel's linear
    -- map (Process_Pool does the detach and the TLB round). Process frames
    -- come from here; a frame a process frees comes back here, never to the
    -- kernel's lists. One record per region of RAM, in use only while the
    -- region is joined.
    ---------------------------------------------------------------------------
    Region_Frames : constant := Pool_Region_Maps.Frames_Per_Region;
    -- A pool region is one block of this order (2 MiB).
    Pool_Region_Order : constant Order := Order (Pool_Region_Maps.Max_Run_Order);
    No_Region : constant Unsigned_32 := Unsigned_32'Last;
    type Pool_Record is record
        Map         : Pool_Region_Maps.Region_Map;
        Free_Frames : Natural range 0 .. Region_Frames := 0;
        -- How many of Map.Clean are set: a region without enough is skipped
        -- when a clean run is wanted.
        Clean_Frames : Natural range 0 .. Region_Frames := 0;
        Hint        : Pool_Region_Maps.Frame_Offset := 0;
        Joined      : Boolean := False;
        Listed      : Boolean := False;   -- on the with-free list
        Next        : Unsigned_32 := No_Region;
        Saved       : Virtmem.PageTableEntry := Virtmem.EMPTY_PTE;
    end record;
    poolBase : System.Address := System.Null_Address;
    maxRegion : Unsigned_64 := 0;
    poolFreeList : Unsigned_32 := No_Region;
    poolRegions : Natural := 0;
    poolReclaim : Pool_Reclaim_Handler := null;
    reclaiming  : Boolean := False with Atomic;

    function poolRecordAddress (region : Unsigned_64) return System.Address is
      (poolBase + Storage_Offset (region) * (Pool_Record'Size / System.Storage_Unit))
      with Inline;

    pragma Compile_Time_Error
      (Free_Link_Codes.Links'Size /= Buddy_Metadata.Link_Bytes * System.Storage_Unit
       or else Natural (Order'Last) >= Free_Link_Codes.List_Count,
       "Free-list link entries must match their metadata slots and name every order");

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
            not (Integer_Address (blockSize (ord)) - 1);
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
        linkPages : constant Buddy_Metadata.Table_Size := Buddy_Metadata.Pages
          (highest, Buddy_Metadata.Free_Links, Buddy_Metadata.Page_Size (Virtmem.FRAME_SIZE));
        linkPhys   : Virtmem.PhysAddress;
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
          or else linkPages > Buddy_Metadata.Byte_Count (BootAllocator.AllocSize'Last)
        then
            raise BootAllocator.OutOfMemoryException with "Buddy metadata exceeds boot allocation capacity";
        end if;
        -- Links name frames in 32 bits (Free_Link_Codes): 16 TiB, the
        -- linear map's own limit.
        if Unsigned_64 (highest) > Free_Link_Codes.Frame_Number'Last then
            raise AllocatorException with "Physical memory beyond free-list link range";
        end if;
        BootAllocator.allocFrames (BootAllocator.AllocSize (statePages), pinPhys);
        BootAllocator.allocFrames (BootAllocator.AllocSize (ownerPages), ownerPhys);
        BootAllocator.allocFrames (BootAllocator.AllocSize (blockPages), blockPhys);
        -- Not cleared: an entry means something only while its frame heads
        -- a listed block, and listing writes both links.
        BootAllocator.allocFrames (BootAllocator.AllocSize (linkPages), linkPhys);
        pinStateBase := Virtmem.P2Va (pinPhys);
        linkBase := Virtmem.P2Va (linkPhys);
        -- One process-pool record per 2 MiB region (KERN-014).
        maxRegion := Unsigned_64 (highest) / Region_Frames;
        declare
            poolBytes : constant Unsigned_64 :=
              (maxRegion + 1) * Unsigned_64 (Pool_Record'Size / System.Storage_Unit);
            poolPages : constant Unsigned_64 :=
              (poolBytes + Unsigned_64 (Virtmem.FRAME_SIZE) - 1) / Unsigned_64 (Virtmem.FRAME_SIZE);
            poolPhys : Virtmem.PhysAddress;
        begin
            if poolPages > Unsigned_64 (BootAllocator.AllocSize'Last) then
                raise BootAllocator.OutOfMemoryException with "Process pool records exceed boot allocation capacity";
            end if;
            BootAllocator.allocFrames (BootAllocator.AllocSize (poolPages), poolPhys);
            poolBase := Virtmem.P2Va (poolPhys);
            for r in 0 .. maxRegion loop
                declare
                    item : Pool_Record with Import, Address => poolRecordAddress (r);
                begin
                    for f in Pool_Region_Maps.Frame_Offset loop
                        item.Map.Free (f) := False;
                        item.Map.Clean (f) := False;
                    end loop;
                    item.Free_Frames := 0;
                    item.Clean_Frames := 0;
                    item.Hint := 0;
                    item.Joined := False;
                    item.Listed := False;
                    item.Next := No_Region;
                    item.Saved := Virtmem.EMPTY_PTE;
                end;
            end loop;
        end;
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
    -- Out-of-band free-list links (KERN-014). A list reference is still the
    -- block's linear address, or the order's list head (getListAddress); only
    -- where a block's links are stored changed. The list heads keep their
    -- own fields in freeLists.
    ---------------------------------------------------------------------------
    List_Stride : constant Integer_Address :=
      Integer_Address (FreeListArray'Component_Size / System.Storage_Unit);

    -- The order whose list head is addr, or -1 when addr is a block.
    function headOrder (addr : System.Address) return Integer
      with SPARK_Mode => Off, Inline
    is
        first : constant Integer_Address := To_Integer (getListAddress (Order'First));
        here : constant Integer_Address := To_Integer (addr);
    begin
        if here >= first and then here - first < List_Stride * (Integer_Address (Order'Last) + 1)
          and then (here - first) mod List_Stride = 0
        then
            return Integer ((here - first) / List_Stride);
        end if;
        return -1;
    end headOrder;

    function linkAddress (addr : System.Address) return System.Address
      with SPARK_Mode => Off, Inline
    is
        pfn : Unsigned_64;
    begin
        if To_Integer (addr) < Virtmem.LINEAR_BASE then
            raise AllocatorException with "Invalid free-list block";
        end if;
        pfn := Unsigned_64 ((To_Integer (addr) - Virtmem.LINEAR_BASE) /
                            Integer_Address (Virtmem.FRAME_SIZE));
        if linkBase = System.Null_Address or else pfn > maxPinPFN then
            raise AllocatorException with "Free-list block outside allocator metadata";
        end if;
        return To_Address (Integer_Address (Buddy_Metadata.Address_Of
          (Unsigned_64 (To_Integer (linkBase)), Buddy_Metadata.Frame (pfn),
           Buddy_Metadata.Free_Links)));
    end linkAddress;

    function encode (addr : System.Address) return Free_Link_Codes.Code
      with SPARK_Mode => Off, Inline
    is
        ord : constant Integer := headOrder (addr);
    begin
        if ord >= 0 then
            return Free_Link_Codes.Of_List (Free_Link_Codes.List_Index (ord));
        end if;
        return Free_Link_Codes.Of_Frame (Free_Link_Codes.Frame_Number
          ((To_Integer (addr) - Virtmem.LINEAR_BASE) / Integer_Address (Virtmem.FRAME_SIZE)));
    end encode;

    function decode (item : Free_Link_Codes.Code) return System.Address
      with SPARK_Mode => Off, Inline
    is
    begin
        if Free_Link_Codes.Is_List (item) then
            return getListAddress (Order (Free_Link_Codes.List_Of (item)));
        end if;
        return Virtmem.P2Va (Virtmem.PhysAddress (Free_Link_Codes.Frame_Of (item)) *
                             Virtmem.PhysAddress (Virtmem.FRAME_SIZE));
    end decode;

    function nextOf (addr : System.Address) return System.Address
      with SPARK_Mode => Off, Inline
    is
        ord : constant Integer := headOrder (addr);
    begin
        if ord >= 0 then
            return freeLists (Order (ord)).nextBlock;
        end if;
        declare
            links : Free_Link_Codes.Links with Import, Volatile, Address => linkAddress (addr);
        begin
            return decode (links.Next);
        end;
    end nextOf;

    function prevOf (addr : System.Address) return System.Address
      with SPARK_Mode => Off, Inline
    is
        ord : constant Integer := headOrder (addr);
    begin
        if ord >= 0 then
            return freeLists (Order (ord)).prevBlock;
        end if;
        declare
            links : Free_Link_Codes.Links with Import, Volatile, Address => linkAddress (addr);
        begin
            return decode (links.Previous);
        end;
    end prevOf;

    procedure setNext (addr, value : System.Address) with SPARK_Mode => Off, Inline is
        ord : constant Integer := headOrder (addr);
    begin
        if ord >= 0 then
            freeLists (Order (ord)).nextBlock := value;
            return;
        end if;
        declare
            links : Free_Link_Codes.Links with Import, Volatile, Address => linkAddress (addr);
        begin
            links.Next := encode (value);
        end;
    end setNext;

    procedure setPrev (addr, value : System.Address) with SPARK_Mode => Off, Inline is
        ord : constant Integer := headOrder (addr);
    begin
        if ord >= 0 then
            freeLists (Order (ord)).prevBlock := value;
            return;
        end if;
        declare
            links : Free_Link_Codes.Links with Import, Volatile, Address => linkAddress (addr);
        begin
            links.Previous := encode (value);
        end;
    end setPrev;

    ---------------------------------------------------------------------------
    -- popFromFreeList
    ---------------------------------------------------------------------------
    procedure popFromFreeList (ord  : in Order;
                               addr : out System.Address) with
        SPARK_Mode => Off, -- out-of-band link table overlays
        Pre     => freeLists(ord).numFreeBlocks > 0,
        Post    => freeLists(ord).numFreeBlocks =
                   freeLists(ord).numFreeBlocks'Old - 1
    is
        following : System.Address;
        headNext, followingPrevious : System.Address;
    begin
        -- set output
        addr := freeLists(ord).nextBlock;
        moveBlock (ord, addr, Buddy_Blocks.Remove);
        following := nextOf (addr);
        -- fwd link to next block in list (may be the head)
        List_Splices.Remove
          (headNext, followingPrevious, getListAddress (ord), following);
        freeLists (ord).nextBlock := headNext;
        setPrev (following, followingPrevious);

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
        SPARK_Mode => Off -- out-of-band link table overlays
    is
        first : constant System.Address := freeLists (ord).nextBlock;
        headNext : System.Address := first;
        firstPrevious, newPrevious, newNext : System.Address;
    begin
        moveBlock (ord, newBlockAddr, Buddy_Blocks.Publish);
        List_Splices.Insert_Front
          (headNext, firstPrevious, newPrevious, newNext,
           getListAddress (ord), newBlockAddr);
        freeLists (ord).nextBlock := headNext;
        setPrev (first, firstPrevious);
        setPrev (newBlockAddr, newPrevious);
        setNext (newBlockAddr, newNext);
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
        SPARK_Mode => Off, -- out-of-band link table overlays
        Pre  => freeLists(ord).numFreeBlocks > 0,
        Post => freeLists(ord).numFreeBlocks =
                freeLists(ord).numFreeBlocks'Old - 1
    is
        previous, following : System.Address;
        previousNext, followingPrevious : System.Address;
    begin
        -- Validate membership before reading its links.
        moveBlock (ord, addr, Buddy_Blocks.Remove);
        previous := prevOf (addr);
        following := nextOf (addr);
        List_Splices.Remove
          (previousNext, followingPrevious, previous, following);
        setNext (previous, previousNext);
        setPrev (following, followingPrevious);

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
                pragma Loop_Invariant (blockSize (ord) < allocSize);
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
    -- adoptFrames
    ---------------------------------------------------------------------------
    procedure adoptFrames (first : Virtmem.PhysAddress; frames : Positive) with
        SPARK_Mode => Off -- lock calls and physical admission
    is
        cursor : Unsigned_64;
        limit  : Unsigned_64;
        ord    : Order;
    begin
        -- Validate before any change, including in the -gnatp kernel.
        if first mod Virtmem.FRAME_SIZE /= 0 or else
          first < Config.MIN_PHYS_ALLOC or else
          Unsigned_64 (first / Virtmem.FRAME_SIZE) + Unsigned_64 (frames) - 1 > maxPinPFN
        then
            raise AllocatorException with "Adopted frames outside allocator metadata";
        end if;
        cursor := Unsigned_64 (first / Virtmem.FRAME_SIZE);
        limit := cursor + Unsigned_64 (frames);
        Spinlocks.enterCriticalSection (lock);
        while cursor < limit loop
            -- The largest block aligned at the cursor that fits the span.
            ord := Order'Last;
            while ord > 0 and then
              (cursor mod 2 ** Natural (ord) /= 0 or else
               2 ** Natural (ord) > limit - cursor)
            loop
                ord := ord - 1;
            end loop;
            admitBootBlock (ord, Virtmem.P2Va
              (Virtmem.PFNToAddr (Virtmem.PFN (cursor))));
            totalManagedBytes := totalManagedBytes + blockSize (ord);
            cursor := cursor + 2 ** Natural (ord);
        end loop;
        Spinlocks.exitCriticalSection (lock);
    end adoptFrames;

    ---------------------------------------------------------------------------
    -- alloc
    ---------------------------------------------------------------------------
    -- The allocation; Zero_It says whether to zero the block here, through
    -- the linear map (kernel blocks), or leave it to the caller (process
    -- frames are zeroed through the kernel window, KERN-014).
    procedure allocBlock (ord : in Order; addr : out System.Address; Zero_It : Boolean;
                          May_Reclaim : Boolean := True)
      with SPARK_Mode => Off;

    procedure alloc (ord : in Order; addr : out System.Address) with
        SPARK_Mode => Off
    is
    begin
        allocBlock (ord, addr, Zero_It => True);
    end alloc;

    procedure allocBlock (ord : in Order; addr : out System.Address; Zero_It : Boolean;
                          May_Reclaim : Boolean := True) with
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
                if Zero_It then
                    declare
                        ignore : System.Address;
                    begin
                        ignore := Util.memset (addr, 0, blockSize (ord));
                    end;
                end if;
                return;
            end if;

            exit findLoop when curOrd = Order'Last;
            curOrd := curOrd + 1;
        end loop findLoop;

        -- no blocks found that can satisfy the request
        addr := NO_BLOCK_AVAILABLE;
        Spinlocks.exitCriticalSection (lock);
        -- The kernel's lists are empty: wholly free process-pool regions go
        -- back (KERN-014), then one more try. Not for the pool's own growth.
        if May_Reclaim and then poolReclaim /= null and then not reclaiming then
            declare
                released : Boolean;
            begin
                reclaiming := True;
                -- A region is an order-9 block: what a larger order needs,
                -- if the regions are neighbours; one for anything smaller.
                poolReclaim
                  ((if ord > Pool_Region_Order then 2 ** Natural (ord - Pool_Region_Order) else 1),
                   released);
                reclaiming := False;
                if released then
                    allocBlock (ord, addr, Zero_It, May_Reclaim => False);
                end if;
            end;
        end if;
    end allocBlock;

    procedure allocBelow
      (ord : Order; ceiling : Unsigned_64; addr : out System.Address) with
        SPARK_Mode => Off -- serialized physical free-list overlays
    is
        Candidate : System.Address;
        Physical : Unsigned_64;
        Needed : constant Unsigned_64 := Unsigned_64 (blockSize (ord));
        Split_Order : Order;
        Ignored : System.Address;
    begin
        addr := NO_BLOCK_AVAILABLE;
        if ceiling < Needed then return; end if;
        Spinlocks.enterCriticalSection (lock);
        for Current in ord .. Order'Last loop
            Candidate := freeLists (Current).nextBlock;
            for Visit in 1 .. freeLists (Current).numFreeBlocks loop
                if Candidate = getListAddress (Current) then
                    raise AllocatorException with "DMA free-list count mismatch";
                end if;
                Physical := Unsigned_64 (Virtmem.V2P (Candidate));
                if Physical <= ceiling - Needed then
                    unlink (Current, Candidate);
                    if Current < Order'Last then toggleBit (Current, Candidate); end if;
                    Split_Order := Current;
                    while Split_Order > ord loop
                        splitBlock (Split_Order, Candidate);
                        Split_Order := Split_Order - 1;
                    end loop;
                    moveBlock (ord, Candidate, Buddy_Blocks.Commit);
                    addr := Candidate;
                    Spinlocks.exitCriticalSection (lock);
                    Ignored := Util.memset (addr, 0, blockSize (ord));
                    return;
                end if;
                Candidate := nextOf (Candidate);
            end loop;
        end loop;
        Spinlocks.exitCriticalSection (lock);
    end allocBelow;

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
    -- Process pool (KERN-014 slice 4). Callers of the pool* helpers hold lock.
    ---------------------------------------------------------------------------
    function inPool (pfn : Unsigned_64) return Boolean with SPARK_Mode => Off is
    begin
        if poolBase = System.Null_Address or else pfn / Region_Frames > maxRegion then
            return False;
        end if;
        declare
            item : Pool_Record with Import, Address => poolRecordAddress (pfn / Region_Frames);
        begin
            return item.Joined;
        end;
    end inPool;

    procedure poolGive (pfn : Unsigned_64; ord : Order) with SPARK_Mode => Off is
        region : constant Unsigned_64 := pfn / Region_Frames;
        item : Pool_Record with Import, Address => poolRecordAddress (region);
        first : constant Pool_Region_Maps.Frame_Offset :=
          Pool_Region_Maps.Frame_Offset (pfn mod Region_Frames);
        frames : constant Natural := 2 ** Natural (ord);
    begin
        if not item.Joined or else ord > Pool_Region_Maps.Max_Run_Order
          or else first mod frames /= 0
        then
            raise AllocatorException with "Invalid process-pool free";
        end if;
        for i in 0 .. frames - 1 loop
            if item.Map.Free (first + i) then
                raise AllocatorException with "Process-pool frame freed twice";
            end if;
        end loop;
        Pool_Region_Maps.Give (item.Map, first, Pool_Region_Maps.Run_Order (ord));
        item.Free_Frames := item.Free_Frames + frames;
        poolFreeFrameCount := poolFreeFrameCount + frames;
        if not item.Listed then
            item.Listed := True;
            item.Next := poolFreeList;
            poolFreeList := Unsigned_32 (region);
        end if;
    end poolGive;

    procedure takeRegion
      (base : out Virtmem.PhysAddress; ceiling : Interfaces.Unsigned_64 := 0)
      with SPARK_Mode => Off
    is
        vaddr : System.Address;
    begin
        -- Never by giving pool regions back: that is what the pool grows from.
        if ceiling = 0 then
            allocBlock (9, vaddr, Zero_It => True, May_Reclaim => False);
        else
            allocBelow (9, ceiling, vaddr);   -- zeroes, as alloc does
        end if;
        base := (if vaddr = NO_BLOCK_AVAILABLE then 0 else Virtmem.V2P (vaddr));
    end takeRegion;

    procedure joinPool (base : Virtmem.PhysAddress; Saved : Virtmem.PageTableEntry)
      with SPARK_Mode => Off
    is
        region : constant Unsigned_64 := Unsigned_64 (Virtmem.addrToPFN (base)) / Region_Frames;
        item : Pool_Record with Import, Address => poolRecordAddress (region);
    begin
        if base mod Virtmem.BIG_FRAME_SIZE /= 0 or else region > maxRegion then
            raise AllocatorException with "Invalid process-pool region";
        end if;
        Spinlocks.enterCriticalSection (lock);
        if item.Joined then
            Spinlocks.exitCriticalSection (lock);
            raise AllocatorException with "Process-pool region joined twice";
        end if;
        -- Field by field: the record is 1 KiB, too big for a stack temporary.
        for f in Pool_Region_Maps.Frame_Offset loop
            item.Map.Free (f) := True;
            item.Map.Clean (f) := True;
        end loop;
        item.Free_Frames := Region_Frames;
        item.Clean_Frames := Region_Frames;
        item.Hint := 0;
        item.Joined := True;
        item.Listed := True;
        item.Next := poolFreeList;
        item.Saved := Saved;
        poolFreeList := Unsigned_32 (region);
        poolFreeFrameCount := poolFreeFrameCount + Region_Frames;
        poolCleanFrameCount := poolCleanFrameCount + Region_Frames;
        poolRegions := poolRegions + 1;
        Spinlocks.exitCriticalSection (lock);
    end joinPool;

    procedure allocFromPool
      (ord : Order; ceiling : Interfaces.Unsigned_64; onlyClean : Boolean;
       base : out Virtmem.PhysAddress; clean : out Boolean) with SPARK_Mode => Off
    is
        frames : constant Natural := 2 ** Natural (ord);
        previous : Unsigned_32 := No_Region;
        region : Unsigned_32;
        first : Pool_Region_Maps.Frame_Offset;
        found : Boolean;
    begin
        base := 0;
        clean := False;
        Spinlocks.enterCriticalSection (lock);
        region := poolFreeList;
        while region /= No_Region loop
            declare
                item : Pool_Record with Import, Address => poolRecordAddress (Unsigned_64 (region));
                regionBase : constant Unsigned_64 :=
                  Unsigned_64 (region) * Region_Frames * Unsigned_64 (Virtmem.FRAME_SIZE);
                following : constant Unsigned_32 := item.Next;
            begin
                if item.Free_Frames = 0 then
                    -- Drop full regions from the list as they are met.
                    item.Listed := False;
                    item.Next := No_Region;
                    if previous = No_Region then
                        poolFreeList := following;
                    else
                        declare
                            before : Pool_Record with Import,
                              Address => poolRecordAddress (Unsigned_64 (previous));
                        begin
                            before.Next := following;
                        end;
                    end if;
                else
                    if item.Free_Frames >= frames
                      and then (not onlyClean or else item.Clean_Frames >= frames)
                    then
                        Pool_Region_Maps.Find
                          ((if onlyClean then item.Map.Clean else item.Map.Free),
                           Pool_Region_Maps.Run_Order (ord), item.Hint, first, found);
                        if found and then
                          (ceiling = 0 or else regionBase + Unsigned_64 (first + frames) *
                             Unsigned_64 (Virtmem.FRAME_SIZE) <= ceiling)
                        then
                            declare
                                taken_clean : Natural := 0;
                            begin
                                for i in 0 .. frames - 1 loop
                                    if item.Map.Clean (first + i) then
                                        taken_clean := taken_clean + 1;
                                    end if;
                                end loop;
                                clean := taken_clean = frames;
                                Pool_Region_Maps.Take (item.Map, first, Pool_Region_Maps.Run_Order (ord));
                                item.Clean_Frames := item.Clean_Frames - taken_clean;
                                poolCleanFrameCount := poolCleanFrameCount - taken_clean;
                            end;
                            item.Free_Frames := item.Free_Frames - frames;
                            poolFreeFrameCount := poolFreeFrameCount - frames;
                            item.Hint := (first + frames) mod Region_Frames;
                            base := Virtmem.PhysAddress (regionBase) +
                              Virtmem.PhysAddress (first) * Virtmem.PhysAddress (Virtmem.FRAME_SIZE);
                            Spinlocks.exitCriticalSection (lock);
                            return;
                        end if;
                    end if;
                    previous := region;
                end if;
                region := following;
            end;
        end loop;
        Spinlocks.exitCriticalSection (lock);
    end allocFromPool;

    procedure leavePool
      (base : out Virtmem.PhysAddress; Saved : out Virtmem.PageTableEntry;
       found : out Boolean) with SPARK_Mode => Off
    is
        previous : Unsigned_32 := No_Region;
        region : Unsigned_32;
    begin
        base := 0;
        Saved := Virtmem.EMPTY_PTE;
        found := False;
        Spinlocks.enterCriticalSection (lock);
        region := poolFreeList;
        while region /= No_Region loop
            declare
                item : Pool_Record with Import, Address => poolRecordAddress (Unsigned_64 (region));
                following : constant Unsigned_32 := item.Next;
            begin
                if item.Free_Frames = Region_Frames then
                    if previous = No_Region then
                        poolFreeList := following;
                    else
                        declare
                            before : Pool_Record with Import,
                              Address => poolRecordAddress (Unsigned_64 (previous));
                        begin
                            before.Next := following;
                        end;
                    end if;
                    item.Joined := False;
                    item.Listed := False;
                    item.Next := No_Region;
                    poolFreeFrameCount := poolFreeFrameCount - Region_Frames;
                    poolCleanFrameCount := poolCleanFrameCount - item.Clean_Frames;
                    item.Clean_Frames := 0;
                    poolRegions := poolRegions - 1;
                    base := Virtmem.PhysAddress (Unsigned_64 (region) * Region_Frames) *
                      Virtmem.PhysAddress (Virtmem.FRAME_SIZE);
                    Saved := item.Saved;
                    found := True;
                    Spinlocks.exitCriticalSection (lock);
                    return;
                end if;
                previous := region;
                region := following;
            end;
        end loop;
        Spinlocks.exitCriticalSection (lock);
    end leavePool;

    procedure takeDirty (Frames : out Frame_List; Count : out Natural) with SPARK_Mode => Off is
        region : Unsigned_32;
    begin
        Frames := (others => 0);
        Count := 0;
        Spinlocks.enterCriticalSection (lock);
        region := poolFreeList;
        while region /= No_Region and then Count < Frames'Length loop
            declare
                item : Pool_Record with Import, Address => poolRecordAddress (Unsigned_64 (region));
            begin
                for f in Pool_Region_Maps.Frame_Offset loop
                    exit when Count = Frames'Length;
                    if item.Map.Free (f) and then not item.Map.Clean (f) then
                        Pool_Region_Maps.Take (item.Map, f, 0);
                        item.Free_Frames := item.Free_Frames - 1;
                        poolFreeFrameCount := poolFreeFrameCount - 1;
                        Count := Count + 1;
                        Frames (Frames'First + Count - 1) :=
                          (Virtmem.PhysAddress (region) * Region_Frames + Virtmem.PhysAddress (f)) *
                          Virtmem.PhysAddress (Virtmem.FRAME_SIZE);
                    end if;
                end loop;
                region := item.Next;
            end;
        end loop;
        Spinlocks.exitCriticalSection (lock);
    end takeDirty;

    procedure returnClean (frame : Virtmem.PhysAddress) with SPARK_Mode => Off is
        pfn : constant Unsigned_64 := Unsigned_64 (Virtmem.addrToPFN (frame));
        offset : constant Pool_Region_Maps.Frame_Offset :=
          Pool_Region_Maps.Frame_Offset (pfn mod Region_Frames);
    begin
        Spinlocks.enterCriticalSection (lock);
        poolGive (pfn, 0);
        declare
            item : Pool_Record with Import, Address => poolRecordAddress (pfn / Region_Frames);
        begin
            Pool_Region_Maps.Mark_Clean (item.Map, offset);
            item.Clean_Frames := item.Clean_Frames + 1;
            poolCleanFrameCount := poolCleanFrameCount + 1;
        end;
        Spinlocks.exitCriticalSection (lock);
    end returnClean;

    function poolCleanFrames return Natural is
      (poolCleanFrameCount) with SPARK_Mode => Off;

    function poolFrameClean (addr : Virtmem.PhysAddress) return Boolean with SPARK_Mode => Off is
        pfn : constant Unsigned_64 := Unsigned_64 (Virtmem.addrToPFN (addr));
        item : Pool_Record with Import, Address => poolRecordAddress (pfn / Region_Frames);
    begin
        return item.Map.Clean (Pool_Region_Maps.Frame_Offset (pfn mod Region_Frames));
    end poolFrameClean;

    procedure installPoolReclaimHandler (Handler : not null Pool_Reclaim_Handler)
      with SPARK_Mode => Off is
    begin
        poolReclaim := Handler;
    end installPoolReclaimHandler;

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

    procedure installChargeRefundHandler
      (Handler : not null Charge_Refund_Handler; Success : out Boolean)
      with SPARK_Mode => Off
    is
    begin
        Spinlocks.enterCriticalSection (lock);
        Success := chargeRefund = null;
        if Success then
            chargeRefund := Handler;
        end if;
        Spinlocks.exitCriticalSection (lock);
    end installChargeRefundHandler;

    -- Read only under buddy lock. Missing nodes mean no charge.
    function chargeSlot (pfn : Unsigned_64) return System.Address
      with SPARK_Mode => Off
    is
        node : System.Address := chargeRoot'Address;
    begin
        for level in reverse 1 .. 4 loop
            declare
                entries : Charge_Node with Import, Address => node;
                child : constant Unsigned_64 := entries
                  (Natural (Shift_Right (pfn, level * 9) and 511));
            begin
                if child = 0 then
                    return System.Null_Address;
                end if;
                node := To_Address (Integer_Address (child));
            end;
        end loop;
        return node + Storage_Offset ((pfn and 511) * 8);
    end chargeSlot;

    function chargeMetadataLimit return Unsigned_64 with SPARK_Mode => Off is
    begin
        return Unsigned_64 (totalManagedBytes) /
          Unsigned_64 (Virtmem.FRAME_SIZE) / Charge_Metadata_Share_Divisor;
    end chargeMetadataLimit;

    procedure chargeMetadataUsage (Pages, Limit : out Unsigned_64)
      with SPARK_Mode => Off is
    begin
        Spinlocks.enterCriticalSection (lock);
        Pages := chargeMetadataPages;
        Limit := chargeMetadataLimit;
        Spinlocks.exitCriticalSection (lock);
    end chargeMetadataUsage;

    -- Caller must keep the allocation alive and unpublished. Allocate at
    -- most four pages, never while holding buddy lock. Concurrent publishers
    -- converge on the same node; losing candidates are returned immediately.
    function prepareChargeSlot (pfn : Unsigned_64) return System.Address
      with SPARK_Mode => Off
    is
        node : System.Address := chargeRoot'Address;
        candidate : System.Address;
        ignore : System.Address;
    begin
        for level in reverse 1 .. 4 loop
            declare
                entries : Charge_Node with Import, Address => node;
                index : constant Natural := Natural
                  (Shift_Right (pfn, level * 9) and 511);
                child : Unsigned_64;
            begin
                Spinlocks.enterCriticalSection (lock);
                child := entries (index);
                if child = 0 then
                    if chargeMetadataPages >= chargeMetadataLimit then
                        Spinlocks.exitCriticalSection (lock);
                        return System.Null_Address;
                    end if;
                    chargeMetadataPages := chargeMetadataPages + 1;
                end if;
                Spinlocks.exitCriticalSection (lock);
                if child = 0 then
                    alloc (0, candidate);
                    if candidate = NO_BLOCK_AVAILABLE then
                        Spinlocks.enterCriticalSection (lock);
                        chargeMetadataPages := chargeMetadataPages - 1;
                        Spinlocks.exitCriticalSection (lock);
                        return System.Null_Address;
                    end if;
                    ignore := Util.memset (candidate, 0, 4096);
                    Spinlocks.enterCriticalSection (lock);
                    if entries (index) = 0 then
                        entries (index) := Unsigned_64 (To_Integer (candidate));
                        candidate := System.Null_Address;
                    end if;
                    child := entries (index);
                    Spinlocks.exitCriticalSection (lock);
                    if candidate /= System.Null_Address then
                        free (0, candidate);
                        Spinlocks.enterCriticalSection (lock);
                        chargeMetadataPages := chargeMetadataPages - 1;
                        Spinlocks.exitCriticalSection (lock);
                    end if;
                end if;
                node := To_Address (Integer_Address (child));
            end;
        end loop;
        return node + Storage_Offset ((pfn and 511) * 8);
    end prepareChargeSlot;

    procedure bindCharge
      (addr : Virtmem.PhysAddress; Identity : Unsigned_64;
       User_Owned : Boolean; Success : out Boolean)
      with SPARK_Mode => Off
    is
        pfn : constant Unsigned_64 := Unsigned_64 (Virtmem.addrToPFN (addr));
        slot : System.Address;
    begin
        Success := False;
        if Identity = 0 or else frameOwnerBase = System.Null_Address or else
          pfn > maxPinPFN or else addr mod Virtmem.FRAME_SIZE /= 0
        then
            return;
        end if;
        Spinlocks.enterCriticalSection (lock);
        declare
            owner : Frame_Owner with Import,
              Address => ownerAddress (pfn);
            pins : Unsigned_8 with Import,
              Address => pinStateBase + Storage_Offset (pfn);
        begin
            Success := chargeRefund /= null and then
              ((owner /= 0) = User_Owned) and then
              pins = 0 and then containsAllocatedFrame (addr);
        end;
        Spinlocks.exitCriticalSection (lock);
        if not Success then
            return;
        end if;
        Success := False;
        slot := prepareChargeSlot (pfn);
        if slot = System.Null_Address then
            return;
        end if;
        Spinlocks.enterCriticalSection (lock);
        declare
            charge : Unsigned_64 with Import,
              Address => slot;
            owner : Frame_Owner with Import,
              Address => ownerAddress (pfn);
            pins : Unsigned_8 with Import,
              Address => pinStateBase + Storage_Offset (pfn);
        begin
            if chargeRefund /= null and then charge = 0 and then
              ((owner /= 0) = User_Owned) and then pins = 0 and then containsAllocatedFrame (addr)
            then
                charge := Identity;
                Success := True;
            end if;
        end;
        Spinlocks.exitCriticalSection (lock);
    end bindCharge;

    procedure claimChargedUserFrame
      (addr : Virtmem.PhysAddress; owner : Frame_Owner; Identity : Unsigned_64;
       Success : out Boolean)
      with SPARK_Mode => Off
    is
        pfn : constant Unsigned_64 := Unsigned_64 (Virtmem.addrToPFN (addr));
        slot : System.Address;
    begin
        Success := False;
        if owner = 0 or else Identity = 0 or else frameOwnerBase = System.Null_Address
          or else pfn > maxPinPFN or else addr mod Virtmem.FRAME_SIZE /= 0
        then
            return;
        end if;
        -- The charge slot may need a directory page: allocated before the
        -- lock, as bindCharge does.
        slot := prepareChargeSlot (pfn);
        if slot = System.Null_Address then
            return;
        end if;
        Spinlocks.enterCriticalSection (lock);
        declare
            currentOwner : Frame_Owner with Import, Volatile, Address => ownerAddress (pfn);
            charge : Unsigned_64 with Import, Address => slot;
            pins : Unsigned_8 with Import, Address => pinStateBase + Storage_Offset (pfn);
        begin
            if chargeRefund /= null and then currentOwner = 0 and then charge = 0
              and then pins = 0 and then containsAllocatedFrame (addr)
            then
                currentOwner := owner;
                charge := Identity;
                Success := True;
            end if;
        end;
        Spinlocks.exitCriticalSection (lock);
    end claimChargedUserFrame;

    procedure bindFrameCharge
      (addr : Virtmem.PhysAddress; Identity : Unsigned_64; Success : out Boolean)
      with SPARK_Mode => Off is
    begin
        bindCharge (addr, Identity, True, Success);
    end bindFrameCharge;

    procedure bindKernelFrameCharge
      (addr : Virtmem.PhysAddress; Identity : Unsigned_64; Success : out Boolean)
      with SPARK_Mode => Off is
    begin
        bindCharge (addr, Identity, False, Success);
    end bindKernelFrameCharge;

    -- Caller holds the buddy lock and has admitted actual reclamation.
    -- Detach before publishing the free block, so immediate reuse cannot
    -- change the identity reported by the subsequent post-unlock callback.
    function detachCharge (pfn : Unsigned_64) return Unsigned_64
      with SPARK_Mode => Off
    is
        slot : constant System.Address := chargeSlot (pfn);
    begin
        if slot = System.Null_Address then
            return 0;
        end if;
        declare
            charge : Unsigned_64 with Import, Address => slot;
            result : constant Unsigned_64 := charge;
        begin
            charge := 0;
            return result;
        end;
    end detachCharge;

    procedure free (ord : in Order; addr : in System.Address) with
        SPARK_Mode => Off  -- lock calls change Global contract
    is
        refundIdentity : Unsigned_64 := 0;
        refundPages : Unsigned_64 := 0;
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
            type Owner_Array is array (Natural range <>) of Frame_Owner;
            owners : Owner_Array (pins'Range) with Import,
              Address => ownerAddress (Unsigned_64 (pfn));
        begin
            for i in pins'Range loop
                if pins (i) /= 0 or else owners (i) /= 0 then
                    raise AllocatorException with "Free of owned or pinned buddy block";
                end if;
                declare
                    slot : constant System.Address := chargeSlot (Unsigned_64 (pfn) + Unsigned_64 (i));
                    charge : Unsigned_64 := 0;
                begin
                    if slot /= System.Null_Address then
                        declare
                            value : Unsigned_64 with Import, Address => slot;
                        begin
                            charge := value;
                        end;
                    end if;
                    if charge /= 0 then
                        if refundIdentity /= 0 and then refundIdentity /= charge then
                            raise AllocatorException with "Mixed buddy block charge identities";
                        end if;
                        refundIdentity := charge;
                        refundPages := refundPages + 1;
                    end if;
                end;
            end loop;
            if not inPool (Unsigned_64 (pfn)) then
                moveBlock (ord, addr, Buddy_Blocks.Release_Block);
            end if;
            for i in pins'Range loop
                declare
                    ignored : constant Unsigned_64 := detachCharge (Unsigned_64 (pfn) + Unsigned_64 (i));
                    pragma Unreferenced (ignored);
                begin
                    null;
                end;
            end loop;
            if inPool (Unsigned_64 (pfn)) then
                -- A process-pool run (DMA, KERN-014) goes back to the pool.
                poolGive (Unsigned_64 (pfn), ord);
            else
                freeLocked (ord, addr);
            end if;
        end;

        Spinlocks.exitCriticalSection (lock);
        if refundIdentity /= 0 then
            chargeRefund (refundIdentity, refundPages);
        end if;
    end free;

    ---------------------------------------------------------------------------
    -- freeFrame
    ---------------------------------------------------------------------------
    procedure freeFrame (addr : in Virtmem.PhysAddress) with
        SPARK_Mode => Off
    is
        pfn : constant Unsigned_64 := Unsigned_64 (Virtmem.addrToPFN (addr));
        use type Frame_Pins.Release_Action;
        refundIdentity : Unsigned_64 := 0;
    begin
        Spinlocks.enterCriticalSection (lock);
        if inPool (pfn) then
            -- A process-pool frame (KERN-014): the same owner, pin and charge
            -- rules; it goes back to the pool, never to the kernel's lists.
            declare
                state : Unsigned_8 with Import, Volatile,
                    Address => pinStateBase + Storage_Offset (pfn);
                owner : Frame_Owner with Import, Volatile, Address => ownerAddress (pfn);
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
            refundIdentity := detachCharge (pfn);
            poolGive (pfn, 0);
            Spinlocks.exitCriticalSection (lock);
            if refundIdentity /= 0 then
                chargeRefund (refundIdentity, 1);
            end if;
            return;
        end if;
        -- Admission occurs before owner/pin mutations as well as list writes.
        moveBlock (0, Virtmem.P2Va (addr), Buddy_Blocks.Defer);
        if pinStateBase /= System.Null_Address and then pfn <= maxPinPFN then
            declare
                state : Unsigned_8 with
                    Import,
                    Volatile,
                    Address => pinStateBase + Storage_Offset (pfn);
                owner : Frame_Owner with
                    Import,
                    Volatile,
                    Address => ownerAddress (pfn);
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
        refundIdentity := detachCharge (pfn);
        freeLocked (0, Virtmem.P2Va (addr));
        Spinlocks.exitCriticalSection (lock);
        if refundIdentity /= 0 then
            chargeRefund (refundIdentity, 1);
        end if;
    end freeFrame;


    procedure pinOwnedFrame
      (addr : Virtmem.PhysAddress; owner : Frame_Owner; success : out Boolean)
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
            currentOwner : Frame_Owner with Import, Volatile,
                Address => ownerAddress (pfn);
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
        refundIdentity : Unsigned_64 := 0;
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
                refundIdentity := detachCharge (pfn);
                if inPool (pfn) then
                    poolGive (pfn, 0);
                else
                    moveBlock (0, Virtmem.P2Va (addr), Buddy_Blocks.Reclaim);
                    freeLocked (0, Virtmem.P2Va (addr));
                end if;
            end if;
        end;
        Spinlocks.exitCriticalSection (lock);
        if refundIdentity /= 0 then
            chargeRefund (refundIdentity, 1);
        end if;
    end unpinFrame;

    procedure claimUserFrame
      (addr    : in Virtmem.PhysAddress;
       owner   : in Frame_Owner;
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
            currentOwner : Frame_Owner with
                Import,
                Volatile,
                Address => ownerAddress (pfn);
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
       owner : in Frame_Owner) with
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
            currentOwner : Frame_Owner with
                Import,
                Volatile,
                Address => ownerAddress (pfn);
        begin
            if currentOwner = owner then
                currentOwner := 0;
            end if;
        end;
        Spinlocks.exitCriticalSection (lock);
    end releaseUserFrame;

    function isUserFrameOwnedBy
      (addr  : Virtmem.PhysAddress;
       owner : Frame_Owner) return Boolean with
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
            currentOwner : Frame_Owner with
                Import,
                Volatile,
                Address => ownerAddress (pfn);
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
        -- Each order contributes at most Natural'Last blocks of at most
        -- the largest block size, so the sum cannot overflow.
        Max_Term : constant := Natural'Last * 2 ** (Virtmem.FRAME_SHIFT + Config.MAX_BUDDY_ORDER);
        ret : Storage_Count := 0;
    begin
        for ord in Order'Range loop
            pragma Loop_Invariant (ret <= Storage_Count (ord) * Max_Term);
            ret := ret + (Storage_Count(freeLists(ord).numFreeBlocks) * blockSize (ord));
        end loop;
        ret := ret + Storage_Count (poolFreeFrameCount) * Virtmem.FRAME_SIZE;

        return ret;
    end getFreeBytes;


    ---------------------------------------------------------------------------
    -- getFreeFrames
    ---------------------------------------------------------------------------
    function getFreeFrames return Natural with
        SPARK_Mode => On
    is
    begin
        -- Saturates (more than 8 TiB free reads as Natural'Last).
        return Natural (Storage_Count'Min (getFreeBytes / Virtmem.FRAME_SIZE,
                                           Storage_Count (Natural'Last)));
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
        
        print ("Total free: "); print (Natural (Storage_Count'Min (getFreeBytes / 16#100000#,
                                                            Storage_Count (Natural'Last))));
        println (" MiB");

    end print;
end BuddyAllocator;
