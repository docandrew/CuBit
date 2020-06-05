-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- Boot Physical Memory Allocator
-------------------------------------------------------------------------------

--with Textmode; use Textmode;
with Util; use Util;
--with x86;

package body BootAllocator with
    Refined_State => (BitmapState => (bitmap, freePhysicalFrames)),
    SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- setup
    ---------------------------------------------------------------------------
    procedure setup(areas : in MemoryAreas.MemoryAreaArray;
                    freeBytes : out Unsigned_64) with
        SPARK_Mode => Off  -- use of 'Address
    is
        use MemoryAreas;    -- for = operator
        
        -- limits of memory area we're considering
        startPFN    : Virtmem.PFN;
        endPFN      : Virtmem.PFN;

        -- note this depends on linear physical -> higher-half mapping
        -- stackBegin  : constant Virtmem.PFN := 
        --     Virtmem.addrToPFN(Virtmem.V2P(Virtmem.STACK_BOTTOM));
        stackEnd    : constant Virtmem.PFN := 
            Virtmem.addrToPFN(Virtmem.V2P(Virtmem.STACK_TOP));

        -- for now, assume everything in the first 1MB is unusable, since there
        -- are weird BIOS structures, EGA text buffers and other stuff in there
        -- we don't want to clobber. Any packages that need to mess around down
        -- there will do it explicitly, but we won't allow allocating memory
        -- there.
        -- kernBegin   : constant Virtmem.PFN := 0;
        -- kernEnd     : constant Virtmem.PFN := 
        --     Virtmem.addrToPFN(To_Integer(Virtmem.KERNEL_END_VIRT'Address) - 
        --     Virtmem.KERNEL_BASE);
        
        freeThisRegion      : Unsigned_64;
        --testCounter         : Natural := 0;
    begin
        -- mark everything as used to begin with.
        freePhysicalFrames := 0;

        highestPFNAllocated := 0;

        -- TODO: why not just a boolean array here?
        for i in FrameBitmapType'Range loop
            bitmap(i) := 16#FFFF_FFFF_FFFF_FFFF#;
        end loop;

        --print("Frame bitmap size: "); println(bitmap'Last);

        for area of areas loop

            freeThisRegion := 0;

            -- Go through each frame in this block and mark it as free
            -- if it's Usable memory and not part of our kernel.
            startPFN    := Virtmem.addrToPFN(area.startAddr);
            endPFN      := Virtmem.addrToPFN(area.endAddr);

            -- Update max phys addressable on our system
            -- (TODO: Don't really need this anymore?)
            if area.endAddr > Virtmem.MAX_PHYS_ADDRESSABLE then
                Virtmem.MAX_PHYS_ADDRESSABLE := area.endAddr;
            end if;

            if (area.kind = MemoryAreas.USABLE) then

                -- update max usable memory address on our system.
                if area.endAddr > Virtmem.MAX_PHYS_USABLE then
                    Virtmem.MAX_PHYS_USABLE := area.endAddr;
                end if;

                --print(" checking frames between "); print(Unsigned_32(startPFN));
                --print(" and "); println(Unsigned_32(endPFN));
                --print(" ignoring frames below "); println(Unsigned_64(stackEnd));

                for frame in startPFN .. endPFN loop

                    -- ignore all physical memory below the end of our stack.
                    -- for memory-constrained systems, we could use the gap between
                    -- the kernel .bss and the stack start, but as the kernel grows
                    -- we'll gradually eat into it, I think I prefer to leave it
                    -- unused for now.
                    if (frame <= stackEnd) then
                        null;
                    else
                        --print("found free frame"); println(Unsigned_64(frame));
                        -- only mark it as free if it's owned by the boot allocator
                        if frame <= MAX_BOOT_PFN then
                            markFree(frame);
                            freeThisRegion := freeThisRegion + 1;
                        end if;
                    end if;
                end loop;

                --print(" Found "); printd(freeThisRegion); println(" frames in this region");
            end if;

        end loop;

        --print("freePhysicalFrames: "); println(freePhysicalFrames);
        freeBytes := (freePhysicalFrames - 1) * Unsigned_64(Virtmem.FRAME_SIZE);
        --print("freeBytes: "); println(freeBytes);
        initialized := True;    --ghost
    end setup;
    

    -- Determines whether a page in this bitmap is free (0) or not (1)
    function isFree(frame : in Virtmem.PFN) return Boolean with
        SPARK_Mode => On
    is
        block   : constant Natural      := getBlock(frame);
        offset  : constant Natural      := getOffset(frame);
        mask    : constant Unsigned_64  := Shift_Left(1, offset);
        result  : constant Boolean      := (mask and bitmap(block)) = 0;
        OutOfBounds : exception;
    begin
        if frame > MAX_BOOT_PFN then
            raise OutOfBounds with "Checking isFree on PFN not owned by Boot Allocator";
        end if;

        return result;
    end isFree;


    -- find a free frame, mark it as used, and return the base address
    -- TODO: Add contracts for out-of-memory situation
    procedure allocFrame(addr : out virtmem.PhysAddress) with
        SPARK_Mode => On
    is
        frame       : Virtmem.PFN;
    begin
        Spinlock.enterCriticalSection(mutex);

        frame := findFreeFrame;
        
        if frame = 0 then
            raise OutOfMemoryException with "Out of Memory";
        end if;

        -- track high-water mark
        if frame > highestPFNAllocated then
            highestPFNAllocated := frame;
        end if;

        markUsed(frame);

        addr := Virtmem.PFNToAddr(frame);       --virtmem.FRAME_SIZE * virtmem.PhysAddress(frame);

        Spinlock.exitCriticalSection(mutex);
        --print("Alloc: "); println(addr);
    end allocFrame;


    -- allocate a number of contigous frames
    procedure allocFrames(num : in AllocSize; addr : out Virtmem.PhysAddress) with
        SPARK_Mode => On
    is
        basePFN                 : Virtmem.PFN;
        endPFN                  : Virtmem.PFN;
    begin
        Spinlock.enterCriticalSection(mutex);

        basePFN := findFreeFrames(num);

        if basePFN = 0 then
            raise OutOfMemoryException with "Out of Memory";
        end if;

        endPFN := basePFN + Virtmem.PFN(num - 1);

        -- track high-water mark
        if endPFN > highestPFNAllocated then
            highestPFNAllocated := endPFN;
        end if;

        for frame in basePFN .. endPFN loop
            markUsed(frame);
        end loop;

        addr := Virtmem.PFNToAddr(basePFN);
        
        Spinlock.exitCriticalSection(mutex);
    end allocFrames;

    -- allocate a sub-page quantity of memory
    -- procedure allocSmall(addr : out Virtmem.PhysAddress)
    -- is
    --     newFrame : Virtmem.PhysAddress;
    -- begin
    --     if Size > Virtmem.FRAME_SIZE then
    --         raise AllocationSizeExceeded 
    --             with "BootAllocPool size exceeded";
    --     elsif (Size + curLevel) > maxLevel then
    --         -- save the old page for reclamation
    --         -- 
    --         -- allocate a new page.
    --         allocFrame(newFrame);
    --         curPage := To_Address(Virtmem.P2V(newFrame));
    --         curLevel := 0;
    --         Addr := curPage;
    --     else
    --         -- use space in current page
    --         curLevel := curLevel + Size;
    --         Addr := curPage + Size;
    --     end if;
    -- end allocSmall;


    -- Find a free frame in the physical memory list and return its PFN
    function findFreeFrame return Virtmem.PFN with
        SPARK_Mode => On
    is
        --block       : Unsigned_64;
        --freeBit     : Natural;
        subtype bootPFNs is Virtmem.PFN range 1..MAX_BOOT_PFN;
    begin
        for i in bootPFNs'Range loop
            if isFree(i) then 
                return i;
            end if;
        end loop;

        return 0;   -- out of memory
    end findFreeFrame;


    -- Find a number of contiguous free frames
    function findFreeFrames(num : in AllocSize) return Virtmem.PFN with
        SPARK_Mode => On
    is
        startPFN        : Virtmem.PFN := 1;
        numFramesSoFar  : Natural := 0;
        subtype bootPFNs is Virtmem.PFN range 1..MAX_BOOT_PFN;
    begin
        -- linearly iterate, looking for largest string of free frames
        for i in bootPFNs'Range loop
            if isFree(i) then
                numFramesSoFar := numFramesSoFar + 1;

                if numFramesSoFar = num then
                    return startPFN;
                end if;
            else
                numFramesSoFar := 0;
                startPFN := i + 1;
            end if;
        end loop;

        return 0;
    end findFreeFrames;


    -- Mark a particular frame in memory as used, update the count
    procedure markUsed(frame : in Virtmem.PFN) with
        SPARK_Mode => On
    is
        block   : constant Natural := getBlock(frame);
        offset  : constant Natural := getOffset(frame);
    begin
        --print(" old value of bitmap(block): "); println(bitmap(block));
        --print(" marking pfn "); print(Unsigned_64(frame)); println(" as used. ");
        setBit(bitmap(block), offset);
        freePhysicalFrames := freePhysicalFrames - 1;
        --print(" new value of bitmap(block): "); println(bitmap(block));
    end markUsed;
    

    -- Free a physical frame allocation at a certain address
    procedure free(addr : in virtmem.PhysAddress) with
        SPARK_Mode => On
    is
        frame   : constant Virtmem.PFN := Virtmem.addrToPFN(addr);
    begin
        markFree(frame);
    end free;


    -- getter
    function getFreeFrameCount return Unsigned_64 with
        SPARK_Mode => On
    is
    begin
        return freePhysicalFrames;
    end getFreeFrameCount;


    -- Mark a particular frame in memory as free, update the count
    procedure markFree(frame : in Virtmem.PFN) with
        SPARK_Mode => On
    is
        block   : constant Natural := getBlock(frame);
        offset  : constant Natural := getOffset(frame);
    begin
        if frame > MAX_BOOT_PFN then
            raise OutOfBoundsException with "Freed PFN not owned by Boot Allocator";
        end if;

        clearBit(bitmap(block), offset);
        freePhysicalFrames := freePhysicalFrames + 1;
    end markFree;


    -- Return the index into bitmap array in which this frame resides.
    function getBlock(frame : in Virtmem.PFN) return Natural with
        SPARK_Mode => On is
    begin
        return Natural(frame / 64);
    end getBlock;


    -- Return the bit within a Unsigned_64 representing this single frame.
    function getOffset(frame : in Virtmem.PFN) return Natural with
        SPARK_Mode => On is
    begin
        return Natural(frame mod 64);
    end getOffset;

end BootAllocator;