-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- Boot Physical Memory Allocator
-------------------------------------------------------------------------------

with Util; use Util;

package body BootAllocator with
    Refined_State => (BitmapState => (bitmap, freePhysicalFrames)),
    SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- setup
    ---------------------------------------------------------------------------
    procedure setup (areas     : in MemoryAreas.MemoryAreaArray) with
        SPARK_Mode => Off  -- use of 'Address
    is
        use MemoryAreas;    -- for = operator
        
        -- limits of memory area we're considering
        startPFN    : Virtmem.PFN;
        endPFN      : Virtmem.PFN;

        -- note this depends on linear physical -> higher-half mapping
        stackEnd    : constant Virtmem.PFN := 
            Virtmem.addrToPFN(Virtmem.V2P(Virtmem.STACK_TOP));
    begin
        -- mark everything as used to begin with.
        freePhysicalFrames := 0;

        highestPFNAllocated := 0;

        -- @TODO why not just a boolean array here?
        for i in FrameBitmapType'Range loop
            bitmap(i) := 16#FFFF_FFFF_FFFF_FFFF#;
        end loop;

        for area of areas loop

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

                for frame in startPFN .. endPFN loop

                    -- ignore all physical memory below the end of our stack.
                    -- for memory-constrained systems, we could use the gap between
                    -- the kernel .bss and the stack start, but as the kernel grows
                    -- we'll gradually eat into it, I think I prefer to leave it
                    -- unused for now.
                    if (frame <= stackEnd) then
                        null;
                    else
                        -- only mark it as free if it's owned by the boot allocator
                        if frame <= MAX_BOOT_PFN then
                            markFree(frame);
                        end if;
                    end if;
                end loop;
            end if;

        end loop;

        initialized := True;    --ghost
    end setup;
    
    ---------------------------------------------------------------------------
    -- Determines whether a page in this bitmap is free (0) or not (1)
    ---------------------------------------------------------------------------
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

    ---------------------------------------------------------------------------
    -- find a free frame, mark it as used, and return the base address
    -- TODO: Add contracts for out-of-memory situation
    ---------------------------------------------------------------------------
    procedure allocFrame(addr : out virtmem.PhysAddress) with
        SPARK_Mode => On
    is
        frame : Virtmem.PFN;
    begin
        frame := findFreeFrame;
        
        if frame = 0 then
            raise OutOfMemoryException with "Out of Memory";
        end if;

        -- track high-water mark
        if frame > highestPFNAllocated then
            highestPFNAllocated := frame;
        end if;

        markUsed(frame);

        addr := Virtmem.PFNToAddr(frame);
    end allocFrame;

    ---------------------------------------------------------------------------
    -- allocate a number of contiguous frames
    ---------------------------------------------------------------------------
    procedure allocFrames(num : in AllocSize; addr : out Virtmem.PhysAddress) with
        SPARK_Mode => On
    is
        basePFN : Virtmem.PFN;
        endPFN  : Virtmem.PFN;
    begin
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
    end allocFrames;

    ---------------------------------------------------------------------------
    -- Find a free frame in the physical memory list and return its PFN
    ---------------------------------------------------------------------------
    function findFreeFrame return Virtmem.PFN with
        SPARK_Mode => On
    is
        subtype bootPFNs is Virtmem.PFN range 1..MAX_BOOT_PFN;
    begin
        for i in bootPFNs'Range loop
            if isFree(i) then 
                return i;
            end if;
        end loop;

        return 0;   -- out of memory
    end findFreeFrame;

    ---------------------------------------------------------------------------
    -- Find a number of contiguous free frames
    ---------------------------------------------------------------------------
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

    ---------------------------------------------------------------------------
    -- Mark a particular frame in memory as used, update the count
    ---------------------------------------------------------------------------
    procedure markUsed(frame : in Virtmem.PFN) with
        SPARK_Mode => On
    is
        block  : constant Natural := getBlock(frame);
        offset : constant Natural := getOffset(frame);
    begin
        setBit(bitmap(block), offset);
        freePhysicalFrames := freePhysicalFrames - 1;
    end markUsed;
    
    ---------------------------------------------------------------------------
    -- Free a physical frame allocation at a certain address
    ---------------------------------------------------------------------------
    procedure free(addr : in virtmem.PhysAddress) with
        SPARK_Mode => On
    is
        frame : constant Virtmem.PFN := Virtmem.addrToPFN(addr);
    begin
        markFree(frame);
    end free;

    ---------------------------------------------------------------------------
    -- numFreeFrames
    ---------------------------------------------------------------------------
    function numFreeFrames return Unsigned_64 with
        SPARK_Mode => On
    is
    begin
        return freePhysicalFrames;
    end numFreeFrames;

    ---------------------------------------------------------------------------
    -- numFreeMB
    ---------------------------------------------------------------------------
    function numFreeMB return Unsigned_64 with
        SPARK_Mode => On
    is
    begin
        return numFreeFrames * Virtmem.FRAME_SIZE / 16#100000#;
    end numFreeMB;

    ---------------------------------------------------------------------------
    -- Mark a particular frame in memory as free, update the count
    ---------------------------------------------------------------------------
    procedure markFree(frame : in Virtmem.PFN) with
        SPARK_Mode => On
    is
        block  : constant Natural := getBlock(frame);
        offset : constant Natural := getOffset(frame);
    begin
        if frame > MAX_BOOT_PFN then
            raise OutOfBoundsException with "Freed PFN not owned by Boot Allocator";
        end if;

        clearBit(bitmap(block), offset);
        freePhysicalFrames := freePhysicalFrames + 1;
    end markFree;

    ---------------------------------------------------------------------------
    -- Return the index into bitmap array in which this frame resides.
    ---------------------------------------------------------------------------
    function getBlock(frame : in Virtmem.PFN) return Natural with
        SPARK_Mode => On is
    begin
        return Natural(frame / 64);
    end getBlock;

    ---------------------------------------------------------------------------
    -- Return the bit within a Unsigned_64 representing this single frame.
    ---------------------------------------------------------------------------
    function getOffset(frame : in Virtmem.PFN) return Natural with
        SPARK_Mode => On is
    begin
        return Natural(frame mod 64);
    end getOffset;

end BootAllocator;
