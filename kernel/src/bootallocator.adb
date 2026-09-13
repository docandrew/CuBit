-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
-- Boot Physical Memory Allocator
-------------------------------------------------------------------------------
package body BootAllocator with
    Refined_State => (BitmapState => reservations),
    SPARK_Mode => On
is
    procedure setup (areas : in MemoryAreas.MemoryAreaArray) with
        SPARK_Mode => Off -- firmware addresses and physical/virtual translation
    is
        use MemoryAreas;
        startPFN : Virtmem.PFN;
        endPFN : Virtmem.PFN;
        -- Depends on the linear physical -> higher-half mapping.
        stackEnd : constant Virtmem.PFN := Virtmem.vaddrToPFN (Virtmem.STACK_TOP);
    begin
        Frames.Initialize (reservations);
        for area of areas loop
            startPFN := Virtmem.addrToPFN (area.startAddr);
            endPFN := Virtmem.addrToPFN (area.endAddr);
            if area.endAddr > Virtmem.MAX_PHYS_ADDRESSABLE then
                Virtmem.MAX_PHYS_ADDRESSABLE := area.endAddr;
            end if;
            if area.kind = USABLE then
                if area.endAddr > Virtmem.MAX_PHYS_USABLE then
                    Virtmem.MAX_PHYS_USABLE := area.endAddr;
                end if;
                -- Keep the existing firmware admission policy. Restrict the
                -- scan to our arena instead of visiting all RAM above 64 MiB.
                for frame in startPFN .. Virtmem.PFN'Min (endPFN, MAX_BOOT_PFN) loop
                    -- Frame zero is never admitted: it denotes exhaustion.
                    -- Kernel/stack frames remain reserved as before.
                    if frame > stackEnd then
                        Frames.Admit (reservations, Frames.Payload_Frame (frame));
                    end if;
                end loop;
            end if;
        end loop;
        initialized := True;
    end setup;

    function isFree (frame : Virtmem.PFN) return Boolean is
    begin
        -- Validate before narrowing/indexing, including in the -gnatp kernel.
        if frame > MAX_BOOT_PFN then
            raise OutOfBoundsException with "Checking PFN not owned by Boot Allocator";
        end if;
        return Frames.Is_Free (reservations, Frames.Frame (frame));
    end isFree;

    function highestPFNAllocated return Virtmem.PFN is
    begin
        return Virtmem.PFN (Frames.Highest (reservations));
    end highestPFNAllocated;

    procedure allocFrame (addr : out Virtmem.PhysAddress) is
    begin
        allocFrames (1, addr);
    end allocFrame;

    procedure allocFrames (num : AllocSize; addr : out Virtmem.PhysAddress) is
        first : Frames.Frame;
    begin
        Frames.Reserve (reservations, Frames.Request_Size (num), first);
        if first = 0 then
            raise OutOfMemoryException with "Out of boot allocation memory";
        end if;
        addr := Virtmem.PFNToAddr (Virtmem.PFN (first));
    end allocFrames;

    function numFreeFrames return Unsigned_64 is
    begin
        return Unsigned_64 (Frames.Free_Count (reservations));
    end numFreeFrames;
end BootAllocator;
