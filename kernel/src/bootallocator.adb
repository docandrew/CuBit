-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
-- Boot Physical Memory Allocator
-------------------------------------------------------------------------------
with Firmware_Frames;
package body BootAllocator with
    Refined_State => (BitmapState => reservations),
    SPARK_Mode => On
is
    procedure setup (areas : in MemoryAreas.MemoryAreaArray;
                     Map : Firmware_Frames.Region_Array) with
        SPARK_Mode => Off -- firmware addresses and physical/virtual translation
    is
        use MemoryAreas;
        package FF renames Firmware_Frames;
        use type FF.Count;
        use type FF.Region_Kind;
        use type FF.Decision;
        -- Depends on the linear physical -> higher-half mapping.
        stackEnd : constant Virtmem.PFN := Virtmem.vaddrToPFN (Virtmem.STACK_TOP);
    begin
        Frames.Initialize (reservations);
        Virtmem.MAX_PHYS_ADDRESSABLE := 0;
        Virtmem.MAX_PHYS_USABLE := 0;
        for area of areas loop
            if area.endAddr > Virtmem.MAX_PHYS_ADDRESSABLE then
                Virtmem.MAX_PHYS_ADDRESSABLE := area.endAddr;
            end if;
            if area.kind = USABLE then
                if area.endAddr > Virtmem.MAX_PHYS_USABLE then
                    Virtmem.MAX_PHYS_USABLE := area.endAddr;
                end if;
            end if;
        end loop;
        for Owner in Map'Range loop
            if Map (Owner).Kind = FF.Usable then
                declare
                    Cursor : FF.Boundary := FF.First (Map (Owner).Pages);
                    Limit : constant FF.Boundary := FF.Boundary'Min
                      (FF.Limit (Map (Owner).Pages), MAX_BOOT_PFN + 1);
                begin
                    while Cursor < Limit loop
                        if Cursor > FF.Count (stackEnd) and then
                          FF.Classify (Map, Owner, Cursor, Cursor) = FF.Admit
                        then
                            Frames.Admit (reservations, Frames.Payload_Frame (Cursor));
                        end if;
                        Cursor := Cursor + 1;
                    end loop;
                end;
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
