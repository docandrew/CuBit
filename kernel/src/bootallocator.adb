-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
-- Boot Physical Memory Allocator
-------------------------------------------------------------------------------
with Firmware_Frames;
package body BootAllocator with
    Refined_State => (BitmapState => (reservations, currentPhase)),
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
        use type FF.Block_Order;
        -- Depends on the linear physical -> higher-half mapping.
        stackEnd : constant Virtmem.PFN := Virtmem.vaddrToPFN (Virtmem.STACK_TOP);
    begin
        if currentPhase /= Unset then
            raise RetiredException with "Boot allocator set up twice";
        end if;
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
        -- Admit by aligned blocks, as buddy setup tiles RAM: a block wholly
        -- free of reservations is admitted at once, one inside a reservation
        -- (such as the retained boot arena) is skipped at once, and only
        -- blocks straddling a boundary are halved. The window is 2**18
        -- frames, so per-frame classification against the whole map would
        -- cost a map walk for every frame.
        for Owner in Map'Range loop
            if Map (Owner).Kind = FF.Usable then
                declare
                    Cursor : FF.Boundary := FF.Boundary'Max
                      (FF.First (Map (Owner).Pages), FF.Count (stackEnd) + 1);
                    Limit : constant FF.Boundary := FF.Boundary'Min
                      (FF.Limit (Map (Owner).Pages), MAX_BOOT_PFN + 1);
                begin
                    while Cursor < Limit loop
                        declare
                            Order : FF.Block_Order := FF.Largest_Block
                              (Cursor, Limit - 1, FF.Block_Order'Last);
                            Pages : FF.Count := FF.Block_Pages (Order);
                            Action : FF.Decision;
                        begin
                            loop
                                Action := FF.Classify
                                  (Map, Owner, Cursor, Cursor + Pages - 1);
                                exit when Action /= FF.Split;
                                -- Classify never splits a single frame.
                                Order := Order - 1;
                                Pages := Pages / 2;
                            end loop;
                            if Action = FF.Admit then
                                for Item in Cursor .. Cursor + Pages - 1 loop
                                    Frames.Admit (reservations, Frames.Payload_Frame (Item));
                                end loop;
                            end if;
                            Cursor := Cursor + Pages;
                        end;
                    end loop;
                end;
            end if;
        end loop;
        currentPhase := Active;
    end setup;

    -- Every entry checks the phase before touching the bitmap: after Retire
    -- its pages belong to the buddy allocator. Proved unreachable for SPARK
    -- callers (each entry's precondition); kept as the run-time refusal for
    -- the kernel's non-SPARK callers, built without assertion checks.
    procedure Require_Active with
        Global => (Input => currentPhase),
        Pre => currentPhase = Active
    is
    begin
        if currentPhase /= Active then
            raise RetiredException with "Boot allocator used outside its active phase";
        end if;
    end Require_Active;

    function isFree (frame : Virtmem.PFN) return Boolean is
    begin
        if currentPhase /= Active then
            raise RetiredException with "Boot allocator queried outside its active phase";
        end if;
        -- Validate before narrowing/indexing, including in the -gnatp kernel.
        if frame > MAX_BOOT_PFN then
            raise OutOfBoundsException with "Checking PFN not owned by Boot Allocator";
        end if;
        return Frames.Is_Free (reservations, Frames.Frame (frame));
    end isFree;

    function highestPFNAllocated return Virtmem.PFN is
    begin
        if currentPhase /= Active then
            raise RetiredException with "Boot allocator queried outside its active phase";
        end if;
        return Virtmem.PFN (Frames.Highest (reservations));
    end highestPFNAllocated;

    procedure allocFrame (addr : out Virtmem.PhysAddress) is
    begin
        allocFrames (1, addr);
    end allocFrame;

    procedure allocFrames (num : AllocSize; addr : out Virtmem.PhysAddress) is
        first : Frames.Frame;
    begin
        Require_Active;
        Frames.Reserve (reservations, Frames.Request_Size (num), first);
        if first = 0 then
            raise OutOfMemoryException with "Out of boot allocation memory";
            pragma Annotate (GNATprove, Intentional, "unexpected exception might be raised",
                             "boot memory exhausted: a kernel panic that halts this CPU");
        end if;
        addr := Virtmem.PFNToAddr (Virtmem.PFN (first));
    end allocFrames;

    function numFreeFrames return Unsigned_64 is
    begin
        if currentPhase /= Active then
            raise RetiredException with "Boot allocator queried outside its active phase";
        end if;
        return Unsigned_64 (Frames.Free_Count (reservations));
    end numFreeFrames;

    procedure Retire is
    begin
        Require_Active;
        currentPhase := Retired;
    end Retire;
end BootAllocator;
