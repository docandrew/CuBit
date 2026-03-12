-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2024 Jon Andrew
--
-- Capability Table Operations - Implementation
-------------------------------------------------------------------------------
package body Capabilities.Operations with
    SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- lookupCap
    ---------------------------------------------------------------------------
    procedure lookupCap (table  : in     CapabilityTable;
                         slot   : in     CapabilitySlot;
                         cap    :    out Capability;
                         status :    out OperationStatus)
    is
    begin
        cap := table(slot);

        if cap.capType = CAP_NULL then
            status := OP_NULL_CAPABILITY;
        else
            status := OP_OK;
        end if;
    end lookupCap;

    ---------------------------------------------------------------------------
    -- insertCap
    ---------------------------------------------------------------------------
    procedure insertCap (table  : in out CapabilityTable;
                         cap    : in     Capability;
                         slot   :    out CapabilitySlot;
                         status :    out OperationStatus)
    is
    begin
        for i in CapabilitySlot loop
            if table(i).capType = CAP_NULL then
                table(i) := cap;
                slot   := i;
                status := OP_OK;
                return;
            end if;
        end loop;

        slot   := CapabilitySlot'First;
        status := OP_TABLE_FULL;
    end insertCap;

    ---------------------------------------------------------------------------
    -- insertCapAt
    ---------------------------------------------------------------------------
    procedure insertCapAt (table : in out CapabilityTable;
                           slot  : in     CapabilitySlot;
                           cap   : in     Capability)
    is
    begin
        table(slot) := cap;
    end insertCapAt;

    ---------------------------------------------------------------------------
    -- removeCap
    ---------------------------------------------------------------------------
    procedure removeCap (table  : in out CapabilityTable;
                         slot   : in     CapabilitySlot;
                         status :    out OperationStatus)
    is
    begin
        if table(slot).capType = CAP_NULL then
            status := OP_NULL_CAPABILITY;
        else
            table(slot) := NULL_CAPABILITY;
            status := OP_OK;
        end if;
    end removeCap;

    ---------------------------------------------------------------------------
    -- clearTable
    ---------------------------------------------------------------------------
    procedure clearTable (table : in out CapabilityTable)
    is
    begin
        table := EMPTY_TABLE;
    end clearTable;

    ---------------------------------------------------------------------------
    -- findByType
    ---------------------------------------------------------------------------
    procedure findByType (table  : in     CapabilityTable;
                          wanted : in     CapabilityType;
                          slot   :    out CapabilitySlot;
                          status :    out OperationStatus)
    is
    begin
        for i in CapabilitySlot loop
            if table(i).capType = wanted then
                slot   := i;
                status := OP_OK;
                return;
            end if;
        end loop;

        slot   := CapabilitySlot'First;
        status := OP_NULL_CAPABILITY;
    end findByType;

    ---------------------------------------------------------------------------
    -- findByBadge
    ---------------------------------------------------------------------------
    procedure findByBadge (table  : in     CapabilityTable;
                           wanted : in     Badge;
                           slot   :    out CapabilitySlot;
                           status :    out OperationStatus)
    is
    begin
        for i in CapabilitySlot loop
            if table(i).capType /= CAP_NULL and then
               table(i).capBadge = wanted then
                slot   := i;
                status := OP_OK;
                return;
            end if;
        end loop;

        slot   := CapabilitySlot'First;
        status := OP_NULL_CAPABILITY;
    end findByBadge;

    ---------------------------------------------------------------------------
    -- resolveEndpoint
    ---------------------------------------------------------------------------
    procedure resolveEndpoint (table    : in     CapabilityTable;
                               slot     : in     CapabilitySlot;
                               rights   : in     CapabilityRights;
                               destPID  :    out Unsigned_64;
                               capBadge :    out Badge;
                               status   :    out OperationStatus)
    is
        cap : constant Capability := table(slot);
    begin
        destPID  := 0;
        capBadge := NO_BADGE;

        if cap.capType = CAP_NULL then
            status := OP_NULL_CAPABILITY;
            return;
        end if;

        if cap.capType /= CAP_ENDPOINT then
            status := OP_INVALID_SLOT;
            return;
        end if;

        if not isSubsetOf (rights, cap.rights) then
            status := OP_INSUFFICIENT_RIGHTS;
            return;
        end if;

        destPID  := cap.object.ref;
        capBadge := cap.capBadge;
        status   := OP_OK;
    end resolveEndpoint;

    ---------------------------------------------------------------------------
    -- grantInitialCaps
    ---------------------------------------------------------------------------
    procedure grantInitialCaps (table : in out CapabilityTable;
                                pid   : in     Unsigned_64;
                                gen   : in     Generation)
    is
    begin
        -- Slot 0: Self endpoint (send + receive)
        table(0) := (
            capType  => CAP_ENDPOINT,
            rights   => ALL_RIGHTS,
            capBadge => pid,
            object   => (ref => pid, param => 0),
            gen      => gen);

        -- Slot 1: Filesystem server endpoint
        table(1) := (
            capType  => CAP_ENDPOINT,
            rights   => READ_WRITE,
            capBadge => pid,
            object   => (ref => Unsigned_64(Config.SERVICE_FILESYSTEM_PID),
                         param => 0),
            gen      => gen);

        -- Slot 2: Reserved (formerly kernel keyboard endpoint)

        -- Slot 3: Self process control
        table(3) := (
            capType  => CAP_PROCESS,
            rights   => ALL_RIGHTS,
            capBadge => NO_BADGE,
            object   => (ref => pid, param => 0),
            gen      => gen);
    end grantInitialCaps;

    ---------------------------------------------------------------------------
    -- checkPortAccess
    ---------------------------------------------------------------------------
    procedure checkPortAccess (table      : in     CapabilityTable;
                               port       : in     Unsigned_64;
                               accessSize : in     Unsigned_64;
                               forWrite   : in     Boolean;
                               allowed    :    out Boolean)
    is
        capBase  : Unsigned_64;
        capCount : Unsigned_64;
    begin
        allowed := False;

        for i in CapabilitySlot loop
            if table(i).capType = CAP_IOPORT then
                capBase  := table(i).object.ref;
                capCount := table(i).object.param;

                -- Check range coverage: [port .. port+accessSize-1] within
                -- [capBase .. capBase+capCount-1]
                if port >= capBase and then
                   port + accessSize <= capBase + capCount then
                    -- Check rights
                    if forWrite then
                        if table(i).rights(RIGHT_WRITE) then
                            allowed := True;
                            return;
                        end if;
                    else
                        if table(i).rights(RIGHT_READ) then
                            allowed := True;
                            return;
                        end if;
                    end if;
                end if;
            end if;
        end loop;
    end checkPortAccess;

    ---------------------------------------------------------------------------
    -- checkDeviceMemAccess
    ---------------------------------------------------------------------------
    procedure checkDeviceMemAccess (table   : in     CapabilityTable;
                                    base    : in     Unsigned_64;
                                    size    : in     Unsigned_64;
                                    allowed :    out Boolean)
    is
        capBase : Unsigned_64;
        capSize : Unsigned_64;
    begin
        allowed := False;

        for i in CapabilitySlot loop
            if table(i).capType = CAP_DEVICE_MEM then
                capBase := table(i).object.ref;
                capSize := table(i).object.param;

                -- Check range coverage: [base .. base+size-1] within
                -- [capBase .. capBase+capSize-1]
                if base >= capBase and then
                   base + size <= capBase + capSize then
                    if table(i).rights(RIGHT_READ) and then
                       table(i).rights(RIGHT_WRITE) then
                        allowed := True;
                        return;
                    end if;
                end if;
            end if;
        end loop;
    end checkDeviceMemAccess;

end Capabilities.Operations;
