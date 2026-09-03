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
    -- moveReplyCap
    ---------------------------------------------------------------------------
    procedure moveReplyCap (table : in out CapabilityTable;
                            dest  : in     CapabilitySlot;
                            moved :    out Boolean)
    is
    begin
        if dest = REPLY_CAP_SLOT
          or else table(REPLY_CAP_SLOT).capType /= CAP_REPLY
          or else table(dest).capType /= CAP_NULL
        then
            moved := False;
            return;
        end if;

        table(dest) := table(REPLY_CAP_SLOT);
        table(REPLY_CAP_SLOT) := NULL_CAPABILITY;
        moved := True;
    end moveReplyCap;

    ---------------------------------------------------------------------------
    -- takeReplyCap
    ---------------------------------------------------------------------------
    procedure takeReplyCap (table : in out CapabilityTable;
                            slot  : in     CapabilitySlot;
                            cap   :    out Capability;
                            taken :    out Boolean)
    is
    begin
        if table(slot).capType /= CAP_REPLY then
            cap := NULL_CAPABILITY;
            taken := False;
            return;
        end if;

        cap := table(slot);
        table(slot) := NULL_CAPABILITY;
        taken := True;
    end takeReplyCap;

    ---------------------------------------------------------------------------
    -- proveReplyCapSingleUse
    ---------------------------------------------------------------------------
    procedure proveReplyCapSingleUse
      (table       : in out CapabilityTable;
       slot        : in     CapabilitySlot;
       firstTaken  :    out Boolean;
       secondTaken :    out Boolean)
    is
        firstCap  : Capability;
        secondCap : Capability;
    begin
        takeReplyCap (table, slot, firstCap, firstTaken);
        takeReplyCap (table, slot, secondCap, secondTaken);
    end proveReplyCapSingleUse;

    ---------------------------------------------------------------------------
    -- advanceGeneration
    ---------------------------------------------------------------------------
    procedure advanceGeneration (current  : in out Generation;
                                 reusable :    out Boolean)
    is
    begin
        if current < Generation'Last then
            current  := current + 1;
            reusable := True;
        else
            reusable := False;
        end if;
    end advanceGeneration;

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
    -- resolveCurrentEndpoint
    ---------------------------------------------------------------------------
    procedure resolveCurrentEndpoint
      (table             : in     CapabilityTable;
       slot              : in     CapabilitySlot;
       rights            : in     CapabilityRights;
       currentGeneration : in     Generation;
       destPID           :    out Unsigned_64;
       capBadge          :    out Badge;
       status            :    out OperationStatus)
    is
    begin
        resolveEndpoint
          (table    => table,
           slot     => slot,
           rights   => rights,
           destPID  => destPID,
           capBadge => capBadge,
           status   => status);

        if status /= OP_OK then
            return;
        end if;

        if table(slot).gen /= currentGeneration then
            destPID  := 0;
            capBadge := NO_BADGE;
            status   := OP_STALE_GENERATION;
        end if;
    end resolveCurrentEndpoint;

    ---------------------------------------------------------------------------
    -- grantInitialCaps
    ---------------------------------------------------------------------------
    procedure grantInitialCaps (table :    out CapabilityTable;
                                pid   : in     Unsigned_64;
                                gen   : in     Generation)
    is
    begin
        -- A process slot may be recycled. Re-establish the empty-table
        -- invariant here rather than relying on every caller to have cleared
        -- stale authority left by the previous occupant.
        table := EMPTY_TABLE;

        -- Slot 0: Self endpoint. Grant/revoke rights are intentionally absent;
        -- this handle cannot be used as capability-construction authority.
        table(0) := (
            capType  => CAP_ENDPOINT,
            rights   => READ_WRITE,
            capBadge => pid,
            object   => (ref => pid, param => 0),
            gen      => gen);

        -- Slot 1 is deliberately empty. Filesystem access must come from the
        -- executable manifest and launcher policy, or explicit delegation.

        -- Slot 3: Self inspection/termination only. In particular, EXECUTE,
        -- GRANT, and REVOKE are absent, so a process cannot spawn or construct
        -- authority merely because it can control its own lifecycle.
        table(3) := (
            capType  => CAP_PROCESS,
            rights   => READ_WRITE,
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
