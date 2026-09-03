-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2024 Jon Andrew
--
-- @summary
-- Capability Table Operations
--
-- Table manipulation for per-process capability tables: lookup, insert,
-- remove, clear, and search. All operations take a CapabilityTable in/out
-- and have no dependency on Process (Process withs Capabilities, not the
-- reverse).
--
-- All routines are SPARK-provable linear scans over the 64-element table.
-------------------------------------------------------------------------------
package Capabilities.Operations with
    SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- Operation status codes returned by table manipulation procedures.
    ---------------------------------------------------------------------------
    type OperationStatus is (
        OP_OK,
        OP_TABLE_FULL,
        OP_INVALID_SLOT,
        OP_STALE_GENERATION,
        OP_INSUFFICIENT_RIGHTS,
        OP_NULL_CAPABILITY
    );

    ---------------------------------------------------------------------------
    -- isSingleSlotUpdate
    -- Proof-only model for a capability-table update: the selected slot has
    -- exactly the requested value and every other slot is unchanged.
    ---------------------------------------------------------------------------
    function isSingleSlotUpdate (updated  : CapabilityTable;
                                 original : CapabilityTable;
                                 slot     : CapabilitySlot;
                                 cap      : Capability) return Boolean is
        (updated(slot) = cap
         and then
           (for all i in CapabilitySlot =>
              (if i /= slot then updated(i) = original(i))))
        with Ghost;

    ---------------------------------------------------------------------------
    -- isReplyMoveResult
    -- Proof-only model for saving the kernel-minted current reply authority.
    -- A successful save is a move, never a copy: the destination receives the
    -- exact source capability, the source becomes null, and no other slot is
    -- changed.  A failed move changes nothing.
    ---------------------------------------------------------------------------
    function isReplyMoveResult (updated  : CapabilityTable;
                                original : CapabilityTable;
                                dest     : CapabilitySlot;
                                moved    : Boolean) return Boolean is
        (if moved then
             dest /= REPLY_CAP_SLOT
             and then original(REPLY_CAP_SLOT).capType = CAP_REPLY
             and then original(dest).capType = CAP_NULL
             and then updated(dest) = original(REPLY_CAP_SLOT)
             and then updated(REPLY_CAP_SLOT) = NULL_CAPABILITY
             and then
               (for all i in CapabilitySlot =>
                  (if i /= dest and then i /= REPLY_CAP_SLOT then
                       updated(i) = original(i)))
         else
             updated = original
             and then
               (dest = REPLY_CAP_SLOT
                or else original(REPLY_CAP_SLOT).capType /= CAP_REPLY
                or else original(dest).capType /= CAP_NULL))
        with Ghost;

    ---------------------------------------------------------------------------
    -- moveReplyCap
    -- Move the current kernel reply capability into an unused deferred slot.
    -- Existing authority is never overwritten.
    ---------------------------------------------------------------------------
    procedure moveReplyCap (table : in out CapabilityTable;
                            dest  : in     CapabilitySlot;
                            moved :    out Boolean) with
        Post => isReplyMoveResult (table, table'Old, dest, moved);

    ---------------------------------------------------------------------------
    -- isReplyTakeResult
    -- Proof-only model for consuming a selected one-use reply capability.
    -- Success returns the exact old capability and clears only its slot;
    -- failure returns no authority and changes nothing.
    ---------------------------------------------------------------------------
    function isReplyTakeResult (updated  : CapabilityTable;
                                original : CapabilityTable;
                                slot     : CapabilitySlot;
                                cap      : Capability;
                                taken    : Boolean) return Boolean is
        (if original(slot).capType = CAP_REPLY then
             taken
             and then cap = original(slot)
             and then isSingleSlotUpdate
               (updated, original, slot, NULL_CAPABILITY)
         else
             not taken
             and then cap = NULL_CAPABILITY
             and then updated = original)
        with Ghost;

    ---------------------------------------------------------------------------
    -- takeReplyCap
    -- Consume and return the reply capability in slot, if present.
    ---------------------------------------------------------------------------
    procedure takeReplyCap (table : in out CapabilityTable;
                            slot  : in     CapabilitySlot;
                            cap   :    out Capability;
                            taken :    out Boolean) with
        Post => isReplyTakeResult
          (table, table'Old, slot, cap, taken);

    ---------------------------------------------------------------------------
    -- proveReplyCapSingleUse
    -- Ghost proof harness: two consecutive takes from the same slot cannot
    -- both return reply authority.
    ---------------------------------------------------------------------------
    procedure proveReplyCapSingleUse
      (table       : in out CapabilityTable;
       slot        : in     CapabilitySlot;
       firstTaken  :    out Boolean;
       secondTaken :    out Boolean) with
        Ghost,
        Post => not (firstTaken and secondTaken);

    ---------------------------------------------------------------------------
    -- isEndpointResolution
    -- Proof-only model of endpoint lookup and rights enforcement.  A failed
    -- resolution discloses neither an object reference nor a badge.  The
    -- target object's live generation is checked by the IPC operation after
    -- this table-local resolution succeeds.
    ---------------------------------------------------------------------------
    function isEndpointResolution (table    : CapabilityTable;
                                   slot     : CapabilitySlot;
                                   rights   : CapabilityRights;
                                   destPID  : Unsigned_64;
                                   capBadge : Badge;
                                   status   : OperationStatus)
                                   return Boolean is
        (if table(slot).capType = CAP_NULL then
             status = OP_NULL_CAPABILITY
             and then destPID = 0
             and then capBadge = NO_BADGE
         elsif table(slot).capType /= CAP_ENDPOINT then
             status = OP_INVALID_SLOT
             and then destPID = 0
             and then capBadge = NO_BADGE
         elsif not isSubsetOf (rights, table(slot).rights) then
             status = OP_INSUFFICIENT_RIGHTS
             and then destPID = 0
             and then capBadge = NO_BADGE
         else
             status = OP_OK
             and then destPID = table(slot).object.ref
             and then capBadge = table(slot).capBadge)
        with Ghost;

    ---------------------------------------------------------------------------
    -- isCurrentEndpointResolution
    -- Proof-only model extending endpoint resolution with the target object's
    -- current generation.  A stale capability cannot disclose or select a
    -- destination even if its type and rights are otherwise valid.
    ---------------------------------------------------------------------------
    function isCurrentEndpointResolution
      (table             : CapabilityTable;
       slot              : CapabilitySlot;
       rights            : CapabilityRights;
       currentGeneration : Generation;
       destPID           : Unsigned_64;
       capBadge          : Badge;
       status            : OperationStatus) return Boolean is
        (if table(slot).capType = CAP_NULL then
             status = OP_NULL_CAPABILITY
             and then destPID = 0
             and then capBadge = NO_BADGE
         elsif table(slot).capType /= CAP_ENDPOINT then
             status = OP_INVALID_SLOT
             and then destPID = 0
             and then capBadge = NO_BADGE
         elsif not isSubsetOf (rights, table(slot).rights) then
             status = OP_INSUFFICIENT_RIGHTS
             and then destPID = 0
             and then capBadge = NO_BADGE
         elsif table(slot).gen /= currentGeneration then
             status = OP_STALE_GENERATION
             and then destPID = 0
             and then capBadge = NO_BADGE
         else
             status = OP_OK
             and then destPID = table(slot).object.ref
             and then capBadge = table(slot).capBadge)
        with Ghost;

    ---------------------------------------------------------------------------
    -- isInsertionResult
    -- Proof-only model for safe automatic insertion.  Success writes only an
    -- originally empty slot; failure reports a full table and changes nothing.
    ---------------------------------------------------------------------------
    function isInsertionResult (updated  : CapabilityTable;
                                original : CapabilityTable;
                                cap      : Capability;
                                slot     : CapabilitySlot;
                                status   : OperationStatus)
                                return Boolean is
        (if status = OP_OK then
             original(slot).capType = CAP_NULL
             and then isSingleSlotUpdate (updated, original, slot, cap)
         else
             status = OP_TABLE_FULL
             and then slot = CapabilitySlot'First
             and then updated = original)
        with Ghost;

    ---------------------------------------------------------------------------
    -- isGenerationAdvance
    -- Proof-only model for revocation generation advancement.  An object may
    -- be reused only when its generation advances; terminal generations force
    -- permanent retirement instead of allowing stale capabilities to revive.
    ---------------------------------------------------------------------------
    function isGenerationAdvance (updated  : Generation;
                                  original : Generation;
                                  reusable : Boolean) return Boolean is
        (if original < Generation'Last then
             reusable and then updated = original + 1
         else
             not reusable and then updated = original)
        with Ghost;

    ---------------------------------------------------------------------------
    -- advanceGeneration
    -- Invalidate capabilities referring to an object before considering its
    -- identifier for reuse.  reusable=False means the identifier must remain
    -- retired because the generation space is exhausted.
    ---------------------------------------------------------------------------
    procedure advanceGeneration (current  : in out Generation;
                                 reusable :    out Boolean) with
        Post => isGenerationAdvance
          (current, current'Old, reusable);

    ---------------------------------------------------------------------------
    -- lookupCap
    -- Return the capability at the given slot. If the slot contains a
    -- NULL_CAPABILITY, status is set to OP_NULL_CAPABILITY.
    ---------------------------------------------------------------------------
    procedure lookupCap (table  : in     CapabilityTable;
                         slot   : in     CapabilitySlot;
                         cap    :    out Capability;
                         status :    out OperationStatus);

    ---------------------------------------------------------------------------
    -- insertCap
    -- Find the first free (CAP_NULL) slot and insert the given capability.
    -- Returns the slot index via the out parameter. OP_TABLE_FULL if none.
    ---------------------------------------------------------------------------
    procedure insertCap (table  : in out CapabilityTable;
                         cap    : in     Capability;
                         slot   :    out CapabilitySlot;
                         status :    out OperationStatus) with
        Post => isInsertionResult (table, table'Old, cap, slot, status);

    ---------------------------------------------------------------------------
    -- insertCapAt
    -- Insert a capability at a specific slot, overwriting whatever was there.
    ---------------------------------------------------------------------------
    procedure insertCapAt (table : in out CapabilityTable;
                           slot  : in     CapabilitySlot;
                           cap   : in     Capability) with
        Post => isSingleSlotUpdate (table, table'Old, slot, cap);

    ---------------------------------------------------------------------------
    -- removeCap
    -- Clear the slot to NULL_CAPABILITY. OP_NULL_CAPABILITY if already empty.
    ---------------------------------------------------------------------------
    procedure removeCap (table  : in out CapabilityTable;
                         slot   : in     CapabilitySlot;
                         status :    out OperationStatus);

    ---------------------------------------------------------------------------
    -- clearTable
    -- Zero all slots to NULL_CAPABILITY.
    ---------------------------------------------------------------------------
    procedure clearTable (table : in out CapabilityTable);

    ---------------------------------------------------------------------------
    -- findByType
    -- Return the first slot whose capability matches the given type.
    -- OP_NULL_CAPABILITY if no match found.
    ---------------------------------------------------------------------------
    procedure findByType (table  : in     CapabilityTable;
                          wanted : in     CapabilityType;
                          slot   :    out CapabilitySlot;
                          status :    out OperationStatus);

    ---------------------------------------------------------------------------
    -- findByBadge
    -- Return the first slot whose capability badge matches the given value.
    -- OP_NULL_CAPABILITY if no match found.
    ---------------------------------------------------------------------------
    procedure findByBadge (table  : in     CapabilityTable;
                           wanted : in     Badge;
                           slot   :    out CapabilitySlot;
                           status :    out OperationStatus);

    ---------------------------------------------------------------------------
    -- resolveEndpoint
    -- Given a capability slot, verify it holds a valid CAP_ENDPOINT with
    -- the required rights. On success, return the destination PID (from
    -- object.ref) and the badge.
    ---------------------------------------------------------------------------
    procedure resolveEndpoint (table    : in     CapabilityTable;
                               slot     : in     CapabilitySlot;
                               rights   : in     CapabilityRights;
                               destPID  :    out Unsigned_64;
                               capBadge :    out Badge;
                               status   :    out OperationStatus) with
        Post => isEndpointResolution
          (table, slot, rights, destPID, capBadge, status);

    ---------------------------------------------------------------------------
    -- resolveCurrentEndpoint
    -- Resolve an endpoint and reject capabilities from an obsolete object
    -- generation.  The caller obtains currentGeneration only after validating
    -- that the capability's object reference is safe to use as an object-table
    -- index.
    ---------------------------------------------------------------------------
    procedure resolveCurrentEndpoint
      (table             : in     CapabilityTable;
       slot              : in     CapabilitySlot;
       rights            : in     CapabilityRights;
       currentGeneration : in     Generation;
       destPID           :    out Unsigned_64;
       capBadge          :    out Badge;
       status            :    out OperationStatus) with
        Post => isCurrentEndpointResolution
          (table, slot, rights, currentGeneration,
           destPID, capBadge, status);

    ---------------------------------------------------------------------------
    -- grantInitialCaps
    -- Populate a freshly-created process' table with authority over its own
    -- mailbox and lifecycle only. Service endpoints are never ambient: the
    -- launcher installs them after manifest and policy admission. Takes
    -- Unsigned_64 for PID to avoid depending on Process. gen is the process's
    -- current capability generation (preserved across PID recycling).
    --
    -- Slot layout:
    --   0: CAP_ENDPOINT  self, READ+WRITE (ref=pid)          badge=pid
    --   3: CAP_PROCESS   self, READ+WRITE (ref=pid)          badge=0
    ---------------------------------------------------------------------------
    procedure grantInitialCaps (table :    out CapabilityTable;
                                pid   : in     Unsigned_64;
                                gen   : in     Generation) with
        Post =>
          table(0) =
            (capType  => CAP_ENDPOINT,
             rights   => READ_WRITE,
             capBadge => pid,
             object   => (ref => pid, param => 0),
             gen      => gen)
          and then table(3) =
            (capType  => CAP_PROCESS,
             rights   => READ_WRITE,
             capBadge => NO_BADGE,
             object   => (ref => pid, param => 0),
             gen      => gen)
          and then
            (for all slot in CapabilitySlot =>
               (if slot /= 0 and then slot /= 3 then
                    table(slot) = NULL_CAPABILITY));

    ---------------------------------------------------------------------------
    -- checkPortAccess
    -- Scan the capability table for a CAP_IOPORT that covers the requested
    -- port range [port .. port+accessSize-1] with the appropriate read/write
    -- right.
    ---------------------------------------------------------------------------
    procedure checkPortAccess (table      : in     CapabilityTable;
                               port       : in     Unsigned_64;
                               accessSize : in     Unsigned_64;
                               forWrite   : in     Boolean;
                               allowed    :    out Boolean);

    ---------------------------------------------------------------------------
    -- checkDeviceMemAccess
    -- Scan the capability table for a CAP_DEVICE_MEM whose range
    -- [object.ref .. object.ref+object.param-1] covers [base .. base+size-1]
    -- with READ_WRITE rights.
    ---------------------------------------------------------------------------
    procedure checkDeviceMemAccess (table   : in     CapabilityTable;
                                    base    : in     Unsigned_64;
                                    size    : in     Unsigned_64;
                                    allowed :    out Boolean);

end Capabilities.Operations;
