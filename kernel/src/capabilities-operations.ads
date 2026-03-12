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
                         status :    out OperationStatus);

    ---------------------------------------------------------------------------
    -- insertCapAt
    -- Insert a capability at a specific slot, overwriting whatever was there.
    ---------------------------------------------------------------------------
    procedure insertCapAt (table : in out CapabilityTable;
                           slot  : in     CapabilitySlot;
                           cap   : in     Capability);

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
                               status   :    out OperationStatus);

    ---------------------------------------------------------------------------
    -- grantInitialCaps
    -- Populate a freshly-created process' capability table with well-known
    -- endpoint capabilities for standard services. Takes Unsigned_64 for
    -- PID to avoid depending on Process. gen is the process's current
    -- capability generation (preserved across PID recycling).
    --
    -- Slot layout:
    --   0: CAP_ENDPOINT  self (ref=pid)                     badge=pid
    --   1: CAP_ENDPOINT  filesystem server (ref=FS_PID)     badge=pid
    --   2: CAP_ENDPOINT  keyboard driver (ref=KB_PID)       badge=pid
    --   3: CAP_PROCESS   self process control (ref=pid)     badge=0
    ---------------------------------------------------------------------------
    procedure grantInitialCaps (table : in out CapabilityTable;
                                pid   : in     Unsigned_64;
                                gen   : in     Generation);

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
