-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2024 Jon Andrew
--
-- @summary
-- Capability Types and Data Structures
--
-- Foundational capability type system for CuBitOS. Defines capability records,
-- rights bitmasks, badges, generation counters, and per-process capability
-- tables. Draws on seL4 (badges, endpoint caps), EROS (generation-counter
-- revocation), and Zircon (flat handle table with rights bitmask).
--
-- This is a spec-only package; all operations are expression functions. More
-- complex table operations (lookup, insert, revoke) will go in future child
-- packages.
--
-- No dependency on Process — Process will `with Capabilities`, not the
-- reverse.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with Config;

package Capabilities with
    SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- Capability Type Enum
    --
    -- Identifies what kernel object a capability refers to.
    ---------------------------------------------------------------------------
    type CapabilityType is (
        CAP_NULL,           -- Empty/invalid slot
        CAP_ENDPOINT,       -- IPC endpoint (send/receive)
        CAP_NOTIFICATION,   -- Async notification/event
        CAP_MEMORY,         -- Physical memory region
        CAP_IOPORT,         -- I/O port range
        CAP_IRQ,            -- Hardware interrupt
        CAP_PROCESS,        -- Process/thread control
        CAP_DEVICE_MEM,     -- Device memory (framebuffer, MMIO)
        CAP_REPLY           -- One-use reply capability (kernel-minted)
    );

    ---------------------------------------------------------------------------
    -- Rights
    --
    -- Packed Boolean array indexed by right enum. Predefined `and`/`or`/`not`
    -- give set intersection, union, and complement for free.
    ---------------------------------------------------------------------------
    type CapabilityRight is (
        RIGHT_READ,
        RIGHT_WRITE,
        RIGHT_EXECUTE,
        RIGHT_GRANT,
        RIGHT_REVOKE
    );

    type CapabilityRights is array (CapabilityRight) of Boolean with Pack;

    NO_RIGHTS  : constant CapabilityRights := (others => False);
    ALL_RIGHTS : constant CapabilityRights := (others => True);

    READ_ONLY  : constant CapabilityRights := (
        RIGHT_READ => True,
        others     => False
    );

    READ_WRITE : constant CapabilityRights := (
        RIGHT_READ  => True,
        RIGHT_WRITE => True,
        others      => False
    );

    ---------------------------------------------------------------------------
    -- Badge
    --
    -- 64-bit badge, matching seL4 convention. When capabilities are
    -- integrated into IPC, the MessageTag.badge (currently 16-bit) will be
    -- restructured to support full 64-bit badges in a separate Message field.
    ---------------------------------------------------------------------------
    subtype Badge is Unsigned_64;

    NO_BADGE : constant Badge := 0;

    ---------------------------------------------------------------------------
    -- Generation Counter
    --
    -- O(1) revocation: each capability stores the generation at which it was
    -- created. Kernel objects store a current generation counter. A capability
    -- whose generation does not match the object's current generation is stale.
    ---------------------------------------------------------------------------
    subtype Generation is Unsigned_32;

    INITIAL_GENERATION : constant Generation := 1;

    ---------------------------------------------------------------------------
    -- Object Reference
    --
    -- Generic two-field record interpreted per capability type:
    --
    -- | Cap Type     | ref                  | param          |
    -- |--------------|----------------------|----------------|
    -- | ENDPOINT     | Mailbox owner PID    | (reserved)     |
    -- | NOTIFICATION | Target PID           | (reserved)     |
    -- | MEMORY       | Base physical address | Size in bytes |
    -- | IOPORT       | Base port number     | Port count     |
    -- | IRQ          | Interrupt vector     | (reserved)     |
    -- | PROCESS      | Target PID           | (reserved)     |
    -- | DEVICE_MEM   | Base physical address | Size in bytes |
    -- | REPLY        | Sender PID           | (reserved)     |
    --
    -- Two plain fields instead of a variant record (README warns against
    -- variant records for overlays).
    ---------------------------------------------------------------------------
    type ObjectRef is record
        ref   : Unsigned_64 := 0;
        param : Unsigned_64 := 0;
    end record;

    NULL_OBJECT : constant ObjectRef := (ref => 0, param => 0);

    ---------------------------------------------------------------------------
    -- Capability Record
    ---------------------------------------------------------------------------
    type Capability is record
        capType : CapabilityType    := CAP_NULL;
        rights  : CapabilityRights  := NO_RIGHTS;
        capBadge : Badge            := NO_BADGE;
        object  : ObjectRef         := NULL_OBJECT;
        gen     : Generation        := 0;
    end record;

    NULL_CAPABILITY : constant Capability := (
        capType => CAP_NULL,
        rights  => NO_RIGHTS,
        capBadge => NO_BADGE,
        object  => NULL_OBJECT,
        gen     => 0
    );

    ---------------------------------------------------------------------------
    -- Per-Process Capability Table
    ---------------------------------------------------------------------------
    subtype CapabilitySlot is Natural range 0 ..
        Config.PER_PROCESS_CAPABILITIES - 1;

    -- Well-known slot for kernel-minted reply capabilities.
    -- Highest slot, not used by any service convention.
    REPLY_CAP_SLOT : constant CapabilitySlot := CapabilitySlot'Last;

    type CapabilityTable is array (CapabilitySlot) of Capability;

    EMPTY_TABLE : constant CapabilityTable := (others => NULL_CAPABILITY);

    ---------------------------------------------------------------------------
    -- Expression Functions
    ---------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- isValid
    -- A capability is valid if its type is not CAP_NULL.
    ---------------------------------------------------------------------------
    function isValid (cap : Capability) return Boolean is
        (cap.capType /= CAP_NULL);

    ---------------------------------------------------------------------------
    -- isSubsetOf
    -- True if every right in `subset` is also present in `superset`.
    -- Equivalently: subset AND (NOT superset) has no bits set.
    ---------------------------------------------------------------------------
    function isSubsetOf (subset   : CapabilityRights;
                         superset : CapabilityRights) return Boolean is
        ((subset and not superset) = NO_RIGHTS);

    ---------------------------------------------------------------------------
    -- hasRights
    -- True if the capability is valid and its rights are a superset of the
    -- required rights.
    ---------------------------------------------------------------------------
    function hasRights (cap      : Capability;
                        required : CapabilityRights) return Boolean is
        (isValid (cap) and then isSubsetOf (required, cap.rights));

    ---------------------------------------------------------------------------
    -- derive
    -- Create a new capability with rights reduced to the intersection of the
    -- parent's rights and the requested rights. The badge, object reference,
    -- and generation are inherited from the parent.
    --
    -- Precondition: newRights must be a subset of the parent's rights
    -- (monotonic rights reduction).
    ---------------------------------------------------------------------------
    function derive (parent    : Capability;
                     newRights : CapabilityRights) return Capability is
        (Capability'(
            capType    => parent.capType,
            rights     => parent.rights and newRights,
            capBadge   => parent.capBadge,
            object     => parent.object,
            gen        => parent.gen
        ))
        with Pre => isSubsetOf (newRights, parent.rights);

    ---------------------------------------------------------------------------
    -- mint
    -- Derive a capability with reduced rights and a new badge stamped on it.
    --
    -- Precondition: newRights must be a subset of the parent's rights.
    ---------------------------------------------------------------------------
    function mint (parent    : Capability;
                   newBadge  : Badge;
                   newRights : CapabilityRights) return Capability is
        (Capability'(
            capType    => parent.capType,
            rights     => parent.rights and newRights,
            capBadge   => newBadge,
            object     => parent.object,
            gen        => parent.gen
        ))
        with Pre => isSubsetOf (newRights, parent.rights);

end Capabilities;
