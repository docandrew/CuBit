-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2024 Jon Andrew
--
-- @summary
-- Capability Types and Data Structures
--
-- Foundational capability type system for CuBitOS. Defines capability records,
-- rights bitmasks, authority tags and generation counters. Draws on seL4
-- (authority tags, endpoint caps), EROS (generation-counter revocation), and
-- Zircon (handle table with rights bitmask).
--
-- This is a spec-only package; all operations are expression functions. A
-- process's table of capabilities is Process_Capabilities (CAP-002: handles
-- with kernel-chosen slots and generations); what may be copied between
-- tables is Capability_Install.
--
-- No dependency on Process — Process will `with Capabilities`, not the
-- reverse.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

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
        CAP_REPLY,          -- One-use reply capability (kernel-minted)
        CAP_RESOURCE,       -- Resource quota (memory/CPU limits)
        CAP_CSPACE,         -- Capability-table administration
        CAP_SCHEDULING,     -- May reserve real-time CPU (docs/scheduler.md)
        CAP_HARDWARE_GROUP, -- Kernel catalog group; no raw-address authority
        CAP_HARDWARE_REGISTER, -- Kernel catalog member; fixed admitted operation
        CAP_FIRMWARE_SNAPSHOT  -- Read the sealed boot firmware-table snapshot
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
    -- Authority tag
    --
    -- A label attached to a capability, copied by the kernel into the separate
    -- 64-bit Message.authorityTag field when that capability authorizes IPC.
    -- It is not independently authority or necessarily a unique capability ID.
    -- MessageTag.reserved is unrelated, caller-controlled header padding.
    ---------------------------------------------------------------------------
    subtype Authority_Tag is Unsigned_64;

    NO_AUTHORITY_TAG : constant Authority_Tag := 0;

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
    -- | NOTIFICATION | First service role   | Role count     |
    -- | MEMORY       | Base physical address | Size in bytes |
    -- | IOPORT       | Base port number     | Port count     |
    -- | IRQ          | First vector         | Vector count   |
    -- | PROCESS      | Target PID (0=all)   | (reserved)     |
    -- | DEVICE_MEM   | Base physical address | Size in bytes |
    -- | REPLY        | Sender thread or PID | Request ID     |
    -- | RESOURCE     | (retired: quotas are RESUME arguments) |  |
    -- | CSPACE       | Target PID (one child, until it runs) | (reserved) |
    -- | SCHEDULING   | Budget µs            | Period µs      |
    -- | FIRMWARE_SNAPSHOT | (reserved, 0)   | (reserved, 0)  |
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
        capType      : CapabilityType   := CAP_NULL;
        rights       : CapabilityRights := NO_RIGHTS;
        authorityTag : Authority_Tag    := NO_AUTHORITY_TAG;
        object       : ObjectRef        := NULL_OBJECT;
        gen          : Generation       := 0;
    end record;

    NULL_CAPABILITY : constant Capability := (
        capType => CAP_NULL,
        rights  => NO_RIGHTS,
        authorityTag => NO_AUTHORITY_TAG,
        object  => NULL_OBJECT,
        gen     => 0
    );

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
    -- isSubrangeOf
    -- For range objects (DEVICE_MEM: base, size in bytes): the child range is
    -- nonempty, nonwrapping and lies within the nonempty, nonwrapping parent
    -- range.
    ---------------------------------------------------------------------------
    function isSubrangeOf (child  : ObjectRef;
                           parent : ObjectRef) return Boolean is
        (child.param > 0 and then parent.param > 0
         and then child.param - 1 <= Unsigned_64'Last - child.ref
         and then parent.param - 1 <= Unsigned_64'Last - parent.ref
         and then child.ref >= parent.ref
         and then child.ref + (child.param - 1) <= parent.ref + (parent.param - 1));

    ---------------------------------------------------------------------------
    -- isAttenuationOf
    -- A derived capability denotes the same object (or, for device memory, a
    -- subrange of it), generation, and authority tag as its parent and
    -- carries no rights absent from the parent.  This is the
    -- ordinary-derivation non-amplification relation; authority tag-changing
    -- minting is intentionally outside this relation.
    ---------------------------------------------------------------------------
    function isAttenuationOf (child  : Capability;
                              parent : Capability) return Boolean is
        (child.capType = parent.capType
         and then (child.object = parent.object
                   or else (child.capType = CAP_DEVICE_MEM
                            and then isSubrangeOf (child.object, parent.object)))
         and then child.gen = parent.gen
         and then child.authorityTag = parent.authorityTag
         and then isSubsetOf (child.rights, parent.rights))
        with Ghost;

end Capabilities;
