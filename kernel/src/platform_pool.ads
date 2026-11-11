-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The platform-register pool (ACPI-POOL-001): the kernel's one pool of
-- chipset and firmware MMIO, what the ACPI pair maps at addresses AML
-- computes, and the claims of device memory delegated to drivers that keep
-- it off a bound device's BARs.
--
-- @description
-- The flow (ACPI-POOL-001), end to end:
--   1. Before ACPI starts, devmgr discovers the chipset's register blocks
--      itself (MCHBAR, the PMC from the LPIT, and, through its sideband
--      subsystem, the P2SB window and the GPIO community ranges: P2SB is
--      unhidden briefly and hidden again) and ADDS them to this pool
--      (Declare_Pool, then Insert, each member a typed Pool_Member_Kind).
--      devmgr maps none of it: the pool only permits mapping. This must
--      happen before ACPI's _INI methods run, because they touch these
--      registers (the IdeaPad's TPD0/TPL1._INI read GPIO PDW registers).
--   2. During ACPI startup the backend maps pool pieces when AML touches
--      them (Map: MAP_DEVICE through its pool copy). The kernel checks a
--      request once, here; after that AML's accesses are plain loads and
--      stores.
--   3. After _INI the ACPI service reports the platform devices' _CRS
--      resources to devmgr (who uses what: GPIO controller ranges and
--      interrupts, I2C controllers, the touchpad's address and GpioInt
--      pin), Windows-style; devmgr cross-checks its own communities and
--      binds drivers from it. That is devmgr's side (future work); the
--      kernel is not involved.
--   4. When devmgr hands a driver device memory, the kernel records a claim
--      (Delegating, Mapping_Into). Over a PCI device's BAR (Device_BAR) the
--      driver owns the device whole: ACPI's mappings there are revoked and
--      new ones refused (Revoked_From_Firmware). Over a chipset block
--      (Chipset_Shared: MCHBAR, PMC, sideband window, GPIO community) the
--      claim is shared, Linux lax-style, so firmware keeps working: AML's
--      mappings stay, new ones are admitted and the overlap is logged once
--      per (member, claimant) (Shared_With_Firmware). Only device BARs are
--      ever revoked from firmware.
--
-- State and decisions are Platform_Pool_Sets (proved); this unit holds the
-- lock, allocates the tables (charged to a memory quota), maps and unmaps
-- pages and writes the log. One spinlock, outermost of the locks it takes:
-- pool, then the memory ledger and the frame allocator, Owned_Memory, an
-- address space. No caller holds a mailbox lock when it calls in.
--
--   * Declare_Pool: devmgr (the pool owner) sizes the members and claims
--     tables from its discovery, once; charged to devmgr's quota.
--   * Insert: devmgr adds a member of a typed kind; the syscall layer has
--     already checked the pages against one of its device-memory roots.
--     Both are refused once the pool is in use (sealed: the first mapping
--     or the first delegation of device memory to another process).
--   * Map: checked once; the holder's mapping table grows on demand,
--     charged to the holder.
--   * Delegating / Mapping_Into: the claim of step 4, recorded before the
--     delegation takes effect. No room (the declared claim count): the
--     delegation is refused (Refused_Capacity), logged; the pool is never
--     switched off.
--   * Release_Process: an exiting process's claims and mapping table go.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Capabilities;
with Platform_Pool_ABI;
with Platform_Pool_Sets;
with Process;

package Platform_Pool with
    SPARK_Mode => Off  -- page tables, locks, TLB shootdown, charged blocks
is
    package Sets renames Platform_Pool_Sets;

    -- The pool's owner: devmgr, which holds the pool root. Set once at
    -- boot, before devmgr runs. Its own delegations are never claims.
    procedure Set_Owner (Owner : Unsigned_64);

    -- Size the pool for Members members and Claims claims (Caller is the
    -- owner, whose quota pays for the tables). 0, or a Capability_ABI
    -- refusal word: Invalid_Argument (a count of zero or past its
    -- ceiling, or declared already), Sealed, Out_Of_Memory.
    function Declare_Pool
      (Caller : Process.ProcessID; Members, Claims : Unsigned_64) return Unsigned_64;

    -- Add [Base, Base + Bytes) (whole pages) as a pool member of Kind. 0,
    -- or a Capability_ABI refusal word: Invalid_Argument (not declared,
    -- malformed, overlapping a member), Sealed, Table_Full (the declared
    -- member count).
    function Insert
      (Base, Bytes : Unsigned_64; Kind : Sets.Pool_Member_Kind) return Unsigned_64;

    -- MAP_DEVICE through Authority (a CAP_DEVICE_MEM tagged
    -- PLATFORM_POOL_TAG): Pages pages at Physical to Virtual in Caller,
    -- read-only when Read_Only. Mapped (Refusal not meaningful) or why not;
    -- Record is filled in either case.
    procedure Map
      (Caller    : Process.ProcessID;
       Authority : Capabilities.Capability;
       Physical, Virtual, Pages : Unsigned_64;
       Read_Only : Boolean;
       Mapped    : out Boolean;
       Why       : out Platform_Pool_ABI.Refusal;
       Record_Out : out Platform_Pool_ABI.Refusal_Record);

    -- What a claim did to firmware's (the ACPI pair's) access.
    type Claim_Outcome is
      (Not_In_Pool,            -- not pool memory (or the owner's own, or
                               -- already held): firmware's access unchanged
       Shared_With_Firmware,   -- chipset blocks only: AML keeps its mappings
       Revoked_From_Firmware,  -- covers a device BAR: its mappings revoked
       Refused_Capacity);      -- no room for the claim: refuse the delegation

    -- Copy (a capability, as derived) is about to be placed in Target: a
    -- claim when it is untagged device memory and Target is not the owner.
    -- Callers pass only a copy the derivation rules accept, so a claim is
    -- never wider than device memory the delegating process already holds.
    -- Refused_Capacity: the caller refuses the delegation
    -- (Capability_ABI.Table_Full).
    function Delegating
      (Target : Process.ProcessID; Copy : Capabilities.Capability) return Claim_Outcome;

    -- The same for a device mapping made into Target directly (MAP_INTO).
    function Mapping_Into
      (Target : Process.ProcessID; Physical, Bytes : Unsigned_64) return Claim_Outcome;

    -- Pid is being reclaimed (its address space still exists): its claims
    -- and its mapping table go.
    procedure Release_Process (Pid : Process.ProcessID);

end Platform_Pool;
