------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The platform-register pool (ACPI-POOL-001): the device-memory tag of
--  the pool capability, and MAP_DEVICE's answer when it maps through one.
--
--  @description
--  The pool is chipset and firmware MMIO devmgr discovered independently
--  (host bridge register blocks such as MCHBAR, the PMC, the P2SB sideband
--  window and its GPIO communities, firmware-reserved spans), held by the
--  kernel as sorted, disjoint page intervals (Platform_Pool_Sets). devmgr
--  holds the pool root (Read, Write, Grant; kernel tag Platform_Pool_Tag)
--  and sets it up with INSTALL_CAPABILITY naming the pool as the target:
--
--    1. Declare (once, before any member): the derivation's Source is the
--       pool handle itself, Flags = Narrow, Base = how many members and
--       Length = how many claims the kernel keeps, both derived from what
--       devmgr discovered (each 1 .. its Maximum_*). The kernel sizes the
--       pool's tables from them and charges them to devmgr's memory quota.
--    2. Install each member: Source is one of devmgr's untagged
--       device-memory roots, narrowed (Narrow) to the member's whole pages,
--       with Set_Tag and Tag = Member_Kind_Word (its kind): Device_BAR for
--       a PCI function's BAR (a driver bound to the device owns it whole),
--       Chipset_Shared for a chipset block firmware keeps using (MCHBAR,
--       PMC, sideband window, GPIO community).
--
--  Both are refused (Capability_ABI.Sealed) once the pool is in use: after
--  the first pool mapping or the first delegation of device memory to
--  another process, the members are fixed. A full member table is
--  Capability_ABI.Table_Full.
--
--  A copy without Grant maps:
--
--    MAP_DEVICE (70): arg0 physical base (page aligned), arg1 virtual base,
--    arg2 pages (1 .. Maximum_Pages), arg3 Read_Write_Mode or
--    Read_Only_Mode, arg4 the pool handle, arg5 the address of a
--    Refusal_Record to fill when refused (0: none).
--    Returns 0 (mapped uncached, user, no execute) or a Refusal code.
--
--  The kernel checks the request once, here: it must lie wholly inside one
--  pool interval, and, in a Device_BAR member, no device memory delegated
--  to another process (a bound driver's BAR or window: a claim) may
--  overlap it. Later loads and stores are plain memory accesses. When a
--  claim over a Device_BAR member arrives after the mapping, the kernel
--  unmaps it from the holder (a later access faults) and logs
--  "platform-pool: revoked". A claim over a Chipset_Shared member is
--  SHARED: mappings stay, new ones are still admitted, and the kernel logs
--  "platform-pool: shared" once per (member, claimant). A claim is
--  withdrawn when its claimant exits. When the kernel has no room for a
--  claim (the declared claim count), the delegation that would make it is
--  refused (Capability_ABI.Table_Full) and logged; the pool is never
--  switched off.
--
--  The kernel's record of a holder's mappings grows as it maps (charged to
--  the holder's memory quota); MAPPING_CAPACITY when it cannot.
--
--  Every admitted mapping, refusal, revocation and shared claim is a kernel
--  log line ("platform-pool: mapped|refused|revoked|shared ..."), drained
--  into logstore.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Platform_Pool_ABI with Pure, SPARK_Mode is

   --  The pool capability's tag, as INSPECT_CAPABILITY reports it. A
   --  sibling of Firmware_Data_ABI.Firmware_Data_Tag ("FWMEM", 6).
   Platform_Pool_Tag : constant Unsigned_64 := 16#4657_4D45_4D00_0006#;

   --  MAP_DEVICE's arg3.
   Read_Write_Mode : constant := 0;
   Read_Only_Mode  : constant := 1;

   --  MAP_DEVICE's bound on one call.
   Maximum_Pages : constant := 1024;

   --  The declaration's ceilings (Platform_Pool_Sets.Member_Ceiling and
   --  Claim_Ceiling; the kernel checks they agree).
   Maximum_Members : constant := 2 ** 12;
   Maximum_Claims  : constant := 2 ** 16;

   --  A member's kind, as the Tag word of its INSTALL_CAPABILITY derivation
   --  (with Set_Tag). Codes are fixed; zero is never a kind.
   type Pool_Member_Kind is
     (Device_BAR,        --  a PCI function's BAR: a bound driver's claim revokes
      Chipset_Shared);   --  a chipset block: a driver's claim is shared
   for Pool_Member_Kind use (Device_BAR => 1, Chipset_Shared => 2);

   function Member_Kind_Word (Kind : Pool_Member_Kind) return Unsigned_64 is
     (Unsigned_64 (Pool_Member_Kind'Enum_Rep (Kind)));

   function Member_Kind_Name (Kind : Pool_Member_Kind) return String is
     (case Kind is
        when Device_BAR     => "DEVICE_BAR",
        when Chipset_Shared => "CHIPSET_SHARED");

   --  Why a pool mapping was refused. Codes are fixed.
   type Refusal is
     (Invalid_Request,     --  malformed range, mode or virtual address
      Not_Covered,         --  outside the capability's range or rights
      Outside_Pool,        --  not wholly inside one pool interval
      Claimed,             --  in a Device_BAR member another process holds
      Mapping_Capacity,    --  the holder's mapping record could not grow
      Mapping_Failed);     --  a page could not be mapped (no page table)
   for Refusal use
     (Invalid_Request  => 1,
      Not_Covered      => 2,
      Outside_Pool     => 3,
      Claimed          => 4,
      Mapping_Capacity => 5,
      Mapping_Failed   => 6);

   function Refusal_Word (Why : Refusal) return Unsigned_64 is
     (Unsigned_64 (Refusal'Enum_Rep (Why)));

   function Name (Why : Refusal) return String is
     (case Why is
        when Invalid_Request  => "INVALID_REQUEST",
        when Not_Covered      => "NOT_COVERED",
        when Outside_Pool     => "OUTSIDE_POOL",
        when Claimed          => "CLAIMED",
        when Mapping_Capacity => "MAPPING_CAPACITY",
        when Mapping_Failed   => "MAPPING_FAILED");

   --  The refusal, as words: Holder is the requester's process identity,
   --  Claimant the identity holding the overlapping claim (Claimed) and
   --  Claim_First_Page/Claim_Pages its pages; Member_First_Page/
   --  Member_Pages the pool interval holding the request (Claimed), else 0.
   type Refusal_Field is
     (Code, Holder, Claimant, Claim_First_Page, Claim_Pages,
      Member_First_Page, Member_Pages);
   type Refusal_Record is array (Refusal_Field) of Unsigned_64;
   Refusal_Record_Bytes : constant := 7 * 8;

end Platform_Pool_ABI;
