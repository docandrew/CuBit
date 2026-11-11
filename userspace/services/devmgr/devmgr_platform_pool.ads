------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  devmgr's independent discovery of platform register blocks for the
--  platform-register pool (ACPI-POOL-001): which host bridges it knows, and
--  where their register blocks are, read from PCI configuration space.
--
--  @description
--  The pool is what the ACPI pair may map at addresses AML computes; it must
--  come from devmgr's own reading of the hardware, never from AML. The flow
--  (ACPI-POOL-001; in full in the kernel's Platform_Pool spec): (1) devmgr
--  discovers these blocks and ADDS them to the pool before ACPI starts,
--  because _INI methods touch them, and maps none of them; (2) the ACPI
--  backend maps what AML touches, checked once by the kernel; (3) after
--  _INI the ACPI service reports devices' _CRS resources and devmgr
--  cross-checks and binds drivers from them (future work); (4) a driver's
--  claim of a chipset block is shared with firmware, and only PCI device
--  BARs are revoked from ACPI. First
--  case: the IdeaPad's TcssSsdt computes MBAR = GMHB () + REGO, GMHB being
--  the host bridge's MCHBAR (00:00.0 offset 0x48).
--
--  Known host bridges: Intel Alder Lake-N (the IdeaPad's i3-N305 and the
--  GMKtec N95), the device IDs both of these list as Alder Lake-N host
--  bridges:
--    * Linux drivers/edac/igen6_edac.c (DID_ADL_N_SKU1..), whose adl_cfg
--      ("Shared by Alder Lake, Alder Lake-N, Arizona Beach, Amston Lake,
--      and Raptor Lake-P") reads MCHBAR at 0x48/0x4C, enable BIT(0), base
--      GENMASK_ULL (41, 17);
--    * coreboot src/include/device/pci_ids.h PCI_DID_INTEL_ADL_N_ID_*,
--      bound by src/soc/intel/common/block/systemagent/systemagent.c
--      (MCHBAR 0x48, intelblocks/systemagent.h); MCHBAR's window is
--      MCH_BASE_SIZE 0x20000 in src/soc/intel/alderlake/include/soc/
--      iomap.h (after Intel's Alder Lake FAS, doc 626540, chapter 4).
--  The window is 128 KiB, the alignment the base field (bit 17 up) gives
--  (igen6 maps two 64 KiB memory-controller windows from the base). The
--  laptop's own DSDT agrees on the register and enable bit (Field HBUS at
--  0x48: MHEN 1 bit, MHBR bits 31:15). Intel's datasheet volume 2 could
--  not be fetched (2026-11-09), so the IDs are the ones both sources list.
--  Every other host bridge is refused (Unknown_Host_Bridge, with its IDs in
--  the log): no guessing at register layouts.
--
--  Test profile: QEMU's q35 host bridge (8086:29C0) has no MCHBAR. Only
--  in an image that carries the declared test-profile file, devmgr stands
--  the BAR0 of QEMU's edu device (1234:11E8, which no CuBit driver binds)
--  in for the register blocks: its first 128 KiB for MCHBAR, the rest for
--  a sideband window with one 64 KiB community at Test_Community_Offset
--  (so the carving and a community claim run in the headless test).
--  Without the profile, q35 is an unknown bridge like any other.
--
--  Alder Lake-N's other register blocks (ACPI-POOL-001 phase 2):
--    * PMC (PWRMBASE): from the LPIT, a static table (never AML). Linux
--      drivers/platform/x86/intel/pmc/core.c get_primary_reg_base:
--        "pmc->base_addr = slp_s0_addr - pmc->map->slp_s0_offset;"
--      where slp_s0_addr is the first enabled native C-state's
--      SystemMemory residency counter (drivers/acpi/acpi_lpit.c
--      lpit_process) and, for INTEL_ATOM_GRACEMONT (Alder Lake-N),
--      tgl_l_pmc_dev / tgl_reg_map ".slp_s0_offset =
--      CNP_PMC_SLP_S0_RES_COUNTER_OFFSET" (0x193C, core.h). The window is
--      coreboot's "PCH_PWRM_BASE_SIZE 0x10000" (alderlake/iomap.h), so the
--      base must be 64 KiB aligned. The IdeaPad's LPIT (UID 1) names
--      0xFE00193C: PWRMBASE 0xFE000000, coreboot's PCH_PWRM_BASE_ADDRESS.
--      Linux falls back to a fixed 0xFE000000 without an LPIT; devmgr
--      does not guess (No_Source, logged).
--    * The P2SB sideband window and its GPIO communities: devmgr's
--      sideband subsystem (Devmgr_Sideband, layout in
--      Devmgr_Sideband_Layout), which owns P2SB (00:1F.1); the pool asks
--      it for the window and the communities.
--  Member kinds (Platform_Pool_ABI.Pool_Member_Kind): every chipset block above
--  is Chipset_Shared (a driver's claim is shared: AML keeps its mappings);
--  a firmware-reserved piece is Chipset_Shared unless a PCI device's BAR
--  devmgr claimed overlaps it (then Device_BAR: the bound driver's claim
--  revokes); the test profile's edu BAR stand-ins are Device_BAR except the
--  community, which is Chipset_Shared.
--
--  Sizes: devmgr declares the pool's member count (what it found) and
--  claim count (Claim_Room: one per member plus one per PCI BAR extent it
--  claimed for devices) to the kernel before installing anything.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Platform_Pool_ABI;

package Devmgr_Platform_Pool with Pure, SPARK_Mode is

   Page_Bytes : constant := 4096;
   --  The kernel's physical limit (Physical_Ranges.Address_Limit).
   Byte_Limit : constant := 2 ** 47;

   --  PCI identities.
   Intel_Vendor : constant Unsigned_16 := 16#8086#;
   QEMU_Edu_Vendor : constant Unsigned_16 := 16#1234#;
   QEMU_Edu_Device : constant Unsigned_16 := 16#11E8#;
   QEMU_Q35_Host_Bridge : constant Unsigned_16 := 16#29C0#;

   --  Alder Lake-N host bridges (00:00.0), both sources above.
   ADL_N_IDs : constant array (Positive range <>) of Unsigned_16 :=
     [16#4614#, 16#4617#, 16#461B#, 16#461C#, 16#4677#, 16#4678#, 16#4679#, 16#467C#];

   --  Host bridge configuration registers (00:00.0).
   MCHBAR_Low_Offset  : constant := 16#48#;
   MCHBAR_High_Offset : constant := 16#4C#;
   MCHBAR_Enable      : constant Unsigned_64 := 2 ** 0;
   MCHBAR_Lowest_Bit  : constant := 17;
   MCHBAR_Highest_Bit : constant := 41;
   MCHBAR_Base_Mask   : constant Unsigned_64 :=
     (2 ** (MCHBAR_Highest_Bit + 1) - 1) - (2 ** MCHBAR_Lowest_Bit - 1);
   MCHBAR_Bytes       : constant := 2 ** MCHBAR_Lowest_Bit;   --  128 KiB

   Edu_BAR0_Bytes : constant := 16#10_0000#;   --  edu's BAR0 (1 MiB, QEMU docs/specs/edu.rst)

   type Bridge_Kind is (Alder_Lake_N, QEMU_Q35_Test, Unknown);

   function Known_ADL_N (Device : Unsigned_16) return Boolean is
     (for some D of ADL_N_IDs => D = Device);

   --  Which host bridge this is. QEMU_Q35_Test only with the declared test
   --  profile; never for real hardware's IDs.
   function Bridge_Of
     (Vendor, Device : Unsigned_16; Test_Profile : Boolean) return Bridge_Kind
   is (if Vendor = Intel_Vendor and then Known_ADL_N (Device) then Alder_Lake_N
       elsif Test_Profile and then Vendor = Intel_Vendor
             and then Device = QEMU_Q35_Host_Bridge then QEMU_Q35_Test
       else Unknown);

   type Block_Status is (Found, Unknown_Host_Bridge, Disabled, Not_Placed, Beyond_Limit, No_Source);

   --  A register block: whole pages [Base, Base + Bytes) below the limit.
   type Block is record
      Status : Block_Status := Unknown_Host_Bridge;
      Base   : Unsigned_64 := 0;
      Bytes  : Unsigned_64 := 0;
   end record;

   function Whole_Pages (B : Block) return Boolean is
     (B.Bytes > 0 and then B.Base mod Page_Bytes = 0 and then B.Bytes mod Page_Bytes = 0
      and then B.Base < Byte_Limit and then B.Bytes <= Byte_Limit - B.Base);

   --  MCHBAR from its two configuration dwords: Found (enabled, nonzero,
   --  below the limit, 128 KiB-aligned by its mask), Disabled (enable bit
   --  clear), Not_Placed (zero base) or Beyond_Limit.
   function Decode_MCHBAR (Low, High : Unsigned_32) return Block
   with Post => (if Decode_MCHBAR'Result.Status = Found then
                   Whole_Pages (Decode_MCHBAR'Result)
                   and then Decode_MCHBAR'Result.Bytes = MCHBAR_Bytes
                   and then Decode_MCHBAR'Result.Base mod MCHBAR_Bytes = 0
                   and then Decode_MCHBAR'Result.Base /= 0
                   and then ((Shift_Left (Unsigned_64 (High), 32) or Unsigned_64 (Low))
                              and MCHBAR_Enable) /= 0
                   and then Decode_MCHBAR'Result.Base =
                     ((Shift_Left (Unsigned_64 (High), 32) or Unsigned_64 (Low))
                      and MCHBAR_Base_Mask));

   --  The test profile's stand-in for MCHBAR: the first 128 KiB of the
   --  edu device's BAR0 (whole pages, below the limit, BAR0 the 1 MiB edu
   --  has), or Not_Placed.
   function Test_Block (BAR_Base, BAR_Bytes : Unsigned_64) return Block
   with Post => (if Test_Block'Result.Status = Found then
                   Whole_Pages (Test_Block'Result)
                   and then BAR_Bytes = Edu_BAR0_Bytes
                   and then Test_Block'Result.Base = BAR_Base
                   and then Test_Block'Result.Bytes = MCHBAR_Bytes);

   --  The host bridge's register block: Unknown_Host_Bridge for every
   --  bridge Bridge_Of does not know.
   function Discover
     (Kind : Bridge_Kind; MCHBAR_Low, MCHBAR_High : Unsigned_32;
      Test_BAR_Base, Test_BAR_Bytes : Unsigned_64) return Block
   with Post => (if Kind = Unknown then Discover'Result.Status = Unknown_Host_Bridge)
                and then (if Discover'Result.Status = Found then Whole_Pages (Discover'Result))
                and then (if Kind = Alder_Lake_N then
                            Discover'Result = Decode_MCHBAR (MCHBAR_Low, MCHBAR_High));

   ---------------------------------------------------------------------------
   --  The PMC (PWRMBASE), from the LPIT's residency counter.
   ---------------------------------------------------------------------------
   --  tgl_reg_map's slp_s0_offset (CNP_PMC_SLP_S0_RES_COUNTER_OFFSET).
   PMC_SLP_S0_Counter_Offset : constant := 16#193C#;
   PWRM_Bytes : constant := 16#1_0000#;   --  PCH_PWRM_BASE_SIZE

   type Byte_Array is array (Natural range <>) of Unsigned_8;

   --  LPIT layout (Intel "Low Power Idle Table" spec, rev 001; Linux
   --  include/acpi/actbl2.h struct acpi_lpit_native): the 36-byte table
   --  header, then descriptors: type (u32, 0 = native C-state), length
   --  (u32), unique id (u16), reserved (u16), flags (u32, bit 0 disabled),
   --  entry trigger (GAS, 12 bytes), residency (u32), latency (u32),
   --  residency counter (GAS at 36: space id u8, bit width u8, bit offset
   --  u8, access size u8, address u64), counter frequency (u64).
   LPIT_Header_Bytes       : constant := 36;
   LPIT_Native_Bytes       : constant := 56;
   LPIT_Type_Offset        : constant := 0;
   LPIT_Length_Offset      : constant := 4;
   LPIT_Flags_Offset       : constant := 12;
   LPIT_Counter_Offset     : constant := 36;
   LPIT_Counter_Address    : constant := LPIT_Counter_Offset + 4;
   LPIT_Native_C_State     : constant := 0;
   GAS_System_Memory       : constant := 0;
   LPIT_Table_Length_Offset : constant := 4;
   LPIT_Signature : constant String := "LPIT";

   --  Little-endian Count bytes at From.
   function Little (Bytes : Byte_Array; From, Count : Natural) return Unsigned_64
   with Pre => Count in 1 .. 8 and then Bytes'Length > 0
               and then From >= Bytes'First and then From <= Bytes'Last
               and then Count - 1 <= Bytes'Last - From;

   --  Linux lpit_process: the first descriptor of type 0 with no flags
   --  whose residency counter is SystemMemory, its address; 0 if none (or
   --  the table is short or malformed: a descriptor shorter than a native
   --  one ends the walk).
   function LPIT_Memory_Counter (Table : Byte_Array) return Unsigned_64
   with Pre => Table'First = 0 and then Table'Last < Natural'Last - LPIT_Native_Bytes;

   --  PWRMBASE from that counter: Found when the counter lies
   --  PMC_SLP_S0_Counter_Offset into a 64 KiB-aligned window below the
   --  limit; No_Source for 0; Not_Placed otherwise.
   function PWRM_Block (Counter : Unsigned_64) return Block
   with Post => (if PWRM_Block'Result.Status = Found then
                   Whole_Pages (PWRM_Block'Result)
                   and then PWRM_Block'Result.Bytes = PWRM_Bytes
                   and then PWRM_Block'Result.Base mod PWRM_Bytes = 0
                   and then PWRM_Block'Result.Base + PMC_SLP_S0_Counter_Offset = Counter)
                and then (if Counter = 0 then PWRM_Block'Result.Status = No_Source);

   ---------------------------------------------------------------------------
   --  The pool's declared sizes (Platform_Pool_ABI): members as found (at
   --  most the ceiling), and a claim for every member and every PCI BAR
   --  extent devmgr claimed for devices (each a device a driver may bind),
   --  at most the ceiling. Neither is ever zero.
   ---------------------------------------------------------------------------
   function Member_Room (Found : Natural) return Unsigned_64 is
     (Unsigned_64 (Natural'Max (1, Natural'Min (Found, Platform_Pool_ABI.Maximum_Members))))
   with Post => Member_Room'Result in 1 .. Platform_Pool_ABI.Maximum_Members;

   function Claim_Room (Members, Device_Extents : Natural) return Unsigned_64 is
     (Unsigned_64 (Natural'Max (1, Natural'Min
        (Natural'Min (Members, Platform_Pool_ABI.Maximum_Claims)
         + Natural'Min (Device_Extents, Platform_Pool_ABI.Maximum_Claims),
         Platform_Pool_ABI.Maximum_Claims))))
   with Post => Claim_Room'Result in 1 .. Platform_Pool_ABI.Maximum_Claims;

   --  Whole pages [First, First + Bytes).
   type Span is record
      First, Bytes : Unsigned_64 := 0;
   end record;

   function Valid (S : Span) return Boolean is
     (S.Bytes > 0 and then S.First mod Page_Bytes = 0 and then S.Bytes mod Page_Bytes = 0
      and then S.First < Byte_Limit and then S.Bytes <= Byte_Limit - S.First);

   function Disjoint (A, B : Span) return Boolean is
     (A.First + A.Bytes <= B.First or else B.First + B.Bytes <= A.First)
   with Pre => Valid (A) and then Valid (B);

   --  What of R lies outside Taken: below it (Low) and above it (High),
   --  each empty (Bytes 0) or valid and disjoint from Taken, together R
   --  minus Taken.
   procedure Outside (R, Taken : Span; Low, High : out Span)
   with Pre  => Valid (R) and then Valid (Taken),
        Post => (if Low.Bytes /= 0 then
                   Valid (Low) and then Disjoint (Low, Taken)
                   and then Low.First = R.First
                   and then Low.First + Low.Bytes <= R.First + R.Bytes)
                and then (if High.Bytes /= 0 then
                            Valid (High) and then Disjoint (High, Taken)
                            and then High.First >= R.First
                            and then High.First + High.Bytes = R.First + R.Bytes)
                and then (if Disjoint (R, Taken) then Low = R and then High.Bytes = 0);

end Devmgr_Platform_Pool;
