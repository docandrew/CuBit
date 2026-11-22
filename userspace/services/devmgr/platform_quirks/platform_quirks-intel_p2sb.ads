------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Intel P2SB (Primary to Sideband bridge, PCI 00:1F.1): its registers,
--  the decision whether and how to read its sideband BAR (SBREG_BAR), and
--  the typed outcome of one discovery, step by step (ACPI-POOL-001).
--
--  @description
--  Pure decoding and classification, proved (tests/platform-pool,
--  tests/platform-quirks). The config-space protocol itself is
--  Platform_Quirks.Sideband (the only code that touches 00:1F.1).
--
--  The protocol is Linux drivers/platform/x86/p2sb.c (torvalds/linux
--  master, last changed by 360c400d0f56, 2024-12), which every Linux boot
--  of an Intel PCH runs at fs_initcall_sync:
--    "#define P2SBC 0xe0", "#define P2SBC_HIDE BIT(8)";
--    "#define P2SB_DEVFN_DEFAULT PCI_DEVFN(31, 1)" (Goldmont and Goldmont
--      Plus use 13.0; Alder Lake-N / Twin Lake use the default, as
--      coreboot alderlake/include/soc/pci_devs.h PCH_DEVFN_P2SB
--      "_PCH_DEVFN(ESPI, 1)" agrees);
--    leave the function alone when "its device class is not
--      PCI_CLASS_MEMORY_OTHER";
--    read P2SBC; only if HIDE is set: "pci_bus_write_config_dword(bus,
--      devfn, P2SBC, 0)", scan the function (pci_scan_single_device, which
--      gives up when the vendor ID reads all-ones), copy BAR0, then
--      "pci_bus_write_config_dword(bus, devfn, P2SBC, P2SBC_HIDE)".
--  Linux reaches offsets below 256 of segment 0 through CF8/CFC
--  (arch/x86/pci/common.c raw_pci_read/raw_pci_write: "if (domain == 0 &&
--  reg < 256 && raw_pci_ops)"), as devmgr does; the values written are the
--  same explicit 0 and HIDE, not a read-modify-write. coreboot
--  (common/block/p2sb/p2sblib.c) writes the hide bit as byte 0xE1 bit 0,
--  the same bit, and checks "pci_vid == 0xffff" for hidden.
--  Intel's 500-series PCH datasheet (631120) documents P2SBC.HIDE as "When
--  this bit is set, the P2SB will return 1s on any PCI Configuration Read";
--  its lock bits are MASKLOCK (bit 17, EPMASK registers) and SBILOCK (bit
--  31, the SBI registers D0h-DFh): none locks HIDE itself.
--
--  The SBREG window is 16 MiB: coreboot alderlake/Kconfig PCR_BASE_ADDRESS
--  "default 0xfd000000" (0xe0000000 only for PCH-S), and the IdeaPad's own
--  PNP0C02 _CRS "LEN3 = ((SBRG + 0x01000000) - BAS3)" for a non-S PCH.
--
--  Each discovery ends in one Outcome, logged as one line naming the step
--  that decided it and every value read (Platform_Quirks.Sideband).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Devmgr_Platform_Pool; use Devmgr_Platform_Pool;

package Platform_Quirks.Intel_P2SB with Pure, SPARK_Mode is

   ---------------------------------------------------------------------------
   --  P2SB registers (PCI configuration space of 00:1F.1).
   ---------------------------------------------------------------------------
   P2SB_Bus : constant := 0;
   P2SB_Slot : constant := 31;
   P2SB_Function : constant := 1;
   PCI_Vendor_Offset : constant := 16#00#;               --  PCI_VENDOR_ID
   PCI_Device_Offset : constant := 16#02#;               --  PCI_DEVICE_ID
   PCI_Class_Offset : constant := 16#0A#;                --  PCI_CLASS_DEVICE
   P2SB_Control_Offset : constant := 16#E0#;             --  P2SBC
   P2SB_Hide : constant Unsigned_32 := 2 ** 8;           --  P2SBC_HIDE
   P2SB_Unhidden : constant Unsigned_32 := 0;            --  Linux's unhide write
   PCI_Class_Memory_Other : constant Unsigned_16 := 16#0580#;
   PCI_Absent : constant Unsigned_16 := 16#FFFF#;        --  a config read nobody answers
   PCI_Absent_32 : constant Unsigned_32 := 16#FFFF_FFFF#;
   PCI_BAR0_Offset : constant := 16#10#;
   PCI_BAR1_Offset : constant := 16#14#;
   SBREG_Bytes : constant := 16#100_0000#;               --  16 MiB

   type P2SB_Action is
     (Leave_Alone,     --  something else answers at 00:1F.1, or nothing does: never touched
      Read_Visible,    --  firmware left it visible: read BAR0 directly
      Unhide_Read);    --  hidden: unhide, read BAR0, hide again

   --  Linux p2sb_cache_resources' decision from the class word and P2SBC.
   function Action_Of (Class : Unsigned_16; Control : Unsigned_32) return P2SB_Action is
     (if Class /= PCI_Absent and then Class /= PCI_Class_Memory_Other then Leave_Alone
      elsif (Control and P2SB_Hide) /= 0 then Unhide_Read
      elsif Class = PCI_Absent then Leave_Alone
      else Read_Visible);

   --  SBREG_BAR from BAR0/BAR1: a memory BAR (32- or 64-bit), nonzero,
   --  16 MiB aligned and below the limit: Found, 16 MiB. A hidden device's
   --  all-ones read is Not_Placed.
   PCI_BAR_IO        : constant Unsigned_32 := 2 ** 0;
   PCI_BAR_Type_Mask : constant Unsigned_32 := 2#110#;
   PCI_BAR_Type_32   : constant Unsigned_32 := 2#000#;
   PCI_BAR_Type_64   : constant Unsigned_32 := 2#100#;
   PCI_BAR_Flag_Mask : constant Unsigned_32 := 16#F#;

   function BAR_Is_Memory (BAR0 : Unsigned_32) return Boolean is
     ((BAR0 and PCI_BAR_IO) = 0
      and then ((BAR0 and PCI_BAR_Type_Mask) = PCI_BAR_Type_32
                or else (BAR0 and PCI_BAR_Type_Mask) = PCI_BAR_Type_64));

   function Decode_SBREG (BAR0, BAR1 : Unsigned_32) return Block
   with Post => (if Decode_SBREG'Result.Status = Found then
                   Whole_Pages (Decode_SBREG'Result)
                   and then Decode_SBREG'Result.Bytes = SBREG_Bytes
                   and then Decode_SBREG'Result.Base mod SBREG_Bytes = 0
                   and then Decode_SBREG'Result.Base /= 0
                   and then (BAR0 and PCI_BAR_IO) = 0)
                and then (if not BAR_Is_Memory (BAR0) then
                            Decode_SBREG'Result.Status = Not_Placed);

   ---------------------------------------------------------------------------
   --  One discovery, step by step: what was read, and the typed outcome.
   ---------------------------------------------------------------------------

   --  The steps, in order. Each outcome names the step that decided it.
   type Step is
     (Identify,          --  class word and P2SBC read; Action_Of decides
      Unhide,            --  P2SBC := 0, then P2SBC read back
      Confirm_Identity,  --  vendor / device / class of the unhidden function
      Read_BAR,          --  BAR0 and BAR1 (SBREG_BAR, SBREG_BARH)
      Rehide);           --  P2SBC := HIDE, then the vendor ID read back

   type Outcome is
     (Found_Visible,         --  firmware left it visible; the window decoded
      Found_Unhidden,        --  hidden, unhidden, read, hidden again; decoded
      Other_Function,        --  Identify: 00:1F.1 is another class; left alone
      Absent,                --  Identify: nothing answers and HIDE reads clear
      Unhide_Ignored,        --  Unhide: P2SBC still reads all-ones after the
                             --  0 write: the function stayed hidden
      Unhidden_Not_P2SB,     --  Confirm_Identity: something answered, but not
                             --  an Intel class 0580h function
      BAR_Not_Memory,        --  Read_BAR: an I/O or reserved-type BAR
      BAR_Not_Placed,        --  Read_BAR: zero, or not 16 MiB aligned
      BAR_Beyond_Limit);     --  Read_BAR: past the physical limit

   subtype Found_Outcome is Outcome range Found_Visible .. Found_Unhidden;

   --  Everything one discovery read. A field the path did not read keeps
   --  its Not_Read value (all-ones, as a read nobody answers).
   type Observation is record
      Action            : P2SB_Action := Leave_Alone;
      Class             : Unsigned_16 := PCI_Absent;
      Control           : Unsigned_32 := PCI_Absent_32;
      Control_Unhidden  : Unsigned_32 := PCI_Absent_32;
      Vendor_Unhidden   : Unsigned_16 := PCI_Absent;
      Device_Unhidden   : Unsigned_16 := PCI_Absent;
      Class_Unhidden    : Unsigned_16 := PCI_Absent;
      BAR0, BAR1        : Unsigned_32 := PCI_Absent_32;
      Vendor_After_Hide : Unsigned_16 := PCI_Absent;
   end record;

   --  The outcome of a discovery that read Obs.
   function Classify (Obs : Observation) return Outcome
   with Post =>
     (if Classify'Result in Found_Outcome then
        Decode_SBREG (Obs.BAR0, Obs.BAR1).Status = Found
        and then Obs.Action /= Leave_Alone
        and then (Classify'Result = Found_Unhidden) = (Obs.Action = Unhide_Read)
        and then (if Obs.Action = Unhide_Read then
                    Obs.Vendor_Unhidden = Intel_Vendor
                    and then Obs.Class_Unhidden = PCI_Class_Memory_Other))
     and then (if Obs.Action = Leave_Alone then Classify'Result in Other_Function | Absent)
     and then (Classify'Result = Unhide_Ignored) =
                (Obs.Action = Unhide_Read and then Obs.Control_Unhidden = PCI_Absent_32
                 and then Obs.Vendor_Unhidden = PCI_Absent);

   --  The step whose evidence decided an outcome.
   function Deciding_Step (O : Outcome) return Step is
     (case O is
        when Other_Function | Absent => Identify,
        when Unhide_Ignored => Unhide,
        when Unhidden_Not_P2SB => Confirm_Identity,
        when BAR_Not_Memory | BAR_Not_Placed | BAR_Beyond_Limit | Found_Outcome => Read_BAR);

   --  The window, when the outcome found one.
   function Window_Of (Obs : Observation) return Block is
     (if Classify (Obs) in Found_Outcome then Decode_SBREG (Obs.BAR0, Obs.BAR1)
      else (Status => Not_Placed, Base => 0, Bytes => 0))
   with Post => (if Window_Of'Result.Status = Found then
                   Whole_Pages (Window_Of'Result)
                   and then Window_Of'Result.Bytes = SBREG_Bytes);

   --  After an Unhide_Read, the function answered again once hidden: the
   --  hide write did not take (logged; the window, if found, still stands).
   function Still_Visible_After_Hide (Obs : Observation) return Boolean is
     (Obs.Action = Unhide_Read and then Obs.Vendor_After_Hide /= PCI_Absent);

   function Outcome_Name (O : Outcome) return String is
     (case O is
        when Found_Visible     => "FOUND_VISIBLE",
        when Found_Unhidden    => "FOUND_UNHIDDEN",
        when Other_Function    => "OTHER_FUNCTION",
        when Absent            => "ABSENT",
        when Unhide_Ignored    => "UNHIDE_IGNORED",
        when Unhidden_Not_P2SB => "UNHIDDEN_NOT_P2SB",
        when BAR_Not_Memory    => "BAR_NOT_MEMORY",
        when BAR_Not_Placed    => "BAR_NOT_PLACED",
        when BAR_Beyond_Limit  => "BAR_BEYOND_LIMIT");

   function Step_Name (S : Step) return String is
     (case S is
        when Identify         => "identify",
        when Unhide           => "unhide",
        when Confirm_Identity => "confirm-identity",
        when Read_BAR         => "read-bar",
        when Rehide           => "rehide");

end Platform_Quirks.Intel_P2SB;
