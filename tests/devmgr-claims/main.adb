--  Hosted tests of devmgr's claim ledgers on the heap (LIM-001b:
--  Devmgr_Port_Claims, Devmgr_Memory_Claims over Devmgr_Growable through
--  Devmgr_Claim_Ledgers) and of its firmware-data decision
--  (Devmgr_Data_Windows). Every bound the old fixed ledgers had is passed:
--  more than 8 firmware windows, more than 48 port claims, a driver's ports
--  released at exit and claimed again, and a heap that refuses on purpose.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Interfaces; use Interfaces;
with Test_Heap;
with Devmgr_Port_Claims;
with Devmgr_Memory_Claims;
with Devmgr_Claim_Ledgers;
with Devmgr_Data_Windows;
with PCI_BAR_Extents;
with CuBit.Firmware_Data_Exchange;

procedure Main is
   package Ledgers is new Devmgr_Claim_Ledgers (Test_Heap.Allocate, Test_Heap.Release);
   package PC renames Devmgr_Port_Claims;
   package MC renames Devmgr_Memory_Claims;
   package DW renames Devmgr_Data_Windows;
   package BX renames PCI_BAR_Extents;
   package FDX renames CuBit.Firmware_Data_Exchange;
   use type PC.Holder;
   use type PC.Claim;
   use type MC.Claim_Result;
   use type FDX.Disposition;
   use type Ledgers.Port_Store.Growth_Status;
   use type BX.BAR_Kind;
   use type BX.Extent_Status;
   use type BX.Extent;

   Checks, Failed : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failed := Failed + 1;
         Put_Line ("FAIL " & Name);
      end if;
   end Check;

   --  The old fixed bounds (Maximum_Claims): every test goes past them.
   Old_Port_Claims : constant := 48;
   Old_Memory_Claims : constant := 8;
   Page : constant := MC.Page_Bytes;

   Report : Ledgers.Capacity_Report;
   Granted : Boolean;
begin
   ---------------------------------------------------------------------------
   --  Ports: the old rules, unchanged.
   ---------------------------------------------------------------------------
   declare
      L : Ledgers.Port_Ledger;
      Driver_A : constant PC.Owner_Word := 16#1_0000_0021#;
      Driver_B : constant PC.Owner_Word := 16#1_0000_0022#;
   begin
      Ledgers.Initialize (L, Report);
      Check (Report.Status = Ledgers.Port_Store.Grown and then Ledgers.Count (L) = PC.Seed_Claims,
             "ports: the kernel's and devmgr's ranges seeded");
      Ledgers.Take (L, 16#600#, 4, PC.ACPI, PC.No_Owner, Granted, Report);
      Check (Granted, "ACPI window on free ports granted");
      Ledgers.Take (L, 16#600#, 4, PC.ACPI, PC.No_Owner, Granted, Report);
      Check (Granted, "the same ACPI window again (restarted backend) granted");
      Ledgers.Take (L, 16#602#, 4, PC.ACPI, PC.No_Owner, Granted, Report);
      Check (not Granted and then Report.Status = Ledgers.Port_Store.Grown,
             "a different overlapping ACPI window refused (not for capacity)");
      Ledgers.Take (L, 16#603#, 1, PC.Driver, Driver_A, Granted, Report);
      Check (not Granted, "a driver claim inside an ACPI window refused");
      Ledgers.Take (L, 16#70#, 2, PC.ACPI, PC.No_Owner, Granted, Report);
      Check (not Granted, "an ACPI window over devmgr's CMOS ports refused");
      Ledgers.Take (L, 16#3F8#, 1, PC.ACPI, PC.No_Owner, Granted, Report);
      Check (not Granted, "an ACPI window over the kernel's console refused");
      Ledgers.Take (L, 16#1F0#, 8, PC.Driver, Driver_A, Granted, Report);
      Check (Granted, "a driver claim elsewhere granted");
      declare
         Before : constant Natural := Ledgers.Count (L);
      begin
         Ledgers.Take (L, 16#1F0#, 8, PC.Driver, Driver_A, Granted, Report);
         Check (Granted and then Ledgers.Count (L) = Before,
                "the same driver claim again granted, not recorded twice");
      end;
      Ledgers.Take (L, 16#1F0#, 8, PC.Driver, Driver_B, Granted, Report);
      Check (Granted, "another driver's overlapping claim granted (devmgr's own concern)");
      Ledgers.Take (L, 16#1F4#, 1, PC.ACPI, PC.No_Owner, Granted, Report);
      Check (not Granted, "an ACPI window over a driver's ports refused");
      Ledgers.Take (L, 16#FFFF#, 2, PC.ACPI, PC.No_Owner, Granted, Report);
      Check (not Granted, "a range past the port space refused");
      Ledgers.Take (L, 16#800#, 0, PC.ACPI, PC.No_Owner, Granted, Report);
      Check (not Granted, "an empty range refused");

      --  LIM-001b: far past the old 48, every claim is granted.
      for K in 1 .. 5 * Old_Port_Claims loop
         Ledgers.Take (L, Unsigned_64 (16#2000# + 4 * K), 1, PC.Driver,
                       16#2_0000_0000# + PC.Owner_Word (K), Granted, Report);
         exit when not Granted;
      end loop;
      Check (Granted and then Ledgers.Count (L) > 5 * Old_Port_Claims,
             "more than 48 port claims, all granted");
      Ledgers.Take (L, 16#9000#, 1, PC.ACPI, PC.No_Owner, Granted, Report);
      Check (Granted, "a new ACPI window past the old full ledger granted");
      Ledgers.Take (L, 16#600#, 4, PC.ACPI, PC.No_Owner, Granted, Report);
      Check (Granted, "a held ACPI window still re-granted");

      --  Release at exit, then claim again.
      declare
         Released : Natural;
         Before : constant Natural := Ledgers.Count (L);
      begin
         Ledgers.Take (L, 16#C000#, 16#20#, PC.Driver, Driver_A, Granted, Report);
         Check (Granted, "driver A takes a second range");
         Ledgers.Take (L, 16#C010#, 1, PC.ACPI, PC.No_Owner, Granted, Report);
         Check (not Granted, "ACPI refused over driver A's ports while it runs");
         Ledgers.Release_Owner (L, Driver_A, Released);
         Check (Released = 2 and then Ledgers.Count (L) = Before + 1 - 2,
                "driver A's two claims released at its exit");
         Check ((for all K in 1 .. Ledgers.Count (L) =>
                   not PC.Owned_By (Ledgers.Claim_At (L, K), Driver_A)),
                "no claim of driver A remains");
         Check ((for some K in 1 .. Ledgers.Count (L) =>
                   Ledgers.Claim_At (L, K) = (First => 16#1F0#, Ends => 16#1F8#, By => PC.Driver,
                                              Owner => Driver_B))
                and then (for some K in 1 .. Ledgers.Count (L) =>
                            Ledgers.Claim_At (L, K).By = PC.ACPI
                            and then Ledgers.Claim_At (L, K).First = 16#600#)
                and then (for some K in 1 .. Ledgers.Count (L) =>
                            Ledgers.Claim_At (L, K).By = PC.Kernel),
                "other drivers', ACPI's and the kernel's claims stay");
         Ledgers.Take (L, 16#C010#, 1, PC.ACPI, PC.No_Owner, Granted, Report);
         Check (Granted, "after the release, ACPI claims a port driver A held");
         Ledgers.Take (L, 16#C000#, 16#20#, PC.Driver, Driver_A, Granted, Report);
         Check (not Granted, "the restarted driver no longer gets ports now ACPI's");
         Ledgers.Take (L, 16#1F0#, 8, PC.Driver, Driver_A, Granted, Report);
         Check (Granted, "the restarted driver claims its old free range again");
         Ledgers.Release_Owner (L, 16#7_7777_7777#, Released);
         Check (Released = 0, "releasing an owner with no claims releases nothing");
      end;

      --  The heap refuses: the claim is not granted and the report names
      --  the demand, the capacity kept and the bytes asked for.
      declare
         Full : Natural := Ledgers.Count (L);
      begin
         --  Fill the current block exactly, then make the next growth fail.
         loop
            Test_Heap.Budget := 0;
            Ledgers.Take (L, 16#E000# + Unsigned_64 (Full mod 16#1000#), 1, PC.Driver,
                          16#3_0000_0000# + PC.Owner_Word (Full), Granted, Report);
            exit when not Granted;
            Full := Ledgers.Count (L);
         end loop;
         Check (Report.Status = Ledgers.Port_Store.Heap_Refused
                and then Report.Demand = Unsigned_64 (Full) + 1
                and then Report.Limit = Unsigned_64 (Full)
                and then Report.Requested > 0
                and then Ledgers.Count (L) = Full,
                "a refused growth: not granted, named demand and limit, nothing changed");
         declare
            Text : constant String := Ledgers.Capacity_Text ("port claims", Report);
         begin
            Check (Ada.Strings.Fixed.Index (Text, "CAPACITY port claims: demand") = 1
                   and then Ada.Strings.Fixed.Index (Text, "heap refused") > 0,
                   "the CAPACITY text names the ledger, the demand and the heap");
         end;
         Ledgers.Take (L, 16#600#, 4, PC.ACPI, PC.No_Owner, Granted, Report);
         Check (Granted, "a full ledger still re-grants a held window without the heap");
         Test_Heap.Budget := Unsigned_64'Last;
         Ledgers.Take (L, 16#E000#, 1, PC.Driver, 16#3_0000_0000# + PC.Owner_Word (Full),
                       Granted, Report);
         Check (Granted and then Ledgers.Count (L) = Full + 1, "with the heap back, it grows");
      end;
   end;

   ---------------------------------------------------------------------------
   --  Physical pages: ACPI windows, data envelopes and device claims.
   ---------------------------------------------------------------------------
   declare
      L : Ledgers.Memory_Ledger;
      Result : MC.Claim_Result;
      NVS_First : constant := 16#3EB7_F000#;
      NVS_Bytes : constant := 16#41B2_F000# - 16#3EB7_F000#;
      FACS_Page : constant := 16#0FFE_0000#;
   begin
      Ledgers.Claim (L, NVS_First, NVS_Bytes, MC.ACPI_Window, Result, Report);
      Check (Result = MC.Granted, "an NVS run claimed for ACPI");
      Ledgers.Claim (L, FACS_Page, Page, MC.ACPI_Window, Result, Report);
      Check (Result = MC.Granted, "the FACS page claimed for ACPI");
      Ledgers.Claim (L, NVS_First, NVS_Bytes, MC.ACPI_Window, Result, Report);
      Check (Result = MC.Granted and then Ledgers.Count (L) = 2,
             "the same window again granted, not duplicated");
      Ledgers.Claim (L, NVS_First + Page, Page, MC.ACPI_Window, Result, Report);
      Check (Result = MC.Overlaps_ACPI, "a different window inside an ACPI claim refused");
      Ledgers.Claim (L, NVS_First - Page, 2 * Page, MC.ACPI_Window, Result, Report);
      Check (Result = MC.Overlaps_ACPI, "a window overlapping an ACPI claim's first page refused");
      Ledgers.Claim (L, 16#5000_0800#, Page, MC.ACPI_Window, Result, Report);
      Check (Result = MC.Malformed, "an unaligned window refused");
      Ledgers.Claim (L, 16#5000_0000#, 16#800#, MC.ACPI_Window, Result, Report);
      Check (Result = MC.Malformed, "a partial page refused");
      Ledgers.Claim (L, 16#5000_0000#, 0, MC.ACPI_Window, Result, Report);
      Check (Result = MC.Malformed, "an empty window refused");
      Ledgers.Claim (L, MC.Byte_Limit - Page, 2 * Page, MC.ACPI_Window, Result, Report);
      Check (Result = MC.Malformed, "a window past the physical limit refused");
      Check (Ledgers.Clear_Of_ACPI (L, 16#FEB0_0000#, 16#4000#), "a BAR elsewhere is clear");
      Check (not Ledgers.Clear_Of_ACPI (L, NVS_First - 1, 2),
             "a range one byte into an ACPI claim is not clear");
      Check (not Ledgers.Clear_Of_ACPI (L, FACS_Page + 16#40#, 4),
             "bytes inside the FACS page are not clear");
      Check (not Ledgers.Clear_Of_ACPI (L, 16#FEB0_0000#, 0)
             and then not Ledgers.Clear_Of_ACPI (L, Unsigned_64'Last, 2),
             "empty and wrapping ranges are not clear");
      --  LIM-001b: far more firmware windows than the old 8.
      for K in 1 .. 6 * Old_Memory_Claims loop
         Ledgers.Claim (L, Unsigned_64 (16#6000_0000# + K * 16#10_0000#), Page,
                        MC.ACPI_Window, Result, Report);
         exit when Result /= MC.Granted;
      end loop;
      Check (Result = MC.Granted and then Ledgers.Count (L) = 2 + 6 * Old_Memory_Claims,
             "more than 8 firmware windows, all claimed");

      --  Device claims: partial pages cover their pages; never ACPI's.
      Ledgers.Claim (L, 16#FEB0_0010#, 16#20#, MC.Device, Result, Report);
      Check (Result = MC.Granted, "a device's register bytes claimed (their page)");
      Ledgers.Claim (L, FACS_Page + 16#80#, 16#10#, MC.Device, Result, Report);
      Check (Result = MC.Overlaps_ACPI, "a device range in the FACS page refused");
      Ledgers.Claim (L, 16#FEB0_0000#, 2 * Page, MC.Device, Result, Report);
      Check (Result = MC.Granted, "device claims may overlap each other");
      Check (not Ledgers.Clear_Of_Devices (L, 16#FEB0_0F00#, 4)
             and then Ledgers.Clear_Of_Devices (L, 16#FEB0_2000#, Page),
             "Clear_Of_Devices sees the device's pages");

      --  Firmware data: disjoint from windows and devices, may share pages
      --  with other data; then ACPI's against later devices.
      Ledgers.Claim (L, 16#FED1_C000#, Page, MC.ACPI_Data, Result, Report);
      Check (Result = MC.Granted, "a data envelope claimed");
      Ledgers.Claim (L, 16#FED1_C000#, Page, MC.ACPI_Data, Result, Report);
      Check (Result = MC.Granted, "the same envelope again granted");
      Ledgers.Claim (L, 16#FED1_C000#, 3 * Page, MC.ACPI_Data, Result, Report);
      Check (Result = MC.Granted, "an overlapping data envelope granted (shared page)");
      Ledgers.Claim (L, 16#FEB0_1000#, Page, MC.ACPI_Data, Result, Report);
      Check (Result = MC.Overlaps_Device, "a data envelope on a device's page refused");
      Ledgers.Claim (L, NVS_First, Page, MC.ACPI_Data, Result, Report);
      Check (Result = MC.Overlaps_ACPI, "a data envelope on an ACPI window refused");
      Ledgers.Claim (L, 16#FED1_D000#, 16#100#, MC.Device, Result, Report);
      Check (Result = MC.Overlaps_ACPI, "a later device range on granted data refused");
      Check (not Ledgers.Clear_Of_ACPI (L, 16#FED1_E000#, Page), "data pages are ACPI's");
      Ledgers.Claim (L, 16#FED1_C800#, Page, MC.ACPI_Data, Result, Report);
      Check (Result = MC.Malformed, "a partial data envelope refused");
      Ledgers.Claim (L, 16#FED1_C000#, Page, MC.ACPI_Window, Result, Report);
      Check (Result = MC.Overlaps_ACPI, "a window over a data envelope refused");

      --  The heap refuses a memory claim too.
      Test_Heap.Budget := 0;
      declare
         Before : constant Natural := Ledgers.Count (L);
      begin
         loop
            Ledgers.Claim (L, 16#8_0000_0000# + Unsigned_64 (Ledgers.Count (L)) * Page, Page,
                           MC.Device, Result, Report);
            exit when Result /= MC.Granted;
         end loop;
         Check (Result = MC.Needs_Room and then Report.Status = Ledgers.Port_Store.Heap_Refused
                and then Ledgers.Count (L) >= Before,
                "memory: a refused growth is Needs_Room with a named report");
      end;
      Test_Heap.Budget := Unsigned_64'Last;
      Ledgers.Claim (L, 16#9_0000_0000#, Page, MC.Device, Result, Report);
      Check (Result = MC.Granted, "memory: with the heap back, it grows");
   end;

   ---------------------------------------------------------------------------
   --  The firmware-data decision on a q35-like layout.
   ---------------------------------------------------------------------------
   declare
      function R (First, Pages : Unsigned_64) return DW.Page_Range is
        ((First_Page => First / Page, Pages => Pages));
      Top_First : constant := 16#7FFE_0000#;   --  SeaBIOS's reserved top
      ECAM      : constant := 16#B000_0000#;
      MiB       : constant := 16#10_0000#;
      NVS_First : constant := 16#3EB7_F000#;
      Firmware : constant DW.Root_Array :=
        [(Pages => R (NVS_First, 16), Kind => DW.NVS_Root),
         (Pages => R (Top_First, 1), Kind => DW.Control_Root),
         (Pages => R (ECAM, 256), Kind => DW.PCI_Config_Root),
         (Pages => R (Top_First + 3 * Page, 29), Kind => DW.Data_Root),
         (Pages => R (ECAM + MiB, 65_280), Kind => DW.Data_Root),
         (Pages => R (16#FED1_C000#, 4), Kind => DW.Data_Root),
         (Pages => R (16#FED2_0000#, 2), Kind => DW.Data_Root)];
      Devices : constant DW.Range_Array :=
        [R (Top_First, 32),                       --  the reserved top is device-mappable
         R (16#8000_0000#, 16#7FFF_F#),           --  the PCI hole, ECAM, reserved MMIO
         R (16#1_8000_0000#, 16#1000#)];          --  above high RAM
      ECAM_All : constant DW.Range_Array := [R (ECAM, 256 * 256)];
      None : constant DW.Range_Array (1 .. 0) := [others => (0, 0)];
      D : DW.Decision;
      --  Two BARs, sized at enumeration and claimed for devices (no driver).
      BARs : Ledgers.Memory_Ledger;
      Result : MC.Claim_Result;
      function Claimed_By_Device (Envelope : DW.Page_Range) return Boolean is
        (not Ledgers.Clear_Of_Devices (BARs, Envelope.First_Page * Page, Envelope.Pages * Page));
      function Ask (Envelope : DW.Page_Range; Claimed : Boolean := False;
                    E : DW.Range_Array := ECAM_All) return DW.Decision is
        (DW.Decide (Envelope, Firmware, Devices, E,
                    Claimed or else Claimed_By_Device (Envelope)));
   begin
      Ledgers.Claim (BARs, 16#FEB0_0000#, Page, MC.Device, Result, Report);
      Ledgers.Claim (BARs, 16#FED2_1000#, Page, MC.Device, Result, Report);
      D := Ask (R (16#FED1_C000#, 1));
      Check (D.Answer = FDX.Granted_Data and then D.Root = 6, "a reserved page: granted data");
      D := Ask (R (16#FED1_C000#, 4));
      Check (D.Answer = FDX.Granted_Data and then D.Root = 6, "the whole reserved block: granted");
      D := Ask (R (Top_First + 4 * Page, 2));
      Check (D.Answer = FDX.Granted_Data and then D.Root = 4, "reserved top: granted");
      D := Ask (R (NVS_First + Page, 2));
      Check (D.Answer = FDX.Reused_NVS and then D.Root = 1, "inside the NVS root: reused NVS");
      D := Ask (R (NVS_First - Page, 2));
      Check (D.Answer = FDX.No_Root, "half in the NVS root: no root");
      D := Ask (R (Top_First, 1));
      Check (D.Answer = FDX.Overlaps_Control, "the FACS page: overlaps control");
      D := Ask (R (Top_First + 3 * Page - Page, 2));
      Check (D.Answer = FDX.No_Root, "the page between the FACS and the data root: no root");
      D := Ask (R (ECAM, 1));
      Check (D.Answer = FDX.Overlaps_PCI_Config, "an ECAM page of bus 0: PCI configuration");
      D := Ask (R (ECAM + MiB, 1));
      Check (D.Answer = FDX.Overlaps_PCI_Config, "an ECAM page of bus 1 (a data root's): PCI configuration");
      D := Ask (R (ECAM + MiB, 1), E => None);
      Check (D.Answer = FDX.Granted_Data, "without the MCFG, bus 1's page is only data");
      D := Ask (R (16#FEB0_0000#, 1));
      Check (D.Answer = FDX.Overlaps_Device, "a BAR's page: overlaps a device");
      D := Ask (R (16#FED2_0000#, 2));
      Check (D.Answer = FDX.Overlaps_Device, "a data root holding a BAR's base: overlaps a device");
      D := Ask (R (16#FED1_C000#, 1), Claimed => True);
      Check (D.Answer = FDX.Overlaps_Device, "a driver's claimed page: overlaps a device");
      D := Ask (R (16#0400_0000#, 1));
      Check (D.Answer = FDX.Not_Firmware_Class, "a RAM page: not firmware class");
      D := Ask (R (16#FED0_0000#, 1));
      Check (D.Answer = FDX.No_Root, "a device page in no data root: no root");
      D := Ask (R (16#FED1_F000#, 2));
      Check (D.Answer = FDX.No_Root, "across a data root's end: no root");
      D := Ask (R (16#7FFF_F000#, 2));
      Check (D.Answer = FDX.No_Root, "across the reserved top into the hole: no root");
      D := Ask ((First_Page => 0, Pages => 0));
      Check (D.Answer = FDX.Malformed, "no pages: malformed");
      D := Ask ((First_Page => DW.Page_Limit, Pages => 1));
      Check (D.Answer = FDX.Malformed, "past the physical limit: malformed");
      --  A huge envelope is answered by the roots, not by its pages.
      D := Ask ((First_Page => 16#10_0000#, Pages => DW.Page_Limit - 16#10_0000#));
      Check (not FDX.Granted (D.Answer), "a huge envelope is refused, quickly");
      Check (FDX.Disposition'Enum_Rep (FDX.Granted_Data) = 1
             and then FDX.Disposition'Enum_Rep (FDX.Malformed) = 10
             and then FDX.Valid_Disposition (6) and then not FDX.Valid_Disposition (0)
             and then not FDX.Valid_Disposition (11),
             "dispositions have their wire values");
      Check (FDX.Data_Window_Request not in 16#2A0# .. 16#2BF#,
             "the request label is outside the ACPI pair's blocks");
      Check (FDX.Data_Label ("hardware") = "hardware-data", "the startup name");
      Check (not FDX.Well_Formed (1, 1, 0) and then FDX.Well_Formed (1, 1, 1)
             and then not FDX.Well_Formed (FDX.Page_Limit - 1, 2, 1),
             "request words: an id, pages, below the limit");
   end;

   ---------------------------------------------------------------------------
   --  PCI BAR extents (PCI_BAR_Extents): the sizing probe's arithmetic.
   ---------------------------------------------------------------------------
   declare
      KiB : constant := 1024;
      MiB : constant := 1024 * KiB;
      GiB : constant Unsigned_64 := 1024 * MiB;
      Limit_Bytes : constant Unsigned_64 := BX.Page_Limit * BX.Page_Bytes;
      --  The size a BAR reads back after all-ones (Low, High: what the
      --  probe returned; Wide: a 64-bit BAR).
      function Size (Low, High : Unsigned_32; Wide : Boolean) return Unsigned_64 is
        (BX.Size_Of (BX.Address_Mask (Low, High, Wide)));
      E : BX.Extent;
   begin
      --  Decoding the BAR register.
      Check (BX.Kind_Of (16#FEB0_0000#) = BX.Memory_32
             and then BX.Kind_Of (16#FEB0_0008#) = BX.Memory_32
             and then BX.Kind_Of (16#0000_C001#) = BX.IO_Space
             and then BX.Kind_Of (16#FE00_000C#) = BX.Memory_64
             and then BX.Kind_Of (16#0000_0002#) = BX.Memory_Other
             and then BX.Kind_Of (16#0000_0006#) = BX.Memory_Other,
             "BAR kinds: 32-bit, I/O, 64-bit, reserved types");
      Check (BX.Prefetchable (16#FE00_000C#) and then BX.Prefetchable (16#FD00_0008#)
             and then not BX.Prefetchable (16#FEB0_0000#)
             and then not BX.Prefetchable (16#0000_C009#),
             "prefetchable memory BARs, never an I/O BAR");
      Check (BX.Registers_Of (16#FE00_000C#) = 2 and then BX.Registers_Of (16#FEB0_0000#) = 1
             and then BX.Registers_Of (16#0000_C001#) = 1
             and then BX.Registers_Of (16#0000_C005#) = 1,
             "a 64-bit BAR takes two registers; an I/O BAR with type-like bits one");
      Check (BX.Base_Of (16#FEB0_1008#, 16#1234#) = 16#FEB0_1000#
             and then BX.Base_Of (16#C000_000C#, 16#0000_0003#) = 16#3_C000_0000#,
             "base addresses: a 32-bit BAR ignores the next register, a 64-bit one uses it");

      --  Sizes from the read-back masks.
      Check (Size (16#FFFF_F000#, 0, False) = 4 * KiB, "32-bit 4 KiB BAR");
      Check (Size (16#FFFF_FFF0#, 0, False) = 16, "32-bit 16-byte BAR (smaller than a page)");
      Check (Size (16#FFF0_0008#, 0, False) = MiB, "32-bit prefetchable 1 MiB BAR");
      Check (Size (16#8000_0000#, 0, False) = 2 * GiB, "32-bit maximum (2 GiB) BAR");
      Check (Size (16#FFF0_000C#, 16#FFFF_FFFF#, True) = MiB, "64-bit prefetchable 1 MiB BAR");
      Check (Size (16#0000_000C#, 16#FFFF_FFFE#, True) = 8 * GiB,
             "64-bit 8 GiB BAR (low dword's address bits all zero)");
      Check (Size (16#0000_0004#, 16#8000_0000#, True) = 2 ** 63, "64-bit maximum (2 ** 63) BAR");
      Check (Size (16#0000_0000#, 0, False) = 0 and then Size (16#0000_0000#, 0, True) = 0
             and then Size (16#0000_0008#, 0, False) = 0,
             "unimplemented BARs: no size");
      Check (Size (16#FFFF_FFF0#, 0, True) = 16 and then Size (16#0000_0000#, 1, True) = 4 * GiB,
             "64-bit: the low dword's size, or the high dword's lowest bit");
      Check (BX.Size_Of (BX.ROM_Mask (16#FFFE_0000#)) = 128 * KiB
             and then BX.Size_Of (BX.ROM_Mask (16#FFFE_07FE#)) = 128 * KiB
             and then BX.Size_Of (BX.ROM_Mask (0)) = 0,
             "expansion ROM: 128 KiB, its flag bits ignored; none");
      Check (BX.Size_Of (16#FFFF_FFFF_FFF0_1000#) = 4 * KiB,
             "a mask with holes: its lowest bit (Linux's reading)");

      --  Extents: every page a byte of the BAR touches.
      E := BX.Extent_Of (16#FEB0_0000#, 16);
      Check (E = (BX.Sized, 16#FEB00#, 16#FEB01#), "a 16-byte BAR covers its page");
      E := BX.Extent_Of (16#FD00_0000#, 16 * MiB);
      Check (E = (BX.Sized, 16#FD000#, 16#FE000#), "a 16 MiB framebuffer BAR, every page");
      E := BX.Extent_Of (16#FEBF_F800#, 4 * KiB);
      Check (E = (BX.Sized, 16#FEBFF#, 16#FEC01#), "a misaligned BAR: both pages it touches");
      E := BX.Extent_Of (0, MiB);
      Check (E.Status = BX.Unassigned and then not BX.Claimable (E), "a BAR at 0: unassigned");
      E := BX.Extent_Of (16#FEB0_0000#, 0);
      Check (E.Status = BX.Unimplemented and then not BX.Claimable (E), "no size: unimplemented");
      E := BX.Extent_Of (Limit_Bytes, MiB);
      Check (E.Status = BX.Beyond_Limit and then not BX.Claimable (E),
             "a BAR above the physical limit: nothing to claim");
      E := BX.Extent_Of (Limit_Bytes - MiB, 2 * MiB);
      Check (E = (BX.Sized, BX.Page_Limit - MiB / Page, BX.Page_Limit),
             "a BAR across the physical limit: clamped to it");
      E := BX.Extent_Of (Limit_Bytes - Page, 2 ** 63);
      Check (E = (BX.Sized, BX.Page_Limit - 1, BX.Page_Limit),
             "the largest BAR below the limit, clamped");
      E := BX.Extent_Of (16#1000#, Unsigned_64'Last);
      Check (E = (BX.Overflows, 1, BX.Page_Limit),
             "an end past 2 ** 64: refused as a size, claimed from its base to the limit");
      E := BX.Extent_Of (Unsigned_64'Last - Page + 1, 2 * Page);
      Check (E.Status = BX.Beyond_Limit, "a wrapping BAR above the limit: nothing to claim");

      --  Type-1 bridge windows.
      E := BX.Window_Extent (BX.Window_Base (16#FE80#, 0, False),
                             BX.Window_Limit (16#FEA0#, 0, False));
      Check (E = (BX.Sized, 16#FE800#, 16#FEB00#), "a bridge memory window, 1 MiB granules");
      E := BX.Window_Extent (BX.Window_Base (16#FFF0#, 0, False),
                             BX.Window_Limit (16#0000#, 0, False));
      Check (E.Status = BX.Unimplemented, "base above limit: a closed window");
      E := BX.Window_Extent (BX.Window_Base (0, 0, False), BX.Window_Limit (0, 0, False));
      Check (E.Status = BX.Unassigned, "base 0 (reset): an unassigned window");
      E := BX.Window_Extent (BX.Window_Base (16#0001#, 16#2#, True),
                             BX.Window_Limit (16#0FF1#, 16#2#, True));
      Check (E = (BX.Sized, 16#20_0000#, 16#21_0000#),
             "a 64-bit prefetchable window above 4 GiB (256 MiB)");
      Check (BX.Window_Base (16#C001#, 16#5#, False) = 16#C000_0000#,
             "a 32-bit window ignores the upper dword");

      --  The regression: a driverless BAR inside a firmware-reserved data
      --  root. Before, devmgr knew only the BAR's base page (it never
      --  probed a running device), so an envelope over the BAR's tail was
      --  granted as firmware data; now the BAR is sized at enumeration and
      --  its whole extent is a device claim, and the envelope is refused.
      declare
         Data_First : constant := 16#FE00_0000#;
         Data_Pages : constant := 16#20#;
         BAR_Base : constant := 16#FE01_0000#;
         BAR_Bytes : constant Unsigned_64 := Size (16#FFFF_C000#, 0, False);   --  16 KiB
         Firmware : constant DW.Root_Array :=
           [(Pages => (First_Page => Data_First / Page, Pages => Data_Pages), Kind => DW.Data_Root)];
         Devices : constant DW.Range_Array := [(First_Page => Data_First / Page, Pages => 16#100#)];
         No_ECAM : constant DW.Range_Array (1 .. 0) := [others => (0, 0)];
         Before, After : Ledgers.Memory_Ledger;
         Result : MC.Claim_Result;
         function Answer (L : Ledgers.Memory_Ledger; First_Page, Pages : Unsigned_64)
           return FDX.Disposition is
           (DW.Decide ((First_Page => First_Page, Pages => Pages), Firmware, Devices, No_ECAM,
                       not Ledgers.Clear_Of_Devices (L, First_Page * Page, Pages * Page)).Answer);
         Base_Page : constant Unsigned_64 := BAR_Base / Page;
         Tail : constant Unsigned_64 := Base_Page + 1;    --  past the base page
      begin
         Check (BAR_Bytes = 16 * KiB, "the reserved-span BAR is 16 KiB");
         --  Before: only the base page was known.
         Ledgers.Claim (Before, BAR_Base, Page, MC.Device, Result, Report);
         Check (Result = MC.Granted, "before: the base page only");
         Check (Answer (Before, Tail, 2) = FDX.Granted_Data,
                "before: an envelope over the BAR's tail, not its base page, was granted");
         Check (Answer (Before, Tail + 2, 2) = FDX.Granted_Data,
                "before: an envelope from the BAR's last page past its end was granted");
         --  After: the sized extent.
         E := BX.Extent_Of (BAR_Base, BAR_Bytes);
         Check (E = (BX.Sized, Base_Page, Base_Page + 4), "the BAR's extent: its four pages");
         Ledgers.Claim (After, E.First_Page * Page, (E.End_Page - E.First_Page) * Page, MC.Device,
                        Result, Report);
         Check (Result = MC.Granted, "the BAR's whole extent claimed for its device");
         Check (Answer (After, Tail, 2) = FDX.Overlaps_Device,
                "an envelope over a driverless BAR's tail is refused (Overlaps_Device)");
         Check (Answer (After, Tail + 2, 2) = FDX.Overlaps_Device,
                "an envelope from a driverless BAR's last page past its end is refused");
         Check (Answer (After, Data_First / Page, Base_Page + 1 - Data_First / Page)
                  = FDX.Overlaps_Device,
                "an envelope from below the BAR into it is refused");
         Check (Answer (After, Data_First / Page, Base_Page - Data_First / Page) = FDX.Granted_Data
                and then Answer (After, Base_Page + 4, 1) = FDX.Granted_Data,
                "the pages below and past the BAR are still data");
         --  A driver bound later: its window over the same BAR, or part of
         --  it, is granted beside the enumeration's claim.
         Ledgers.Claim (After, BAR_Base, BAR_Bytes, MC.Device, Result, Report);
         Check (Result = MC.Granted, "the driver's window over its whole BAR is granted");
         Ledgers.Claim (After, BAR_Base + Page, 16#20#, MC.Device, Result, Report);
         Check (Result = MC.Granted, "the driver's window over part of its BAR is granted");
         Ledgers.Claim (After, BAR_Base + 3 * Page, Page, MC.ACPI_Data, Result, Report);
         Check (Result = MC.Overlaps_Device, "a data claim over the BAR's last page is refused");
      end;

      --  The active boot display is never probed: which display holds the
      --  firmware framebuffer, and its extents from non-probing sources.
      declare
         FB : constant := 16#FD00_0000#;
         --  q35 desktop-display: QEMU VGA (00:01.0) BAR0, BAR2; virtio-gpu
         --  (00:03.0) BAR1, BAR4; six slots per display function.
         Bases : constant BX.Base_Array :=
           [16#FD00_0000#, 0, 16#FEBD_8000#, 0, 0, 0,
            0, 16#FEBD_9000#, 0, 0, 16#FE10_0000#, 0];
         Slots : constant := 6;
         Holder : Natural;
         Display : Ledgers.Memory_Ledger;
         Result : MC.Claim_Result;
         Next : Unsigned_64;
      begin
         Check (BX.Display_Class (16#0300#) and then BX.Display_Class (16#0380#)
                and then not BX.Display_Class (16#0200#) and then not BX.Display_Class (16#0C03#),
                "display class: VGA and other displays; not network or USB");
         Holder := BX.Scanout_Holder (Bases, FB, 16#30_0000#);
         Check (Holder = 1 and then (Holder - 1) / Slots + 1 = 1,
                "the VGA's BAR0 holds the framebuffer: that function is not probed");
         Check (BX.Scanout_Holder (Bases, FB + 16#80_0000#, 16#30_0000#) = 1,
                "a framebuffer inside the BAR, past its base: the same BAR");
         Check (BX.Scanout_Holder (Bases, FB, 0) = 0, "no firmware framebuffer: every display is probed");
         Check (BX.Scanout_Holder (Bases, 16#C000_0000#, Page) = 0,
                "a framebuffer below every display BAR: none holds it");
         Check (BX.Scanout_Holder ([16#C000_0000#, 16#E000_0000#, 16#F000_0000#], 16#E080_0000#, Page) = 2,
                "the greatest base at or below the framebuffer holds it, not a lower one");
         Check (BX.Documented_Size (16#8086#, 16#46D0#, 0) = 16 * MiB
                and then BX.Documented_Size (16#8086#, 16#46D4#, 0) = 16 * MiB
                and then BX.Documented_Size (16#8086#, 16#46D0#, 2) = 0
                and then BX.Documented_Size (16#8086#, 16#9A49#, 0) = 0,
                "documented sizes: Alder Lake-N GTTMMADR 16 MiB; its aperture and other GPUs none");
         Check (BX.Documented_Size (16#1234#, 16#1111#, 2) = 4 * KiB
                and then BX.Documented_Size (16#1234#, 16#1111#, 0) = 0,
                "documented sizes: QEMU VGA MMIO 4 KiB; its framebuffer none");
         --  The other devices are probed and claimed first (pci-testdev at
         --  0xFE00_0000), then the boot display from what is known.
         Ledgers.Claim (Display, 16#FE00_0000#, MiB, MC.Device, Result, Report);
         E := BX.Extent_Of (16#FEBD_8000#, BX.Documented_Size (16#1234#, 16#1111#, 2));
         Check (E = (BX.Sized, 16#FEBD8#, 16#FEBD9#), "the VGA's MMIO from its documented size");
         Ledgers.Claim (Display, E.First_Page * Page, Page, MC.Device, Result, Report);
         Next := Ledgers.Next_Start (Display, FB / Page);
         Check (Next = 16#FE000#, "the next claimed range above the framebuffer BAR: pci-testdev's");
         E := BX.Bounded_Extent (FB, False, Next);
         Check (E = (BX.Bounded, 16#FD000#, 16#FE000#),
                "the framebuffer BAR, not probed: from its base up to the next claimed range");
         Ledgers.Claim (Display, E.First_Page * Page, (E.End_Page - E.First_Page) * Page, MC.Device,
                        Result, Report);
         Check (Result = MC.Granted, "claimed for the device");
         Check (not Ledgers.Clear_Of_Devices (Display, FB, Page)
                and then not Ledgers.Clear_Of_Devices (Display, 16#FDFF_F000#, Page),
                "no data envelope reaches the framebuffer or the BAR's possible tail");
         Check (Ledgers.Next_Start (Display, 16#FEBD8#) = MC.Page_Limit,
                "nothing claimed above the last BAR: the physical limit");
         Check (BX.Bounded_Extent (FB, False, MC.Page_Limit)
                  = (BX.Bounded, 16#FD000#, 16#FD000# + 2 ** 31 / Page),
                "nothing above a 32-bit BAR: at most the largest 32-bit BAR (2 GiB)");
         Check (BX.Bounded_Extent (16#40_0000_0000#, True, MC.Page_Limit)
                  = (BX.Bounded, 16#400_0000#, MC.Page_Limit),
                "nothing above a 64-bit BAR: up to the physical limit");
         Check (BX.Bounded_Extent (FB, False, 16#FD000#) = (BX.Bounded, 16#FD000#, 16#FD001#)
                and then BX.Bounded_Extent (FB, False, 16#FC000#) = (BX.Bounded, 16#FD000#, 16#FD001#),
                "a claim already at its base: at least its base page");
         Check (BX.Bounded_Extent (0, False, 16#FE000#).Status = BX.Unassigned
                and then BX.Bounded_Extent (Limit_Bytes, True, MC.Page_Limit).Status = BX.Beyond_Limit,
                "unassigned and beyond the limit: nothing to claim");
      end;
   end;

   Put_Line ("devmgr-claims:" & Checks'Image & " checks," & Failed'Image & " failed");
   if Failed /= 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;
