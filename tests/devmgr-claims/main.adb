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
with CuBit.Firmware_Data_Exchange;

procedure Main is
   package Ledgers is new Devmgr_Claim_Ledgers (Test_Heap.Allocate, Test_Heap.Release);
   package PC renames Devmgr_Port_Claims;
   package MC renames Devmgr_Memory_Claims;
   package DW renames Devmgr_Data_Windows;
   package FDX renames CuBit.Firmware_Data_Exchange;
   use type PC.Holder;
   use type PC.Claim;
   use type MC.Claim_Result;
   use type FDX.Disposition;
   use type Ledgers.Port_Store.Growth_Status;

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
      BARs : constant DW.Range_Array := [R (16#FEB0_0000#, 1), R (16#FED2_1000#, 1)];
      None : constant DW.Range_Array (1 .. 0) := [others => (0, 0)];
      D : DW.Decision;
      function Ask (Envelope : DW.Page_Range; Claimed : Boolean := False;
                    E : DW.Range_Array := ECAM_All) return DW.Decision is
        (DW.Decide (Envelope, Firmware, Devices, E, BARs, Claimed));
   begin
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

   Put_Line ("devmgr-claims:" & Checks'Image & " checks," & Failed'Image & " failed");
   if Failed /= 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;
