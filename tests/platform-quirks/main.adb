--  Hosted tests of devmgr's platform quirks (ACPI-POOL-001): the shared
--  Platform_Identity and quirk-table pattern, the quirk table, the P2SB
--  step classification (with the IdeaPad's image-12 reads), the GPIO
--  bootstrap payload codec and the admission decision (every refusal, and
--  the IdeaPad's GPI0._CRS spans against its memory map).
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with CuBit.GPIO_Bootstrap_Exchange;
with Devmgr_Platform_Pool;
with Devmgr_Data_Windows;
with Platform_Identity;
with GPIO_Bootstrap_Payload;
with Platform_Quirks;
with Platform_Quirks.Intel_P2SB;
with Platform_Quirks.Intel_GPIO_Communities;
with Platform_Quirks.GPIO_Bootstrap_Admission;

procedure Main is
   package PP renames Devmgr_Platform_Pool;
   package DW renames Devmgr_Data_Windows;
   package PI renames Platform_Identity;
   package GP renames GPIO_Bootstrap_Payload;
   package GX renames CuBit.GPIO_Bootstrap_Exchange;
   package Q renames Platform_Quirks;
   package P2 renames Platform_Quirks.Intel_P2SB;
   package GC renames Platform_Quirks.Intel_GPIO_Communities;
   package A renames Platform_Quirks.GPIO_Bootstrap_Admission;
   use type GX.Disposition;
   use type GP.Decode_Status;
   use type P2.Outcome;
   use type P2.Step;
   use type P2.P2SB_Action;
   use type PP.Block_Status;
   use type PI.Identity_Source;
   use type Q.Sideband_Source;
   use type Q.Community_Layout;
   use type Q.GPIO_Controller;
   use type PI.Identity;
   use type PP.Bridge_Kind;
   use type GP.Request;

   Checks, Failures : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   Page : constant := 4096;

   ---------------------------------------------------------------------------
   --  The IdeaPad Slim 3i's facts.
   ---------------------------------------------------------------------------
   --  .acpi-proof-work/laptop-acpi-1-tables/facp.dat, its first 36 bytes.
   Laptop_FADT_Header : constant PI.Byte_Array (0 .. 35) :=
     [70, 65, 67, 80, 20, 1, 0, 0, 6, 150, 76, 69, 78, 79, 86, 79, 67, 66, 45, 48, 49, 32, 32, 32,
      1, 0, 0, 0, 65, 67, 80, 73, 0, 0, 4, 0];

   --  SBRG as coreboot alderlake/Kconfig PCR_BASE_ADDRESS (0xFD000000)
   --  for a non-S PCH; the live value comes from PNVA at run time
   --  (CAPTURE-FACTS.json: "no live value inferred").
   Laptop_SBRG : constant := 16#FD00_0000#;
   Community_Offsets : constant array (1 .. 4) of Unsigned_64 :=
     [16#6E_0000#, 16#6D_0000#, 16#6A_0000#, 16#69_0000#];   --  GPI0._CRS CML0, 1, 4, 5

   function Name_Seg (S : String) return GP.Name_Segment is (S);

   --  GPI0 as acpi.svc would observe it before _INI: \_SB_.GPI0,
   --  _HID INTC1057 (PCHS = 3), _STA 0Fh, _CRS: the Extended Interrupt
   --  (flags 0Dh: consumer, level, active-low, shared; INTL = SGIR, 14 in
   --  the template) and four Memory32Fixed 64 KiB windows, read-write.
   function Laptop_GPI0 (SBRG : Unsigned_64 := Laptop_SBRG) return GP.Request is
      R : GP.Request;
   begin
      R.Spans := 4;
      R.Segments := 2;
      R.UID_Type := GP.No_UID;
      R.STA_From := GP.Evaluated_STA;
      R.HID := "INTC1057";
      R.STA := 16#0F#;
      R.IRQ := (Present => True, Mode => GP.Level, Level_Of => GP.Active_Low,
                Share => GP.Shared, Wakes => GP.Not_Wake_Capable, GSI => 14);
      R.Table := "DSDT";
      R.Table_ID := "CB-01   ";
      R.Epoch := 1;
      R.Path (1) := Name_Seg ("_SB_");
      R.Path (2) := Name_Seg ("GPI0");
      for I in 1 .. 4 loop
         R.Items (I) := (Base => SBRG + Community_Offsets (I), Length => GC.GPIO_Community_Bytes,
                         From => GP.Memory32_Fixed, Writable => True);
      end loop;
      return R;
   end Laptop_GPI0;

   function Pages (First, Bytes : Unsigned_64) return DW.Page_Range is
     ((First_Page => First / Page, Pages => Bytes / Page));

   --  The IdeaPad's memory map as devmgr would hold it, reconstructed from
   --  the image-12 logs (laptop12-kernel.log: the first usable run;
   --  laptop12-devmgr.log: the pool members and the GPU's BARs) and the
   --  saved MCFG (ECAM 0xC0000000, buses 0-255). The kernel log ring kept
   --  only the first three memory areas, so the device-memory root is the
   --  32-bit PCI hole as the firmware-reserved members bound it: 0xC0000000
   --  (ECAM, firmware-reserved MMIO, inside the root) up to the IOAPIC.
   Laptop_View : constant A.Memory_View :=
     (Devices => 1, RAM => 1, BARs => 2, ECAM => 1, Pool => 9,
      Device_Roots => [Pages (16#C000_0000#, 16#3EC0_0000#)],          --  .. 0xFEC00000
      RAM_Ranges   => [Pages (16#0100_0000#, 16#3A3C_9000# - 16#0100_0000#)],
      BAR_Extents  => [Pages (16#60_0000_0000#, 16#100_0000#),            --  intel-gpu BAR0
                       Pages (16#40_0000_0000#, 16#1000_0000#)],          --  intel-gpu BAR2
      ECAM_Ranges  => [Pages (16#C000_0000#, 16#1000_0000#)],
      Pool_Members => [Pages (16#FEDC_0000#, 16#2_0000#),                 --  MCHBAR
                       Pages (16#FE00_0000#, 16#1_0000#),                 --  PMC PWRMBASE
                       Pages (16#3A3C_9000#, 16#90_0000#),
                       Pages (16#3CAD_F000#, 16#20A_0000#),
                       Pages (16#41C0_0000#, 16#640_0000#),
                       Pages (16#4840_0000#, 16#20_0000#),
                       Pages (16#4920_0000#, 16#720_0000#),
                       Pages (16#FED2_0000#, 16#6_0000#),
                       Pages (16#FF00_0000#, 16#100_0000#)]);

   Failed_P2SB : constant A.Gate :=
     (Discovery_Ran => True, Discovery_Found => False, Admitted_Before => False);

   ---------------------------------------------------------------------------
   procedure Identity_And_Table is
      Id : constant PI.Identity := PI.From_Header (Laptop_FADT_Header);
      Not_FADT : PI.Byte_Array := Laptop_FADT_Header;
      ADL_N, QEMU, Other : Q.Observed_Platform;
      M : PI.Firmware_Match;
   begin
      Check (Id.Source = PI.FADT_Header and then Id.OEM_ID = "LENOVO"
             and then Id.OEM_Table_ID = "CB-01   " and then Id.OEM_Revision = 1,
             "the IdeaPad's FADT: LENOVO / CB-01 / revision 1");
      Not_FADT (0) := Character'Pos ('X');
      Check (PI.From_Header (Not_FADT) = PI.No_Identity, "another table's header names no identity");
      M := (Kind => PI.OEM_Table, OEM_ID => "LENOVO", OEM_Table_ID => "CB-01   ", others => <>);
      Check (PI.Matches (M, Id), "an OEM + table match finds the IdeaPad");
      M.OEM_Table_ID := "CB-02   ";
      Check (not PI.Matches (M, Id), "another OEM table ID does not match");
      M := (Kind => PI.OEM_Table_Revised, OEM_ID => "LENOVO", OEM_Table_ID => "CB-01   ",
            Revision_Low => 2, Revision_High => 5);
      Check (not PI.Matches (M, Id), "a revision range excluding 1 does not match");
      Check (not PI.Matches ((Kind => PI.OEM, OEM_ID => "LENOVO", others => <>), PI.No_Identity),
             "an unknown identity matches only Any_Firmware");
      Check (PI.Matches (PI.Any, PI.No_Identity), "Any_Firmware matches without an FADT");

      ADL_N := (Bridge => PP.Bridge_Of (16#8086#, 16#461C#, Test_Profile => False), Firmware => Id);
      Check (ADL_N.Bridge = PP.Alder_Lake_N, "the IdeaPad's host bridge 8086:461C is Alder Lake-N");
      Check (Q.Sideband_Of (ADL_N) = Q.P2SB_SBREG, "Alder Lake-N: sideband from the P2SB");
      Check (Q.Communities_Of (ADL_N) = Q.ADL_N_Communities, "Alder Lake-N: four communities");
      Check (Q.Controller_Of (ADL_N) = Q.ADL_N_INTC1057, "Alder Lake-N: bootstrap controller INTC1057");
      QEMU := (Bridge => PP.Bridge_Of (16#8086#, 16#29C0#, Test_Profile => True), Firmware => PI.No_Identity);
      Check (Q.Sideband_Of (QEMU) = Q.Test_Stand_In and then Q.Communities_Of (QEMU) = Q.Test_Community
             and then Q.Controller_Of (QEMU) = Q.No_Controller,
             "QEMU q35 + edu: the test stand-in, no bootstrap controller");
      Other := (Bridge => PP.Bridge_Of (16#1022#, 16#1480#, Test_Profile => False), Firmware => Id);
      Check (Q.Sideband_Of (Other) = Q.No_Sideband and then Q.Controller_Of (Other) = Q.No_Controller,
             "an unknown host bridge: no quirk applies");
      for K in Q.Quirk loop
         Check (Q.Citation (K)'Length > 0, "every quirk cites its source");
      end loop;
      Check (GC.Test_Community (16#FEA0_0000#).Bytes = GC.GPIO_Community_Bytes,
             "the test profile's community is one 64 KiB community");
   end Identity_And_Table;

   ---------------------------------------------------------------------------
   procedure P2SB_Steps is
      Hidden : constant Unsigned_32 := 16#FFFF_FFFF#;
      --  laptop12-devmgr.log: "P2SB unhidden for its BAR (vendor 0xFFFF
      --  class 0xFFFF), hidden again (vendor 0xFFFF)".
      Laptop_12 : constant P2.Observation :=
        (Action => P2.Action_Of (16#FFFF#, Hidden), Class => 16#FFFF#, Control => Hidden,
         Control_Unhidden => Hidden, Vendor_Unhidden => 16#FFFF#, Device_Unhidden => 16#FFFF#,
         Class_Unhidden => 16#FFFF#, BAR0 => Hidden, BAR1 => Hidden, Vendor_After_Hide => 16#FFFF#);
      Unhidden : P2.Observation :=
        (Action => P2.Unhide_Read, Class => 16#FFFF#, Control => Hidden,
         Control_Unhidden => 0, Vendor_Unhidden => PP.Intel_Vendor, Device_Unhidden => 16#54A0#,
         Class_Unhidden => P2.PCI_Class_Memory_Other, BAR0 => 16#FD00_0004#, BAR1 => 0,
         Vendor_After_Hide => 16#FFFF#);
      Visible : P2.Observation :=
        (Action => P2.Read_Visible, Class => P2.PCI_Class_Memory_Other, Control => 0,
         BAR0 => 16#FD00_0000#, BAR1 => 0, others => <>);
      O : P2.Outcome;
   begin
      O := P2.Classify (Laptop_12);
      Check (Laptop_12.Action = P2.Unhide_Read and then O = P2.Unhide_Ignored
             and then P2.Deciding_Step (O) = P2.Unhide,
             "laptop image 12: the unhide write did not make 00:1F.1 answer (UNHIDE_IGNORED, step unhide)");
      Check (P2.Window_Of (Laptop_12).Status /= PP.Found, "no window from an ignored unhide");
      Check (not P2.Still_Visible_After_Hide (Laptop_12), "image 12: hidden afterwards");

      O := P2.Classify (Unhidden);
      Check (O = P2.Found_Unhidden and then P2.Window_Of (Unhidden).Base = 16#FD00_0000#
             and then P2.Window_Of (Unhidden).Bytes = P2.SBREG_Bytes,
             "hidden P2SB that unhides: 16 MiB at SBREG");
      Unhidden.Vendor_After_Hide := PP.Intel_Vendor;
      Check (P2.Still_Visible_After_Hide (Unhidden) and then P2.Classify (Unhidden) = P2.Found_Unhidden,
             "still visible after the hide write: logged, the window stands");
      Unhidden.Control_Unhidden := Hidden;
      Unhidden.Vendor_Unhidden := 16#FFFF#;
      Check (P2.Classify (Unhidden) = P2.Unhide_Ignored, "all-ones after the unhide: Unhide_Ignored");
      Unhidden.Control_Unhidden := 0;
      Check (P2.Classify (Unhidden) = P2.Unhidden_Not_P2SB,
             "P2SBC answers but the vendor reads absent: Unhidden_Not_P2SB, step confirm-identity");
      Check (P2.Deciding_Step (P2.Unhidden_Not_P2SB) = P2.Confirm_Identity, "confirm-identity step");
      Unhidden.Vendor_Unhidden := PP.Intel_Vendor;
      Unhidden.Class_Unhidden := 16#0C05#;
      Check (P2.Classify (Unhidden) = P2.Unhidden_Not_P2SB, "an Intel function of another class");
      Unhidden.Class_Unhidden := P2.PCI_Class_Memory_Other;
      Unhidden.BAR0 := 16#FD00_0001#;
      Check (P2.Classify (Unhidden) = P2.BAR_Not_Memory and then
             P2.Deciding_Step (P2.BAR_Not_Memory) = P2.Read_BAR, "an I/O BAR: BAR_Not_Memory");
      Unhidden.BAR0 := 16#FD80_0004#;
      Check (P2.Classify (Unhidden) = P2.BAR_Not_Placed, "a misaligned SBREG: BAR_Not_Placed");
      Unhidden.BAR0 := 4;
      Check (P2.Classify (Unhidden) = P2.BAR_Not_Placed, "a zero SBREG: BAR_Not_Placed");
      Unhidden.BAR1 := 16#0001_0000#;
      Check (P2.Classify (Unhidden) = P2.BAR_Beyond_Limit, "beyond the physical limit");

      Check (P2.Classify (Visible) = P2.Found_Visible, "firmware left it visible: read directly");
      Visible := (Action => P2.Action_Of (16#0C05#, Hidden), Class => 16#0C05#, Control => Hidden,
                  others => <>);
      Check (P2.Classify (Visible) = P2.Other_Function and then P2.Deciding_Step (P2.Other_Function) = P2.Identify,
             "another class at 00:1F.1: left alone (step identify)");
      Visible := (Action => P2.Action_Of (16#FFFF#, 0), Class => 16#FFFF#, Control => 0, others => <>);
      Check (Visible.Action = P2.Leave_Alone and then P2.Classify (Visible) = P2.Absent,
             "nothing answers and HIDE clear: Absent");
      for K in P2.Outcome loop
         Check (P2.Outcome_Name (K)'Length > 0, "every outcome has a log name");
      end loop;
   end P2SB_Steps;

   ---------------------------------------------------------------------------
   procedure Codec is
      R : constant GP.Request := Laptop_GPI0;
      Bytes : GP.Byte_Array (0 .. GP.Encoded_Length (R.Spans) - 1);
      Back : GP.Request;
      Result : GP.Decode_Result;

      procedure Expect_Malformed (B : GP.Byte_Array; Offset : Natural; What : String) is
         Out_R : GP.Request;
         Res : GP.Decode_Result;
      begin
         GP.Decode (B, Out_R, Res);
         Check (Res.Status = GP.Malformed and then Res.Offset = Offset,
                "malformed payload (" & What & ") at offset" & Natural'Image (Offset) &
                " (got" & Natural'Image (Res.Offset) & ")");
      end Expect_Malformed;

      procedure Corrupt (Offset : Natural; Value : Unsigned_8; Expected : Natural; What : String) is
         B : GP.Byte_Array := Bytes;
      begin
         B (Offset) := Value;
         Expect_Malformed (B, Expected, What);
      end Corrupt;
   begin
      Check (GP.Valid (R), "the laptop's GPI0 observation is a valid request");
      Check (Bytes'Length = 102 + 4 * 18, "header 102 bytes (v2), four 18-byte spans");
      GP.Encode (R, Bytes);
      GP.Decode (Bytes, Back, Result);
      Check (Result.Status = GP.Decoded and then Back = R, "encode / decode round trip");
      Check (Bytes (GP.HID_Offset) = Character'Pos ('I') and then Bytes (GP.Path_Offset + 4) = Character'Pos ('G'),
             "HID and path bytes where the layout says");

      Expect_Malformed (Bytes (0 .. 50), 51, "shorter than the header");
      Expect_Malformed (Bytes (0 .. Bytes'Last - 1), GP.Span_Count_Offset, "length not header + spans");
      Corrupt (GP.Version_Offset, GP.Format_Version + 1, GP.Version_Offset, "a version after Format_Version");
      Corrupt (GP.Span_Count_Offset, 0, GP.Span_Count_Offset, "no spans");
      Corrupt (GP.Span_Count_Offset, 17, GP.Span_Count_Offset, "past Max_Spans");
      Corrupt (GP.Path_Count_Offset, 0, GP.Path_Count_Offset, "empty path");
      Corrupt (GP.Path_Count_Offset, 9, GP.Path_Count_Offset, "path past its ceiling");
      Corrupt (GP.UID_Kind_Offset, 3, GP.UID_Kind_Offset, "unknown UID kind");
      Corrupt (GP.STA_Source_Offset, 0, GP.STA_Source_Offset, "unknown STA source");
      Corrupt (GP.HID_Offset, 0, GP.HID_Offset, "empty HID");
      Corrupt (GP.HID_Offset + 3, 0, GP.HID_Offset, "a printable after NUL padding in the HID");
      Corrupt (GP.UID_Offset, 1, GP.UID_Offset, "a UID value with No_UID");
      Corrupt (GP.Phase_Offset, 3, GP.Phase_Offset, "an unknown phase (after GPIO-consumer initialization)");
      Corrupt (GP.IRQ_Present_Offset, 2, GP.IRQ_Present_Offset, "IRQ present not 0/1");
      Corrupt (GP.Trigger_Offset, 2, GP.Trigger_Offset, "unknown trigger");
      Corrupt (GP.Polarity_Offset, 2, GP.Polarity_Offset, "unknown polarity");
      Corrupt (GP.Sharing_Offset, 2, GP.Sharing_Offset, "unknown sharing");
      Corrupt (GP.Wake_Offset, 2, GP.Wake_Offset, "unknown wake");
      Corrupt (GP.Table_Signature_Offset, 0, GP.Table_Signature_Offset, "a NUL in the table signature");
      Corrupt (GP.Path_Offset, Character'Pos ('1'), GP.Path_Offset, "a NameSeg starting with a digit");
      Corrupt (GP.Path_Offset + 2 * 4, Character'Pos ('X'), GP.Path_Offset, "an unused path slot not zero");
      Corrupt (GP.Header_Bytes + GP.Span_Descriptor_Offset, 9,
               GP.Header_Bytes + GP.Span_Descriptor_Offset, "unknown descriptor");
      Corrupt (GP.Header_Bytes + 18 + GP.Span_Writable_Offset, 2,
               GP.Header_Bytes + 18 + GP.Span_Writable_Offset, "writable not 0/1");
      declare
         B : GP.Byte_Array := Bytes;
      begin
         B (GP.IRQ_Present_Offset) := 0;   --  no IRQ, yet flags and a GSI
         Expect_Malformed (B, GP.IRQ_Present_Offset, "IRQ fields without an IRQ");
         B (GP.STA_Source_Offset) := GP.STA_Source'Enum_Rep (GP.Default_STA);
         B (GP.STA_Offset) := 16#0B#;
         Expect_Malformed (B, GP.STA_Offset, "Default_STA with a value other than 0Fh");
      end;
   end Codec;

   ---------------------------------------------------------------------------
   procedure Admission is
      R : GP.Request := Laptop_GPI0;
      D : A.Decision;
      Small_View : constant A.Memory_View :=
        (Devices => 1, RAM => 0, BARs => 0, ECAM => 0, Pool => 0,
         Device_Roots => Laptop_View.Device_Roots, others => []);
   begin
      --  The laptop's real spans against its map: admitted.
      D := A.Decide (Failed_P2SB, Q.ADL_N_INTC1057, R, Laptop_View);
      Check (D.Answer = GX.Admitted, "the IdeaPad's four communities at SBRG + 6E/6D/6A/69_0000 are admitted");

      --  The gate.
      D := A.Decide ((others => False), Q.ADL_N_INTC1057, R, Laptop_View);
      Check (D.Answer = GX.No_Discovery_Failure, "no P2SB discovery ran: No_Discovery_Failure");
      D := A.Decide ((Discovery_Ran => True, Discovery_Found => True, Admitted_Before => False),
                     Q.ADL_N_INTC1057, R, Laptop_View);
      Check (D.Answer = GX.P2SB_Available, "the P2SB was read: P2SB_Available");
      D := A.Decide ((Discovery_Ran => True, Discovery_Found => False, Admitted_Before => True),
                     Q.ADL_N_INTC1057, R, Laptop_View);
      Check (D.Answer = GX.Already_Admitted, "a second admission: Already_Admitted");

      --  The controller.
      D := A.Decide (Failed_P2SB, Q.No_Controller, R, Laptop_View);
      Check (D.Answer = GX.Unknown_Controller, "no bootstrap controller for this platform");
      R.HID := "PNP0C02" & GP.NUL;
      D := A.Decide (Failed_P2SB, Q.ADL_N_INTC1057, R, Laptop_View);
      Check (D.Answer = GX.Unknown_Controller, "GPI0 hidden by GPHD (_HID PNP0C02): Unknown_Controller");
      R := Laptop_GPI0;
      R.STA := 16#08#;
      D := A.Decide (Failed_P2SB, Q.ADL_N_INTC1057, R, Laptop_View);
      Check (D.Answer = GX.Unknown_Controller, "_STA 08h (GPHD): not present: Unknown_Controller");

      --  Shapes: Malformed with the span ordinal.
      R := Laptop_GPI0;
      R.Items (2).Length := 0;
      D := A.Decide (Failed_P2SB, Q.ADL_N_INTC1057, R, Laptop_View);
      Check (D.Answer = GX.Malformed and then D.Span = 2, "an empty span: Malformed, span 2");
      R := Laptop_GPI0;
      R.Items (3).Base := R.Items (3).Base + 16#100#;
      D := A.Decide (Failed_P2SB, Q.ADL_N_INTC1057, R, Laptop_View);
      Check (D.Answer = GX.Malformed and then D.Span = 3, "a span not on a page: Malformed, span 3");
      R := Laptop_GPI0;
      R.Items (4) := R.Items (1);
      D := A.Decide (Failed_P2SB, Q.ADL_N_INTC1057, R, Laptop_View);
      Check (D.Answer = GX.Malformed and then D.Span = 4, "two spans overlapping: Malformed, span 4");
      R := Laptop_GPI0;
      R.Items (1).Base := 16#FFFF_FFFF_FFFF_0000#;
      D := A.Decide (Failed_P2SB, Q.ADL_N_INTC1057, R, Laptop_View);
      Check (D.Answer = GX.Malformed and then D.Span = 1, "a span past the physical limit: Malformed");

      --  The memory map.
      R := Laptop_GPI0 (SBRG => 16#3000_0000#);
      D := A.Decide (Failed_P2SB, Q.ADL_N_INTC1057, R, Laptop_View);
      Check (D.Answer = GX.Overlaps_RAM and then D.Span = 1, "spans in usable RAM: Overlaps_RAM");
      R := Laptop_GPI0;
      R.Items (2).Base := 16#60_0000_0000#;
      D := A.Decide (Failed_P2SB, Q.ADL_N_INTC1057, R, Laptop_View);
      Check (D.Answer = GX.Overlaps_BAR and then D.Span = 2, "a span over the GPU's BAR0: Overlaps_BAR");
      R := Laptop_GPI0 (SBRG => 16#C000_0000#);
      D := A.Decide (Failed_P2SB, Q.ADL_N_INTC1057, R, Laptop_View);
      Check (D.Answer = GX.Overlaps_PCI_Config and then D.Span = 1, "spans in ECAM: Overlaps_PCI_Config");
      R := Laptop_GPI0;
      R.Items (4).Base := 16#FE00_0000#;
      D := A.Decide (Failed_P2SB, Q.ADL_N_INTC1057, R, Laptop_View);
      Check (D.Answer = GX.Overlaps_Pool_Member and then D.Span = 4,
             "a span over the PMC member: Overlaps_Pool_Member");
      R := Laptop_GPI0;
      R.Items (1).Base := 16#FEBF_0000#;
      R.Items (1).Length := 16#2_0000#;
      D := A.Decide (Failed_P2SB, Q.ADL_N_INTC1057, R, Laptop_View);
      Check (D.Answer = GX.Straddles and then D.Span = 1, "a span across the root's end: Straddles");
      R := Laptop_GPI0;
      R.Items (3).Base := 16#FEC0_0000#;
      D := A.Decide (Failed_P2SB, Q.ADL_N_INTC1057, R, Laptop_View);
      Check (D.Answer = GX.Not_Chipset_MMIO and then D.Span = 3,
             "a span outside every device root (IOAPIC): Not_Chipset_MMIO");
      --  No RAM list (devmgr's run-time view): RAM is then outside the roots.
      R := Laptop_GPI0 (SBRG => 16#3000_0000#);
      D := A.Decide (Failed_P2SB, Q.ADL_N_INTC1057, R, Small_View);
      Check (D.Answer = GX.Not_Chipset_MMIO, "without a RAM list, RAM is outside every root: Not_Chipset_MMIO");
      D := A.Decide (Failed_P2SB, Q.ADL_N_INTC1057, Laptop_GPI0, Small_View);
      Check (D.Answer = GX.Admitted, "the laptop's spans with the run-time view: admitted");
   end Admission;

begin
   Identity_And_Table;
   P2SB_Steps;
   Codec;
   Admission;
   Put_Line ("platform-quirks:" & Natural'Image (Checks) & " checks," & Natural'Image (Failures) & " failures");
   if Failures /= 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;
