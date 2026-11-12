--  Hosted tests of devmgr's platform quirks (ACPI-POOL-001): the shared
--  Platform_Identity and quirk-table pattern, the quirk table and the P2SB
--  step classification (with the IdeaPad's image-12 reads).
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Devmgr_Platform_Pool;
with Platform_Identity;
with Platform_Quirks;
with Platform_Quirks.Intel_P2SB;
with Platform_Quirks.Intel_GPIO_Communities;

procedure Main is
   package PP renames Devmgr_Platform_Pool;
   package PI renames Platform_Identity;
   package Q renames Platform_Quirks;
   package P2 renames Platform_Quirks.Intel_P2SB;
   package GC renames Platform_Quirks.Intel_GPIO_Communities;
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

   Checks, Failures : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   ---------------------------------------------------------------------------
   --  The IdeaPad Slim 3i's facts.
   ---------------------------------------------------------------------------
   --  .acpi-proof-work/laptop-acpi-1-tables/facp.dat, its first 36 bytes.
   Laptop_FADT_Header : constant PI.Byte_Array (0 .. 35) :=
     [70, 65, 67, 80, 20, 1, 0, 0, 6, 150, 76, 69, 78, 79, 86, 79, 67, 66, 45, 48, 49, 32, 32, 32,
      1, 0, 0, 0, 65, 67, 80, 73, 0, 0, 4, 0];


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


begin
   Identity_And_Table;
   P2SB_Steps;
   Put_Line ("platform-quirks:" & Natural'Image (Checks) & " checks," & Natural'Image (Failures) & " failures");
   if Failures /= 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;
