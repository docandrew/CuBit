--  DISP-009/010 step 1: port detection against a register model. The
--  hot-plug words come from a table (every other read is all ones), the
--  DDC reads go through the real Intel_GPU_GMBUS against GMBUS_Model.
--  Cases: each live-status bit names its own port and path; a port with
--  nothing attached; an attached port whose EDID read fails (typed
--  outcome); a Type-C port in legacy mode with its EDID; DP-alt and TBT
--  (needs AUX, never read); detection disabled by the firmware (DDC probe);
--  unreadable words; the lit port is never probed.
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Monitor_EDID.Catalog;
with EDID_Fixtures;
with GMBUS_Model;
with Intel_GPU_Display_Registers;
with Intel_GPU_GMBUS;
with Intel_GPU_GMBUS_Outcome;
with Intel_GPU_Port_Detection;
with Intel_GPU_Port_Scan;
procedure Detection_Tests is
   package C renames CuBit.Monitor_EDID.Catalog;
   package R renames Intel_GPU_Display_Registers;
   package PD renames Intel_GPU_Port_Detection;
   package G is new Intel_GPU_GMBUS (GMBUS_Model.Read, GMBUS_Model.Write, GMBUS_Model.Now_Us);
   function Outcome_Of is new Intel_GPU_GMBUS_Outcome (G);
   use type PD.Sense, PD.Sink_Path, PD.Port_State, PD.EDID_Outcome, PD.EDID_Channel, PD.Connector,
            PD.Digital_Interface, PD.Port, PD.Port_Sense, PD.PHY_Kind;
   use type G.Status;

   Words : PD.Hotplug_Words;
   Other_Reads : Natural := 0;
   Lit : PD.Port := PD.DDI_A;
   Any_Lit : Boolean := False;
   DDC_Reads : array (PD.Port) of Natural := [others => 0];

   --  The register model: the six hot-plug words; anything else is a
   --  read the detection must not make.
   function Read_MMIO (Offset : Unsigned_32) return Unsigned_32 is
   begin
      case Offset is
         when R.SDEISR => return Words.South_Live;
         when R.SHOTPLUG_CTL_DDI => return Words.South_DDI_Control;
         when R.SHOTPLUG_CTL_TC => return Words.South_TC_Control;
         when R.GEN11_DE_HPD_ISR => return Words.North_Live;
         when R.GEN11_TC_HOTPLUG_CTL => return Words.North_TC_Control;
         when R.GEN11_TBT_HOTPLUG_CTL => return Words.North_TBT_Control;
         when others =>
            Other_Reads := Other_Reads + 1;
            return PD.Unreadable_Word;
      end case;
   end Read_MMIO;
   function Is_Lit (Item : PD.Port) return Boolean is (Any_Lit and then Item = Lit);
   --  GMBUS pins are in port order (i915 gmbus_pins_icp: DDI A, B, TC1..4).
   procedure Read_DDC_Base (Item : PD.Port; Base : out C.Block; Outcome : out PD.EDID_Outcome) is
      Data : G.Bytes (1 .. G.EDID_Block_Bytes);
      Status : G.Status;
   begin
      DDC_Reads (Item) := DDC_Reads (Item) + 1;
      G.Read_EDID_Block (G.Pin'Val (PD.Port'Pos (Item)), 0, Data, Status);
      for I in Base'Range loop
         Base (I) := Data (I + 1);
      end loop;
      Outcome := Outcome_Of (Status, Status = G.OK and then C.Valid_Checksum (Base));
   end Read_DDC_Base;
   package Scan is new Intel_GPU_Port_Scan (Read_MMIO, Is_Lit, Read_DDC_Base);

   All_Enabled : constant Unsigned_32 := 16#8888#;
   Quiet : constant PD.Hotplug_Words :=
     (South_Live => 0, South_DDI_Control => All_Enabled, South_TC_Control => All_Enabled,
      North_Live => 0, North_TC_Control => All_Enabled, North_TBT_Control => All_Enabled);
   Found : Scan.Results;
   Blocks : Scan.Bases;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         raise Program_Error with "detection check failed: " & What;
      end if;
   end Check;
   procedure Run is
   begin
      DDC_Reads := [others => 0];
      Other_Reads := 0;
      Scan.Capture (Words);
      Scan.Scan (Words, Found, Blocks);
      Check (Other_Reads = 0, "reads outside the hot-plug words");
   end Run;
   --  The fixture monitor on GMBUS pin code Pin (zero: none).
   procedure Attach (Pin : Unsigned_32) is
      Image : GMBUS_Model.Byte_Array (0 .. 511) := [others => 0];
   begin
      for I in 0 .. 127 loop
         Image (I) := EDID_Fixtures.UHD_HDMI_Base (I);
      end loop;
      GMBUS_Model.No_Ready := False;
      GMBUS_Model.Not_Writable := False;
      GMBUS_Model.Reset (Image, Pin);
   end Attach;
   function Code (Item : PD.Port) return Unsigned_32 is (G.Pin_Code (G.Pin'Val (PD.Port'Pos (Item))));
begin
   --  Bit positions are i915's (hpd_icp, hpd_gen11, HOTPLUG_CTL enables).
   Check (PD.South_Live_Bit (PD.DDI_A) = 16 and PD.South_Live_Bit (PD.DDI_B) = 17, "SDE DDI bits");
   Check (PD.South_Live_Bit (PD.DDI_TC1) = 24 and PD.South_Live_Bit (PD.DDI_TC4) = 27, "SDE TC bits");
   Check (PD.DP_Alt_Live_Bit (PD.DDI_TC1) = 16 and PD.TBT_Live_Bit (PD.DDI_TC3) = 2, "DE HPD bits");
   Check (PD.Enable_Bit (PD.DDI_A) = 3 and PD.Enable_Bit (PD.DDI_B) = 7 and
          PD.Enable_Bit (PD.DDI_TC4) = 15, "HOTPLUG_CTL enable bits");

   --  Nothing attached anywhere: every port disconnected, nothing read.
   Attach (0);
   Words := Quiet;
   Run;
   for P in PD.Port loop
      Check (Found (P).Sense = (PD.No_Sink, PD.No_Path) and then Found (P).State = PD.Disconnected and then
             Found (P).Outcome = PD.Not_Read and then DDC_Reads (P) = 0, "quiet " & PD.Port_Name (P));
   end loop;

   --  Each live bit alone names its own port (and path), and only it.
   for P in PD.Port loop
      for Path in PD.Sink_Path range PD.Native .. PD.Thunderbolt_Alternate loop
         if (P in PD.Combo_Port) = (Path = PD.Native) then
            Words := Quiet;
            case Path is
               when PD.Native | PD.Legacy =>
                  Words.South_Live := Shift_Left (1, PD.South_Live_Bit (P));
               when PD.DP_Alternate => Words.North_Live := Shift_Left (1, PD.DP_Alt_Live_Bit (P));
               when PD.Thunderbolt_Alternate => Words.North_Live := Shift_Left (1, PD.TBT_Live_Bit (P));
            end case;
            Attach (Code (P));
            Run;
            for Q in PD.Port loop
               if Q = P then
                  Check (Found (Q).Sense = (PD.Sink, Path) and then Found (Q).State = PD.Connected_Not_Lit,
                         "live " & PD.Port_Name (P) & " " & PD.Path_Name (Path));
                  Check (Found (Q).Channel =
                           (if Path in PD.DP_Alternate | PD.Thunderbolt_Alternate then PD.AUX_Channel
                            else PD.DDC_Channel), "channel");
                  Check ((if Found (Q).Channel = PD.AUX_Channel
                          then Found (Q).Outcome = PD.Needs_AUX and then DDC_Reads (Q) = 0
                          else Found (Q).Outcome = PD.Read_Valid and then DDC_Reads (Q) = 1),
                         "edid " & PD.Port_Name (P));
               else
                  Check (Found (Q).State = PD.Disconnected and then DDC_Reads (Q) = 0, "others quiet");
               end if;
            end loop;
         end if;
      end loop;
   end loop;

   --  A Type-C port in legacy (static) mode, as a laptop HDMI may be: the
   --  PCH senses it, the EDID comes over its DDC pin (code 9 for TC1).
   Words := Quiet;
   Words.South_Live := Shift_Left (1, PD.South_Live_Bit (PD.DDI_TC1));
   Attach (Code (PD.DDI_TC1));
   Run;
   Check (Code (PD.DDI_TC1) = 9 and then PD.PHY_Of (PD.DDI_TC1) = PD.Type_C_PHY, "TC1 pin and PHY");
   Check (Found (PD.DDI_TC1).Sense = (PD.Sink, PD.Legacy) and then
          Found (PD.DDI_TC1).Outcome = PD.Read_Valid and then
          Found (PD.DDI_TC1).State = PD.Connected_Not_Lit and then
          C.Identify (Blocks (PD.DDI_TC1)).Name.Text (1 .. 11) = "CuBit UHD27", "legacy TC1 with EDID");
   --  Legacy wins over an alternate-mode bit on the same port.
   Words.North_Live := Shift_Left (1, PD.DP_Alt_Live_Bit (PD.DDI_TC1));
   Run;
   Check (Found (PD.DDI_TC1).Sense.Path = PD.Legacy, "legacy first");

   --  Attached (live bit) but the EDID read fails: still connected, with the
   --  typed outcome. No answer on the pin; the controller times out; the
   --  GMBUS page refuses writes.
   Words := Quiet;
   Words.South_Live := Shift_Left (1, PD.South_Live_Bit (PD.DDI_B));
   Attach (Code (PD.DDI_A));
   Run;
   Check (Found (PD.DDI_B).State = PD.Connected_Not_Lit and then Found (PD.DDI_B).Outcome = PD.No_Answer,
          "nak");
   Attach (Code (PD.DDI_B));
   GMBUS_Model.No_Ready := True;
   Run;
   Check (Found (PD.DDI_B).State = PD.Connected_Not_Lit and then Found (PD.DDI_B).Outcome = PD.Timed_Out,
          "timeout");
   Attach (Code (PD.DDI_B));
   GMBUS_Model.Not_Writable := True;
   Run;
   Check (Found (PD.DDI_B).State = PD.Connected_Not_Lit and then Found (PD.DDI_B).Outcome = PD.Not_Permitted,
          "not writable");
   Check (PD.Connector_Of (Found (PD.DDI_B).Sense, Found (PD.DDI_B).Outcome, PD.HDMI_A_Interface, True) =
          PD.Unknown_Connector, "no EDID: connector unknown");

   --  The lit port is never probed, whatever its live state says.
   Words := Quiet;
   Words.South_Live := Shift_Left (1, PD.South_Live_Bit (PD.DDI_A));
   Attach (Code (PD.DDI_A));
   Any_Lit := True;
   Lit := PD.DDI_A;
   Run;
   Check (Found (PD.DDI_A).State = PD.Lit and then DDC_Reads (PD.DDI_A) = 0, "lit port untouched");
   Any_Lit := False;

   --  Detection disabled by the firmware: live bits mean nothing; the DDC
   --  probe decides (an EDID: connected; no answer: disconnected).
   Words := (others => 0);
   Words.South_Live := Unsigned_32'Last - 1;   --  garbage live bits
   Attach (Code (PD.DDI_B));
   Run;
   for P in PD.Port loop
      Check (Found (P).Sense.Live = PD.Detection_Off and then DDC_Reads (P) = 1, "probe " & PD.Port_Name (P));
      Check (Found (P).State = (if P = PD.DDI_B then PD.Connected_Not_Lit else PD.Disconnected),
             "probe verdict " & PD.Port_Name (P));
   end loop;

   --  Unreadable words: nothing is claimed and nothing probed.
   Words := (others => PD.Unreadable_Word);
   Run;
   for P in PD.Port loop
      Check (Found (P).Sense = (PD.Unreadable, PD.No_Path) and then Found (P).State = PD.Disconnected and then
             DDC_Reads (P) = 0, "unreadable " & PD.Port_Name (P));
   end loop;

   --  The connector from the evidence.
   Check (PD.Interface_Of (4, 16#A2#) = PD.HDMI_A_Interface and then PD.Interface_Of (3, 16#A2#) =
          PD.Undefined_Interface and then PD.Interface_Of (4, 16#22#) = PD.Undefined_Interface and then
          PD.Interface_Of (4, 16#AF#) = PD.Undefined_Interface and then
          PD.Interface_Of (4, 16#A5#) = PD.DisplayPort_Interface, "interface field");
   Check (PD.Connector_Of ((PD.Sink, PD.Native), PD.Read_Valid, PD.Undefined_Interface, True) = PD.HDMI and then
          PD.Connector_Of ((PD.Sink, PD.Native), PD.Read_Valid, PD.HDMI_B_Interface, False) = PD.HDMI and then
          PD.Connector_Of ((PD.Sink, PD.Legacy), PD.Read_Valid, PD.DVI_Interface, False) = PD.DVI and then
          PD.Connector_Of ((PD.Sink, PD.DP_Alternate), PD.Needs_AUX, PD.Undefined_Interface, False) =
            PD.DisplayPort, "connector");
   Put_Line ("PASS port detection:" & Checks'Image & " checks");
end Detection_Tests;
