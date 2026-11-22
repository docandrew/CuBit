with Interfaces; use Interfaces;
with Intel_GPU_Display_Registers;
--  DISP-009/010 step 1: which display ports have a monitor, decided from
--  words read once, never written. Pure: the executor reads the hot-plug
--  words (Intel_GPU_Display_Registers, "Hot-plug detection") and the EDID,
--  this unit says what they mean.
--
--  Ports are the ADL-P/N set i915 v6.16 enables (xe_lpd_display port_mask:
--  A and B on combo PHYs, TC1..TC4 on Type-C PHYs). Without the VBT, CuBit
--  cannot know which of them the board wires or as what connector; the VBT
--  child device table would refine both (DISP-002 VBT, DISP-004 DDC pins).
--
--  A port's sense follows i915's live-status functions: a combo port has a
--  sink when its SDEISR bit is set (icp_digital_port_connected); a Type-C
--  port in legacy (static) mode likewise through the PCH (SDEISR TC bit),
--  in DP-alt or Thunderbolt mode through GEN11_DE_HPD_ISR
--  (adlp_tc_phy_hpd_live_status). A live bit counts only while its pin's
--  detection enable bit is set; the firmware may leave a pin unsensed.
package Intel_GPU_Port_Detection with SPARK_Mode, Pure is
   package R renames Intel_GPU_Display_Registers;

   type Port is (DDI_A, DDI_B, DDI_TC1, DDI_TC2, DDI_TC3, DDI_TC4);
   subtype Combo_Port is Port range DDI_A .. DDI_B;
   subtype Type_C_Port is Port range DDI_TC1 .. DDI_TC4;
   DDI_Of : constant array (Port) of R.DDI_Port :=
     [DDI_A => R.Port_A, DDI_B => R.Port_B, DDI_TC1 => R.Port_TC1, DDI_TC2 => R.Port_TC2,
      DDI_TC3 => R.Port_TC3, DDI_TC4 => R.Port_TC4];
   type PHY_Kind is (Combo_PHY, Type_C_PHY);
   function PHY_Of (Item : Port) return PHY_Kind is
     (if Item in Combo_Port then Combo_PHY else Type_C_PHY);

   --  The pin's position within its group (HPD_PORT_A.., HPD_PORT_TC1..).
   subtype Pin_Position is Natural range 0 .. 3;
   function Position (Item : Port) return Pin_Position is
     (if Item in Combo_Port then Port'Pos (Item) - Port'Pos (Combo_Port'First)
      else Port'Pos (Item) - Port'Pos (Type_C_Port'First));

   --  The words, exactly as read (all ones: the read failed).
   Unreadable_Word : constant Unsigned_32 := Unsigned_32'Last;
   type Hotplug_Words is record
      South_Live : Unsigned_32 := Unreadable_Word;          --  SDEISR
      South_DDI_Control : Unsigned_32 := Unreadable_Word;   --  SHOTPLUG_CTL_DDI
      South_TC_Control : Unsigned_32 := Unreadable_Word;    --  SHOTPLUG_CTL_TC
      North_Live : Unsigned_32 := Unreadable_Word;          --  GEN11_DE_HPD_ISR
      North_TC_Control : Unsigned_32 := Unreadable_Word;    --  GEN11_TC_HOTPLUG_CTL
      North_TBT_Control : Unsigned_32 := Unreadable_Word;   --  GEN11_TBT_HOTPLUG_CTL
   end record;

   --  One bit of a word.
   subtype Bit_Number is Natural range 0 .. 31;
   function Bit (Word : Unsigned_32; Number : Bit_Number) return Boolean is
     ((Shift_Right (Word, Number) and 1) = 1);
   function Enable_Bit (Item : Port) return Bit_Number is
     (R.Hotplug_Field_Bits * Position (Item) + R.Hotplug_Enable_In_Field);
   function South_Live_Bit (Item : Port) return Bit_Number is
     (if Item in Combo_Port then R.SDE_DDI_Hotplug_First_Bit + Position (Item)
      else R.SDE_TC_Hotplug_First_Bit + Position (Item));
   function DP_Alt_Live_Bit (Item : Type_C_Port) return Bit_Number is
     (R.GEN11_TC_Hotplug_First_Bit + Position (Item));
   function TBT_Live_Bit (Item : Type_C_Port) return Bit_Number is
     (R.GEN11_TBT_Hotplug_First_Bit + Position (Item));

   --  What the live state says. Detection_Off: no source of this port has
   --  its detection enabled, so its live bits mean nothing.
   type Sense is (Unreadable, Detection_Off, No_Sink, Sink);
   --  Where the sink was seen: Native is a combo port's own pin; the rest
   --  are the Type-C modes (i915 TC_PORT_LEGACY, DP_ALT, TBT_ALT).
   type Sink_Path is (No_Path, Native, Legacy, DP_Alternate, Thunderbolt_Alternate);
   type Port_Sense is record
      Live : Sense := Unreadable;
      Path : Sink_Path := No_Path;
   end record;
   function Well_Formed (Item : Port; Value : Port_Sense) return Boolean is
     (if Value.Live = Sink
      then (if Item in Combo_Port then Value.Path = Native
            else Value.Path in Legacy | DP_Alternate | Thunderbolt_Alternate)
      else Value.Path = No_Path);
   type Sense_Set is array (Port) of Port_Sense;

   function Decode_Port (Words : Hotplug_Words; Item : Port) return Port_Sense
     with Post => Well_Formed (Item, Decode_Port'Result);
   function Decode (Words : Hotplug_Words) return Sense_Set
     with Post => (for all P in Port => Decode'Result (P) = Decode_Port (Words, P));

   ---------------------------------------------------------------------------
   --  The EDID read and the verdict.
   ---------------------------------------------------------------------------
   --  Over which channel a port's EDID is read: the DDC pins (GMBUS, i915's
   --  default pin for the port) for a combo port or a legacy Type-C port;
   --  AUX for DP-alt / Thunderbolt. None: no read is due.
   type EDID_Channel is (No_Channel, DDC_Channel, AUX_Channel);
   function Channel_For (Is_Lit : Boolean; Value : Port_Sense) return EDID_Channel is
     (if Is_Lit then No_Channel
      elsif Value.Live = Sink then
        (if Value.Path in DP_Alternate | Thunderbolt_Alternate then AUX_Channel else DDC_Channel)
      elsif Value.Live = Detection_Off then DDC_Channel
      else No_Channel);

   --  The typed outcome of the EDID read. Needs_AUX: a DP-alt/TBT sink whose
   --  EDID needs the AUX channel and its power well, which detection does
   --  not touch (DISP-011).
   type EDID_Outcome is
     (Not_Read, Read_Valid, No_Answer, Timed_Out, Bad_Checksum, Not_Permitted, Needs_AUX);

   --  The output's state as display.svc's catalog carries it.
   type Port_State is (Lit, Connected_Not_Lit, Disconnected);
   function Decide (Is_Lit : Boolean; Value : Port_Sense; EDID : EDID_Outcome) return Port_State is
     (if Is_Lit then Lit
      elsif Value.Live = Sink then Connected_Not_Lit
      elsif Value.Live in Unreadable | Detection_Off and then EDID = Read_Valid then Connected_Not_Lit
      else Disconnected);

   --  What the connector is, as far as the evidence says: DP-alt/TBT is
   --  DisplayPort; an EDID over the DDC pins is HDMI when the monitor
   --  says so (HDMI vendor block, or the EDID 1.4 digital interface field),
   --  else DVI. A lit port's from its DDI function (Lit_Connector).
   type Connector is (Unknown_Connector, HDMI, DVI, DisplayPort, Embedded_DisplayPort);
   --  EDID base block bytes 18/19 (version, revision) and 20 (video input:
   --  bit 7 digital; bits 3:0 the interface from revision 4 on).
   EDID_Revision_Byte : constant := 19;
   EDID_Video_Input_Byte : constant := 20;
   EDID_Interface_Revision : constant := 4;
   Digital_Input : constant Unsigned_8 := 16#80#;
   Interface_Mask : constant Unsigned_8 := 16#0F#;
   type Digital_Interface is (Undefined_Interface, DVI_Interface, HDMI_A_Interface,
                              HDMI_B_Interface, MDDI_Interface, DisplayPort_Interface);
   for Digital_Interface use (Undefined_Interface => 0, DVI_Interface => 1, HDMI_A_Interface => 2,
                              HDMI_B_Interface => 3, MDDI_Interface => 4, DisplayPort_Interface => 5);
   function Interface_Of (Revision, Video_Input : Unsigned_8) return Digital_Interface is
     (if Revision < EDID_Interface_Revision or else (Video_Input and Digital_Input) = 0
      then Undefined_Interface
      elsif (Video_Input and Interface_Mask) > Digital_Interface'Enum_Rep (Digital_Interface'Last)
      then Undefined_Interface
      --  The codes are contiguous from zero: position = code.
      else Digital_Interface'Val (Video_Input and Interface_Mask));
   function Connector_Of
     (Value : Port_Sense; EDID : EDID_Outcome; Kind : Digital_Interface; HDMI_Sink : Boolean)
      return Connector is
     (if Value.Live = Sink and then Value.Path in DP_Alternate | Thunderbolt_Alternate then DisplayPort
      elsif EDID /= Read_Valid then Unknown_Connector
      elsif HDMI_Sink or else Kind in HDMI_A_Interface | HDMI_B_Interface then HDMI
      else DVI);

   ---------------------------------------------------------------------------
   --  Names for the boot log.
   ---------------------------------------------------------------------------
   function Port_Name (Item : Port) return String is
     (case Item is
        when DDI_A => "DDI-A", when DDI_B => "DDI-B", when DDI_TC1 => "DDI-TC1",
        when DDI_TC2 => "DDI-TC2", when DDI_TC3 => "DDI-TC3", when DDI_TC4 => "DDI-TC4");
   function PHY_Name (Item : Port) return String is
     (case PHY_Of (Item) is when Combo_PHY => "combo", when Type_C_PHY => "type-c");
   function Sense_Name (Item : Sense) return String is
     (case Item is
        when Unreadable => "unreadable", when Detection_Off => "hpd-detection-off",
        when No_Sink => "no-sink", when Sink => "sink");
   function Path_Name (Item : Sink_Path) return String is
     (case Item is
        when No_Path => "none", when Native => "hpd", when Legacy => "legacy",
        when DP_Alternate => "dp-alt", when Thunderbolt_Alternate => "tbt-alt");
   function Outcome_Name (Item : EDID_Outcome) return String is
     (case Item is
        when Not_Read => "not-read", when Read_Valid => "ok", when No_Answer => "no-answer",
        when Timed_Out => "timeout", when Bad_Checksum => "bad-checksum",
        when Not_Permitted => "not-permitted", when Needs_AUX => "needs-aux");
   function State_Name (Item : Port_State) return String is
     (case Item is
        when Lit => "lit", when Connected_Not_Lit => "connected",
        when Disconnected => "disconnected");
   function Connector_Name (Item : Connector) return String is
     (case Item is
        when Unknown_Connector => "unknown", when HDMI => "HDMI", when DVI => "DVI",
        when DisplayPort => "DP", when Embedded_DisplayPort => "eDP");
end Intel_GPU_Port_Detection;
