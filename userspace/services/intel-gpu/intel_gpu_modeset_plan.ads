with Interfaces; use Interfaces;
with CuBit.Monitor_EDID;
with Intel_GPU_Display_Registers;
with Intel_GPU_Display_Decode;
with Intel_GPU_Transcoder_Timing;
--  DISP-006/008: every register value one HDMI output's mode change writes,
--  as a value (Configuration), and the target configuration for an EDID
--  mode computed from the firmware's (Target). No MMIO: the sequencer
--  (Intel_GPU_HDMI_Modeset) writes these words in i915's order and restores
--  the firmware Configuration on any failure.
--
--  What changes for the target: the combo PLL dividers (WRPLL search), the
--  transcoder timings and pipe source, TRANS_DDI_FUNC_CTL's sync polarity
--  and HDMI 2.0 scrambling bits, plane 1's size and stride, plane 1's
--  watermarks (conservative: level 0 nearly the whole allocation, deeper
--  levels off, SAGV like level 0, transition off), infoframes off, and the
--  sink's SCDC TMDS configuration. Everything else is the firmware's: the
--  port, PLL, transcoder, DDI buffer word, TRANSCONF, plane format and DDB.
package Intel_GPU_Modeset_Plan with SPARK_Mode, Pure is
   package R renames Intel_GPU_Display_Registers;
   package D renames Intel_GPU_Display_Decode;
   package T renames Intel_GPU_Transcoder_Timing;

   --  Plane 1 watermarks: six levels, SAGV, SAGV transition, transition
   --  (i915 skl_universal_plane_regs.h order).
   type Watermark_Slot is
     (Level_0, Level_1, Level_2, Level_3, Level_4, Level_5, SAGV, SAGV_Transition, Transition);
   type Watermarks is array (Watermark_Slot) of Unsigned_32;

   --  SCDC TMDS_Config (HDMI 2.0 SCDC register 0x20): bit 1 TMDS bit clock
   --  ratio 1/40, bit 0 scrambling enable.
   SCDC_TMDS_Config : constant := 16#20#;
   SCDC_Scrambler_Status : constant := 16#21#;
   TMDS_Ratio_1_40 : constant Unsigned_8 := 2;
   TMDS_Scrambling : constant Unsigned_8 := 1;

   type Configuration is record
      CFGCR0, CFGCR1 : Unsigned_32 := 0;
      Timing : T.Words;
      Function_Word : Unsigned_32 := 0;
      Transcoder_Configuration : Unsigned_32 := 0;
      Buffer_Control : Unsigned_32 := 0;
      Infoframes : Unsigned_32 := 0;
      Plane_Control, Plane_Stride, Plane_Position, Plane_Size, Plane_Offset, Plane_Surface : Unsigned_32 := 0;
      Plane_Buffer : Unsigned_32 := 0;
      Plane_WM : Watermarks := [others => 0];
      --  Write TMDS_Config to the sink's SCDC (HDMI sinks with SCDC only).
      Write_SCDC : Boolean := False;
      TMDS_Config : Unsigned_8 := 0;
   end record;

   --  Which output: fixed by the firmware's choice.
   type Output is record
      Transcoder : R.Transcoder := R.A;
      Port : R.DDI_Port := R.Port_A;
      PLL : R.Combo_PLL := R.DPLL0;
   end record;

   --  Plane 1 register offsets in the pipe's page.
   PLANE_CTL : constant := 16#180#;
   PLANE_STRIDE : constant := 16#188#;
   PLANE_POS : constant := 16#18C#;
   PLANE_SIZE : constant := 16#190#;
   PLANE_SURF : constant := 16#19C#;
   PLANE_OFFSET : constant := 16#1A4#;
   PLANE_SURFLIVE : constant := 16#1AC#;
   PLANE_WM_First : constant := 16#240#;
   PLANE_BUF_CFG : constant := 16#27C#;
   PIPEDSL : constant := 16#000#;
   PIPE_FRMCOUNT : constant := 16#040#;
   Plane_Enable : constant Unsigned_32 := 16#8000_0000#;
   function Watermark_Offset (Slot : Watermark_Slot) return Unsigned_32 is
     (case Slot is
        when Level_0 .. Level_5 => PLANE_WM_First + 4 * Watermark_Slot'Pos (Slot),
        when SAGV => 16#258#, when SAGV_Transition => 16#25C#, when Transition => 16#268#);
   --  Linear stride unit (PLANE_STRIDE counts 64-byte chunks).
   Stride_Unit : constant := 64;
   --  HSW_TVIDEO_DIP_CTL in the transcoder page.
   VIDEO_DIP_CTL : constant := 16#200#;

   ---------------------------------------------------------------------------
   --  Watermarks (i915 skl_compute_plane_wm facts, as in Intel_GPU_Cursor_DDB):
   --  a level is valid when blocks + ceil (blocks / 10) + 1 fits the plane's
   --  allocation; a linear plane needs ceil (bytes per line / 512) + 1 blocks
   --  per line. Level 0 is set as high as the allocation allows, so the
   --  plane refills as early as possible at any pixel rate (more power,
   --  never an underrun from a stale firmware value).
   ---------------------------------------------------------------------------
   DDB_Block_Bytes : constant := 512;
   WM_Enable : constant Unsigned_32 := 16#8000_0000#;
   WM_Lines_Shift : constant := 14;
   WM_Lines_Max : constant := 16#1FFF#;
   WM_Blocks_Max : constant := 16#FFF#;
   subtype Allocation is Natural range 0 .. 4_096;
   function Allocation_Of (Buffer_Word : Unsigned_32) return Allocation;
   --  Largest blocks with blocks + ceil (blocks / 10) + 1 <= Size.
   function Level_0_Blocks (Size : Allocation) return Natural
     with Post => (if Size >= 3 then
                     Level_0_Blocks'Result >= 1 and then
                     Level_0_Blocks'Result + (Level_0_Blocks'Result + 9) / 10 + 1 <= Size
                   else Level_0_Blocks'Result = 0);
   function Conservative_Watermarks (Buffer_Word : Unsigned_32; Stride_Bytes : Positive) return Watermarks
     with Pre => Stride_Bytes <= 2 ** 20;

   ---------------------------------------------------------------------------
   type Target_Result (Ready : Boolean := False) is record
      case Ready is
         when True => Value : Configuration;
         when False => null;
      end case;
   end record;
   type Refusal is (None, No_PLL_Dividers, Pitch_Too_Small);

   --  The target for Mode from the firmware configuration. Pitch is the
   --  scanout buffer's row bytes (a multiple of 64). Scrambling above
   --  340 MHz needs an SCDC sink (admission checked it).
   function Target
     (Firmware : Configuration; Mode : CuBit.Monitor_EDID.Timing; Pitch : Positive;
      Reference : D.Reference_kHz; Halved : Boolean; SCDC_Sink : Boolean) return Target_Result
     with Pre => Mode.Horizontal_Front_Porch + Mode.Horizontal_Sync_Width <= Mode.Horizontal_Blank
                 and then Mode.Vertical_Front_Porch + Mode.Vertical_Sync_Width <= Mode.Vertical_Blank
                 and then Pitch mod Stride_Unit = 0 and then Pitch <= 2 ** 20;
   function Why_Not
     (Mode : CuBit.Monitor_EDID.Timing; Pitch : Positive) return Refusal;
end Intel_GPU_Modeset_Plan;
