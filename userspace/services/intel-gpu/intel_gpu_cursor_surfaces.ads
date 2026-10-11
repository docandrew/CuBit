with Interfaces;
with Intel_GPU_Cursor_Program;
--  Intel cursor plane surfaces and buffer facts, pure (no MMIO).
--
--  display.svc writes cursor images into a staging buffer (the Map_Plane
--  grant, Edge x Edge premultiplied ARGB, pitch 4 * Edge). Show copies the
--  image into one of Surface_Count GGTT surfaces, packed at the pitch of
--  the chosen cursor width, and arms CURBASE to it. The display engine may
--  be reading the surface CURBASE was last armed with, or the one
--  CURSURFLIVE last reported, so Show writes neither: with three distinct
--  surfaces one is always free (Pick).
--
--  Hardware facts (Linux v6.16 i915, facts only): CURSURFLIVE at
--  0x700AC + 0x1000 * pipe (display/intel_cursor_regs.h); cursor DDB
--  allocation CUR_BUF_CFG at 0x7017C (start bits 11:0, end bits 27:16 on
--  ADL-P and newer, end inclusive) and level-0 watermark CUR_WM at 0x70140
--  (enable bit 31, ignore-lines bit 30, blocks bits 11:0); i915 reserves a
--  fixed cursor allocation for every active pipe (skl_watermark.c
--  skl_crtc_allocate_plane_ddb, skl_cursor_allocation).
package Intel_GPU_Cursor_Surfaces with SPARK_Mode is
   use Interfaces;
   package CP renames Intel_GPU_Cursor_Program;
   use type CP.Cursor_Width;

   Edge : constant := 256;
   Bytes_Per_Pixel : constant := 4;
   Surface_Bytes : constant := Edge * Edge * Bytes_Per_Pixel;
   Surface_Count : constant := 3;
   type Surface_Index is range 1 .. Surface_Count;
   type Bases is array (Surface_Index) of Unsigned_32;

   --  GGTT surfaces: nonzero, aligned for the VT-d scanout rule, distinct.
   function Valid_Bases (Items : Bases) return Boolean is
     (for all I in Surface_Index =>
        Items (I) /= 0 and then Items (I) mod CP.VTd_Base_Alignment = 0 and then
        (for all J in Surface_Index => (if I /= J then Items (I) /= Items (J))));

   --  A surface the display engine cannot be reading: neither the armed
   --  CURBASE nor the CURSURFLIVE value read after that arm.
   function Writable (Items : Bases; Index : Surface_Index;
                      Programmed, Live : Unsigned_32) return Boolean is
     (Items (Index) /= Programmed and then Items (Index) /= Live);

   function Pick (Items : Bases; Programmed, Live : Unsigned_32)
      return Surface_Index
     with Global => null,
          Pre => Valid_Bases (Items),
          Post => Writable (Items, Pick'Result, Programmed, Live);

   --  Bounding extent of the nonzero pixels of a staged image (top-left
   --  anchored), 0 x 0 for an empty image.
   subtype Image_Extent is Natural range 0 .. Edge;
   type Extent is record
      Width, Height : Image_Extent := 0;
   end record;

   --  Smallest legal cursor (widths 64/128/256, heights 8 .. width) that
   --  holds a non-empty image.
   function Fit (Item : Extent) return CP.Image
     with Global => null,
          Pre => Item.Width > 0 and then Item.Height > 0,
          Post => CP.Pixels (Fit'Result.Width) >= Item.Width and then
                  CP.Pixels (Fit'Result.Width) >= Item.Height and then
                  Fit'Result.Height >= Item.Height and then
                  Fit'Result.Height <= CP.Pixels (Fit'Result.Width) and then
                  Fit'Result.Height = Integer'Max (CP.Minimum_Height, Item.Height) and then
                  (Fit'Result.Width = CP.Width_64 or else
                   Integer'Max (Item.Width, Item.Height) >
                     CP.Pixels (CP.Cursor_Width'Pred (Fit'Result.Width)));

   --  Register page offsets beyond Intel_GPU_Cursor_Program's four fields.
   Live_Base_Offset    : constant Unsigned_32 := 16#0AC#;
   Watermark_Offset    : constant Unsigned_32 := 16#140#;
   Buffer_Config_Offset : constant Unsigned_32 := 16#17C#;

   Buffer_Start_Mask : constant Unsigned_32 := 16#FFF#;
   Buffer_End_Shift  : constant := 16;
   Buffer_End_Mask   : constant Unsigned_32 := 16#FFF#;
   Block_Limit : constant := 4_096;
   subtype Block_Count is Natural range 0 .. Block_Limit;

   --  Blocks the cursor owns in the data buffer; zero when unallocated.
   function Allocation (Buffer_Config : Unsigned_32) return Block_Count is
     (if (Shift_Right (Buffer_Config, Buffer_End_Shift) and Buffer_End_Mask) = 0 or else
         (Shift_Right (Buffer_Config, Buffer_End_Shift) and Buffer_End_Mask) <
           (Buffer_Config and Buffer_Start_Mask)
      then 0
      else Block_Count
        ((Shift_Right (Buffer_Config, Buffer_End_Shift) and Buffer_End_Mask) -
           (Buffer_Config and Buffer_Start_Mask) + 1));

   Watermark_Enable       : constant Unsigned_32 := 16#8000_0000#;
   Watermark_Ignore_Lines : constant Unsigned_32 := 16#4000_0000#;
   Watermark_Blocks_Mask  : constant Unsigned_32 := 16#FFF#;
   function Watermark_Enabled (Word : Unsigned_32) return Boolean is
     ((Word and Watermark_Enable) /= 0);

   --  Minimum allocation the cursor needs before it is offered: a level-0
   --  watermark below the whole allocation needs at least two blocks.
   Minimum_Allocation : constant := 2;

   --  Level-0 watermark for a firmware mode that allocated the cursor but
   --  left its watermark disabled: the deepest legal value (one block short
   --  of the allocation, lines ignored), which asks for data earliest. A
   --  conservative stand-in for i915's latency-based value until DISP-001
   --  computes watermarks (docs/display-modesetting.md 3.12) [HW?].
   function Fallback_Watermark (Blocks : Block_Count) return Unsigned_32 is
     (Watermark_Enable or Watermark_Ignore_Lines or Unsigned_32 (Blocks - 1))
     with Pre => Blocks >= Minimum_Allocation,
          Post => Watermark_Enabled (Fallback_Watermark'Result) and then
                  Natural (Fallback_Watermark'Result and Watermark_Blocks_Mask) < Blocks;

   --  Ready_Driver_Allocation: the firmware left none and the driver
   --  allocated it (Intel_GPU_Cursor_DDB); Allocation_Refused: that plan
   --  was not possible; Latch_Timeout: a programmed change was not seen to
   --  latch within the deadline (no cursor offered).
   type Readiness is (Ready_Firmware_Watermark, Ready_Fallback_Watermark,
                      Ready_Driver_Allocation, No_Allocation,
                      Allocation_Refused, Latch_Timeout);
   function Classify (Buffer_Config, Watermark : Unsigned_32) return Readiness is
     (if Allocation (Buffer_Config) < Minimum_Allocation then No_Allocation
      elsif Watermark_Enabled (Watermark) then Ready_Firmware_Watermark
      else Ready_Fallback_Watermark);
   function Readiness_Name (Item : Readiness) return String is
     (case Item is
        when Ready_Firmware_Watermark => "firmware-watermark",
        when Ready_Fallback_Watermark => "fallback-watermark",
        when Ready_Driver_Allocation => "driver-allocated-DDB",
        when No_Allocation => "no-cursor-DDB-allocation",
        when Allocation_Refused => "DDB-allocation-refused",
        when Latch_Timeout => "latch-timeout");
end Intel_GPU_Cursor_Surfaces;
