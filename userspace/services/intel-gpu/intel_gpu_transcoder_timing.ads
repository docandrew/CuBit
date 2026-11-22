with Interfaces; use Interfaces;
with CuBit.Monitor_EDID;
--  A transcoder's timing generator (TRANS_HTOTAL .. TRANS_VSYNC) and its
--  pipe's source size (PIPESRC), as values. Each word holds two 16-bit
--  fields stored minus one: end/total in 31:16, start/active in 15:0
--  (i915 intel_set_transcoder_timings / intel_get_transcoder_timings).
--  Decode reads any words (DISP-002 readout); Encode writes a progressive
--  mode whose blanking starts at the active edge (no borders, as i915
--  programs every mode on display version 13).
package Intel_GPU_Transcoder_Timing with SPARK_Mode, Pure is
   --  One field plus one: 1 .. 65536.
   subtype Count is Positive range 1 .. 2 ** 16;
   Field_Bits : constant := 16;
   Field_Mask : constant Unsigned_32 := 2 ** Field_Bits - 1;

   type Words is record
      H_Total, H_Blank, H_Sync, V_Total, V_Blank, V_Sync, Source : Unsigned_32 := 0;
   end record;

   type Axis is record
      Active, Total, Blank_Start, Blank_End, Sync_Start, Sync_End : Count := 1;
   end record;
   type Timing is record
      Horizontal, Vertical : Axis;
      --  PIPESRC: what the pipe scans (no scaler: equals the active size).
      Source_Width, Source_Height : Count := 1;
   end record;

   function Low (Word : Unsigned_32) return Count is (Natural (Word and Field_Mask) + 1);
   function High (Word : Unsigned_32) return Count is (Natural (Shift_Right (Word, Field_Bits)) + 1);
   --  Low (Pack (S, F)) = S and High (Pack (S, F)) = F: tested exhaustively
   --  over one field and at every bit boundary of the other (provers do not
   --  close the integer/bit-vector conversion).
   function Pack (Start, Finish : Count) return Unsigned_32 is
     (Unsigned_32 (Start - 1) or Shift_Left (Unsigned_32 (Finish - 1), Field_Bits));

   function Decode (Item : Words) return Timing is
     ((Horizontal => (Active => Low (Item.H_Total), Total => High (Item.H_Total),
                      Blank_Start => Low (Item.H_Blank), Blank_End => High (Item.H_Blank),
                      Sync_Start => Low (Item.H_Sync), Sync_End => High (Item.H_Sync)),
       Vertical => (Active => Low (Item.V_Total), Total => High (Item.V_Total),
                    Blank_Start => Low (Item.V_Blank), Blank_End => High (Item.V_Blank),
                    Sync_Start => Low (Item.V_Sync), Sync_End => High (Item.V_Sync)),
       Source_Width => High (Item.Source), Source_Height => Low (Item.Source)));

   --  Decode (Encode (T)) = T follows from Pack (tested).
   function Encode (Item : Timing) return Words is
     ((H_Total => Pack (Item.Horizontal.Active, Item.Horizontal.Total),
       H_Blank => Pack (Item.Horizontal.Blank_Start, Item.Horizontal.Blank_End),
       H_Sync => Pack (Item.Horizontal.Sync_Start, Item.Horizontal.Sync_End),
       V_Total => Pack (Item.Vertical.Active, Item.Vertical.Total),
       V_Blank => Pack (Item.Vertical.Blank_Start, Item.Vertical.Blank_End),
       V_Sync => Pack (Item.Vertical.Sync_Start, Item.Vertical.Sync_End),
       Source => Pack (Item.Source_Height, Item.Source_Width)));

   --  The ordering a progressive mode needs: active ≤ blank start ≤ sync
   --  start < sync end ≤ blank end ≤ total, active < total.
   function Ordered (Item : Axis) return Boolean is
     (Item.Active <= Item.Blank_Start and then Item.Blank_Start <= Item.Sync_Start and then
      Item.Sync_Start < Item.Sync_End and then Item.Sync_End <= Item.Blank_End and then
      Item.Blank_End <= Item.Total and then Item.Active < Item.Total);
   function Plausible (Item : Timing) return Boolean is
     (Ordered (Item.Horizontal) and then Ordered (Item.Vertical) and then
      Item.Source_Width = Item.Horizontal.Active and then
      Item.Source_Height = Item.Vertical.Active);

   --  The generator words for an EDID timing (i915 drm_mode crtc_* fields:
   --  sync start = active + front porch, sync end = start + pulse width,
   --  blanking from the active edge to the total).
   function From_EDID (Mode : CuBit.Monitor_EDID.Timing) return Timing is
     ((Horizontal =>
         (Active => Mode.Width, Total => Mode.Width + Mode.Horizontal_Blank,
          Blank_Start => Mode.Width, Blank_End => Mode.Width + Mode.Horizontal_Blank,
          Sync_Start => Mode.Width + Mode.Horizontal_Front_Porch,
          Sync_End => Mode.Width + Mode.Horizontal_Front_Porch + Mode.Horizontal_Sync_Width),
       Vertical =>
         (Active => Mode.Height, Total => Mode.Height + Mode.Vertical_Blank,
          Blank_Start => Mode.Height, Blank_End => Mode.Height + Mode.Vertical_Blank,
          Sync_Start => Mode.Height + Mode.Vertical_Front_Porch,
          Sync_End => Mode.Height + Mode.Vertical_Front_Porch + Mode.Vertical_Sync_Width),
       Source_Width => Mode.Width, Source_Height => Mode.Height))
     with Pre => Mode.Horizontal_Front_Porch + Mode.Horizontal_Sync_Width <= Mode.Horizontal_Blank
                 and then Mode.Vertical_Front_Porch + Mode.Vertical_Sync_Width <= Mode.Vertical_Blank,
          Post => Plausible (From_EDID'Result);

   --  Nominal refresh in millihertz from a pixel clock in kHz.
   function Refresh_Millihertz (Item : Timing; Pixel_kHz : Natural) return Unsigned_64 is
     (Unsigned_64 (Pixel_kHz) * 1_000_000 /
        (Unsigned_64 (Item.Horizontal.Total) * Unsigned_64 (Item.Vertical.Total)));
end Intel_GPU_Transcoder_Timing;
