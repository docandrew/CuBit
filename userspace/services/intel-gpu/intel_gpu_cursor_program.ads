with Interfaces;
with Intel_GPU_Display_Topology;
--  Hardware cursor plane programming for ADL-P/ADL-N (display version 13)
--  and TGL-family (12) pipes: the register values only, no MMIO.
--
--  Reference (hardware facts only, no code copied): Linux v6.16 i915
--  display/intel_cursor_regs.h (CURCNTR/CURBASE/CURPOS/CUR_FBC_CTL layout,
--  MCURSOR_* modes, CURSOR_POS_* sign-magnitude fields) and
--  display/intel_cursor.c (i9xx_cursor_ctl: ARGB modes by width and the
--  Wa_22012358565 arbitration-slot setting for display version 13;
--  i9xx_cursor_size_ok: widths 64/128/256, heights 8 .. width via
--  CUR_FBC_CTL when unrotated; intel_cursor_position; i9xx_cursor_update_arm:
--  CUR_FBC_CTL, CURCNTR, CURPOS, then CURBASE, whose write arms the update;
--  i9xx_cursor_min_alignment: 4 KiB, 64 KiB with the VT-d scanout
--  workaround). Linear, unrotated, premultiplied ARGB8888 only.
package Intel_GPU_Cursor_Program with SPARK_Mode is
   use Interfaces;

   type Cursor_Width is (Width_64, Width_128, Width_256);
   function Pixels (Width : Cursor_Width) return Positive is
     (case Width is
        when Width_64 => 64, when Width_128 => 128, when Width_256 => 256);
   Minimum_Height : constant := 8;
   subtype Cursor_Height is Positive range Minimum_Height .. 256;

   --  i915 builds the ADL/TGL cursor list with display versions 12 and 13.
   type Display_Version is range 12 .. 13;
   Arbitration_Workaround_Version : constant Display_Version := 13;

   --  Image top-left relative to the pipe; 15-bit magnitudes.
   Magnitude_Bits : constant := 15;
   Magnitude_Limit : constant := 2 ** Magnitude_Bits - 1;
   type Position is range -Magnitude_Limit .. Magnitude_Limit;

   Base_Alignment : constant := 4_096;
   VTd_Base_Alignment : constant := 65_536;

   type Image is record
      Width  : Cursor_Width := Width_64;
      Height : Cursor_Height := 64;
      --  GGTT offset of the linear ARGB surface, pitch = 4 * width.
      Base   : Unsigned_32 := Base_Alignment;
   end record;

   function Alignment (VTd_Workaround : Boolean) return Unsigned_32 is
     (if VTd_Workaround then VTd_Base_Alignment else Base_Alignment);

   function Valid_Image (Item : Image; VTd_Workaround : Boolean)
      return Boolean is
     (Item.Height <= Pixels (Item.Width) and then Item.Base /= 0 and then
      Item.Base mod Alignment (VTd_Workaround) = 0);

   --  A visible cursor must overlap the pipe: never place it entirely off.
   function Valid_Position
     (Item : Image; X, Y : Position; Pipe_Width, Pipe_Height : Positive)
      return Boolean is
     (Integer (X) > -Pixels (Item.Width) and then
      Integer (Y) > -Item.Height and then
      Integer (X) < Pipe_Width and then Integer (Y) < Pipe_Height);

   type Register_Values is record
      FBC_Control, Control, Position_Word, Base : Unsigned_32 := 0;
   end record;

   --  CURCNTR bits.
   ARGB_Mode_Bit   : constant Unsigned_32 := 16#20#;
   Mode_64         : constant Unsigned_32 := 16#07#;
   Mode_128        : constant Unsigned_32 := 16#02#;
   Mode_256        : constant Unsigned_32 := 16#03#;
   Arbitration_Shift : constant := 28;
   Arbitration_Slots : constant Unsigned_32 := 1;
   --  CUR_FBC_CTL: enable plus (height - 1) for non-square cursors.
   FBC_Enable : constant Unsigned_32 := 16#8000_0000#;
   --  CURPOS sign bits and field positions.
   X_Sign  : constant Unsigned_32 := 16#0000_8000#;
   Y_Sign  : constant Unsigned_32 := 16#8000_0000#;
   Y_Shift : constant := 16;

   function Mode (Width : Cursor_Width) return Unsigned_32 is
     (ARGB_Mode_Bit or
        (case Width is
           when Width_64 => Mode_64, when Width_128 => Mode_128,
           when Width_256 => Mode_256));

   function Magnitude (Value : Position) return Unsigned_32 is
     (Unsigned_32 (abs Integer (Value)))
     with Post => Magnitude'Result <= Magnitude_Limit;

   function Encode_Position (X, Y : Position) return Unsigned_32
     with Post =>
       (Encode_Position'Result and (2 ** Magnitude_Bits - 1)) = Magnitude (X)
       and then
       (Shift_Right (Encode_Position'Result, Y_Shift) and
          (2 ** Magnitude_Bits - 1)) = Magnitude (Y)
       and then ((Encode_Position'Result and X_Sign) /= 0) = (X < 0)
       and then ((Encode_Position'Result and Y_Sign) /= 0) = (Y < 0);

   --  Decoded position for diagnostics and tests.
   function Decode_X (Word : Unsigned_32) return Position is
     (if (Word and X_Sign) /= 0 then -Position (Word and Magnitude_Limit)
      else Position (Word and Magnitude_Limit));
   function Decode_Y (Word : Unsigned_32) return Position is
     (if (Word and Y_Sign) /= 0
      then -Position (Shift_Right (Word, Y_Shift) and Magnitude_Limit)
      else Position (Shift_Right (Word, Y_Shift) and Magnitude_Limit));

   function Encode
     (Item : Image; X, Y : Position; Version : Display_Version;
      VTd_Workaround : Boolean) return Register_Values
     with Pre => Valid_Image (Item, VTd_Workaround),
          Post =>
            Encode'Result.Base = Item.Base and then
            Encode'Result.Position_Word = Encode_Position (X, Y) and then
            Encode'Result.Control =
              (Mode (Item.Width) or
                 (if Version = Arbitration_Workaround_Version
                  then Shift_Left (Arbitration_Slots, Arbitration_Shift)
                  else 0)) and then
            Encode'Result.FBC_Control =
              (if Item.Height = Pixels (Item.Width) then 0
               else FBC_Enable or Unsigned_32 (Item.Height - 1));

   --  All zero: CURCNTR mode zero disables the plane; base zero.
   Disabled : constant Register_Values := (others => 0);

   type Field is (FBC_Control, Control, Position_Field, Base);
   --  i915 order: FBC, control, position, then base, which arms the update.
   type Write_Sequence is array (Positive range <>) of Field;
   Full_Update : constant Write_Sequence :=
     [FBC_Control, Control, Position_Field, Base];
   --  Position-only update still rewrites base to arm it.
   Move_Update : constant Write_Sequence := [Position_Field, Base];

   Pipe_Stride : constant := 16#1000#;
   Pipe_A_Page : constant := 16#70000#;
   function Page (Pipe : Intel_GPU_Display_Topology.Pipe) return Unsigned_32 is
     (Pipe_A_Page + Pipe_Stride * Intel_GPU_Display_Topology.Pipe'Pos (Pipe))
     with Post => Page'Result mod Pipe_Stride = 0;
   function Page_Offset (Item : Field) return Unsigned_32 is
     (case Item is
        when Control => 16#080#, when Base => 16#084#,
        when Position_Field => 16#088#, when FBC_Control => 16#0A0#);
   function Offset (Pipe : Intel_GPU_Display_Topology.Pipe; Item : Field)
      return Unsigned_32 is (Page (Pipe) + Page_Offset (Item));
   function Value (Values : Register_Values; Item : Field) return Unsigned_32 is
     (case Item is
        when FBC_Control => Values.FBC_Control, when Control => Values.Control,
        when Position_Field => Values.Position_Word, when Base => Values.Base);
end Intel_GPU_Cursor_Program;
