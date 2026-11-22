with Interfaces; use Interfaces;
with System;
with Intel_GPU_ADLN_Pixel_Blend;
package Intel_GPU_ADLN_Coarse_Pixel with SPARK_Mode is
   -- TGL Vol2a40; Vol2d18,282-285. Raw fixed-point/focal fields below
   -- encode bit patterns, NOT validated enabled radial-mode parameters.
   type B1 is mod 2 ** 1 with Size => 1;
   type B2 is mod 2 ** 2 with Size => 2;
   type B3 is mod 2 ** 3 with Size => 3;
   type B5 is mod 2 ** 5 with Size => 5;
   type B8 is mod 2 ** 8 with Size => 8;
   type B11 is mod 2 ** 11 with Size => 11;
   type B16 is mod 2 ** 16 with Size => 16;
   type B27 is mod 2 ** 27 with Size => 27;
   -- Unlike many state packets, CPS specifies a full16-bit length field.
   type Header is record
      Length : B16 := 0;
      Subopcode : B8 := 16#22#;
      Opcode : B3 := 0;
      Subtype_Code : B2 := 3;
      Command_Type : B3 := 3;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Header use record
      Length at 0 range 0 .. 15;
      Subopcode at 0 range 16 .. 23;
      Opcode at 0 range 24 .. 26;
      Subtype_Code at 0 range 27 .. 28;
      Command_Type at 0 range 29 .. 31;
   end record;
   function Encode (V : Header) return Unsigned_32 is
     (Unsigned_32 (V.Length) or Shift_Left (Unsigned_32 (V.Subopcode), 16) or
      Shift_Left (Unsigned_32 (V.Opcode), 24) or
      Shift_Left (Unsigned_32 (V.Subtype_Code), 27) or
      Shift_Left (Unsigned_32 (V.Command_Type), 29));
   type Pointer_Control is record
      Reserved_0 : B5 := 0;
      Offset_32B : B27 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Pointer_Control use record
      Reserved_0 at 0 range 0 .. 4;
      Offset_32B at 0 range 5 .. 31;
   end record;
   function Encode (V : Pointer_Control) return Unsigned_32 is
     (Unsigned_32 (V.Reserved_0) or
      Shift_Left (Unsigned_32 (V.Offset_32B), 5));
   type Minimum_Control is record
      X_S3_7_Bits : B11 := 0;
      Statistics : B1 := 0;
      Mode : B2 := 0;
      Scale_Axis : B1 := 0;
      Reserved_15 : B1 := 0;
      Y_S3_7_Bits : B11 := 0;
      Reserved_27 : B5 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Minimum_Control use record
      X_S3_7_Bits at 0 range 0 .. 10;
      Statistics at 0 range 11 .. 11;
      Mode at 0 range 12 .. 13;
      Scale_Axis at 0 range 14 .. 14;
      Reserved_15 at 0 range 15 .. 15;
      Y_S3_7_Bits at 0 range 16 .. 26;
      Reserved_27 at 0 range 27 .. 31;
   end record;
   function Encode (V : Minimum_Control) return Unsigned_32 is
     (Unsigned_32 (V.X_S3_7_Bits) or
      Shift_Left (Unsigned_32 (V.Statistics), 11) or
      Shift_Left (Unsigned_32 (V.Mode), 12) or
      Shift_Left (Unsigned_32 (V.Scale_Axis), 14) or
      Shift_Left (Unsigned_32 (V.Reserved_15), 15) or
      Shift_Left (Unsigned_32 (V.Y_S3_7_Bits), 16) or
      Shift_Left (Unsigned_32 (V.Reserved_27), 27));
   type Maximum_Control is record
      X_S3_7_Bits : B11 := 0;
      Reserved_11 : B5 := 0;
      Y_S3_7_Bits : B11 := 0;
      Reserved_27 : B5 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Maximum_Control use record
      X_S3_7_Bits at 0 range 0 .. 10;
      Reserved_11 at 0 range 11 .. 15;
      Y_S3_7_Bits at 0 range 16 .. 26;
      Reserved_27 at 0 range 27 .. 31;
   end record;
   function Encode (V : Maximum_Control) return Unsigned_32 is
     (Unsigned_32 (V.X_S3_7_Bits) or
      Shift_Left (Unsigned_32 (V.Reserved_11), 11) or
      Shift_Left (Unsigned_32 (V.Y_S3_7_Bits), 16) or
      Shift_Left (Unsigned_32 (V.Reserved_27), 27));
   type Focal_Control is record
      Signed_15_Bits : B16 := 0;
      Reserved_16 : B16 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Focal_Control use record
      Signed_15_Bits at 0 range 0 .. 15;
      Reserved_16 at 0 range 16 .. 31;
   end record;
   function Encode (V : Focal_Control) return Unsigned_32 is
     (Unsigned_32 (V.Signed_15_Bits) or
      Shift_Left (Unsigned_32 (V.Reserved_16), 16));
   type State_Words is array (Natural range 0 .. 7) of Unsigned_32;
   Disabled : constant State_Words :=
     [Encode (Minimum_Control'(others => <>)),
      Encode (Maximum_Control'(others => <>)),
      Encode (Focal_Control'(others => <>)),
      Encode (Focal_Control'(others => <>)),
      0, 0, 0, 0]; -- IEEE +0: My, Mx, Rmin, Aspect; ignored in NONE.
   State_Offset : constant := 736;
   Viewport_Count : constant := 16;
   type Array_Words is array (Natural range 0 .. Viewport_Count * 8 - 1) of Unsigned_32;
   function Initial_Array return Array_Words;
   type Words is array (Natural range 0 .. 1) of Unsigned_32;
   Pointer : constant Words :=
     [Encode (Header'(others => <>)),
      Encode (Pointer_Control'(Offset_32B => State_Offset / 32, others => <>))];
   pragma Compile_Time_Error
     (State_Offset mod 32 /= 0 or
      Intel_GPU_ADLN_Pixel_Blend.State_Offset +
        Intel_GPU_ADLN_Pixel_Blend.State'Length * 4 > State_Offset or
      State_Offset + Array_Words'Length * 4 > 4096,
      "CPS array overlaps blend state or exceeds dynamic state page");
end Intel_GPU_ADLN_Coarse_Pixel;
