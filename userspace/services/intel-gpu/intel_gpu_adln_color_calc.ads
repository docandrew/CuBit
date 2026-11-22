with Interfaces; use Interfaces;
with System;
with Intel_GPU_ADLN_Vertex_Fetch;
with Intel_GPU_ADLN_Coarse_Pixel;
package Intel_GPU_ADLN_Color_Calc with SPARK_Mode is
   -- TGL Vol2a21; Vol2d4,242-243. Fixed initial state, alpha test and
   -- blending disabled separately. Color channels are whole IEEE32 fields.
   type B1 is mod 2 ** 1 with Size => 1;
   type B5 is mod 2 ** 5 with Size => 5;
   type B8 is mod 2 ** 8 with Size => 8;
   type B14 is mod 2 ** 14 with Size => 14;
   type B16 is mod 2 ** 16 with Size => 16;
   type B24 is mod 2 ** 24 with Size => 24;
   type B26 is mod 2 ** 26 with Size => 26;
   type Control is record
      Alpha_Format : B1 := 0;
      Reserved_1 : B14 := 0;
      Round_Disable_Function_Disable : B1 := 0;
      Reserved_16 : B16 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Control use record
      Alpha_Format at 0 range 0 .. 0;
      Reserved_1 at 0 range 1 .. 14;
      Round_Disable_Function_Disable at 0 range 15 .. 15;
      Reserved_16 at 0 range 16 .. 31;
   end record;
   function Encode (V : Control) return Unsigned_32 is
     (Unsigned_32 (V.Alpha_Format) or Shift_Left (Unsigned_32 (V.Reserved_1), 1) or
      Shift_Left (Unsigned_32 (V.Round_Disable_Function_Disable), 15) or
      Shift_Left (Unsigned_32 (V.Reserved_16), 16));
   -- Union view when Alpha_Format=0; format1 uses the entire IEEE32 word.
   type UNorm_Reference is record
      Value : B8 := 0;
      Unused_8 : B24 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for UNorm_Reference use record
      Value at 0 range 0 .. 7;
      Unused_8 at 0 range 8 .. 31;
   end record;
   function Encode (V : UNorm_Reference) return Unsigned_32 is
     (Unsigned_32 (V.Value) or Shift_Left (Unsigned_32 (V.Unused_8), 8));
   type Pointer_Control is record
      Valid : B1 := 1;
      Reserved_1 : B5 := 0;
      Offset_64B : B26 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Pointer_Control use record
      Valid at 0 range 0 .. 0;
      Reserved_1 at 0 range 1 .. 5;
      Offset_64B at 0 range 6 .. 31;
   end record;
   function Encode (V : Pointer_Control) return Unsigned_32 is
     (Unsigned_32 (V.Valid) or Shift_Left (Unsigned_32 (V.Reserved_1), 1) or
      Shift_Left (Unsigned_32 (V.Offset_64B), 6));
   type State_Words is array (Natural range 0 .. 5) of Unsigned_32;
   State : constant State_Words :=
     [Encode (Control'(others => <>)), Encode (UNorm_Reference'(others => <>)),
      0, 0, 0, 0]; -- Blend RGBA = IEEE +0, not NaN.
   State_Offset : constant := 1280;
   type Words is array (Natural range 0 .. 1) of Unsigned_32;
   Pointer : constant Words :=
     [Intel_GPU_ADLN_Vertex_Fetch.Encode
        (Intel_GPU_ADLN_Vertex_Fetch.Header'
           (Subopcode => 16#0E#, others => <>)),
      Encode (Pointer_Control'(Offset_64B => State_Offset / 64, others => <>))];
   pragma Compile_Time_Error
     (State_Offset mod 64 /= 0 or State_Offset + State'Length * 4 > 4096 or
      Intel_GPU_ADLN_Coarse_Pixel.State_Offset +
        Intel_GPU_ADLN_Coarse_Pixel.Array_Words'Length * 4 > State_Offset,
      "color calc state overlaps CPS or exceeds dynamic state page");
end Intel_GPU_ADLN_Color_Calc;
