with Interfaces; use Interfaces;
with System;
with Intel_GPU_ADLN_Vertex_Fetch;
with Intel_GPU_ADLN_Viewport;
package Intel_GPU_ADLN_Pixel_Blend with SPARK_Mode is
   -- Intel TGL Vol2d3,56-57,199-204. Fixed one-RT probe, not general blending.
   type B1 is mod 2 ** 1 with Size => 1;
   type B2 is mod 2 ** 2 with Size => 2;
   type B3 is mod 2 ** 3 with Size => 3;
   type B4 is mod 2 ** 4 with Size => 4;
   type B5 is mod 2 ** 5 with Size => 5;
   type B7 is mod 2 ** 7 with Size => 7;
   type B19 is mod 2 ** 19 with Size => 19;
   type B22 is mod 2 ** 22 with Size => 22;
   type B26 is mod 2 ** 26 with Size => 26;
   type PS_Control is record
      Reserved_0 : B7 := 0;
      Independent_Alpha : B1 := 0;
      Alpha_Test : B1 := 0;
      Destination_Factor : B5 := 0;
      Source_Factor : B5 := 0;
      Destination_Alpha_Factor : B5 := 0;
      Source_Alpha_Factor : B5 := 0;
      Blend_Enable : B1 := 0;
      Writable_RT : B1 := 0;
      Alpha_To_Coverage : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for PS_Control use record
      Reserved_0 at 0 range 0 .. 6;
      Independent_Alpha at 0 range 7 .. 7;
      Alpha_Test at 0 range 8 .. 8;
      Destination_Factor at 0 range 9 .. 13;
      Source_Factor at 0 range 14 .. 18;
      Destination_Alpha_Factor at 0 range 19 .. 23;
      Source_Alpha_Factor at 0 range 24 .. 28;
      Blend_Enable at 0 range 29 .. 29;
      Writable_RT at 0 range 30 .. 30;
      Alpha_To_Coverage at 0 range 31 .. 31;
   end record;
   function Encode (V : PS_Control) return Unsigned_32 is
     (Unsigned_32 (V.Reserved_0) or
      Shift_Left (Unsigned_32 (V.Independent_Alpha), 7) or
      Shift_Left (Unsigned_32 (V.Alpha_Test), 8) or
      Shift_Left (Unsigned_32 (V.Destination_Factor), 9) or
      Shift_Left (Unsigned_32 (V.Source_Factor), 14) or
      Shift_Left (Unsigned_32 (V.Destination_Alpha_Factor), 19) or
      Shift_Left (Unsigned_32 (V.Source_Alpha_Factor), 24) or
      Shift_Left (Unsigned_32 (V.Blend_Enable), 29) or
      Shift_Left (Unsigned_32 (V.Writable_RT), 30) or
      Shift_Left (Unsigned_32 (V.Alpha_To_Coverage), 31));

   type Common_Control is record
      Reserved_0 : B19 := 0;
      Y_Dither_Offset : B2 := 0;
      X_Dither_Offset : B2 := 0;
      Color_Dither : B1 := 0;
      Alpha_Test_Function : B3 := 0;
      Alpha_Test : B1 := 0;
      Alpha_To_Coverage_Dither : B1 := 0;
      Alpha_To_One : B1 := 0;
      Independent_Alpha : B1 := 0;
      Alpha_To_Coverage : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Common_Control use record
      Reserved_0 at 0 range 0 .. 18;
      Y_Dither_Offset at 0 range 19 .. 20;
      X_Dither_Offset at 0 range 21 .. 22;
      Color_Dither at 0 range 23 .. 23;
      Alpha_Test_Function at 0 range 24 .. 26;
      Alpha_Test at 0 range 27 .. 27;
      Alpha_To_Coverage_Dither at 0 range 28 .. 28;
      Alpha_To_One at 0 range 29 .. 29;
      Independent_Alpha at 0 range 30 .. 30;
      Alpha_To_Coverage at 0 range 31 .. 31;
   end record;
   function Encode (V : Common_Control) return Unsigned_32 is
     (Unsigned_32 (V.Reserved_0) or
      Shift_Left (Unsigned_32 (V.Y_Dither_Offset), 19) or
      Shift_Left (Unsigned_32 (V.X_Dither_Offset), 21) or
      Shift_Left (Unsigned_32 (V.Color_Dither), 23) or
      Shift_Left (Unsigned_32 (V.Alpha_Test_Function), 24) or
      Shift_Left (Unsigned_32 (V.Alpha_Test), 27) or
      Shift_Left (Unsigned_32 (V.Alpha_To_Coverage_Dither), 28) or
      Shift_Left (Unsigned_32 (V.Alpha_To_One), 29) or
      Shift_Left (Unsigned_32 (V.Independent_Alpha), 30) or
      Shift_Left (Unsigned_32 (V.Alpha_To_Coverage), 31));

   type Entry_Color is record
      Disable_Blue : B1 := 0;
      Disable_Green : B1 := 0;
      Disable_Red : B1 := 0;
      Disable_Alpha : B1 := 0;
      Reserved_4 : B1 := 0;
      Alpha_Function : B3 := 0;
      Destination_Alpha_Factor : B5 := 0;
      Source_Alpha_Factor : B5 := 0;
      Color_Function : B3 := 0;
      Destination_Factor : B5 := 0;
      Source_Factor : B5 := 0;
      Blend_Enable : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Entry_Color use record
      Disable_Blue at 0 range 0 .. 0;
      Disable_Green at 0 range 1 .. 1;
      Disable_Red at 0 range 2 .. 2;
      Disable_Alpha at 0 range 3 .. 3;
      Reserved_4 at 0 range 4 .. 4;
      Alpha_Function at 0 range 5 .. 7;
      Destination_Alpha_Factor at 0 range 8 .. 12;
      Source_Alpha_Factor at 0 range 13 .. 17;
      Color_Function at 0 range 18 .. 20;
      Destination_Factor at 0 range 21 .. 25;
      Source_Factor at 0 range 26 .. 30;
      Blend_Enable at 0 range 31 .. 31;
   end record;
   function Encode (V : Entry_Color) return Unsigned_32 is
     (Unsigned_32 (V.Disable_Blue) or
      Shift_Left (Unsigned_32 (V.Disable_Green), 1) or
      Shift_Left (Unsigned_32 (V.Disable_Red), 2) or
      Shift_Left (Unsigned_32 (V.Disable_Alpha), 3) or
      Shift_Left (Unsigned_32 (V.Reserved_4), 4) or
      Shift_Left (Unsigned_32 (V.Alpha_Function), 5) or
      Shift_Left (Unsigned_32 (V.Destination_Alpha_Factor), 8) or
      Shift_Left (Unsigned_32 (V.Source_Alpha_Factor), 13) or
      Shift_Left (Unsigned_32 (V.Color_Function), 18) or
      Shift_Left (Unsigned_32 (V.Destination_Factor), 21) or
      Shift_Left (Unsigned_32 (V.Source_Factor), 26) or
      Shift_Left (Unsigned_32 (V.Blend_Enable), 31));

   type Entry_Clamp is record
      Post_Clamp : B1 := 0;
      Pre_Clamp : B1 := 0;
      Clamp_Range : B2 := 0;
      Source_Only_Clamp : B1 := 0;
      Reserved_5 : B22 := 0;
      Logic_Function : B4 := 0;
      Logic_Enable : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Entry_Clamp use record
      Post_Clamp at 0 range 0 .. 0;
      Pre_Clamp at 0 range 1 .. 1;
      Clamp_Range at 0 range 2 .. 3;
      Source_Only_Clamp at 0 range 4 .. 4;
      Reserved_5 at 0 range 5 .. 26;
      Logic_Function at 0 range 27 .. 30;
      Logic_Enable at 0 range 31 .. 31;
   end record;
   function Encode (V : Entry_Clamp) return Unsigned_32 is
     (Unsigned_32 (V.Post_Clamp) or
      Shift_Left (Unsigned_32 (V.Pre_Clamp), 1) or
      Shift_Left (Unsigned_32 (V.Clamp_Range), 2) or
      Shift_Left (Unsigned_32 (V.Source_Only_Clamp), 4) or
      Shift_Left (Unsigned_32 (V.Reserved_5), 5) or
      Shift_Left (Unsigned_32 (V.Logic_Function), 27) or
      Shift_Left (Unsigned_32 (V.Logic_Enable), 31));

   type State_Pointer is record
      Valid : B1 := 0;
      Reserved_1 : B5 := 0;
      Offset_64 : B26 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for State_Pointer use record
      Valid at 0 range 0 .. 0;
      Reserved_1 at 0 range 1 .. 5;
      Offset_64 at 0 range 6 .. 31;
   end record;
   function Encode (V : State_Pointer) return Unsigned_32 is
     (Unsigned_32 (V.Valid) or
      Shift_Left (Unsigned_32 (V.Reserved_1), 1) or
      Shift_Left (Unsigned_32 (V.Offset_64), 6));

   State_Offset : constant := 640;
   pragma Compile_Time_Error
     (State_Offset mod 64 /= 0 or else
      Intel_GPU_ADLN_Viewport.CC_Offset + 8 > State_Offset or else
      State_Offset + 68 > 4096, "blend state overlap or alignment");
   -- No blending, logic operations, alpha test, dithering, or alpha coverage.
   -- All channels of RT0 writable; entries1..7 explicitly write-disabled.
   -- Pre/post clamp must agree. RT-format range covers the BGRA8 UNORM target.
   type State_Words is array (Natural range 0 .. 16) of Unsigned_32;
   State : constant State_Words :=
     [0 => Encode (Common_Control'(others => <>)),
      1 => Encode (Entry_Color'(others => <>)),
      3 | 5 | 7 | 9 | 11 | 13 | 15 =>
        Encode (Entry_Color'(Disable_Blue => 1, Disable_Green => 1,
          Disable_Red => 1, Disable_Alpha => 1, others => <>)),
      others => Encode (Entry_Clamp'
        (Pre_Clamp => 1, Post_Clamp => 1, Clamp_Range => 2, others => <>))];
   type Words is array (Natural range 0 .. 3) of Unsigned_32;
   Initial : constant Words :=
     [Intel_GPU_ADLN_Vertex_Fetch.Encode
        (Intel_GPU_ADLN_Vertex_Fetch.Header'
           (Subopcode => 16#4D#, others => <>)),
      Encode (PS_Control'(Writable_RT => 1, others => <>)),
      Intel_GPU_ADLN_Vertex_Fetch.Encode
        (Intel_GPU_ADLN_Vertex_Fetch.Header'
           (Subopcode => 16#24#, others => <>)),
      Encode (State_Pointer'(Valid => 1, Offset_64 => State_Offset / 64,
                            others => <>))];
end Intel_GPU_ADLN_Pixel_Blend;
