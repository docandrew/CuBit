with Interfaces; use Interfaces;
with System;
package Intel_GPU_ADLN_Pixel_Shader with SPARK_Mode is
   -- Intel TGL Vol2d58-67. Fixed, single-sample, constant-red probe only.
   -- Kernel/scratch layouts are shared with the VS packet.
   type B1 is mod 2 ** 1 with Size => 1;
   type B2 is mod 2 ** 2 with Size => 2;
   type B3 is mod 2 ** 3 with Size => 3;
   type B7 is mod 2 ** 7 with Size => 7;
   type B8 is mod 2 ** 8 with Size => 8;
   type B9 is mod 2 ** 9 with Size => 9;
   type Shader_Control is record
      Reserved_0 : B7 := 0;
      Software_Exception : B1 := 0;
      Reserved_8 : B3 := 0;
      Mask_Stack_Exception : B1 := 0;
      Reserved_12 : B1 := 0;
      Opcode_Exception : B1 := 0;
      Rounding_Mode : B2 := 0;
      Alternate_FP : B1 := 0;
      Priority : B1 := 0;
      Binding_Count : B8 := 0;
      Retain_Denormals : B1 := 0;
      Sampler_Count : B3 := 0;
      Vector_Mask : B1 := 0;
      Single_Flow : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Shader_Control use record
      Reserved_0 at 0 range 0 .. 6;
      Software_Exception at 0 range 7 .. 7;
      Reserved_8 at 0 range 8 .. 10;
      Mask_Stack_Exception at 0 range 11 .. 11;
      Reserved_12 at 0 range 12 .. 12;
      Opcode_Exception at 0 range 13 .. 13;
      Rounding_Mode at 0 range 14 .. 15;
      Alternate_FP at 0 range 16 .. 16;
      Priority at 0 range 17 .. 17;
      Binding_Count at 0 range 18 .. 25;
      Retain_Denormals at 0 range 26 .. 26;
      Sampler_Count at 0 range 27 .. 29;
      Vector_Mask at 0 range 30 .. 30;
      Single_Flow at 0 range 31 .. 31;
   end record;
   function Encode (V : Shader_Control) return Unsigned_32 is
     (Unsigned_32 (V.Reserved_0) or
      Shift_Left (Unsigned_32 (V.Software_Exception), 7) or
      Shift_Left (Unsigned_32 (V.Reserved_8), 8) or
      Shift_Left (Unsigned_32 (V.Mask_Stack_Exception), 11) or
      Shift_Left (Unsigned_32 (V.Reserved_12), 12) or
      Shift_Left (Unsigned_32 (V.Opcode_Exception), 13) or
      Shift_Left (Unsigned_32 (V.Rounding_Mode), 14) or
      Shift_Left (Unsigned_32 (V.Alternate_FP), 16) or
      Shift_Left (Unsigned_32 (V.Priority), 17) or
      Shift_Left (Unsigned_32 (V.Binding_Count), 18) or
      Shift_Left (Unsigned_32 (V.Retain_Denormals), 26) or
      Shift_Left (Unsigned_32 (V.Sampler_Count), 27) or
      Shift_Left (Unsigned_32 (V.Vector_Mask), 30) or
      Shift_Left (Unsigned_32 (V.Single_Flow), 31));
   type Dispatch_Control is record
      SIMD8 : B1 := 0;
      SIMD16 : B1 := 0;
      SIMD32 : B1 := 0;
      XY_Offset : B2 := 0;
      Dual_SIMD8 : B1 := 0;
      Resolve : B2 := 0;
      Fast_Clear : B1 := 0;
      Overlapping_Subspans : B1 := 0;
      Scoreboard_Address_Size : B1 := 0;
      Push_Constants : B1 := 0;
      Clear_Resolve_BTI : B8 := 0;
      Reserved_20 : B1 := 0;
      Scoreboard_Disable : B1 := 0;
      Reserved_22 : B1 := 0;
      Threads_Minus_One : B9 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Dispatch_Control use record
      SIMD8 at 0 range 0 .. 0;
      SIMD16 at 0 range 1 .. 1;
      SIMD32 at 0 range 2 .. 2;
      XY_Offset at 0 range 3 .. 4;
      Dual_SIMD8 at 0 range 5 .. 5;
      Resolve at 0 range 6 .. 7;
      Fast_Clear at 0 range 8 .. 8;
      Overlapping_Subspans at 0 range 9 .. 9;
      Scoreboard_Address_Size at 0 range 10 .. 10;
      Push_Constants at 0 range 11 .. 11;
      Clear_Resolve_BTI at 0 range 12 .. 19;
      Reserved_20 at 0 range 20 .. 20;
      Scoreboard_Disable at 0 range 21 .. 21;
      Reserved_22 at 0 range 22 .. 22;
      Threads_Minus_One at 0 range 23 .. 31;
   end record;
   function Encode (V : Dispatch_Control) return Unsigned_32 is
     (Unsigned_32 (V.SIMD8) or
      Shift_Left (Unsigned_32 (V.SIMD16), 1) or
      Shift_Left (Unsigned_32 (V.SIMD32), 2) or
      Shift_Left (Unsigned_32 (V.XY_Offset), 3) or
      Shift_Left (Unsigned_32 (V.Dual_SIMD8), 5) or
      Shift_Left (Unsigned_32 (V.Resolve), 6) or
      Shift_Left (Unsigned_32 (V.Fast_Clear), 8) or
      Shift_Left (Unsigned_32 (V.Overlapping_Subspans), 9) or
      Shift_Left (Unsigned_32 (V.Scoreboard_Address_Size), 10) or
      Shift_Left (Unsigned_32 (V.Push_Constants), 11) or
      Shift_Left (Unsigned_32 (V.Clear_Resolve_BTI), 12) or
      Shift_Left (Unsigned_32 (V.Reserved_20), 20) or
      Shift_Left (Unsigned_32 (V.Scoreboard_Disable), 21) or
      Shift_Left (Unsigned_32 (V.Reserved_22), 22) or
      Shift_Left (Unsigned_32 (V.Threads_Minus_One), 23));
   type Payload_Control is record
      GRF_Slot2 : B7 := 0;
      Reserved_7 : B1 := 0;
      GRF_Slot1 : B7 := 0;
      Reserved_15 : B1 := 0;
      GRF_Slot0 : B7 := 0;
      Reserved_23 : B9 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Payload_Control use record
      GRF_Slot2 at 0 range 0 .. 6;
      Reserved_7 at 0 range 7 .. 7;
      GRF_Slot1 at 0 range 8 .. 14;
      Reserved_15 at 0 range 15 .. 15;
      GRF_Slot0 at 0 range 16 .. 22;
      Reserved_23 at 0 range 23 .. 31;
   end record;
   function Encode (V : Payload_Control) return Unsigned_32 is
     (Unsigned_32 (V.GRF_Slot2) or
      Shift_Left (Unsigned_32 (V.Reserved_7), 7) or
      Shift_Left (Unsigned_32 (V.GRF_Slot1), 8) or
      Shift_Left (Unsigned_32 (V.Reserved_15), 15) or
      Shift_Left (Unsigned_32 (V.GRF_Slot0), 16) or
      Shift_Left (Unsigned_32 (V.Reserved_23), 23));
   type Words is array (Natural range 0 .. 11) of Unsigned_32;
   type Image is record
      Valid : Boolean := False;
      Data : Words := [others => 0];
   end record;
   -- Initial state only. Changing thread limit between draws requires a
   -- PIPE_CONTROL pixel-scoreboard stall. PS_EXTRA must separately enable PS.
   function Build (Thread_Limit : Natural) return Image;
end Intel_GPU_ADLN_Pixel_Shader;
