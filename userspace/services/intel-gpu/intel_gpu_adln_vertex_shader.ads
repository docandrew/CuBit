with Interfaces; use Interfaces;
with System;
package Intel_GPU_ADLN_Vertex_Shader with SPARK_Mode is
   -- TGL Vol2d142-149. Fixed position-only shader, SBE attribute count zero.
   type B1 is mod 2 ** 1 with Size => 1;
   type B2 is mod 2 ** 2 with Size => 2;
   type B3 is mod 2 ** 3 with Size => 3;
   type B4 is mod 2 ** 4 with Size => 4;
   type B5 is mod 2 ** 5 with Size => 5;
   type B6 is mod 2 ** 6 with Size => 6;
   type B7 is mod 2 ** 7 with Size => 7;
   type B8 is mod 2 ** 8 with Size => 8;
   type B10 is mod 2 ** 10 with Size => 10;
   type B11 is mod 2 ** 11 with Size => 11;
   type B22 is mod 2 ** 22 with Size => 22;
   type B32 is mod 2 ** 32 with Size => 32;
   type B58 is mod 2 ** 58 with Size => 58;
   type Kernel_Control is record
      Reserved : B6 := 0;
      Offset_64B : B58 := 0;
   end record with Size => 64, Bit_Order => System.Low_Order_First;
   for Kernel_Control use record
      Reserved at 0 range 0 .. 5;
      Offset_64B at 0 range 6 .. 63;
   end record;
   function Encode (V : Kernel_Control) return Unsigned_64 is
     (Unsigned_64 (V.Reserved) or
      Shift_Left (Unsigned_64 (V.Offset_64B), 6));
   type Shader_Control is record
      Reserved_0 : B7 := 0;
      Software_Exception : B1 := 0;
      Reserved_8 : B4 := 0;
      Accesses_UAV : B1 := 0;
      Opcode_Exception : B1 := 0;
      Reserved_14 : B2 := 0;
      Alternate_FP : B1 := 0;
      Priority : B1 := 0;
      Binding_Count : B8 := 0;
      Reserved_26 : B1 := 0;
      Sampler_Count : B3 := 0;
      Vector_Mask : B1 := 0;
      Reserved_31 : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Shader_Control use record
      Reserved_0 at 0 range 0 .. 6;
      Software_Exception at 0 range 7 .. 7;
      Reserved_8 at 0 range 8 .. 11;
      Accesses_UAV at 0 range 12 .. 12;
      Opcode_Exception at 0 range 13 .. 13;
      Reserved_14 at 0 range 14 .. 15;
      Alternate_FP at 0 range 16 .. 16;
      Priority at 0 range 17 .. 17;
      Binding_Count at 0 range 18 .. 25;
      Reserved_26 at 0 range 26 .. 26;
      Sampler_Count at 0 range 27 .. 29;
      Vector_Mask at 0 range 30 .. 30;
      Reserved_31 at 0 range 31 .. 31;
   end record;
   function Encode (V : Shader_Control) return Unsigned_32 is
     (Unsigned_32 (V.Reserved_0) or
      Shift_Left (Unsigned_32 (V.Software_Exception), 7) or
      Shift_Left (Unsigned_32 (V.Reserved_8), 8) or
      Shift_Left (Unsigned_32 (V.Accesses_UAV), 12) or
      Shift_Left (Unsigned_32 (V.Opcode_Exception), 13) or
      Shift_Left (Unsigned_32 (V.Reserved_14), 14) or
      Shift_Left (Unsigned_32 (V.Alternate_FP), 16) or
      Shift_Left (Unsigned_32 (V.Priority), 17) or
      Shift_Left (Unsigned_32 (V.Binding_Count), 18) or
      Shift_Left (Unsigned_32 (V.Reserved_26), 26) or
      Shift_Left (Unsigned_32 (V.Sampler_Count), 27) or
      Shift_Left (Unsigned_32 (V.Vector_Mask), 30) or
      Shift_Left (Unsigned_32 (V.Reserved_31), 31));
   type Scratch_Control is record
      Per_Thread_Size : B4 := 0;
      Reserved_4 : B6 := 0;
      Base_1KiB : B22 := 0;
      Reserved_High : B32 := 0;
   end record with Size => 64, Bit_Order => System.Low_Order_First;
   for Scratch_Control use record
      Per_Thread_Size at 0 range 0 .. 3;
      Reserved_4 at 0 range 4 .. 9;
      Base_1KiB at 0 range 10 .. 31;
      Reserved_High at 0 range 32 .. 63;
   end record;
   function Encode (V : Scratch_Control) return Unsigned_64 is
     (Unsigned_64 (V.Per_Thread_Size) or
      Shift_Left (Unsigned_64 (V.Reserved_4), 4) or
      Shift_Left (Unsigned_64 (V.Base_1KiB), 10) or
      Shift_Left (Unsigned_64 (V.Reserved_High), 32));
   type Payload_Control is record
      Reserved_0 : B4 := 0;
      Read_Offset : B6 := 0;
      Reserved_10 : B1 := 0;
      Read_Length : B6 := 1;
      Reserved_17 : B3 := 0;
      GRF_Start : B5 := 2;
      Reserved_25 : B7 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Payload_Control use record
      Reserved_0 at 0 range 0 .. 3;
      Read_Offset at 0 range 4 .. 9;
      Reserved_10 at 0 range 10 .. 10;
      Read_Length at 0 range 11 .. 16;
      Reserved_17 at 0 range 17 .. 19;
      GRF_Start at 0 range 20 .. 24;
      Reserved_25 at 0 range 25 .. 31;
   end record;
   function Encode (V : Payload_Control) return Unsigned_32 is
     (Unsigned_32 (V.Reserved_0) or
      Shift_Left (Unsigned_32 (V.Read_Offset), 4) or
      Shift_Left (Unsigned_32 (V.Reserved_10), 10) or
      Shift_Left (Unsigned_32 (V.Read_Length), 11) or
      Shift_Left (Unsigned_32 (V.Reserved_17), 17) or
      Shift_Left (Unsigned_32 (V.GRF_Start), 20) or
      Shift_Left (Unsigned_32 (V.Reserved_25), 25));
   type Dispatch_Control is record
      Enable : B1 := 1;
      Cache_Disable : B1 := 0;
      SIMD8 : B1 := 1;
      Reserved_3 : B6 := 0;
      Single_Instance : B1 := 0;
      Statistics : B1 := 1;
      Reserved_11 : B11 := 0;
      Threads_Minus_One : B10 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Dispatch_Control use record
      Enable at 0 range 0 .. 0;
      Cache_Disable at 0 range 1 .. 1;
      SIMD8 at 0 range 2 .. 2;
      Reserved_3 at 0 range 3 .. 8;
      Single_Instance at 0 range 9 .. 9;
      Statistics at 0 range 10 .. 10;
      Reserved_11 at 0 range 11 .. 21;
      Threads_Minus_One at 0 range 22 .. 31;
   end record;
   function Encode (V : Dispatch_Control) return Unsigned_32 is
     (Unsigned_32 (V.Enable) or
      Shift_Left (Unsigned_32 (V.Cache_Disable), 1) or
      Shift_Left (Unsigned_32 (V.SIMD8), 2) or
      Shift_Left (Unsigned_32 (V.Reserved_3), 3) or
      Shift_Left (Unsigned_32 (V.Single_Instance), 9) or
      Shift_Left (Unsigned_32 (V.Statistics), 10) or
      Shift_Left (Unsigned_32 (V.Reserved_11), 11) or
      Shift_Left (Unsigned_32 (V.Threads_Minus_One), 22));
   type Output_Control is record
      Cull_Mask : B8 := 0;
      Clip_Mask : B8 := 0;
      Read_Length : B5 := 0;
      Read_Offset : B6 := 0;
      Reserved_27 : B5 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Output_Control use record
      Cull_Mask at 0 range 0 .. 7;
      Clip_Mask at 0 range 8 .. 15;
      Read_Length at 0 range 16 .. 20;
      Read_Offset at 0 range 21 .. 26;
      Reserved_27 at 0 range 27 .. 31;
   end record;
   function Encode (V : Output_Control) return Unsigned_32 is
     (Unsigned_32 (V.Cull_Mask) or
      Shift_Left (Unsigned_32 (V.Clip_Mask), 8) or
      Shift_Left (Unsigned_32 (V.Read_Length), 16) or
      Shift_Left (Unsigned_32 (V.Read_Offset), 21) or
      Shift_Left (Unsigned_32 (V.Reserved_27), 27));
   type Words is array (Natural range 0 .. 8) of Unsigned_32;
   type Image is record
      Valid : Boolean := False;
      Data : Words := [others => 0];
   end record;
   -- Limit from admitted ADL-N profile, not derived from raw EU count.
   function Build (Thread_Limit : Natural) return Image;
end Intel_GPU_ADLN_Vertex_Shader;
