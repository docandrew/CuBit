with Interfaces; use Interfaces;
with System;
with Intel_GPU_ADLN_Vertex_Fetch;
with Intel_GPU_ADLN_Hull_Shader;
package Intel_GPU_ADLN_Domain_Shader with SPARK_Mode is
   -- Intel TGL Vol2a54 / Vol2d20-27. Disabled fixed probe only, not
   -- validation for enabled tessellation. All HS/TE/DS must be off at draw.
   -- Kernel and scratch layouts are identical to HS in the primary PRM.
   -- Scratch upper32 are reserved (unlike Mesa's generic address packer).
   package Addresses renames Intel_GPU_ADLN_Hull_Shader;
   type B1 is mod 2 ** 1 with Size => 1;
   type B2 is mod 2 ** 2 with Size => 2;
   type B3 is mod 2 ** 3 with Size => 3;
   type B4 is mod 2 ** 4 with Size => 4;
   type B5 is mod 2 ** 5 with Size => 5;
   type B6 is mod 2 ** 6 with Size => 6;
   type B7 is mod 2 ** 7 with Size => 7;
   type B8 is mod 2 ** 8 with Size => 8;
   type B10 is mod 2 ** 10 with Size => 10;
   type Resource_Control is record
      Reserved_0 : B7 := 0;
      Software_Exception : B1 := 0;
      Reserved_8 : B5 := 0;
      Illegal_Opcode_Exception : B1 := 0;
      Accesses_UAV : B1 := 0;
      Reserved_15 : B1 := 0;
      Floating_Point_Mode : B1 := 0;
      Thread_Priority : B1 := 0;
      Binding_Table_Count : B8 := 0;
      Reserved_26 : B1 := 0;
      Sampler_Count : B3 := 0;
      Vector_Mask : B1 := 0;
      Reserved_31 : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Resource_Control use record
      Reserved_0 at 0 range 0 .. 6;
      Software_Exception at 0 range 7 .. 7;
      Reserved_8 at 0 range 8 .. 12;
      Illegal_Opcode_Exception at 0 range 13 .. 13;
      Accesses_UAV at 0 range 14 .. 14;
      Reserved_15 at 0 range 15 .. 15;
      Floating_Point_Mode at 0 range 16 .. 16;
      Thread_Priority at 0 range 17 .. 17;
      Binding_Table_Count at 0 range 18 .. 25;
      Reserved_26 at 0 range 26 .. 26;
      Sampler_Count at 0 range 27 .. 29;
      Vector_Mask at 0 range 30 .. 30;
      Reserved_31 at 0 range 31 .. 31;
   end record;
   function Encode (V : Resource_Control) return Unsigned_32 is
     (Unsigned_32 (V.Reserved_0) or
      Shift_Left (Unsigned_32 (V.Software_Exception), 7) or
      Shift_Left (Unsigned_32 (V.Reserved_8), 8) or
      Shift_Left (Unsigned_32 (V.Illegal_Opcode_Exception), 13) or
      Shift_Left (Unsigned_32 (V.Accesses_UAV), 14) or
      Shift_Left (Unsigned_32 (V.Reserved_15), 15) or
      Shift_Left (Unsigned_32 (V.Floating_Point_Mode), 16) or
      Shift_Left (Unsigned_32 (V.Thread_Priority), 17) or
      Shift_Left (Unsigned_32 (V.Binding_Table_Count), 18) or
      Shift_Left (Unsigned_32 (V.Reserved_26), 26) or
      Shift_Left (Unsigned_32 (V.Sampler_Count), 27) or
      Shift_Left (Unsigned_32 (V.Vector_Mask), 30) or
      Shift_Left (Unsigned_32 (V.Reserved_31), 31));

   type Payload_Control is record
      Reserved_0 : B4 := 0;
      URB_Read_Offset : B6 := 0;
      Reserved_10 : B1 := 0;
      URB_Read_Length : B7 := 0;
      Reserved_18 : B2 := 0;
      GRF_Start : B5 := 0;
      Reserved_25 : B7 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Payload_Control use record
      Reserved_0 at 0 range 0 .. 3;
      URB_Read_Offset at 0 range 4 .. 9;
      Reserved_10 at 0 range 10 .. 10;
      URB_Read_Length at 0 range 11 .. 17;
      Reserved_18 at 0 range 18 .. 19;
      GRF_Start at 0 range 20 .. 24;
      Reserved_25 at 0 range 25 .. 31;
   end record;
   function Encode (V : Payload_Control) return Unsigned_32 is
     (Unsigned_32 (V.Reserved_0) or
      Shift_Left (Unsigned_32 (V.URB_Read_Offset), 4) or
      Shift_Left (Unsigned_32 (V.Reserved_10), 10) or
      Shift_Left (Unsigned_32 (V.URB_Read_Length), 11) or
      Shift_Left (Unsigned_32 (V.Reserved_18), 18) or
      Shift_Left (Unsigned_32 (V.GRF_Start), 20) or
      Shift_Left (Unsigned_32 (V.Reserved_25), 25));

   type Dispatch_Control is record
      Enable_DS : B1 := 0;
      Cache_Disable : B1 := 0;
      Compute_W : B1 := 0;
      Dispatch_Mode : B2 := 0;
      Reserved_5 : B4 := 0;
      Primitive_ID_Not_Required : B1 := 0;
      Statistics : B1 := 0;
      Reserved_11 : B10 := 0;
      Max_Threads_Minus_One : B10 := 0;
      Reserved_31 : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Dispatch_Control use record
      Enable_DS at 0 range 0 .. 0;
      Cache_Disable at 0 range 1 .. 1;
      Compute_W at 0 range 2 .. 2;
      Dispatch_Mode at 0 range 3 .. 4;
      Reserved_5 at 0 range 5 .. 8;
      Primitive_ID_Not_Required at 0 range 9 .. 9;
      Statistics at 0 range 10 .. 10;
      Reserved_11 at 0 range 11 .. 20;
      Max_Threads_Minus_One at 0 range 21 .. 30;
      Reserved_31 at 0 range 31 .. 31;
   end record;
   function Encode (V : Dispatch_Control) return Unsigned_32 is
     (Unsigned_32 (V.Enable_DS) or
      Shift_Left (Unsigned_32 (V.Cache_Disable), 1) or
      Shift_Left (Unsigned_32 (V.Compute_W), 2) or
      Shift_Left (Unsigned_32 (V.Dispatch_Mode), 3) or
      Shift_Left (Unsigned_32 (V.Reserved_5), 5) or
      Shift_Left (Unsigned_32 (V.Primitive_ID_Not_Required), 9) or
      Shift_Left (Unsigned_32 (V.Statistics), 10) or
      Shift_Left (Unsigned_32 (V.Reserved_11), 11) or
      Shift_Left (Unsigned_32 (V.Max_Threads_Minus_One), 21) or
      Shift_Left (Unsigned_32 (V.Reserved_31), 31));

   type Output_Control is record
      Cull_Distance_Mask : B8 := 0;
      Clip_Distance_Mask : B8 := 0;
      URB_Output_Length : B5 := 0;
      URB_Output_Offset : B6 := 0;
      Reserved_27 : B5 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Output_Control use record
      Cull_Distance_Mask at 0 range 0 .. 7;
      Clip_Distance_Mask at 0 range 8 .. 15;
      URB_Output_Length at 0 range 16 .. 20;
      URB_Output_Offset at 0 range 21 .. 26;
      Reserved_27 at 0 range 27 .. 31;
   end record;
   function Encode (V : Output_Control) return Unsigned_32 is
     (Unsigned_32 (V.Cull_Distance_Mask) or
      Shift_Left (Unsigned_32 (V.Clip_Distance_Mask), 8) or
      Shift_Left (Unsigned_32 (V.URB_Output_Length), 16) or
      Shift_Left (Unsigned_32 (V.URB_Output_Offset), 21) or
      Shift_Left (Unsigned_32 (V.Reserved_27), 27));

   type Words is array (Natural range 0 .. 10) of Unsigned_32;
   -- Accesses_UAV MUST remain clear when disabled (Vol2d22).
   -- Zero Dispatch_Mode is not valid for enabled DS; it is ignored here.
   Disabled : constant Words :=
     [Intel_GPU_ADLN_Vertex_Fetch.Encode
        (Intel_GPU_ADLN_Vertex_Fetch.Header'
           (Length => 9, Subopcode => 16#1D#, others => <>)),
      Unsigned_32 (Addresses.Encode (Addresses.Kernel_Address'(others => <>))),
      Unsigned_32 (Shift_Right
        (Addresses.Encode (Addresses.Kernel_Address'(others => <>)), 32)),
      Encode (Resource_Control'(others => <>)),
      Unsigned_32 (Addresses.Encode (Addresses.Scratch_Address'(others => <>))),
      Unsigned_32 (Shift_Right
        (Addresses.Encode (Addresses.Scratch_Address'(others => <>)), 32)),
      Encode (Payload_Control'(others => <>)),
      Encode (Dispatch_Control'(others => <>)),
      Encode (Output_Control'(others => <>)),
      Unsigned_32 (Addresses.Encode (Addresses.Kernel_Address'(others => <>))),
      Unsigned_32 (Shift_Right
        (Addresses.Encode (Addresses.Kernel_Address'(others => <>)), 32))];
end Intel_GPU_ADLN_Domain_Shader;
