with Interfaces; use Interfaces;
with System;
with Intel_GPU_ADLN_Vertex_Fetch;
with Intel_GPU_ADLN_Hull_Shader;
with Intel_GPU_ADLN_Domain_Shader;
package Intel_GPU_ADLN_Geometry_Shader with SPARK_Mode is
   -- Intel TGL Vol2a55 / Vol2d28-37. Fixed disabled pass-through only.
   -- Kernel/scratch layouts match HS; output layout matches DS.
   package Addresses renames Intel_GPU_ADLN_Hull_Shader;
   package Outputs renames Intel_GPU_ADLN_Domain_Shader;
   type B1 is mod 2 ** 1 with Size => 1;
   type B2 is mod 2 ** 2 with Size => 2;
   type B3 is mod 2 ** 3 with Size => 3;
   type B4 is mod 2 ** 4 with Size => 4;
   type B5 is mod 2 ** 5 with Size => 5;
   type B6 is mod 2 ** 6 with Size => 6;
   type B7 is mod 2 ** 7 with Size => 7;
   type B8 is mod 2 ** 8 with Size => 8;
   type B9 is mod 2 ** 9 with Size => 9;
   type B11 is mod 2 ** 11 with Size => 11;
   type Resource_Control is record
      Expected_Vertex_Count : B6 := 0;
      Reserved_6 : B1 := 0;
      Software_Exception : B1 := 0;
      Reserved_8 : B3 := 0;
      Mask_Stack_Exception : B1 := 0;
      Accesses_UAV : B1 := 0;
      Illegal_Opcode_Exception : B1 := 0;
      Reserved_14 : B2 := 0;
      Floating_Point_Mode : B1 := 0;
      Thread_Priority : B1 := 0;
      Binding_Table_Count : B8 := 0;
      Reserved_26 : B1 := 0;
      Sampler_Count : B3 := 0;
      Vector_Mask : B1 := 0;
      Single_Program_Flow : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Resource_Control use record
      Expected_Vertex_Count at 0 range 0 .. 5;
      Reserved_6 at 0 range 6 .. 6;
      Software_Exception at 0 range 7 .. 7;
      Reserved_8 at 0 range 8 .. 10;
      Mask_Stack_Exception at 0 range 11 .. 11;
      Accesses_UAV at 0 range 12 .. 12;
      Illegal_Opcode_Exception at 0 range 13 .. 13;
      Reserved_14 at 0 range 14 .. 15;
      Floating_Point_Mode at 0 range 16 .. 16;
      Thread_Priority at 0 range 17 .. 17;
      Binding_Table_Count at 0 range 18 .. 25;
      Reserved_26 at 0 range 26 .. 26;
      Sampler_Count at 0 range 27 .. 29;
      Vector_Mask at 0 range 30 .. 30;
      Single_Program_Flow at 0 range 31 .. 31;
   end record;
   function Encode (V : Resource_Control) return Unsigned_32 is
     (Unsigned_32 (V.Expected_Vertex_Count) or
      Shift_Left (Unsigned_32 (V.Reserved_6), 6) or
      Shift_Left (Unsigned_32 (V.Software_Exception), 7) or
      Shift_Left (Unsigned_32 (V.Reserved_8), 8) or
      Shift_Left (Unsigned_32 (V.Mask_Stack_Exception), 11) or
      Shift_Left (Unsigned_32 (V.Accesses_UAV), 12) or
      Shift_Left (Unsigned_32 (V.Illegal_Opcode_Exception), 13) or
      Shift_Left (Unsigned_32 (V.Reserved_14), 14) or
      Shift_Left (Unsigned_32 (V.Floating_Point_Mode), 16) or
      Shift_Left (Unsigned_32 (V.Thread_Priority), 17) or
      Shift_Left (Unsigned_32 (V.Binding_Table_Count), 18) or
      Shift_Left (Unsigned_32 (V.Reserved_26), 26) or
      Shift_Left (Unsigned_32 (V.Sampler_Count), 27) or
      Shift_Left (Unsigned_32 (V.Vector_Mask), 30) or
      Shift_Left (Unsigned_32 (V.Single_Program_Flow), 31));

   type Payload_Control is record
      GRF_Start_Low : B4 := 0;
      URB_Read_Offset : B6 := 0;
      Include_Vertex_Handles : B1 := 0;
      URB_Read_Length : B6 := 0;
      Output_Topology : B6 := 0;
      Output_Vertex_Size_Minus_One : B6 := 0;
      GRF_Start_High : B2 := 0;
      Reserved_31 : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Payload_Control use record
      GRF_Start_Low at 0 range 0 .. 3;
      URB_Read_Offset at 0 range 4 .. 9;
      Include_Vertex_Handles at 0 range 10 .. 10;
      URB_Read_Length at 0 range 11 .. 16;
      Output_Topology at 0 range 17 .. 22;
      Output_Vertex_Size_Minus_One at 0 range 23 .. 28;
      GRF_Start_High at 0 range 29 .. 30;
      Reserved_31 at 0 range 31 .. 31;
   end record;
   function Encode (V : Payload_Control) return Unsigned_32 is
     (Unsigned_32 (V.GRF_Start_Low) or
      Shift_Left (Unsigned_32 (V.URB_Read_Offset), 4) or
      Shift_Left (Unsigned_32 (V.Include_Vertex_Handles), 10) or
      Shift_Left (Unsigned_32 (V.URB_Read_Length), 11) or
      Shift_Left (Unsigned_32 (V.Output_Topology), 17) or
      Shift_Left (Unsigned_32 (V.Output_Vertex_Size_Minus_One), 23) or
      Shift_Left (Unsigned_32 (V.GRF_Start_High), 29) or
      Shift_Left (Unsigned_32 (V.Reserved_31), 31));

   type Dispatch_Control is record
      Enable_GS : B1 := 0;
      Discard_Adjacency : B1 := 0;
      Reorder_Mode : B1 := 0;
      Hint : B1 := 0;
      Include_Primitive_ID : B1 := 0;
      Invocations_Increment : B5 := 0;
      Statistics : B1 := 0;
      Dispatch_Mode : B2 := 0;
      Default_Stream : B2 := 0;
      Instance_Count_Minus_One : B5 := 0;
      Control_Header_Size : B4 := 0;
      Reserved_24 : B8 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Dispatch_Control use record
      Enable_GS at 0 range 0 .. 0;
      Discard_Adjacency at 0 range 1 .. 1;
      Reorder_Mode at 0 range 2 .. 2;
      Hint at 0 range 3 .. 3;
      Include_Primitive_ID at 0 range 4 .. 4;
      Invocations_Increment at 0 range 5 .. 9;
      Statistics at 0 range 10 .. 10;
      Dispatch_Mode at 0 range 11 .. 12;
      Default_Stream at 0 range 13 .. 14;
      Instance_Count_Minus_One at 0 range 15 .. 19;
      Control_Header_Size at 0 range 20 .. 23;
      Reserved_24 at 0 range 24 .. 31;
   end record;
   function Encode (V : Dispatch_Control) return Unsigned_32 is
     (Unsigned_32 (V.Enable_GS) or
      Shift_Left (Unsigned_32 (V.Discard_Adjacency), 1) or
      Shift_Left (Unsigned_32 (V.Reorder_Mode), 2) or
      Shift_Left (Unsigned_32 (V.Hint), 3) or
      Shift_Left (Unsigned_32 (V.Include_Primitive_ID), 4) or
      Shift_Left (Unsigned_32 (V.Invocations_Increment), 5) or
      Shift_Left (Unsigned_32 (V.Statistics), 10) or
      Shift_Left (Unsigned_32 (V.Dispatch_Mode), 11) or
      Shift_Left (Unsigned_32 (V.Default_Stream), 13) or
      Shift_Left (Unsigned_32 (V.Instance_Count_Minus_One), 15) or
      Shift_Left (Unsigned_32 (V.Control_Header_Size), 20) or
      Shift_Left (Unsigned_32 (V.Reserved_24), 24));

   type Thread_Control is record
      Max_Threads_Minus_One : B9 := 0;
      Reserved_9 : B7 := 0;
      Static_Vertex_Count : B11 := 0;
      Reserved_27 : B3 := 0;
      Static_Output : B1 := 0;
      Control_Data_Format : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Thread_Control use record
      Max_Threads_Minus_One at 0 range 0 .. 8;
      Reserved_9 at 0 range 9 .. 15;
      Static_Vertex_Count at 0 range 16 .. 26;
      Reserved_27 at 0 range 27 .. 29;
      Static_Output at 0 range 30 .. 30;
      Control_Data_Format at 0 range 31 .. 31;
   end record;
   function Encode (V : Thread_Control) return Unsigned_32 is
     (Unsigned_32 (V.Max_Threads_Minus_One) or
      Shift_Left (Unsigned_32 (V.Reserved_9), 9) or
      Shift_Left (Unsigned_32 (V.Static_Vertex_Count), 16) or
      Shift_Left (Unsigned_32 (V.Reserved_27), 27) or
      Shift_Left (Unsigned_32 (V.Static_Output), 30) or
      Shift_Left (Unsigned_32 (V.Control_Data_Format), 31));

   type Words is array (Natural range 0 .. 9) of Unsigned_32;
   -- UAV access must be clear even when disabled (Vol2d30).
   -- No GS threads: zero payload/dispatch fields follow Mesa's disabled
   -- simple-shader state, NOT a valid enabled-GS configuration.
   -- Enabled SIMD8 with <16 handles requires a CS stall after state changes
   -- (Vol2d33-34); do not reuse this fragment as an enabled shader builder.
   Disabled : constant Words :=
     [Intel_GPU_ADLN_Vertex_Fetch.Encode
        (Intel_GPU_ADLN_Vertex_Fetch.Header'
           (Length => 8, Subopcode => 16#11#, others => <>)),
      Unsigned_32 (Addresses.Encode (Addresses.Kernel_Address'(others => <>))),
      Unsigned_32 (Shift_Right
        (Addresses.Encode (Addresses.Kernel_Address'(others => <>)), 32)),
      Encode (Resource_Control'(others => <>)),
      Unsigned_32 (Addresses.Encode (Addresses.Scratch_Address'(others => <>))),
      Unsigned_32 (Shift_Right
        (Addresses.Encode (Addresses.Scratch_Address'(others => <>)), 32)),
      Encode (Payload_Control'(others => <>)),
      Encode (Dispatch_Control'(others => <>)),
      Encode (Thread_Control'(others => <>)),
      Outputs.Encode (Outputs.Output_Control'(others => <>))];
end Intel_GPU_ADLN_Geometry_Shader;
