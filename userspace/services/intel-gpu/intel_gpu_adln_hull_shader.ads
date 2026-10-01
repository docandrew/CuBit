with Interfaces; use Interfaces;
with System;
with Intel_GPU_ADLN_Vertex_Fetch;
package Intel_GPU_ADLN_Hull_Shader with SPARK_Mode is
   -- Intel TGL Vol2d41-47, 3DSTATE_HS_BODY. Fixed disabled-stage packet,
   -- NOT an admission validator for executing arbitrary hull shaders.
   -- HS, TE and DS must all be disabled before issuing the probe draw.
   -- PRM reserves scratch bits63:32; Mesa's generic address packer differs.
   -- No scratch address is used here; retain the primary PRM field layout.
   type B1 is mod 2 ** 1 with Size => 1;
   type B2 is mod 2 ** 2 with Size => 2;
   type B3 is mod 2 ** 3 with Size => 3;
   type B4 is mod 2 ** 4 with Size => 4;
   type B5 is mod 2 ** 5 with Size => 5;
   type B6 is mod 2 ** 6 with Size => 6;
   type B8 is mod 2 ** 8 with Size => 8;
   type B9 is mod 2 ** 9 with Size => 9;
   type B12 is mod 2 ** 12 with Size => 12;
   type B22 is mod 2 ** 22 with Size => 22;
   type B32 is mod 2 ** 32 with Size => 32;
   type B58 is mod 2 ** 58 with Size => 58;
   type Resource_Control is record
      Reserved_0 : B12 := 0;
      Software_Exception : B1 := 0;
      Illegal_Opcode_Exception : B1 := 0;
      Reserved_14 : B2 := 0;
      Floating_Point_Mode : B1 := 0;
      Thread_Priority : B1 := 0;
      Binding_Table_Count : B8 := 0;
      Reserved_26 : B1 := 0;
      Sampler_Count : B3 := 0;
      Reserved_30 : B2 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Resource_Control use record
      Reserved_0 at 0 range 0 .. 11;
      Software_Exception at 0 range 12 .. 12;
      Illegal_Opcode_Exception at 0 range 13 .. 13;
      Reserved_14 at 0 range 14 .. 15;
      Floating_Point_Mode at 0 range 16 .. 16;
      Thread_Priority at 0 range 17 .. 17;
      Binding_Table_Count at 0 range 18 .. 25;
      Reserved_26 at 0 range 26 .. 26;
      Sampler_Count at 0 range 27 .. 29;
      Reserved_30 at 0 range 30 .. 31;
   end record;
   function Encode (V : Resource_Control) return Unsigned_32 is
     (Unsigned_32 (V.Reserved_0) or
      Shift_Left (Unsigned_32 (V.Software_Exception), 12) or
      Shift_Left (Unsigned_32 (V.Illegal_Opcode_Exception), 13) or
      Shift_Left (Unsigned_32 (V.Reserved_14), 14) or
      Shift_Left (Unsigned_32 (V.Floating_Point_Mode), 16) or
      Shift_Left (Unsigned_32 (V.Thread_Priority), 17) or
      Shift_Left (Unsigned_32 (V.Binding_Table_Count), 18) or
      Shift_Left (Unsigned_32 (V.Reserved_26), 26) or
      Shift_Left (Unsigned_32 (V.Sampler_Count), 27) or
      Shift_Left (Unsigned_32 (V.Reserved_30), 30));

   type Dispatch_Control is record
      Instance_Count_Minus_One : B5 := 0;
      Reserved_5 : B3 := 0;
      Max_Threads_Minus_One : B9 := 0;
      Reserved_17 : B12 := 0;
      Statistics : B1 := 0;
      Reserved_30 : B1 := 0;
      Enable_HS : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Dispatch_Control use record
      Instance_Count_Minus_One at 0 range 0 .. 4;
      Reserved_5 at 0 range 5 .. 7;
      Max_Threads_Minus_One at 0 range 8 .. 16;
      Reserved_17 at 0 range 17 .. 28;
      Statistics at 0 range 29 .. 29;
      Reserved_30 at 0 range 30 .. 30;
      Enable_HS at 0 range 31 .. 31;
   end record;
   function Encode (V : Dispatch_Control) return Unsigned_32 is
     (Unsigned_32 (V.Instance_Count_Minus_One) or
      Shift_Left (Unsigned_32 (V.Reserved_5), 5) or
      Shift_Left (Unsigned_32 (V.Max_Threads_Minus_One), 8) or
      Shift_Left (Unsigned_32 (V.Reserved_17), 17) or
      Shift_Left (Unsigned_32 (V.Statistics), 29) or
      Shift_Left (Unsigned_32 (V.Reserved_30), 30) or
      Shift_Left (Unsigned_32 (V.Enable_HS), 31));

   type Kernel_Address is record
      Reserved_0 : B6 := 0;
      Instruction_Offset : B58 := 0;
   end record with Size => 64, Bit_Order => System.Low_Order_First;
   for Kernel_Address use record
      Reserved_0 at 0 range 0 .. 5;
      Instruction_Offset at 0 range 6 .. 63;
   end record;
   function Encode (V : Kernel_Address) return Unsigned_64 is
     (Unsigned_64 (V.Reserved_0) or
      Shift_Left (Unsigned_64 (V.Instruction_Offset), 6));

   type Scratch_Address is record
      Per_Thread_Size : B4 := 0;
      Reserved_4 : B6 := 0;
      General_State_Offset : B22 := 0;
      Reserved_32 : B32 := 0;
   end record with Size => 64, Bit_Order => System.Low_Order_First;
   for Scratch_Address use record
      Per_Thread_Size at 0 range 0 .. 3;
      Reserved_4 at 0 range 4 .. 9;
      General_State_Offset at 0 range 10 .. 31;
      Reserved_32 at 0 range 32 .. 63;
   end record;
   function Encode (V : Scratch_Address) return Unsigned_64 is
     (Unsigned_64 (V.Per_Thread_Size) or
      Shift_Left (Unsigned_64 (V.Reserved_4), 4) or
      Shift_Left (Unsigned_64 (V.General_State_Offset), 10) or
      Shift_Left (Unsigned_64 (V.Reserved_32), 32));

   type Payload_Control is record
      Include_Primitive_ID : B1 := 0;
      Patch_Count_Threshold : B3 := 0;
      URB_Read_Offset : B6 := 0;
      Reserved_10 : B1 := 0;
      URB_Read_Length : B6 := 0;
      Dispatch_Mode : B2 := 0;
      GRF_Start_Low : B5 := 0;
      Include_Vertex_Handles : B1 := 0;
      Accesses_UAV : B1 := 0;
      Vector_Mask : B1 := 0;
      Single_Program_Flow : B1 := 0;
      GRF_Start_High : B1 := 0;
      Reserved_29 : B3 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Payload_Control use record
      Include_Primitive_ID at 0 range 0 .. 0;
      Patch_Count_Threshold at 0 range 1 .. 3;
      URB_Read_Offset at 0 range 4 .. 9;
      Reserved_10 at 0 range 10 .. 10;
      URB_Read_Length at 0 range 11 .. 16;
      Dispatch_Mode at 0 range 17 .. 18;
      GRF_Start_Low at 0 range 19 .. 23;
      Include_Vertex_Handles at 0 range 24 .. 24;
      Accesses_UAV at 0 range 25 .. 25;
      Vector_Mask at 0 range 26 .. 26;
      Single_Program_Flow at 0 range 27 .. 27;
      GRF_Start_High at 0 range 28 .. 28;
      Reserved_29 at 0 range 29 .. 31;
   end record;
   function Encode (V : Payload_Control) return Unsigned_32 is
     (Unsigned_32 (V.Include_Primitive_ID) or
      Shift_Left (Unsigned_32 (V.Patch_Count_Threshold), 1) or
      Shift_Left (Unsigned_32 (V.URB_Read_Offset), 4) or
      Shift_Left (Unsigned_32 (V.Reserved_10), 10) or
      Shift_Left (Unsigned_32 (V.URB_Read_Length), 11) or
      Shift_Left (Unsigned_32 (V.Dispatch_Mode), 17) or
      Shift_Left (Unsigned_32 (V.GRF_Start_Low), 19) or
      Shift_Left (Unsigned_32 (V.Include_Vertex_Handles), 24) or
      Shift_Left (Unsigned_32 (V.Accesses_UAV), 25) or
      Shift_Left (Unsigned_32 (V.Vector_Mask), 26) or
      Shift_Left (Unsigned_32 (V.Single_Program_Flow), 27) or
      Shift_Left (Unsigned_32 (V.GRF_Start_High), 28) or
      Shift_Left (Unsigned_32 (V.Reserved_29), 29));

   type Words is array (Natural range 0 .. 8) of Unsigned_32;
   -- Disabled pass-through: UAV access MUST be clear even when HS is off.
   -- Read length/vertex handles are ignored with HS off (Vol2d45-46).
   -- SingleProgramFlow=0 is NOT a valid enabled-HS configuration.
   Disabled : constant Words :=
     [Intel_GPU_ADLN_Vertex_Fetch.Encode
        (Intel_GPU_ADLN_Vertex_Fetch.Header'
           (Length => 7, Subopcode => 16#1B#, others => <>)),
      Encode (Resource_Control'(others => <>)),
      Encode (Dispatch_Control'(others => <>)),
      Unsigned_32 (Encode (Kernel_Address'(others => <>))),
      Unsigned_32 (Shift_Right (Encode (Kernel_Address'(others => <>)), 32)),
      Unsigned_32 (Encode (Scratch_Address'(others => <>))),
      Unsigned_32 (Shift_Right (Encode (Scratch_Address'(others => <>)), 32)),
      Encode (Payload_Control'(others => <>)), 0];
end Intel_GPU_ADLN_Hull_Shader;
