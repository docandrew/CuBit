with Interfaces; use Interfaces;
with System;
with Intel_GPU_ADLN_Vertex_Fetch; use Intel_GPU_ADLN_Vertex_Fetch;
package Intel_GPU_ADLN_Constants with SPARK_Mode is
   -- TGL Vol2a pp26-28 and81-90. Fixed shaders consume no push constants.
   type B4 is mod 2 ** 4 with Size => 4;
   type B5 is mod 2 ** 5 with Size => 5;
   type B10 is mod 2 ** 10 with Size => 10;
   type B11 is mod 2 ** 11 with Size => 11;
   type Allocation_Control is record
      Size_KiB : B6 := 0;
      Reserved_Low : B10 := 0;
      Offset_KiB : B5 := 0;
      Reserved_High : B11 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Allocation_Control use record
      Size_KiB at 0 range 0 .. 5;
      Reserved_Low at 0 range 6 .. 15;
      Offset_KiB at 0 range 16 .. 20;
      Reserved_High at 0 range 21 .. 31;
   end record;
   function Encode (V : Allocation_Control) return Unsigned_32 is
     (Unsigned_32 (V.Size_KiB) or
      Shift_Left (Unsigned_32 (V.Reserved_Low), 6) or
      Shift_Left (Unsigned_32 (V.Offset_KiB), 16) or
      Shift_Left (Unsigned_32 (V.Reserved_High), 21));
   type Clear_Header is record
      Length : B8 := 0;
      Shader_Mask : B5 := 31;
      Reserved : B2 := 0;
      POSH_Optimize : B1 := 0;
      Subopcode : B8 := 16#6D#;
      Opcode : B3 := 0;
      Subtype_Code : B2 := 3;
      Command_Type : B3 := 3;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Clear_Header use record
      Length at 0 range 0 .. 7;
      Shader_Mask at 0 range 8 .. 12;
      Reserved at 0 range 13 .. 14;
      POSH_Optimize at 0 range 15 .. 15;
      Subopcode at 0 range 16 .. 23;
      Opcode at 0 range 24 .. 26;
      Subtype_Code at 0 range 27 .. 28;
      Command_Type at 0 range 29 .. 31;
   end record;
   function Encode (V : Clear_Header) return Unsigned_32 is
     (Unsigned_32 (V.Length) or
      Shift_Left (Unsigned_32 (V.Shader_Mask), 8) or
      Shift_Left (Unsigned_32 (V.Reserved), 13) or
      Shift_Left (Unsigned_32 (V.POSH_Optimize), 15) or
      Shift_Left (Unsigned_32 (V.Subopcode), 16) or
      Shift_Left (Unsigned_32 (V.Opcode), 24) or
      Shift_Left (Unsigned_32 (V.Subtype_Code), 27) or
      Shift_Left (Unsigned_32 (V.Command_Type), 29));
   type Clear_Control is record
      MOCS : B7 := 0;
      Reserved_Low : B9 := 0;
      Pointer_Mask : B4 := 0;
      Reserved_High : B11 := 0;
      Retain_Invalid : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Clear_Control use record
      MOCS at 0 range 0 .. 6;
      Reserved_Low at 0 range 7 .. 15;
      Pointer_Mask at 0 range 16 .. 19;
      Reserved_High at 0 range 20 .. 30;
      Retain_Invalid at 0 range 31 .. 31;
   end record;
   function Encode (V : Clear_Control) return Unsigned_32 is
     (Unsigned_32 (V.MOCS) or
      Shift_Left (Unsigned_32 (V.Reserved_Low), 7) or
      Shift_Left (Unsigned_32 (V.Pointer_Mask), 16) or
      Shift_Left (Unsigned_32 (V.Reserved_High), 20) or
      Shift_Left (Unsigned_32 (V.Retain_Invalid), 31));
   type Words is array (Natural range 0 .. 11) of Unsigned_32;
   type Image is record
      Valid : Boolean := False;
      Data : Words := [others => 0];
   end record;
   -- No committing/preemptible command may split allocation from clear.
   -- Reissue binding-table pointers afterward before drawing (gather mode).
   function Build (MOCS : Unsigned_32) return Image;
end Intel_GPU_ADLN_Constants;
