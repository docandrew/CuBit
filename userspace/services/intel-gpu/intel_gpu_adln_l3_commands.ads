with Interfaces; use Interfaces;
with System;
with Intel_GPU_ADLN_L3;
with Intel_GPU_Submission_Backing;
package Intel_GPU_ADLN_L3_Commands with SPARK_Mode is
   -- TGL Vol2a1002-1005 (LRI),1056-1058 (SRM). Fixed initial RCS probe.
   -- Caller must issue a stalling flush before this sequence, establish
   -- command privilege/allowlisting and retain the private completion page.
   -- No rendering may follow until completion and field-level readback
   -- admission. These packets alone neither grant ownership nor capacity.
   type B1 is mod 2 ** 1 with Size => 1;
   type B2 is mod 2 ** 2 with Size => 2;
   type B3 is mod 2 ** 3 with Size => 3;
   type B4 is mod 2 ** 4 with Size => 4;
   type B5 is mod 2 ** 5 with Size => 5;
   type B6 is mod 2 ** 6 with Size => 6;
   type B8 is mod 2 ** 8 with Size => 8;
   type B9 is mod 2 ** 9 with Size => 9;
   type B21 is mod 2 ** 21 with Size => 21;
   type B62 is mod 2 ** 62 with Size => 62;
   type Load_Header is record
      Length : B8 := 1;
      Byte_Write_Disables : B4 := 0;
      Reserved_12 : B5 := 0;
      MMIO_Remap : B1 := 0;
      Reserved_18 : B1 := 0;
      Add_CS_Offset : B1 := 0;
      Reserved_20 : B3 := 0;
      Opcode : B6 := 34;
      Command_Type : B3 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Load_Header use record
      Length at 0 range 0 .. 7;
      Byte_Write_Disables at 0 range 8 .. 11;
      Reserved_12 at 0 range 12 .. 16;
      MMIO_Remap at 0 range 17 .. 17;
      Reserved_18 at 0 range 18 .. 18;
      Add_CS_Offset at 0 range 19 .. 19;
      Reserved_20 at 0 range 20 .. 22;
      Opcode at 0 range 23 .. 28;
      Command_Type at 0 range 29 .. 31;
   end record;
   function Encode (V : Load_Header) return Unsigned_32 is
     (Unsigned_32 (V.Length) or
      Shift_Left (Unsigned_32 (V.Byte_Write_Disables), 8) or
      Shift_Left (Unsigned_32 (V.Reserved_12), 12) or
      Shift_Left (Unsigned_32 (V.MMIO_Remap), 17) or
      Shift_Left (Unsigned_32 (V.Reserved_18), 18) or
      Shift_Left (Unsigned_32 (V.Add_CS_Offset), 19) or
      Shift_Left (Unsigned_32 (V.Reserved_20), 20) or
      Shift_Left (Unsigned_32 (V.Opcode), 23) or
      Shift_Left (Unsigned_32 (V.Command_Type), 29));
   type Store_Header is record
      Length : B8 := 2;
      Reserved_8 : B9 := 0;
      MMIO_Remap : B1 := 0;
      Reserved_18 : B1 := 0;
      Add_CS_Offset : B1 := 0;
      Reserved_20 : B1 := 0;
      Predicate_Enable : B1 := 0;
      Global_GTT : B1 := 0;
      Opcode : B6 := 36;
      Command_Type : B3 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Store_Header use record
      Length at 0 range 0 .. 7;
      Reserved_8 at 0 range 8 .. 16;
      MMIO_Remap at 0 range 17 .. 17;
      Reserved_18 at 0 range 18 .. 18;
      Add_CS_Offset at 0 range 19 .. 19;
      Reserved_20 at 0 range 20 .. 20;
      Predicate_Enable at 0 range 21 .. 21;
      Global_GTT at 0 range 22 .. 22;
      Opcode at 0 range 23 .. 28;
      Command_Type at 0 range 29 .. 31;
   end record;
   function Encode (V : Store_Header) return Unsigned_32 is
     (Unsigned_32 (V.Length) or
      Shift_Left (Unsigned_32 (V.Reserved_8), 8) or
      Shift_Left (Unsigned_32 (V.MMIO_Remap), 17) or
      Shift_Left (Unsigned_32 (V.Reserved_18), 18) or
      Shift_Left (Unsigned_32 (V.Add_CS_Offset), 19) or
      Shift_Left (Unsigned_32 (V.Reserved_20), 20) or
      Shift_Left (Unsigned_32 (V.Predicate_Enable), 21) or
      Shift_Left (Unsigned_32 (V.Global_GTT), 22) or
      Shift_Left (Unsigned_32 (V.Opcode), 23) or
      Shift_Left (Unsigned_32 (V.Command_Type), 29));
   type Register_Offset is record
      Reserved_0 : B2 := 0;
      Dword_Offset : B21 := 0;
      Reserved_23 : B9 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Register_Offset use record
      Reserved_0 at 0 range 0 .. 1;
      Dword_Offset at 0 range 2 .. 22;
      Reserved_23 at 0 range 23 .. 31;
   end record;
   function Encode (V : Register_Offset) return Unsigned_32 is
     (Unsigned_32 (V.Reserved_0) or
      Shift_Left (Unsigned_32 (V.Dword_Offset), 2) or
      Shift_Left (Unsigned_32 (V.Reserved_23), 23));
   type Memory_Address is record
      Reserved_0 : B2 := 0;
      Dword_Address : B62 := 0;
   end record with Size => 64, Bit_Order => System.Low_Order_First;
   for Memory_Address use record
      Reserved_0 at 0 range 0 .. 1;
      Dword_Address at 0 range 2 .. 63;
   end record;
   function Encode (V : Memory_Address) return Unsigned_64 is
     (Unsigned_64 (V.Reserved_0) or
      Shift_Left (Unsigned_64 (V.Dword_Address), 2));
   Readback_Offset : constant := 16;
   Readback_VA : constant Unsigned_64 :=
     Intel_GPU_Submission_Backing.Completion_GPU_VA + Readback_Offset;
   Parameters_VA : constant Unsigned_64 := Readback_VA + 4;
   pragma Compile_Time_Error (Readback_Offset mod 4 /= 0 or
     Readback_Offset + 8 > 4096, "L3 readback must stay in completion page");
   type Words is array (Natural range <>) of Unsigned_32;
   Initialize_And_Sample : constant Words (0 .. 10) :=
     [Encode (Load_Header'(others => <>)),
      Encode (Register_Offset'(Dword_Offset =>
        B21 (Intel_GPU_ADLN_L3.Allocation_Offset / 4), others => <>)),
      Intel_GPU_ADLN_L3.Encode (Intel_GPU_ADLN_L3.Render_Allocation),
      Encode (Store_Header'(others => <>)),
      Encode (Register_Offset'(Dword_Offset =>
        B21 (Intel_GPU_ADLN_L3.Allocation_Offset / 4), others => <>)),
      Unsigned_32 (Readback_VA), Unsigned_32 (Shift_Right (Readback_VA, 32)),
      Encode (Store_Header'(others => <>)),
      Encode (Register_Offset'(Dword_Offset =>
        B21 (Intel_GPU_ADLN_L3.Parameters_Offset / 4), others => <>)),
      Unsigned_32 (Parameters_VA), Unsigned_32 (Shift_Right (Parameters_VA, 32))];
end Intel_GPU_ADLN_L3_Commands;
