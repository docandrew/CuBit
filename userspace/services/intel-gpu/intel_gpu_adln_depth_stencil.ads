with Interfaces; use Interfaces;
with System;
package Intel_GPU_ADLN_Depth_Stencil with SPARK_Mode is
   -- Intel TGL Vol2a41,163-164; Vol2d19,155-157.
   -- Fixed probe has no depth/stencil attachments. These packets disable
   -- testing/writes, but do not replace the required null buffer bindings.
   type B1 is mod 2 ** 1 with Size => 1;
   type B2 is mod 2 ** 2 with Size => 2;
   type B3 is mod 2 ** 3 with Size => 3;
   type B6 is mod 2 ** 6 with Size => 6;
   type B8 is mod 2 ** 8 with Size => 8;
   type B16 is mod 2 ** 16 with Size => 16;
   type B31 is mod 2 ** 31 with Size => 31;
   type Stencil_Header is record
      Length : B8 := 2;
      Keep_Reference : B1 := 0;
      Keep_Test_Mask : B1 := 0;
      Keep_Write_Mask : B1 := 0;
      Keep_Stencil : B1 := 0;
      Keep_Depth : B1 := 0;
      Reserved_13 : B3 := 0;
      Subopcode : B8 := 78;
      Opcode : B3 := 0;
      Command_Subtype : B2 := 3;
      Command_Type : B3 := 3;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Stencil_Header use record
      Length at 0 range 0 .. 7;
      Keep_Reference at 0 range 8 .. 8;
      Keep_Test_Mask at 0 range 9 .. 9;
      Keep_Write_Mask at 0 range 10 .. 10;
      Keep_Stencil at 0 range 11 .. 11;
      Keep_Depth at 0 range 12 .. 12;
      Reserved_13 at 0 range 13 .. 15;
      Subopcode at 0 range 16 .. 23;
      Opcode at 0 range 24 .. 26;
      Command_Subtype at 0 range 27 .. 28;
      Command_Type at 0 range 29 .. 31;
   end record;
   function Encode (V : Stencil_Header) return Unsigned_32 is
     (Unsigned_32 (V.Length) or
      Shift_Left (Unsigned_32 (V.Keep_Reference), 8) or
      Shift_Left (Unsigned_32 (V.Keep_Test_Mask), 9) or
      Shift_Left (Unsigned_32 (V.Keep_Write_Mask), 10) or
      Shift_Left (Unsigned_32 (V.Keep_Stencil), 11) or
      Shift_Left (Unsigned_32 (V.Keep_Depth), 12) or
      Shift_Left (Unsigned_32 (V.Reserved_13), 13) or
      Shift_Left (Unsigned_32 (V.Subopcode), 16) or
      Shift_Left (Unsigned_32 (V.Opcode), 24) or
      Shift_Left (Unsigned_32 (V.Command_Subtype), 27) or
      Shift_Left (Unsigned_32 (V.Command_Type), 29));

   type Test_Control is record
      Depth_Write : B1 := 0;
      Depth_Test : B1 := 0;
      Stencil_Write : B1 := 0;
      Stencil_Test : B1 := 0;
      Double_Sided : B1 := 0;
      Depth_Function : B3 := 0;
      Stencil_Function : B3 := 0;
      Back_Pass_Depth_Pass : B3 := 0;
      Back_Pass_Depth_Fail : B3 := 0;
      Back_Fail : B3 := 0;
      Back_Function : B3 := 0;
      Pass_Depth_Pass : B3 := 0;
      Pass_Depth_Fail : B3 := 0;
      Stencil_Fail : B3 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Test_Control use record
      Depth_Write at 0 range 0 .. 0;
      Depth_Test at 0 range 1 .. 1;
      Stencil_Write at 0 range 2 .. 2;
      Stencil_Test at 0 range 3 .. 3;
      Double_Sided at 0 range 4 .. 4;
      Depth_Function at 0 range 5 .. 7;
      Stencil_Function at 0 range 8 .. 10;
      Back_Pass_Depth_Pass at 0 range 11 .. 13;
      Back_Pass_Depth_Fail at 0 range 14 .. 16;
      Back_Fail at 0 range 17 .. 19;
      Back_Function at 0 range 20 .. 22;
      Pass_Depth_Pass at 0 range 23 .. 25;
      Pass_Depth_Fail at 0 range 26 .. 28;
      Stencil_Fail at 0 range 29 .. 31;
   end record;
   function Encode (V : Test_Control) return Unsigned_32 is
     (Unsigned_32 (V.Depth_Write) or
      Shift_Left (Unsigned_32 (V.Depth_Test), 1) or
      Shift_Left (Unsigned_32 (V.Stencil_Write), 2) or
      Shift_Left (Unsigned_32 (V.Stencil_Test), 3) or
      Shift_Left (Unsigned_32 (V.Double_Sided), 4) or
      Shift_Left (Unsigned_32 (V.Depth_Function), 5) or
      Shift_Left (Unsigned_32 (V.Stencil_Function), 8) or
      Shift_Left (Unsigned_32 (V.Back_Pass_Depth_Pass), 11) or
      Shift_Left (Unsigned_32 (V.Back_Pass_Depth_Fail), 14) or
      Shift_Left (Unsigned_32 (V.Back_Fail), 17) or
      Shift_Left (Unsigned_32 (V.Back_Function), 20) or
      Shift_Left (Unsigned_32 (V.Pass_Depth_Pass), 23) or
      Shift_Left (Unsigned_32 (V.Pass_Depth_Fail), 26) or
      Shift_Left (Unsigned_32 (V.Stencil_Fail), 29));

   type Masks is record
      Back_Write : B8 := 0;
      Back_Test : B8 := 0;
      Front_Write : B8 := 0;
      Front_Test : B8 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Masks use record
      Back_Write at 0 range 0 .. 7;
      Back_Test at 0 range 8 .. 15;
      Front_Write at 0 range 16 .. 23;
      Front_Test at 0 range 24 .. 31;
   end record;
   function Encode (V : Masks) return Unsigned_32 is
     (Unsigned_32 (V.Back_Write) or
      Shift_Left (Unsigned_32 (V.Back_Test), 8) or
      Shift_Left (Unsigned_32 (V.Front_Write), 16) or
      Shift_Left (Unsigned_32 (V.Front_Test), 24));

   type References is record
      Back_Reference : B8 := 0;
      Front_Reference : B8 := 0;
      Reserved_16 : B16 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for References use record
      Back_Reference at 0 range 0 .. 7;
      Front_Reference at 0 range 8 .. 15;
      Reserved_16 at 0 range 16 .. 31;
   end record;
   function Encode (V : References) return Unsigned_32 is
     (Unsigned_32 (V.Back_Reference) or
      Shift_Left (Unsigned_32 (V.Front_Reference), 8) or
      Shift_Left (Unsigned_32 (V.Reserved_16), 16));

   type Bounds_Header is record
      Length : B8 := 2;
      Reserved_8 : B6 := 0;
      Keep_Values : B1 := 0;
      Keep_Enable : B1 := 0;
      Subopcode : B8 := 113;
      Opcode : B3 := 0;
      Command_Subtype : B2 := 3;
      Command_Type : B3 := 3;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Bounds_Header use record
      Length at 0 range 0 .. 7;
      Reserved_8 at 0 range 8 .. 13;
      Keep_Values at 0 range 14 .. 14;
      Keep_Enable at 0 range 15 .. 15;
      Subopcode at 0 range 16 .. 23;
      Opcode at 0 range 24 .. 26;
      Command_Subtype at 0 range 27 .. 28;
      Command_Type at 0 range 29 .. 31;
   end record;
   function Encode (V : Bounds_Header) return Unsigned_32 is
     (Unsigned_32 (V.Length) or
      Shift_Left (Unsigned_32 (V.Reserved_8), 8) or
      Shift_Left (Unsigned_32 (V.Keep_Values), 14) or
      Shift_Left (Unsigned_32 (V.Keep_Enable), 15) or
      Shift_Left (Unsigned_32 (V.Subopcode), 16) or
      Shift_Left (Unsigned_32 (V.Opcode), 24) or
      Shift_Left (Unsigned_32 (V.Command_Subtype), 27) or
      Shift_Left (Unsigned_32 (V.Command_Type), 29));

   type Bounds_Control is record
      Test_Enable : B1 := 0;
      Reserved_1 : B31 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Bounds_Control use record
      Test_Enable at 0 range 0 .. 0;
      Reserved_1 at 0 range 1 .. 31;
   end record;
   function Encode (V : Bounds_Control) return Unsigned_32 is
     (Unsigned_32 (V.Test_Enable) or
      Shift_Left (Unsigned_32 (V.Reserved_1), 1));

   type Words is array (Natural range 0 .. 7) of Unsigned_32;
   -- Keep/modify-disable bits MUST be clear: overwrite prior context state.
   -- Tests and writes all disabled; stencil masks/references explicitly zero.
   -- Bounds values are IEEE754 +0 and +1, not integer encodings.
   Initial : constant Words :=
     [Encode (Stencil_Header'(others => <>)),
      Encode (Test_Control'(others => <>)),
      Encode (Masks'(others => <>)),
      Encode (References'(others => <>)),
      Encode (Bounds_Header'(others => <>)),
      Encode (Bounds_Control'(others => <>)), 0, 16#3F800000#];
end Intel_GPU_ADLN_Depth_Stencil;
