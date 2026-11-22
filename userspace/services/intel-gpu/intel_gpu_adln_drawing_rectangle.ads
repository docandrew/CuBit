with Interfaces; use Interfaces;
with System;
package Intel_GPU_ADLN_Drawing_Rectangle with SPARK_Mode is
   -- Intel TGL Vol2a51-53. Inclusive clipping coordinates, zero origin.
   type B2 is mod 2 ** 2 with Size => 2;
   type B3 is mod 2 ** 3 with Size => 3;
   type B6 is mod 2 ** 6 with Size => 6;
   type B8 is mod 2 ** 8 with Size => 8;
   type Header is record
      Length : B8 := 2;
      Reserved_8 : B6 := 0;
      Core_Mode : B2 := 0;
      Subopcode : B8 := 0;
      Opcode : B3 := 1;
      Command_Subtype : B2 := 3;
      Command_Type : B3 := 3;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Header use record
      Length at 0 range 0 .. 7;
      Reserved_8 at 0 range 8 .. 13;
      Core_Mode at 0 range 14 .. 15;
      Subopcode at 0 range 16 .. 23;
      Opcode at 0 range 24 .. 26;
      Command_Subtype at 0 range 27 .. 28;
      Command_Type at 0 range 29 .. 31;
   end record;
   function Encode (V : Header) return Unsigned_32 is
     (Unsigned_32 (V.Length) or
      Shift_Left (Unsigned_32 (V.Reserved_8), 8) or
      Shift_Left (Unsigned_32 (V.Core_Mode), 14) or
      Shift_Left (Unsigned_32 (V.Subopcode), 16) or
      Shift_Left (Unsigned_32 (V.Opcode), 24) or
      Shift_Left (Unsigned_32 (V.Command_Subtype), 27) or
      Shift_Left (Unsigned_32 (V.Command_Type), 29));
   -- Hardware ignores each coordinate's upper two bits; Build forbids them.
   type Coordinates is record
      X : Unsigned_16 := 0;
      Y : Unsigned_16 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Coordinates use record
      X at 0 range 0 .. 15;
      Y at 0 range 16 .. 31;
   end record;
   function Encode (V : Coordinates) return Unsigned_32 is
     (Unsigned_32 (V.X) or Shift_Left (Unsigned_32 (V.Y), 16));
   -- Signed16 storage; legal origins are signed15 with bit15 sign-extended.
   -- The fixed builder admits only zero origin.
   type Origin is record
      X : Integer_16 := 0;
      Y : Integer_16 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Origin use record
      X at 0 range 0 .. 15;
      Y at 0 range 16 .. 31;
   end record;
   function Signed_Bits (V : Integer_16) return Unsigned_32 is
     (Unsigned_32 (if V < 0 then Integer_32 (V) + 65_536 else Integer_32 (V)))
     with Post => Signed_Bits'Result <= 65_535;
   function Encode (V : Origin) return Unsigned_32 is
     (Signed_Bits (V.X) or Shift_Left (Signed_Bits (V.Y), 16));
   type Words is array (Natural range 0 .. 3) of Unsigned_32;
   type Image is record
      Valid : Boolean := False;
      Data : Words := [others => 0];
   end record;
   function Build (Width, Height : Natural) return Image
     with Post =>
       Build'Result.Valid = (Width in 1 .. 16_384 and Height in 1 .. 16_384)
       and then (if not Build'Result.Valid then
                   Build'Result.Data = Words'(others => 0));
end Intel_GPU_ADLN_Drawing_Rectangle;
