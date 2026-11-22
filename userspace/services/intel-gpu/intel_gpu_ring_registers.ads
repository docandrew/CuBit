with Interfaces; use Interfaces;
with System;
package Intel_GPU_Ring_Registers with SPARK_Mode is
   -- Intel TGL PRM Vol2c-12.21, pp1125-26,1130-31.
   -- Register layout only: decoding a saved context does NOT prove freshness,
   -- ownership, execution completion, or permission to reuse ring storage.
   type B2 is mod 2 ** 2 with Size => 2;
   type B3 is mod 2 ** 3 with Size => 3;
   type B11 is mod 2 ** 11 with Size => 11;
   type B18 is mod 2 ** 18 with Size => 18;
   type B19 is mod 2 ** 19 with Size => 19;
   type Head_Register is record
      Reserved : B2 := 0;
      Dword_Offset : B19 := 0;
      Wrap_Count : B11 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Head_Register use record
      Reserved at 0 range 0 .. 1;
      Dword_Offset at 0 range 2 .. 20;
      Wrap_Count at 0 range 21 .. 31;
   end record;
   type Tail_Register is record
      Reserved_Low : B3 := 0;
      Qword_Offset : B18 := 0;
      Reserved_High : B11 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Tail_Register use record
      Reserved_Low at 0 range 0 .. 2;
      Qword_Offset at 0 range 3 .. 20;
      Reserved_High at 0 range 21 .. 31;
   end record;
   function Decode_Head (Raw : Unsigned_32) return Head_Register is
     (Reserved => B2 (Raw and 3),
      Dword_Offset => B19 (Shift_Right (Raw, 2) and 16#7FFFF#),
      Wrap_Count => B11 (Shift_Right (Raw, 21)));
   function Decode_Tail (Raw : Unsigned_32) return Tail_Register is
     (Reserved_Low => B3 (Raw and 7),
      Qword_Offset => B18 (Shift_Right (Raw, 3) and 16#3FFFF#),
      Reserved_High => B11 (Shift_Right (Raw, 21)));
   function Offset (Value : Head_Register) return Unsigned_32 is
     (Unsigned_32 (Value.Dword_Offset) * 4);
   function Offset (Value : Tail_Register) return Unsigned_32 is
     (Unsigned_32 (Value.Qword_Offset) * 8);
   function Valid (Value : Head_Register; Ring_Bytes : Unsigned_32) return Boolean is
     (Value.Reserved = 0 and then Offset (Value) < Ring_Bytes);
   function Valid (Value : Tail_Register; Ring_Bytes : Unsigned_32) return Boolean is
     (Value.Reserved_Low = 0 and then Value.Reserved_High = 0 and then
      Offset (Value) < Ring_Bytes);
end Intel_GPU_Ring_Registers;
