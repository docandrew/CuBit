with Ada.Unchecked_Conversion;
with Interfaces;
with System;
package Intel_GPU_L3_MOCS_Registers with SPARK_Mode, Pure is
   -- Intel TGL Vol2c-12.21 pp1287-1289, LNCFCMOCS0, same paired
   -- layout through LNCFCMOCS31. Linux v6.16 intel_mocs.c cross-check.
   -- ADLN path only; later platforms assign additional fields at bits6/7.
   type Bits_1 is mod 2 ** 1 with Size => 1;
   type Bits_2 is mod 2 ** 2 with Size => 2;
   type Bits_3 is mod 2 ** 3 with Size => 3;
   type Bits_7 is mod 2 ** 7 with Size => 7;
   Uncached : constant Bits_2 := 1;
   Write_Back : constant Bits_2 := 3;
   type Pair_Register is record
      Lower_Skip_Enable : Bits_1 := 0;
      Lower_Skip_Control : Bits_3 := 0;
      Lower_Cache : Bits_2 := 0;
      Lower_Reserved_6 : Bits_2 := 0;
      Lower_Reserved_8 : Bits_7 := 0;
      Lower_Write_Mask : Bits_1 := 0;
      Upper_Skip_Enable : Bits_1 := 0;
      Upper_Skip_Control : Bits_3 := 0;
      Upper_Cache : Bits_2 := 0;
      Upper_Reserved_6 : Bits_2 := 0;
      Upper_Reserved_8 : Bits_7 := 0;
      Upper_Write_Mask : Bits_1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Pair_Register use record
      Lower_Skip_Enable at 0 range 0 .. 0;
      Lower_Skip_Control at 0 range 1 .. 3;
      Lower_Cache at 0 range 4 .. 5;
      Lower_Reserved_6 at 0 range 6 .. 7;
      Lower_Reserved_8 at 0 range 8 .. 14;
      Lower_Write_Mask at 0 range 15 .. 15;
      Upper_Skip_Enable at 0 range 16 .. 16;
      Upper_Skip_Control at 0 range 17 .. 19;
      Upper_Cache at 0 range 20 .. 21;
      Upper_Reserved_6 at 0 range 22 .. 23;
      Upper_Reserved_8 at 0 range 24 .. 30;
      Upper_Write_Mask at 0 range 31 .. 31;
   end record;
   function Decode is new Ada.Unchecked_Conversion (Interfaces.Unsigned_32, Pair_Register);
   function Encode is new Ada.Unchecked_Conversion (Pair_Register, Interfaces.Unsigned_32);
   -- All reserved fields are RO/MBZ. Masks are WO, not readback state.
   -- Zero write masks program BOTH entries, matching Linux's pair writes.
   function Pack (Lower, Upper : Bits_2) return Interfaces.Unsigned_32 is
     (Encode (Pair_Register'(Lower_Cache => Lower, Upper_Cache => Upper, others => <>)));
   function Matches (Raw, Expected : Interfaces.Unsigned_32) return Boolean;
end Intel_GPU_L3_MOCS_Registers;
