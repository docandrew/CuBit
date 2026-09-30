with Ada.Unchecked_Conversion;
with Interfaces;
with System;
package Intel_GPU_PAT_Registers with SPARK_Mode is
   -- Intel IHD-OS-TGL-Vol 2c-12.21 pp645-646: PAT_INDEX[0..7].
   -- Cross-check: Linux v6.16 gt/intel_gtt.c:tgl_setup_private_ppat.
   -- This is the Gen12/TGL-style layout used by the ADLN path, NOT the
   -- later Xe/MTL layout. Decode copied words; do not overlay live MMIO.
   type Memory_Type is (Uncacheable, Write_Combining, Write_Through, Write_Back)
     with Size => 2;
   for Memory_Type use
     (Uncacheable => 0, Write_Combining => 1, Write_Through => 2, Write_Back => 3);
   type Bits_2 is mod 2 ** 2 with Size => 2;
   type Bits_28 is mod 2 ** 28 with Size => 28;
   type PAT_Register is record
      Cache : Memory_Type := Uncacheable; -- R/W, all four encodings defined
      Reserved_Low : Bits_2 := 0;         -- RO/MBZ, not copied into writes
      Reserved_High : Bits_28 := 0;       -- RO/MBZ, not copied into writes
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for PAT_Register use record
      Cache at 0 range 0 .. 1;
      Reserved_Low at 0 range 2 .. 3;
      Reserved_High at 0 range 4 .. 31;
   end record;
   function Decode is new Ada.Unchecked_Conversion (Interfaces.Unsigned_32, PAT_Register);
   function Encode is new Ada.Unchecked_Conversion (PAT_Register, Interfaces.Unsigned_32);
   function Matches (Raw : Interfaces.Unsigned_32; Wanted : Memory_Type) return Boolean is
     (Decode (Raw).Cache = Wanted and then Decode (Raw).Reserved_Low = 0 and then
      Decode (Raw).Reserved_High = 0);
end Intel_GPU_PAT_Registers;
