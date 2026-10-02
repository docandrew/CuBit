with Ada.Unchecked_Conversion;
with Interfaces;
with System;
package Intel_GPU_TGL_PTE_Registers with SPARK_Mode, Pure is
   -- Intel IHD-OS-TGL-Vol6-5.23 printed pp36-37, 4KiB PTE, client HAW=39.
   -- The table's address prose says64KiB, but heading/diagram/present field
   -- identify4KiB. This describes bits, not permission to enable null mappings
   -- on every engine/platform. N is documented for tiled resources; command
   -- streamer prefetch behavior must be established separately.
   type Bits_1 is mod 2 ** 1 with Size => 1;
   type Bits_2 is mod 2 ** 2 with Size => 2;
   type Bits_27 is mod 2 ** 27 with Size => 27;
   type Bits_25 is mod 2 ** 25 with Size => 25;
   type Leaf is record
      Present, Writable, Ignored_2, Write_Through, Cache_Disable : Bits_1 := 0;
      Ignored_5_6 : Bits_2 := 0;
      PAT, Ignored_8, Null_Page : Bits_1 := 0;
      Ignored_10_11 : Bits_2 := 0;
      Address_Page : Bits_27 := 0;
      Ignored_High : Bits_25 := 0;
   end record with Size => 64, Bit_Order => System.Low_Order_First;
   for Leaf use record
      Present at 0 range 0 .. 0;
      Writable at 0 range 1 .. 1;
      Ignored_2 at 0 range 2 .. 2;
      Write_Through at 0 range 3 .. 3;
      Cache_Disable at 0 range 4 .. 4;
      Ignored_5_6 at 0 range 5 .. 6;
      PAT at 0 range 7 .. 7;
      Ignored_8 at 0 range 8 .. 8;
      Null_Page at 0 range 9 .. 9;
      Ignored_10_11 at 0 range 10 .. 11;
      Address_Page at 0 range 12 .. 38;
      Ignored_High at 0 range 39 .. 63;
   end record;
   function Decode is new Ada.Unchecked_Conversion (Interfaces.Unsigned_64, Leaf);
   function Encode is new Ada.Unchecked_Conversion (Leaf, Interfaces.Unsigned_64);
end Intel_GPU_TGL_PTE_Registers;
