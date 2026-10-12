------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Block and inode bitmaps: bit n of a group's bitmap is byte n / 8, bit
--  n mod 8 (least significant first), set when the block or inode is in
--  use, as Linux's ext2_set_bit. Pure and proved (SPARK): a reservation
--  only takes bits that were free, each once, and never frees one; a
--  release only frees bits that were in use.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Ext2_Format;

package Ext2_Bitmaps with Pure, SPARK_Mode is

   --  Proved free of run-time errors (SPARK, level 2): no checks needed.
   pragma Suppress (All_Checks);

   Bits_Per_Byte : constant := Ext2_Format.Bits_Per_Byte;
   Maximum_Bytes : constant := Ext2_Format.Maximum_Bitmap_Bytes;
   Maximum_Bits  : constant := Maximum_Bytes * Bits_Per_Byte;

   subtype Byte_Index is Natural range 0 .. Maximum_Bytes - 1;
   type Bitmap is array (Byte_Index range <>) of Unsigned_8;

   --  A bit of a group's bitmap.
   subtype Bit_Index is Natural range 0 .. Maximum_Bits - 1;

   function Has (Map : Bitmap; Bit : Bit_Index) return Boolean is
     (Map'First = 0 and then Bit / Bits_Per_Byte < Map'Length);

   function Mask (Bit : Bit_Index) return Unsigned_8 is
     (Shift_Left (Unsigned_8'(1), Bit mod Bits_Per_Byte));

   function Is_Set (Map : Bitmap; Bit : Bit_Index) return Boolean is
     ((Map (Bit / Bits_Per_Byte) and Mask (Bit)) /= 0)
     with Pre => Has (Map, Bit);

   --  No bit set in Before is clear in After.
   function Only_Set (Before, After : Bitmap) return Boolean is
     (for all K in Before'Range => (Before (K) and not After (K)) = 0)
     with Pre => Before'First = After'First and then Before'Last = After'Last;

   procedure Set (Map : in out Bitmap; Bit : Bit_Index)
     with Pre  => Has (Map, Bit),
          Post => Is_Set (Map, Bit) and then
                  Map (Bit / Bits_Per_Byte) =
                    (Map'Old (Bit / Bits_Per_Byte) or Mask (Bit)) and then
                  (for all K in Map'Range =>
                     (if K /= Bit / Bits_Per_Byte then Map (K) = Map'Old (K)));

   procedure Clear (Map : in out Bitmap; Bit : Bit_Index)
     with Pre  => Has (Map, Bit),
          Post => not Is_Set (Map, Bit) and then
                  Map (Bit / Bits_Per_Byte) =
                    (Map'Old (Bit / Bits_Per_Byte) and not Mask (Bit)) and then
                  (for all K in Map'Range =>
                     (if K /= Bit / Bits_Per_Byte then Map (K) = Map'Old (K)));

   --  Bits a reservation took, in the order taken.
   subtype Take_Count is Natural range 0 .. Maximum_Bits;
   type Bit_List is array (Positive range <>) of Bit_Index;

   --  Take up to Limit free bits among the first Scan_Bits, visiting bits
   --  (Start + 0) mod Scan_Bits, (Start + 1) mod Scan_Bits, ... once each
   --  (a goal, wrapping), marking each in use. Taken (1 .. Count) lists them.
   procedure Take_Free
     (Map       : in out Bitmap;
      Scan_Bits : Positive;
      Start     : Natural;
      Limit     : Natural;
      Taken     : in out Bit_List;
      Count     : out Take_Count)
     with Pre  => Map'First = 0 and then
                  Scan_Bits <= Map'Length * Bits_Per_Byte and then
                  Start <= Maximum_Bits and then
                  Taken'First = 1 and then Limit <= Taken'Length,
          Post => Count <= Limit and then
                  Only_Set (Map'Old, Map) and then
                  (for all I in 1 .. Count =>
                     Taken (I) < Scan_Bits and then
                     Is_Set (Map, Taken (I)) and then not Is_Set (Map'Old, Taken (I))) and then
                  (for all I in 1 .. Count =>
                     (for all J in I + 1 .. Count => Taken (I) /= Taken (J)));

   pragma Inline_Always (Has, Mask, Is_Set, Set, Clear);

end Ext2_Bitmaps;
