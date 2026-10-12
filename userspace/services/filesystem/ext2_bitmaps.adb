------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Block and inode bitmaps (see the spec).
------------------------------------------------------------------------------
--  The loop invariants are proved; checked at run time (hosted -gnata
--  builds) their quantifiers would make Take_Free cubic in its count.
pragma Assertion_Policy (Loop_Invariant => Ignore);

package body Ext2_Bitmaps with SPARK_Mode is

   procedure Set (Map : in out Bitmap; Bit : Bit_Index) is
      Index : constant Natural := Bit / Bits_Per_Byte;
   begin
      Map (Index) := Map (Index) or Mask (Bit);
   end Set;

   procedure Clear (Map : in out Bitmap; Bit : Bit_Index) is
      Index : constant Natural := Bit / Bits_Per_Byte;
   begin
      Map (Index) := Map (Index) and not Mask (Bit);
   end Clear;

   procedure Take_Free
     (Map       : in out Bitmap;
      Scan_Bits : Positive;
      Start     : Natural;
      Limit     : Natural;
      Taken     : in out Bit_List;
      Count     : out Take_Count)
   is
      Bit : Bit_Index;
   begin
      Count := 0;
      for Scanned in 0 .. Scan_Bits - 1 loop
         exit when Count = Limit;
         Bit := (Start + Scanned) mod Scan_Bits;
         if not Is_Set (Map, Bit) then
            Set (Map, Bit);
            Count := Count + 1;
            Taken (Count) := Bit;
         end if;
         pragma Loop_Invariant (Count <= Limit);
         pragma Loop_Invariant (Count <= Scanned + 1);
         pragma Loop_Invariant (Only_Set (Map'Loop_Entry, Map));
         pragma Loop_Invariant
           (for all I in 1 .. Count =>
              Taken (I) < Scan_Bits and then Is_Set (Map, Taken (I)) and then
              not Is_Set (Map'Loop_Entry, Taken (I)));
         pragma Loop_Invariant
           (for all I in 1 .. Count =>
              (for all J in I + 1 .. Count => Taken (I) /= Taken (J)));
      end loop;
   end Take_Free;

end Ext2_Bitmaps;
