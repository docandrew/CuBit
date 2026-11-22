------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  CuBit.Launch_Envelopes - implementation
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Launch_Envelopes with SPARK_Mode is

   function Word_At (Item : Block; First : Positive) return Unsigned_64 is
      Value : Unsigned_64 := 0;
   begin
      for I in reverse 0 .. Word_Bytes - 1 loop
         Value := Shift_Left (Value, 8) or Unsigned_64 (Item (First + I));
      end loop;
      return Value;
   end Word_At;

   procedure Put_Word (Item : in out Block; First : Positive; Value : Unsigned_64)
   with Pre  => Item'First = 1 and then First <= Item'Last
                and then Item'Last - First >= Word_Bytes - 1,
        Post => (for all I in Item'Range =>
                   (if I not in First .. First + Word_Bytes - 1
                    then Item (I) = Item'Old (I)));

   procedure Put_Word (Item : in out Block; First : Positive; Value : Unsigned_64) is
   begin
      for I in 0 .. Word_Bytes - 1 loop
         Item (First + I) := Unsigned_8 (Shift_Right (Value, 8 * I) and 16#FF#);
         pragma Loop_Invariant
           (for all J in Item'Range =>
              (if J not in First .. First + Word_Bytes - 1
               then Item (J) = Item'Loop_Entry (J)));
      end loop;
   end Put_Word;

   procedure Append
     (Item   : in out Block;
      Length : in out Block_Length;
      Table  : CuBit.Startup_Tables.Bytes;
      Fits   : out Boolean)
   is
      Pad  : constant Natural := Padded (Table'Length);
      Base : constant Block_Length := Length;
   begin
      if Item'Last - Base < Pad + Trailer_Bytes
        or else Maximum_Block_Bytes - Base < Pad + Trailer_Bytes
      then
         Fits := False;
         return;
      end if;
      for I in 1 .. Pad loop
         Item (Base + I) :=
           (if I <= Table'Length then Table (Table'First + I - 1) else 0);
         pragma Loop_Invariant
           (Item (1 .. Base) = Item'Loop_Entry (1 .. Base));
      end loop;
      Put_Word (Item, Base + Pad + 1, Unsigned_64 (Table'Length));
      Put_Word (Item, Base + Pad + Word_Bytes + 1, Magic);
      Length := Base + Pad + Trailer_Bytes;
      Fits := True;
   end Append;

   procedure Split
     (Item           : Block;
      Payload_Length : out Block_Length;
      Table_First    : out Positive;
      Table_Bytes    : out Table_Count;
      Present        : out Boolean)
   is
      Count : Unsigned_64;
      Total : constant Natural := Item'Length;
   begin
      Payload_Length := Natural'Min (Total, Maximum_Block_Bytes);
      Table_First := 1;
      Table_Bytes := 0;
      Present := False;
      if Total < Trailer_Bytes or else Total > Maximum_Block_Bytes
        or else Word_At (Item, Total - Word_Bytes + 1) /= Magic
      then
         return;
      end if;
      Count := Word_At (Item, Total - Trailer_Bytes + 1);
      if Count > Unsigned_64 (Maximum_Table_Bytes)
        or else Padded (Natural (Count)) > Total - Trailer_Bytes
      then
         return;
      end if;
      Table_Bytes := Natural (Count);
      Payload_Length := Total - Trailer_Bytes - Padded (Table_Bytes);
      Table_First := Payload_Length + 1;
      Present := True;
   end Split;

end CuBit.Launch_Envelopes;
