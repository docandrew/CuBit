------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  CuBit.Parameter_Sections - implementation
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Parameter_Sections with SPARK_Mode is

   procedure Measure (Item : Bytes; Present : out Boolean; Length : out Section_Length) is
   begin
      Present := Header_Valid (Item);
      Length := (if Present then Header_Bytes + Declared_Value (Item) else 0);
   end Measure;

   function Validate (Item : Bytes) return Validation is
   begin
      if Item'Length < Header_Bytes then
         return Wrong_Length;
      elsif Item (1) /= Magic_0 or else Item (2) /= Magic_1
        or else Item (3) /= Magic_2 or else Item (4) /= Magic_3
      then
         return Wrong_Magic;
      elsif Declared_Value (Item) not in Value_Length
        or else Item'Length /= Header_Bytes + Declared_Value (Item)
      then
         return Wrong_Length;
      elsif not (for all K in Header_Bytes + 1 .. Item'Last => Signatures.Printable (Item (K))) then
         return Invalid_Value;
      end if;
      return Valid;
   end Validate;

   procedure Encode
     (Digest   : Signatures.Identity;
      Value    : String;
      Item     : out Bytes;
      Length   : out Section_Length;
      Accepted : out Boolean)
   is
      Total : Section_Length;
   begin
      Item := [others => 0];
      Length := 0;
      Accepted := False;
      if Value'Length not in Value_Length then
         return;
      end if;
      Total := Header_Bytes + Value'Length;
      Item (1) := Magic_0;
      Item (2) := Magic_1;
      Item (3) := Magic_2;
      Item (4) := Magic_3;
      Item (Value_Length_Offset + 1) := Unsigned_8 (Value'Length mod 2 ** Byte_Bits);
      Item (Value_Length_Offset + 2) := Unsigned_8 (Value'Length / 2 ** Byte_Bits);
      for I in Signatures.Identity_Index loop
         Item (Identity_Offset + I) := Digest (I);
      end loop;
      for K in Value'Range loop
         pragma Loop_Invariant (Item'First = 1);
         Item (Header_Bytes + (K - Value'First) + 1) := Unsigned_8 (Character'Pos (Value (K)));
      end loop;
      --  Checked as any section is.
      if Validate (Item (1 .. Total)) = Valid then
         Length := Total;
         Accepted := True;
      end if;
   end Encode;

end CuBit.Parameter_Sections;
