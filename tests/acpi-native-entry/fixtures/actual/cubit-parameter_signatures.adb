------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  CuBit.Parameter_Signatures - implementation
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Parameter_Signatures with SPARK_Mode is

   function Validate (Item : Bytes) return Validation is
   begin
      if Item'Length < Header_Bytes or else Item'Length > Maximum_Descriptor_Bytes then
         return Wrong_Length;
      elsif Item (1) /= Magic_0 or else Item (2) /= Magic_1
        or else Item (3) /= Magic_2 or else Item (4) /= Magic_3
      then
         return Wrong_Magic;
      elsif Field_16 (Item, Version_Offset) /= Format_Version then
         return Unknown_Format;
      elsif not Lengths_Valid (Item) then
         return Length_Mismatch;
      elsif not Identifier_Valid (Item, Root_First (Item), Root_Last (Item)) then
         return Invalid_Root;
      elsif not (for all K in Signature_First (Item) .. Item'Last => Printable (Item (K))) then
         return Invalid_Signature;
      end if;
      return Valid;
   end Validate;

   function Root_Of (Item : Bytes) return String is
     (String'[for I in 1 .. Declared_Root (Item) => Character'Val (Item (Root_First (Item) + I - 1))]);

   function Signature_Of (Item : Bytes) return String is
     (String'[for I in 1 .. Declared_Signature (Item) => Character'Val (Item (Signature_First (Item) + I - 1))]);

   procedure Encode
     (Root      : String;
      Signature : String;
      Digest    : Identity;
      Item      : out Bytes;
      Length    : out Descriptor_Length;
      Accepted  : out Boolean)
   is
      Total : Descriptor_Length;
   begin
      Item := [others => 0];
      Length := 0;
      Accepted := False;
      if Root'Length not in Root_Length or else Signature'Length not in Signature_Length then
         return;
      end if;
      Total := Header_Bytes + Root'Length + Signature'Length;
      Item (1) := Magic_0;
      Item (2) := Magic_1;
      Item (3) := Magic_2;
      Item (4) := Magic_3;
      Item (Version_Offset + 1) := Unsigned_8 (Format_Version mod 2 ** Byte_Bits);
      Item (Version_Offset + 2) := Unsigned_8 (Format_Version / 2 ** Byte_Bits);
      Item (Signature_Length_Offset + 1) := Unsigned_8 (Signature'Length mod 2 ** Byte_Bits);
      Item (Signature_Length_Offset + 2) := Unsigned_8 (Signature'Length / 2 ** Byte_Bits);
      for I in Identity_Index loop
         Item (Identity_Offset + I) := Digest (I);
      end loop;
      Item (Root_Length_Offset + 1) := Unsigned_8 (Root'Length);
      for K in Root'Range loop
         pragma Loop_Invariant (Item'First = 1);
         Item (Header_Bytes + (K - Root'First) + 1) := Unsigned_8 (Character'Pos (Root (K)));
      end loop;
      for K in Signature'Range loop
         pragma Loop_Invariant (Item'First = 1);
         Item (Header_Bytes + Root'Length + (K - Signature'First) + 1) := Unsigned_8 (Character'Pos (Signature (K)));
      end loop;
      --  The result is checked as any descriptor is: what Encode accepts is
      --  well formed whatever the writing above did.
      if Validate (Item (1 .. Total)) = Valid then
         Length := Total;
         Accepted := True;
      end if;
   end Encode;

end CuBit.Parameter_Signatures;
