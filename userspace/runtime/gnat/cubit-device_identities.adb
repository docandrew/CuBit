------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Device_Identities with SPARK_Mode is

   Byte_Bits : constant := 8;
   Byte_Mask : constant := 16#FF#;

   function To_Text (S : String) return Identity_Text is
      Value : String (1 .. Maximum_Text) := [others => ' '];
      Length : Text_Length := 0;
   begin
      for K in S'Range loop
         pragma Loop_Invariant (for all I in 1 .. Length => Printable (Value (I)));
         exit when Length = Maximum_Text or else not Printable (S (K));
         Length := Length + 1;
         Value (Length) := S (K);
      end loop;
      return (Length => Length, Value => Value);
   end To_Text;

   procedure Put_Field (Data : in out Record_Image; At_Byte : Natural; Item : Identity_Text)
     with Pre => At_Byte in 0 | Field_Bytes;
   procedure Put_Field (Data : in out Record_Image; At_Byte : Natural; Item : Identity_Text)
   is
   begin
      Data (At_Byte + 1) := Unsigned_8 (Item.Length mod 2 ** Byte_Bits);
      Data (At_Byte + 2) := Unsigned_8 (Item.Length / 2 ** Byte_Bits);
      for I in 1 .. Item.Length loop
         Data (At_Byte + 2 + I) := Character'Pos (Item.Value (I));
      end loop;
   end Put_Field;

   function Encode (Item : Identity) return Record_Image is
      Result : Record_Image := [others => 0];
   begin
      Put_Field (Result, 0, Item.Model);
      Put_Field (Result, Field_Bytes, Item.Serial);
      return Result;
   end Encode;

   procedure Get_Field
     (Data : Record_Image; At_Byte : Natural; Item : out Identity_Text; Valid : out Boolean)
     with Pre => At_Byte in 0 | Field_Bytes,
          Post => (if Valid then Well_Formed (Item));
   procedure Get_Field
     (Data : Record_Image; At_Byte : Natural; Item : out Identity_Text; Valid : out Boolean)
   is
      Length : constant Natural :=
        Natural (Data (At_Byte + 1)) + Natural (Data (At_Byte + 2)) * 2 ** Byte_Bits;
   begin
      Item := (others => <>);
      Valid := Length <= Maximum_Text;
      if not Valid then
         return;
      end if;
      for I in 1 .. Length loop
         pragma Loop_Invariant (Item.Length = I - 1 and then Well_Formed (Item));
         if Data (At_Byte + 2 + I) > Byte_Mask / 2
           or else not Printable (Character'Val (Data (At_Byte + 2 + I)))
         then
            Item := (others => <>);
            Valid := False;
            return;
         end if;
         Item.Value (I) := Character'Val (Data (At_Byte + 2 + I));
         Item.Length := I;
      end loop;
   end Get_Field;

   procedure Decode (Data : Record_Image; Item : out Identity; Valid : out Boolean) is
      Model_OK, Serial_OK : Boolean;
   begin
      Item := (others => <>);
      Get_Field (Data, 0, Item.Model, Model_OK);
      Get_Field (Data, Field_Bytes, Item.Serial, Serial_OK);
      Valid := Model_OK and then Serial_OK;
   end Decode;

end CuBit.Device_Identities;
