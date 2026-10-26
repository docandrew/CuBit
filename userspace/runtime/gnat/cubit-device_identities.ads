------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A block device's identity, as OP_IDENTIFY_DEVICE reports it: its model
--  and serial number. The installer lists disks by it, and an install's
--  confirmation must name it exactly (SYS-001).
--
--  Wire form, at the start of the caller's grant (little-endian):
--    0      model length (16 bits), then Maximum_Text bytes of model
--    66     serial length (16 bits), then Maximum_Text bytes of serial
--  Text is printable ASCII; unused bytes are zero.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Device_Identities with Pure, SPARK_Mode is

   Maximum_Text : constant := 64;
   subtype Text_Length is Natural range 0 .. Maximum_Text;

   type Identity_Text is record
      Length : Text_Length := 0;
      Value  : String (1 .. Maximum_Text) := [others => ' '];
   end record;

   function Printable (Item : Character) return Boolean is
     (Item in ' ' .. '~');

   function Well_Formed (Item : Identity_Text) return Boolean is
     (for all I in 1 .. Item.Length => Printable (Item.Value (I)));

   type Identity is record
      Model  : Identity_Text;
      Serial : Identity_Text;
   end record;

   --  Text from S: its printable prefix, at most Maximum_Text characters.
   function To_Text (S : String) return Identity_Text
     with Post => Well_Formed (To_Text'Result);

   function Same (Left, Right : Identity_Text) return Boolean is
     (Left.Length = Right.Length
      and then Left.Value (1 .. Left.Length) = Right.Value (1 .. Right.Length));

   Field_Bytes : constant := 2 + Maximum_Text;
   Record_Bytes : constant := 2 * Field_Bytes;
   type Byte_Array is array (Positive range <>) of Unsigned_8;
   subtype Record_Image is Byte_Array (1 .. Record_Bytes);

   function Encode (Item : Identity) return Record_Image
     with Pre => Well_Formed (Item.Model) and then Well_Formed (Item.Serial);

   --  Valid: both lengths in range and the text printable.
   procedure Decode (Data : Record_Image; Item : out Identity; Valid : out Boolean)
     with Post => (if Valid then Well_Formed (Item.Model) and then Well_Formed (Item.Serial));

end CuBit.Device_Identities;
