pragma Ada_2022;
package body HID_Descriptor_Hex with SPARK_Mode is
   use type HID_I2C_Descriptors.Byte;
   function Encode (Data : HID_I2C_Descriptors.Bytes) return String is
      Alphabet : constant String := "0123456789abcdef";
      Text : String (1 .. 2 * Data'Length) := [others => '0'];
      Used : Natural := 0;
   begin
      for I in Data'Range loop
         pragma Loop_Invariant (Used = 2 * (I - Data'First));
         pragma Loop_Invariant
           (for all C of Text => C in '0' .. '9' | 'a' .. 'f');
         Text (Used + 1) := Alphabet (Natural (Data (I) / 16) + 1);
         Text (Used + 2) := Alphabet (Natural (Data (I) mod 16) + 1);
         Used := Used + 2;
      end loop;
      return Text;
   end Encode;
   function Chunk (Data : HID_I2C_Descriptors.Bytes; Offset : Natural) return String is
      Length : constant Positive := Positive'Min (Bytes_Per_Line, Data'Length - Offset);
      First : constant Natural := Data'First + Offset;
   begin
      return Encode (Data (First .. First + (Length - 1)));
   end Chunk;
end HID_Descriptor_Hex;
