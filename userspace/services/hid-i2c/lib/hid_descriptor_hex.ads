pragma Ada_2022;
with HID_I2C_Descriptors;
package HID_Descriptor_Hex with SPARK_Mode, Pure is
   Bytes_Per_Line : constant := 192;
   Maximum_Bytes : constant := 65_535;
   function Encode (Data : HID_I2C_Descriptors.Bytes) return String
     with Pre => Data'Length <= Bytes_Per_Line,
       Post => Encode'Result'Length = 2 * Data'Length
         and then (for all C of Encode'Result => C in '0' .. '9' | 'a' .. 'f');
   function Chunk (Data : HID_I2C_Descriptors.Bytes; Offset : Natural) return String
     with Pre => Data'Length <= Maximum_Bytes and then Offset < Data'Length,
       Post => Chunk'Result'Length =
         2 * Natural'Min (Bytes_Per_Line, Data'Length - Offset)
         and then (for all C of Chunk'Result => C in '0' .. '9' | 'a' .. 'f');
end HID_Descriptor_Hex;
