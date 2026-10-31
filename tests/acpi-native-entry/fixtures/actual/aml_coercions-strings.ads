pragma Ada_2022;
package AML_Coercions.Strings with SPARK_Mode, Pure is
   subtype Nibble is AML_Decode.Byte range 0 .. 15;
   function Hex (Value : Nibble) return AML_Decode.Byte is
     (if Value < 10 then 48 + Value else 65 + Value - 10);
   function Hex_Length (Width : AML_Decode.Integer_Width) return Positive is
     (if Width = AML_Decode.Bits_32 then 8 else 16);
   -- Implicit source conversion, not ToHexString or buffer ASCII decoding.
   function From_Integer
     (Value : AML_Decode.Integer_Value; Width : AML_Decode.Integer_Width)
      return AML_Decode.Bytes with
      Post => From_Integer'Result'First = 1 and then
        From_Integer'Result'Length = Hex_Length (Width) and then
        (for all I in From_Integer'Result'Range =>
           From_Integer'Result (I) = Hex (AML_Decode.Byte
             (Interfaces.Shift_Right (Value, (Hex_Length (Width) - I) * 4) and 15)));
   function From_Buffer (Data : AML_Decode.Bytes) return AML_Decode.Bytes with
      Pre => Data'Length <= Natural'Last / 5,
      Post => From_Buffer'Result'First = 1 and then
        From_Buffer'Result'Length = (if Data'Length = 0 then 0 else Data'Length * 5 - 1)
        and then (for all I in 1 .. Data'Length =>
          From_Buffer'Result ((I - 1) * 5 + 1) = Character'Pos ('0') and then
          From_Buffer'Result ((I - 1) * 5 + 2) = Character'Pos ('x') and then
          From_Buffer'Result ((I - 1) * 5 + 3) = Hex (Data (Data'First + (I - 1)) / 16) and then
          From_Buffer'Result ((I - 1) * 5 + 4) = Hex (Data (Data'First + (I - 1)) mod 16) and then
          (if I < Data'Length then From_Buffer'Result (I * 5) = Character'Pos (' ')));
end AML_Coercions.Strings;
