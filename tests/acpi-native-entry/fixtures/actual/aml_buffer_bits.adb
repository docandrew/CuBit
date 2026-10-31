package body AML_Buffer_Bits with SPARK_Mode is
   function Fits (Buffer_Length : Natural; Offset : Bit_Offset;
      Length : Bit_Count) return Boolean
   is
      First_Byte : constant AML_Decode.Integer_Value := Offset / Bits_Per_Byte;
      Byte_Length : constant AML_Decode.Integer_Value :=
        AML_Decode.Integer_Value (Buffer_Length);
      Whole_Bytes : AML_Decode.Integer_Value;
      Available : AML_Decode.Integer_Value;
      Last_Bit_Remainder : AML_Decode.Integer_Value;
   begin
      if Length = 0 or else First_Byte >= Byte_Length then
         return False;
      end if;
      -- Number of complete bytes crossed after the first bit. The two
      -- remainders may contribute one additional byte, accounted for below.
      Whole_Bytes := (Length - 1) / Bits_Per_Byte;
      Available := Byte_Length - 1 - First_Byte;
      Last_Bit_Remainder := Offset mod Bits_Per_Byte
        + (Length - 1) mod Bits_Per_Byte;
      return Whole_Bytes < Available or else
        (Whole_Bytes = Available and then Last_Bit_Remainder < Bits_Per_Byte);
   end Fits;

   function Replace_Bit (Value : AML_Decode.Byte; Index : Bit_Index;
      Set : Boolean) return AML_Decode.Byte is
     (if Set then Value or Mask (Index) else Value and not Mask (Index));
end AML_Buffer_Bits;
