with AML_Decode;

-- Pure policy for fields within owned Buffers. No address or object authority.
package AML_Buffer_Bits with SPARK_Mode, Pure is
   use type AML_Decode.Byte;
   use type AML_Decode.Integer_Value;

   Bits_Per_Byte : constant AML_Decode.Integer_Value := 8;
   subtype Bit_Index is Natural range 0 .. Natural (Bits_Per_Byte) - 1;
   subtype Bit_Offset is AML_Decode.Integer_Value;
   subtype Bit_Count is AML_Decode.Integer_Value;

   -- Reject empty fields. Work in byte quotients and bit remainders so neither
   -- Offset + Length nor Buffer_Length * Bits_Per_Byte can wrap or overflow.
   function Fits (Buffer_Length : Natural; Offset : Bit_Offset;
      Length : Bit_Count) return Boolean;

   function Mask (Index : Bit_Index) return AML_Decode.Byte is
     (2 ** Index);

   function Bit_Set (Value : AML_Decode.Byte; Index : Bit_Index)
      return Boolean is ((Value and Mask (Index)) /= 0);

   function Replace_Bit (Value : AML_Decode.Byte; Index : Bit_Index;
      Set : Boolean) return AML_Decode.Byte
     with Post => Bit_Set (Replace_Bit'Result, Index) = Set
       and then (Replace_Bit'Result and not Mask (Index)) =
         (Value and not Mask (Index));
end AML_Buffer_Bits;
