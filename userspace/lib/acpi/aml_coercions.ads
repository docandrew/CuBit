pragma Ada_2022;
with AML_Decode;
with Interfaces;
package AML_Coercions with SPARK_Mode, Pure is
   use type AML_Decode.Integer_Value;
   use type AML_Decode.Byte;
   use type AML_Decode.Integer_Width;
   type Conversion_Status is (Converted, Empty_Buffer, Not_Convertible);
   type Result is record
      Status : Conversion_Status := Not_Convertible;
      Value : AML_Decode.Integer_Value := 0;
   end record;
   function Maximum (Width : AML_Decode.Integer_Width) return AML_Decode.Integer_Value;
   subtype Byte_Index is Natural range 0 .. 7;
   function Octet (Value : AML_Decode.Integer_Value; Index : Byte_Index) return AML_Decode.Byte is
     (AML_Decode.Byte (Interfaces.Shift_Right (Value, Index * 8) and 255));
   function Buffer_Count (Length : Natural; Width : AML_Decode.Integer_Width) return Natural is
     (Natural'Min (Length, (if Width = AML_Decode.Bits_32 then 4 else 8)));
   -- Implicit (hexadecimal) string conversion, including ACPICA's whitespace,
   -- optional 0x prefix and empty-string extensions. Stops before overflow.
   function From_String (Data : AML_Decode.Bytes; Width : AML_Decode.Integer_Width) return Result
     with Post => From_String'Result.Status = Converted
       and then From_String'Result.Value <= Maximum (Width);
   -- Explicit ToInteger conversion: decimal unless prefixed with 0x/0X.
   -- Stop at the first non-digit or before active-width overflow.
   function From_Explicit_String
     (Data : AML_Decode.Bytes; Width : AML_Decode.Integer_Width) return Result
     with Post => From_Explicit_String'Result.Status = Converted
       and then From_Explicit_String'Result.Value <= Maximum (Width);
   -- Least-significant byte first; ignore bytes beyond the table integer width.
   function From_Buffer (Data : AML_Decode.Bytes; Width : AML_Decode.Integer_Width) return Result
     with Post => From_Buffer'Result.Value <= Maximum (Width)
       and then From_Buffer'Result.Status = (if Data'Length = 0 then Empty_Buffer else Converted)
       and then (for all I in Byte_Index =>
         Octet (From_Buffer'Result.Value, I) =
           (if I < Buffer_Count (Data'Length, Width) then Data (Data'First + I) else 0));
end AML_Coercions;
