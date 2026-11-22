with Interfaces;
with AML_Decode;
package body ACPI_Fatal_Diagnostics with SPARK_Mode is
   use type Interfaces.Unsigned_64;
   use type AML_Decode.Integer_Width;
   Hexadecimal_Radix : constant Interfaces.Unsigned_64 := 16;
   Byte_Hex_Digits : constant Positive := 2;
   Code_Hex_Digits : constant Positive := 8;
   Argument_Hex_Digits : constant Positive := 16;
   Context_Hex_Digits : constant Positive := 16;
   function Hex (Value : Interfaces.Unsigned_64; Hex_Digits : Positive) return String is
      Alphabet : constant String := "0123456789ABCDEF";
      Result : String (1 .. Hex_Digits);
      Remaining : Interfaces.Unsigned_64 := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Alphabet (Natural (Remaining mod Hexadecimal_Radix) + 1);
         Remaining := Remaining / Hexadecimal_Radix;
      end loop;
      return Result;
   end Hex;
   function Format (Seen : Observation) return String is
   begin
      if not Seen.Present then return "acpi fatal: none"; end if;
      return "acpi fatal origin=" &
        (if Seen.Source.Kind = Definition_Block then
           "module table=" & Hex (Interfaces.Unsigned_64 (Seen.Source.Table_ID), Context_Hex_Digits)
         else "initializer scope=" & Hex (Interfaces.Unsigned_64 (Seen.Source.Initializer_Scope), Context_Hex_Digits)) &
        " type=" & Hex (Interfaces.Unsigned_64 (Seen.Item.Literal.Kind), Byte_Hex_Digits) &
        " code=" & Hex (Interfaces.Unsigned_64 (Seen.Item.Literal.Code), Code_Hex_Digits) &
        " arg=" & Hex (Seen.Item.Evaluated_Argument, Argument_Hex_Digits) &
        " width=" & (if Seen.Item.Width = AML_Decode.Bits_32 then "32" else "64");
   end Format;
end ACPI_Fatal_Diagnostics;
