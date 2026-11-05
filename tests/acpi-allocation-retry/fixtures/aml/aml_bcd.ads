pragma Ada_2022;
with AML_Decode;
with Interfaces;
package AML_BCD with SPARK_Mode, Pure is
   use type AML_Decode.Integer_Value;
   use type AML_Decode.Integer_Width;
   subtype Digit_Index is Natural range 0 .. 15;
   subtype Digit_Count is Positive range 8 .. 16;
   subtype Nibble is Natural range 0 .. 15;
   Decimal_Radix : constant := 10;
   Nibble_Bits : constant := 4;
   -- Static factors keep checked postconditions independent of an optional
   -- run-time exponentiation helper on the native CuBit profile.
   type Decimal_Power_Table is array (Digit_Index) of AML_Decode.Integer_Value;
   Decimal_Powers : constant Decimal_Power_Table :=
     [Decimal_Radix ** 0, Decimal_Radix ** 1, Decimal_Radix ** 2, Decimal_Radix ** 3,
      Decimal_Radix ** 4, Decimal_Radix ** 5, Decimal_Radix ** 6, Decimal_Radix ** 7,
      Decimal_Radix ** 8, Decimal_Radix ** 9, Decimal_Radix ** 10, Decimal_Radix ** 11,
      Decimal_Radix ** 12, Decimal_Radix ** 13, Decimal_Radix ** 14, Decimal_Radix ** 15];
   function Decimal_Power (Index : Digit_Index) return AML_Decode.Integer_Value is
     (Decimal_Powers (Index));
   function Active_Digits (Width : AML_Decode.Integer_Width) return Digit_Count is
     (if Width = AML_Decode.Bits_32 then 8 else 16);
   function Decimal_Maximum (Width : AML_Decode.Integer_Width)
     return AML_Decode.Integer_Value is
       (Decimal_Power (Digit_Index (Active_Digits (Width) - 1)) * Decimal_Radix - 1);
   function Digit (Value : AML_Decode.Integer_Value; Index : Digit_Index)
     return Nibble is
       (Nibble (Interfaces.Shift_Right (Value, Nibble_Bits * Index) and 15));
   function Valid_BCD (Value : AML_Decode.Integer_Value;
                       Width : AML_Decode.Integer_Width) return Boolean is
     (for all I in 0 .. Active_Digits (Width) - 1 => Digit (Value, I) < Decimal_Radix);
   type Conversion_Status is (Converted, Numeric_Overflow);
   type Result is record
      Status : Conversion_Status := Numeric_Overflow;
      Value : AML_Decode.Integer_Value := 0;
   end record;
   -- Raw operand semantics: inspect only active BCD digits. Inactive high
   -- bits of a 32-bit conversion are ignored; no integer coercion is done.
   function From_BCD (Value : AML_Decode.Integer_Value;
                      Width : AML_Decode.Integer_Width) return Result
     with Post =>
       (From_BCD'Result.Status = Converted) = Valid_BCD (Value, Width)
       and then (if From_BCD'Result.Status = Numeric_Overflow then
         From_BCD'Result.Value = 0
       else From_BCD'Result.Value <= Decimal_Maximum (Width)
         and then (for all I in 0 .. Active_Digits (Width) - 1 =>
           (From_BCD'Result.Value / Decimal_Power (Digit_Index (I))) mod Decimal_Radix =
             AML_Decode.Integer_Value (Digit (Value, I))));
   -- Raw operand semantics: do not normalize before conversion. Any value
   -- exceeding the active decimal capacity fails, including inactive bits.
   function To_BCD (Value : AML_Decode.Integer_Value;
                    Width : AML_Decode.Integer_Width) return Result
     with Post =>
       (To_BCD'Result.Status = Converted) = (Value <= Decimal_Maximum (Width))
       and then (if To_BCD'Result.Status = Numeric_Overflow then
         To_BCD'Result.Value = 0
       else Valid_BCD (To_BCD'Result.Value, Width)
         and then (if Width = AML_Decode.Bits_32 then To_BCD'Result.Value <= 16#FFFF_FFFF#)
         and then (for all I in 0 .. Active_Digits (Width) - 1 =>
           AML_Decode.Integer_Value (Digit (To_BCD'Result.Value, I)) =
             (Value / Decimal_Power (Digit_Index (I))) mod Decimal_Radix));
end AML_BCD;
