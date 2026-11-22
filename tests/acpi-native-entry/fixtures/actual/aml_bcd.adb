pragma Ada_2022;
package body AML_BCD with SPARK_Mode is
   function From_BCD (Value : AML_Decode.Integer_Value;
                      Width : AML_Decode.Integer_Width) return Result
   is
      Number : AML_Decode.Integer_Value := 0;
   begin
      -- Most significant digit first, independent of ACPICA's weighted sum.
      for I in reverse 0 .. Active_Digits (Width) - 1 loop
         if Digit (Value, I) >= Decimal_Radix then
            return (Numeric_Overflow, 0);
         end if;
         Number := Number * Decimal_Radix + AML_Decode.Integer_Value (Digit (Value, I));
      end loop;
      return (Converted, Number);
   end From_BCD;
   function To_BCD (Value : AML_Decode.Integer_Value;
                    Width : AML_Decode.Integer_Width) return Result
   is
      Remaining : AML_Decode.Integer_Value := Value;
      Packed : AML_Decode.Integer_Value := 0;
   begin
      if Value > Decimal_Maximum (Width) then
         return (Numeric_Overflow, 0);
      end if;
      for I in 0 .. Active_Digits (Width) - 1 loop
         Packed := Packed or Interfaces.Shift_Left
           (Remaining mod Decimal_Radix, Nibble_Bits * I);
         Remaining := Remaining / Decimal_Radix;
      end loop;
      return (Converted, Packed);
   end To_BCD;
end AML_BCD;
