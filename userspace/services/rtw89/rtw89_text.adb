------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body RTW89_Text is

   Digit_Text : constant String := "0123456789abcdef";
   Hex_Base   : constant := 16;
   Dec_Base   : constant := 10;
   Max_Decimal_Digits : constant := 20;   --  Unsigned_64'Last

   function Hex (Value : Unsigned_64; Digits_Count : Positive) return String is
      Result : String (1 .. Digits_Count);
      Rest   : Unsigned_64 := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Digit_Text (Natural (Rest mod Hex_Base) + 1);
         Rest := Rest / Hex_Base;
      end loop;
      return Result;
   end Hex;

   function Dec (Value : Unsigned_64) return String is
      Text  : String (1 .. Max_Decimal_Digits);
      First : Positive := Max_Decimal_Digits;
      Rest  : Unsigned_64 := Value;
   begin
      loop
         Text (First) := Digit_Text (Natural (Rest mod Dec_Base) + 1);
         Rest := Rest / Dec_Base;
         exit when Rest = 0;
         First := First - 1;
      end loop;
      return Text (First .. Max_Decimal_Digits);
   end Dec;

end RTW89_Text;
