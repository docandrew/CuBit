------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body UFS_Text with SPARK_Mode is
   Decimal_Base : constant := 10;
   Hex_Base     : constant := 16;
   Nibble_Bits  : constant := 4;
   Hex_Symbols  : constant String (1 .. Hex_Base) := "0123456789abcdef";

   subtype Digit_Count is Positive range 1 .. Decimal_Digits;
   subtype Power_Index is Natural range 0 .. Decimal_Digits - 1;
   --  Powers (K) = 10 ** K.
   Powers : constant array (Power_Index) of Unsigned_64 :=
     [10 ** 0, 10 ** 1, 10 ** 2, 10 ** 3, 10 ** 4,
      10 ** 5, 10 ** 6, 10 ** 7, 10 ** 8, 10 ** 9,
      10 ** 10, 10 ** 11, 10 ** 12, 10 ** 13, 10 ** 14,
      10 ** 15, 10 ** 16, 10 ** 17, 10 ** 18, 10 ** 19];

   function Count_Digits (Value : Unsigned_64) return Digit_Count is
      Count : Digit_Count := 1;
   begin
      while Count < Decimal_Digits and then Value >= Powers (Count) loop
         pragma Loop_Variant (Increases => Count);
         Count := Count + 1;
      end loop;
      return Count;
   end Count_Digits;

   Digit_Symbols : constant String (1 .. Decimal_Base) := "0123456789";
   subtype Digit is Unsigned_64 range 0 .. Decimal_Base - 1;

   function Last_Digit (Value : Unsigned_64) return Digit is
     (Value mod Decimal_Base);

   function Decimal (Value : Unsigned_64) return String is
      Count : constant Digit_Count := Count_Digits (Value);
   begin
      return
        [for Index in 1 .. Count =>
           Digit_Symbols
             (Natural (Last_Digit (Value / Powers (Count - Index))) + 1)];
   end Decimal;

   Kibi : constant := 2 ** 10;
   Unit_Threshold : constant := 10;

   function Size (Bytes : Unsigned_64) return String is
     (if Bytes >= Unit_Threshold * Kibi ** 4
      then Decimal (Bytes / Kibi ** 4) & "T"
      elsif Bytes >= Unit_Threshold * Kibi ** 3
      then Decimal (Bytes / Kibi ** 3) & "G"
      elsif Bytes >= Unit_Threshold * Kibi ** 2
      then Decimal (Bytes / Kibi ** 2) & "M"
      else Decimal (Bytes / Kibi) & "K");

   function Hex (Value : Unsigned_64; Width : Hex_Width) return String is
     ([for Index in 1 .. Width =>
         Hex_Symbols
           (Natural (Shift_Right (Value, Nibble_Bits * (Width - Index))
                     and (Hex_Base - 1)) + 1)]);
end UFS_Text;
