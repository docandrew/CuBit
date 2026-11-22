pragma Ada_2022;
with Interfaces;
package BCD_Binary_Lemmas with SPARK_Mode, Pure is
   subtype Word is Interfaces.Unsigned_64;
   use type Word;
   Nibble_Bits : constant := 4;
   Nibble_Mask : constant Word := 15;
   subtype Digit is Word range 0 .. Nibble_Mask;
   subtype Digit_Index is Natural range 0 .. 15;
   function At_Digit (Value : Word; Index : Digit_Index) return Digit is
     (Interfaces.Shift_Right (Value, Nibble_Bits * Index) and Nibble_Mask) with Ghost;
   procedure Insert_Empty_Digit (Value : Word; Number : Digit; Index : Digit_Index) with
     Ghost,
     Pre => At_Digit (Value, Index) = 0,
     Post => (for all Position in Digit_Index =>
       At_Digit (Value or Interfaces.Shift_Left (Number, Nibble_Bits * Index), Position) =
         (if Position = Index then Number else At_Digit (Value, Position)));
   subtype Narrow_Digit_Index is Digit_Index range 0 .. 7;
   Narrow_Maximum : constant Word := 16#FFFF_FFFF#;
   procedure Preserve_Narrow (Value : Word; Number : Digit; Index : Narrow_Digit_Index) with
     Ghost, Pre => Value <= Narrow_Maximum,
     Post => (Value or Interfaces.Shift_Left (Number, Nibble_Bits * Index)) <= Narrow_Maximum;
end BCD_Binary_Lemmas;
