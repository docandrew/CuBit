with Interfaces;

--  Lowercase hexadecimal names of identities: Storehouse entry names and
--  the identities pinned in CCL declarations. Exactly one spelling per
--  identity, so a name compares equal exactly when the identities do.
package Storehouse.Hex with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_8;

   Text_Length : constant := 2 * Digest_Bytes;
   subtype Text is String (1 .. Text_Length);

   subtype Nibble is Byte range 0 .. 15;
   Nibble_Base : constant := 16;
   Decimal_Digits : constant := 10;

   function Is_Digit (C : Character) return Boolean is
     (C in '0' .. '9' | 'a' .. 'f');

   function Digit (N : Nibble) return Character is
     (if N < Decimal_Digits then Character'Val (Character'Pos ('0') + Natural (N))
      else Character'Val (Character'Pos ('a') + Natural (N) - Decimal_Digits))
   with Post => Is_Digit (Digit'Result);

   function Value (C : Character) return Nibble is
     (if C in '0' .. '9' then Nibble (Character'Pos (C) - Character'Pos ('0'))
      else Nibble (Character'Pos (C) - Character'Pos ('a') + Decimal_Digits))
   with Pre => Is_Digit (C),
        Post => Digit (Value'Result) = C;

   function High (B : Byte) return Nibble is (B / Nibble_Base);
   function Low (B : Byte) return Nibble is (B mod Nibble_Base);

   --  Character J of the name of D: two digits per byte, high one first.
   function Image (D : Digest) return Text is
     ([for J in Text'Range =>
         (if J mod 2 = 1 then Digit (High (D ((J + 1) / 2))) else Digit (Low (D (J / 2))))]);

   function Is_Name (S : String) return Boolean is
     (S'Length = Text_Length and then (for all C of S => Is_Digit (C)));

   function Parse (S : Text) return Digest
   with Pre => Is_Name (S),
        Post => Image (Parse'Result) = S;

end Storehouse.Hex;
