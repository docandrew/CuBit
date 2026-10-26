with Interfaces; use Interfaces;
with Ada.Text_IO;

package body Checks is
   Passed : Natural := 0;
   Failed : Natural := 0;

   Digits_Per_Byte : constant := 2;
   Nibble_Bits     : constant := 16;

   function Nibble (C : Character) return Byte is
     (case C is
         when '0' .. '9' => Character'Pos (C) - Character'Pos ('0'),
         when 'a' .. 'f' => Character'Pos (C) - Character'Pos ('a') + 10,
         when 'A' .. 'F' => Character'Pos (C) - Character'Pos ('A') + 10,
         when others     => raise Constraint_Error with "bad hex digit");

   function Hex (S : String) return Byte_Seq is
      Clean : String (1 .. S'Length);
      Last  : Natural := 0;
   begin
      for C of S loop
         if C not in ' ' | ASCII.LF | ASCII.CR | ASCII.HT then
            Last := Last + 1;
            Clean (Last) := C;
         end if;
      end loop;
      if Last mod Digits_Per_Byte /= 0 then
         raise Constraint_Error with "odd hex length";
      end if;
      return R : Byte_Seq (0 .. N32 (Last / Digits_Per_Byte) - 1) do
         for I in R'Range loop
            R (I) := Nibble (Clean (Natural (I) * Digits_Per_Byte + 1)) * Nibble_Bits
              + Nibble (Clean (Natural (I) * Digits_Per_Byte + 2));
         end loop;
      end return;
   end Hex;

   function Text (S : String) return Byte_Seq is
   begin
      return R : Byte_Seq (0 .. N32 (S'Length) - 1) do
         for I in R'Range loop
            R (I) := Character'Pos (S (S'First + Natural (I)));
         end loop;
      end return;
   end Text;

   function Repeat (B : Byte; N : Natural) return Byte_Seq is
     (Byte_Seq'(0 .. N32 (N) - 1 => B));

   function To_Hex (B : Byte_Seq) return String is
      Digit : constant String := "0123456789abcdef";
      R : String (1 .. B'Length * Digits_Per_Byte);
      P : Positive := 1;
   begin
      for X of B loop
         R (P) := Digit (Natural (X / Nibble_Bits) + 1);
         R (P + 1) := Digit (Natural (X mod Nibble_Bits) + 1);
         P := P + Digits_Per_Byte;
      end loop;
      return R;
   end To_Hex;

   procedure Check (Name : String; Got, Expected : Byte_Seq) is
   begin
      if Got'Length = Expected'Length
        and then (for all I in 0 .. Got'Length - 1 =>
                    Got (Got'First + N32 (I)) = Expected (Expected'First + N32 (I)))
      then
         Passed := Passed + 1;
      else
         Failed := Failed + 1;
         Ada.Text_IO.Put_Line ("FAIL " & Name);
         Ada.Text_IO.Put_Line ("  got      " & To_Hex (Got));
         Ada.Text_IO.Put_Line ("  expected " & To_Hex (Expected));
      end if;
   end Check;

   procedure Check (Name : String; Condition : Boolean) is
   begin
      if Condition then
         Passed := Passed + 1;
      else
         Failed := Failed + 1;
         Ada.Text_IO.Put_Line ("FAIL " & Name);
      end if;
   end Check;

   function Report return Boolean is
   begin
      Ada.Text_IO.Put_Line
        ("wifi-crypto:" & Passed'Image & " passed," & Failed'Image & " failed");
      return Failed = 0;
   end Report;
end Checks;
