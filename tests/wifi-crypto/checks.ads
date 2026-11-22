--  Test helpers for the Wi-Fi crypto known-answer tests.
with SPARKNaCl; use SPARKNaCl;

package Checks is
   --  Hex text (spaces and newlines ignored) to bytes, indexed from 0.
   function Hex (S : String) return Byte_Seq;

   --  ASCII text to bytes, indexed from 0.
   function Text (S : String) return Byte_Seq;

   --  Count of N copies of B, indexed from 0.
   function Repeat (B : Byte; N : Natural) return Byte_Seq;

   procedure Check (Name : String; Got, Expected : Byte_Seq);
   procedure Check (Name : String; Condition : Boolean);

   --  Prints the totals; True when every check passed.
   function Report return Boolean;
end Checks;
