with Calc_Numbers; use Calc_Numbers;

--  Machine words for programmer mode (docs/calculator.md, "Programmer"):
--  an integer kept in a word of 8, 16, 32 or 64 bits, read signed (two's
--  complement) or unsigned, or unbounded. Wrapping is explicit and typed:
--  Wrap says whether the value changed, and the app and calc.* show it.
--
--  Proved (level 2): no run-time errors, every result within the budget.
package Calc_Words with SPARK_Mode is
   type Word_Size is (Unbounded, Bits_8, Bits_16, Bits_32, Bits_64);
   function Width (Word : Word_Size) return Natural is
     (case Word is
         when Unbounded => 0, when Bits_8 => 8, when Bits_16 => 16, when Bits_32 => 32, when Bits_64 => 64);

   --  X (an integer) in the word: X mod 2 ** width, then the upper half
   --  negative when Signed. Wrapped: the value changed.
   procedure Wrap
     (X : Number; Word : Word_Size; Signed : Boolean; Budget : Size_Budget;
      Result : in out Number; Wrapped : out Boolean; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Within (X, Budget),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result));

   --  The word's bits of X as an unsigned number (X mod 2 ** width): what
   --  hexadecimal, octal and binary show.
   procedure Pattern
     (X : Number; Word : Word_Size; Budget : Size_Budget; Result : in out Number; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Within (X, Budget) and then Word /= Unbounded,
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result));

   --  X's bits rotated by Count (an integer, taken mod the width) within
   --  the word, then read signed or unsigned.
   procedure Rotate
     (X, Count : Number; Left : Boolean; Word : Word_Size; Signed : Boolean; Budget : Size_Budget;
      Result : in out Number; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Within (X, Budget) and then Within (Count, Budget)
                 and then Word /= Unbounded,
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result));
end Calc_Words;
