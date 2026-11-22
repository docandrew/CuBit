with Calc_Balls;
with Calc_Naturals;
with Calc_Numbers; use Calc_Numbers;
with Calc_Words;

--  How the calculator writes a number (docs/calculator.md, "Display").
--  Exact whenever it can be: an integer in full; a fraction as -5/6; a
--  decimal that terminates within the places shown (0.125), repeats
--  within them (0.8(3), 0.(142857)), or has few enough significant digits
--  for scientific notation (1.5e-30). Otherwise rounded (halves away from
--  zero) to the places shown, and the form says so.
--
--  An approximation (a ball) shows only digits its bound guarantees, up to
--  the places as significant digits, and the bound itself: 1.41421356 with
--  bound 5e-9 means the true value is within 5e-9 of what is shown.
--
--  Proved (level 2): no run-time errors; the text fits Into (the bounds
--  below are sufficient, and a write past them is refused, not made).
package Calc_Format with SPARK_Mode is
   MAXIMUM_PLACES : constant := 10_000;
   --  Decimal places after the point (significant digits in scientific form).
   subtype Places_Count is Positive range 1 .. MAXIMUM_PLACES;

   type Decimal_Form is
     (Integer_Form, Terminating, Repeating, Scientific_Exact, Rounded, Scientific_Rounded,
      Approximate_Decimal, Approximate_Scientific);
   subtype Exact_Form is Decimal_Form range Integer_Form .. Scientific_Exact;
   subtype Approximate_Form is Decimal_Form range Approximate_Decimal .. Approximate_Scientific;

   --  Room enough for any text of X.
   function Fraction_Bound (X : Number) return Positive
     with Pre => Size (X) <= Size_Budget'Last;
   function Decimal_Bound (X : Number; Places : Places_Count) return Positive
     with Pre => Size (X) <= Size_Budget'Last;

   --  -5/6, or 42 for an integer.
   procedure Fraction_Text (X : Number; Into : out String; Last : out Natural)
     with Pre => Size (X) <= Size_Budget'Last and then Into'First = 1
                 and then Into'Length >= Fraction_Bound (X),
          Post => Last <= Into'Last;

   procedure Decimal_Text
     (X : Number; Places : Places_Count; Into : out String; Last : out Natural; Form : out Decimal_Form)
     with Pre => Size (X) <= Size_Budget'Last and then Into'First = 1
                 and then Into'Length >= Decimal_Bound (X, Places),
          Post => Last <= Into'Last;

   --  The decimal digits of X's numerator and denominator.
   function Digit_Bound (X : Number) return Positive
     with Pre => Size (X) <= Size_Budget'Last,
          Post => Digit_Bound'Result <= Natural'Last / 4;
   procedure Numerator_Text (X : Number; Into : out String; Last : out Natural)
     with Pre => Size (X) <= Size_Budget'Last and then Into'First = 1 and then Into'Length >= Digit_Bound (X),
          Post => Last <= Into'Last;
   procedure Denominator_Text (X : Number; Into : out String; Last : out Natural)
     with Pre => Size (X) <= Size_Budget'Last and then Into'First = 1 and then Into'Length >= Digit_Bound (X),
          Post => Last <= Into'Last;

   --  A ball: an exact one as Decimal_Text; an approximate one as its
   --  guaranteed digits (Into) and the bound on their error (Bound_Into,
   --  "3e-21"; empty for an exact ball).
   function Ball_Bound (X : Calc_Balls.Ball; Places : Places_Count) return Positive
     with Pre => Calc_Balls.Size (X) <= Size_Budget'Last;
   BOUND_TEXT : constant := 32;
   procedure Ball_Text
     (X : Calc_Balls.Ball; Places : Places_Count; Into : out String; Last : out Natural;
      Bound_Into : out String; Bound_Last : out Natural; Form : out Decimal_Form)
     with Pre => Calc_Balls.Size (X) <= Size_Budget'Last and then Into'First = 1
                 and then Into'Length >= Ball_Bound (X, Places)
                 and then Bound_Into'First = 1 and then Bound_Into'Length >= BOUND_TEXT,
          Post => Last <= Into'Last and then Bound_Last <= Bound_Into'Last;

   --  An integer in base 2, 8, 10 or 16 (upper case digits, no prefix). In
   --  a word, 2, 8 and 16 show the word's bits (two's complement), padded
   --  to the word's width; unbounded, a sign and the magnitude.
   subtype Display_Base is Calc_Naturals.Digit_Base
     with Static_Predicate => Display_Base in 2 | 8 | 10 | 16;
   function Base_Bound (X : Number) return Positive
     with Pre => Size (X) <= Size_Budget'Last,
          Post => Base_Bound'Result <= Natural'Last / 2;
   procedure Base_Text
     (X : Number; Base : Display_Base; Word : Calc_Words.Word_Size; Into : out String; Last : out Natural)
     with Pre => Size (X) <= Size_Budget'Last and then Is_Integer (X) and then Into'First = 1
                 and then Into'Length >= Base_Bound (X),
          Post => Last <= Into'Last;
end Calc_Format;
