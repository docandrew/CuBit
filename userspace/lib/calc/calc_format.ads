with Calc_Numbers; use Calc_Numbers;

--  How the calculator writes a number (docs/calculator.md, "Display").
--  Exact whenever it can be: an integer in full; a fraction as -5/6; a
--  decimal that terminates within the places shown (0.125), repeats
--  within them (0.8(3), 0.(142857)), or has few enough significant digits
--  for scientific notation (1.5e-30). Otherwise rounded (halves away from
--  zero) to the places shown, and the form says so.
--
--  Proved (level 2): no run-time errors; the text fits Into (the bounds
--  below are sufficient, and a write past them is refused, not made).
package Calc_Format with SPARK_Mode is
   MAXIMUM_PLACES : constant := 10_000;
   --  Decimal places after the point (significant digits in scientific form).
   subtype Places_Count is Positive range 1 .. MAXIMUM_PLACES;

   type Decimal_Form is
     (Integer_Form, Terminating, Repeating, Scientific_Exact, Rounded, Scientific_Rounded);
   subtype Exact_Form is Decimal_Form range Integer_Form .. Scientific_Exact;

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
end Calc_Format;
