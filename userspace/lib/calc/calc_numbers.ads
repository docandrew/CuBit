with Interfaces; use Interfaces;
with Calc_Limbs; use Calc_Limbs;
with Calc_Naturals; use Calc_Naturals;

--  The calculator's exact numbers (docs/calculator.md, "Numbers"):
--  rationals of any size, always in lowest terms with a positive
--  denominator. An operation whose exact result is irrational (sqrt of a
--  non-square, a fractional power without an exact root) says Inexact;
--  Calc_Balls then approximates it with a stated error bound.
--
--  Every operation takes the caller's Budget: the most limbs (32 bits each)
--  a numerator or denominator may have, its own or any step's. A step that
--  would go beyond it fails with Too_Large before it allocates; nothing here
--  has a limit of its own. Failures are typed (Status), never exceptions.
--  Every result is new, owned by the caller, who frees it.
package Calc_Numbers with SPARK_Mode is
   --  A quarter of the representation's limbs: a step's operands together
   --  stay in range.
   subtype Size_Budget is Limb_Count range 1 .. MAXIMUM_LIMBS / 4;
   --  A literal's exponent (1e-300): beyond these no budget could hold it.
   MAXIMUM_EXPONENT : constant := 2 ** 62;
   subtype Decimal_Exponent is Integer_64 range -MAXIMUM_EXPONENT .. MAXIMUM_EXPONENT;

   type Status is
     (Done,
      --  x / 0, mod (x, 0), 0 ^ -n.
      Division_By_Zero,
      --  A numerator or denominator would exceed the budget.
      Too_Large,
      --  An integer operation (gcd, lcm, factorial, bitwise) on a fraction.
      Not_An_Integer,
      --  sqrt or factorial of a negative number, a negative shift.
      Outside_Domain,
      --  The exact result is irrational (sqrt (2), 2 ^ (1/3)).
      Inexact);

   type Number is private
     with Default_Initial_Condition => Is_Reclaimed (Number);

   --  A reclaimed number owns nothing; it reads as exact zero.
   function Is_Reclaimed (X : Number) return Boolean;

   --  Numerator and denominator each have at most Budget limbs.
   function Within (X : Number; Budget : Size_Budget) return Boolean;
   function Is_Zero (X : Number) return Boolean
     with Post => (if Is_Reclaimed (X) then Is_Zero'Result);
   function Is_Negative (X : Number) return Boolean;
   function Is_Integer (X : Number) return Boolean;
   --  Bits of the numerator and of the denominator.
   function Numerator_Bits (X : Number) return Bit_Count;
   function Denominator_Bits (X : Number) return Bit_Count;
   --  Structural equality: the same rational.
   function Same (A, B : Number) return Boolean;
   --  The value when it is an integer that fits a Natural.
   function Fits_Natural (X : Number) return Boolean;
   function To_Natural (X : Number) return Natural
     with Pre => Fits_Natural (X);

   --  Limbs of the larger of numerator and denominator.
   function Size (X : Number) return Limb_Count;
   procedure Compare (A, B : Number; Order : out Ordering)
     with Pre => Size (A) <= Size_Budget'Last and then Size (B) <= Size_Budget'Last;

   procedure Free (X : in out Number)
     with Post => Is_Reclaimed (X);
   --  Into takes From's value; From is left reclaimed.
   procedure Move (From, Into : in out Number)
     with Pre => Is_Reclaimed (Into),
          Post => Size (Into) = Size (From)'Old and then Is_Reclaimed (From);
   procedure Copy (X : Number; Result : in out Number)
     with Pre => Is_Reclaimed (Result),
          Post => Size (Result) = Size (X);
   --  Result := -X.
   procedure Copy_Negated (X : Number; Result : in out Number)
     with Pre => Is_Reclaimed (Result),
          Post => Size (Result) = Size (X);
   procedure From_Integer (Value : Integer_64; Result : in out Number)
     with Pre => Is_Reclaimed (Result),
          Post => Size (Result) <= 2;
   --  The decimal literal Whole.Fraction * 10 ** Exponent, exact.
   procedure From_Decimal
     (Whole, Fraction : String; Exponent : Decimal_Exponent; Budget : Size_Budget;
      Result : in out Number; State : out Status)
     with Pre => Is_Reclaimed (Result)
                 and then Whole'Length <= MAXIMUM_LIMBS / 4 and then Fraction'Length <= MAXIMUM_LIMBS / 4
                 and then (for all C of Whole => C in '0' .. '9')
                 and then (for all C of Fraction => C in '0' .. '9'),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result));
   --  The number (-1 if Negative) * Num / Den in lowest terms; Num and Den
   --  are taken. Division_By_Zero for a zero Den.
   procedure From_Parts
     (Negative : Boolean; Num, Den : in out Magnitude; Budget : Size_Budget;
      Result : in out Number; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Length (Num) < MAXIMUM_LIMBS and then Length (Den) < MAXIMUM_LIMBS,
          Post => Calc_Naturals.Is_Zero (Num) and then Calc_Naturals.Is_Zero (Den)
                  and then (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result));
   --  Copies of the numerator (its magnitude) and denominator (1 for an integer).
   procedure Parts (X : Number; Numerator, Denominator : in out Magnitude)
     with Pre => Is_Zero (Numerator) and then Is_Zero (Denominator),
          Post => not Is_Zero (Denominator) and then Length (Numerator) <= Size (X)
                  and then Length (Denominator) <= Size (X) + 2;

   --  2 ** Exponent, exact (Exponent may be negative).
   procedure Power_Of_Two (Exponent : Integer; Budget : Size_Budget; Result : in out Number; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Exponent /= Integer'First,
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result));

   --  Bit_Not and the bitwise binary operations treat integers as infinite
   --  two's complement (-1 is all ones), as Python's int does.
   type Unary_Operation is
     (Negate, Absolute, Floor, Ceiling, Round, Truncate, Factorial, Square_Root,
      Numerator_Of, Denominator_Of, Bit_Not);
   type Binary_Operation is
     (Add, Subtract, Multiply, Divide, Power, Modulo, Greatest_Common_Divisor,
      Least_Common_Multiple, Round_To, Root, Bit_And, Bit_Or, Bit_Xor, Shift_Left, Shift_Right,
      Integer_Quotient);
   --  Round_To (x, n): x to n decimal places, halves away from zero.
   --  Root (x, n): the exact n-th root, or Inexact.
   --  Shift_Right (x, n): floor (x / 2 ** n). Integer_Quotient: a / b
   --  truncated toward zero.

   procedure Apply
     (Operation : Unary_Operation; X : Number; Budget : Size_Budget;
      Result : in out Number; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Within (X, Budget),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result));
   procedure Apply
     (Operation : Binary_Operation; A, B : Number; Budget : Size_Budget;
      Result : in out Number; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Within (A, Budget) and then Within (B, Budget),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result));
private
   --  Numerator: the magnitude. Denominator: zero storage means 1 (an
   --  integer owns no denominator); otherwise at least 2.
   type Number is record
      Negative : Boolean := False;
      Numerator : Magnitude;
      Denominator : Magnitude;
   end record;
   function Is_Reclaimed (X : Number) return Boolean is
     (Calc_Naturals.Is_Zero (X.Numerator) and then Calc_Naturals.Is_Zero (X.Denominator));
   function Size (X : Number) return Limb_Count is
     (Natural'Max (Length (X.Numerator), Length (X.Denominator)));
   function Within (X : Number; Budget : Size_Budget) return Boolean is (Size (X) <= Budget);
end Calc_Numbers;
