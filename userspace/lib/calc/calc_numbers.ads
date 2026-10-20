with Interfaces; use Interfaces;
with Calc_Limbs; use Calc_Limbs;
with Calc_Naturals; use Calc_Naturals;

--  The calculator's numbers (docs/calculator.md, "Numbers"): exact
--  rationals of any size, always in lowest terms with a positive
--  denominator, and marked Approximate when an irrational function
--  (sqrt of a non-square, pi, e) or such an operand went into them. An
--  approximate number is still a rational: within 10 ** (-Digits) of the
--  true value where it was made, for the Digits its caller asked for; the
--  mark says that the error is not tracked further (no interval yet).
--
--  Every operation takes the caller's Budget: the most limbs (32 bits each)
--  a numerator or denominator may have, its own or any step's. A step that
--  would go beyond it fails with Too_Large before it allocates; nothing here
--  has a limit of its own. Failures are typed (Status), never exceptions.
--  Every result is new, owned by the caller, who frees it.
package Calc_Numbers with SPARK_Mode is
   type Exactness is (Exact, Approximate);

   --  A quarter of the representation's limbs: a step's operands together
   --  stay in range.
   subtype Size_Budget is Limb_Count range 1 .. MAXIMUM_LIMBS / 4;
   --  Decimal digits of an approximation.
   MAXIMUM_APPROXIMATE_DIGITS : constant := 100_000;
   subtype Approximate_Digits is Positive range 1 .. MAXIMUM_APPROXIMATE_DIGITS;

   --  A literal's exponent (1e-300): beyond these no budget could hold it.
   MAXIMUM_EXPONENT : constant := 2 ** 62;
   subtype Decimal_Exponent is Integer_64 range -MAXIMUM_EXPONENT .. MAXIMUM_EXPONENT;

   type Status is
     (Done,
      --  x / 0, mod (x, 0), 0 ^ -n.
      Division_By_Zero,
      --  A numerator or denominator would exceed the budget.
      Too_Large,
      --  An integer operation (gcd, lcm, factorial) on a fraction.
      Not_An_Integer,
      --  x ^ (p/q) with q > 1: use sqrt.
      Fractional_Exponent,
      --  sqrt or factorial of a negative number.
      Outside_Domain);

   type Number is private
     with Default_Initial_Condition => Is_Reclaimed (Number);

   --  A reclaimed number owns nothing; it reads as exact zero.
   function Is_Reclaimed (X : Number) return Boolean;

   --  Numerator and denominator each have at most Budget limbs.
   function Within (X : Number; Budget : Size_Budget) return Boolean;
   function Kind (X : Number) return Exactness;
   function Is_Zero (X : Number) return Boolean;
   function Is_Negative (X : Number) return Boolean;
   function Is_Integer (X : Number) return Boolean;
   --  Bits of the numerator and of the denominator.
   function Numerator_Bits (X : Number) return Bit_Count;
   function Denominator_Bits (X : Number) return Bit_Count;
   --  Structural equality: the same rational and the same exactness.
   function Same (A, B : Number) return Boolean;

   --  Limbs of the larger of numerator and denominator.
   function Size (X : Number) return Limb_Count;

   procedure Free (X : in out Number)
     with Post => Is_Reclaimed (X);
   --  Into takes From's value; From is left reclaimed.
   procedure Move (From, Into : in out Number)
     with Pre => Is_Reclaimed (Into),
          Post => Size (Into) = Size (From)'Old and then Is_Reclaimed (From);
   procedure Copy (X : Number; Result : in out Number)
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
   --  Copies of the numerator (its magnitude) and denominator (1 for an integer).
   procedure Parts (X : Number; Numerator, Denominator : in out Magnitude)
     with Pre => Is_Zero (Numerator) and then Is_Zero (Denominator),
          Post => not Is_Zero (Denominator) and then Length (Numerator) <= Size (X)
                  and then Length (Denominator) <= Size (X) + 2;

   type Unary_Operation is
     (Negate, Absolute, Floor, Ceiling, Round, Truncate, Factorial, Square_Root,
      Numerator_Of, Denominator_Of);
   type Binary_Operation is
     (Add, Subtract, Multiply, Divide, Power, Modulo, Greatest_Common_Divisor,
      Least_Common_Multiple, Round_To);
   --  Round_To (x, n): x to n decimal places, halves away from zero.
   type Constant_Value is (Pi, Euler);

   procedure Apply
     (Operation : Unary_Operation; X : Number; Budget : Size_Budget; Precision : Approximate_Digits;
      Result : in out Number; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Within (X, Budget),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result));
   procedure Apply
     (Operation : Binary_Operation; A, B : Number; Budget : Size_Budget;
      Result : in out Number; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Within (A, Budget) and then Within (B, Budget),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result));
   --  Within 10 ** (-Precision) of the constant, marked Approximate.
   procedure Make_Constant
     (Which : Constant_Value; Precision : Approximate_Digits; Budget : Size_Budget;
      Result : in out Number; State : out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result));
private
   --  Numerator: the magnitude. Denominator: zero storage means 1 (an
   --  integer owns no denominator); otherwise at least 2.
   type Number is record
      Kind : Exactness := Exact;
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
