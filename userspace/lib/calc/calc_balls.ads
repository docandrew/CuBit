with Calc_Limbs; use Calc_Limbs;
with Calc_Numbers; use Calc_Numbers;

--  Approximate values with a stated error bound (docs/calculator.md,
--  "Approximations"): a ball, an exact rational midpoint and an exact
--  rational radius, standing for every real within the radius of the
--  midpoint. A ball of radius zero is an exact number, and arithmetic on
--  exact balls is Calc_Numbers' exact arithmetic. Every operation here
--  returns a ball that contains the true result whenever its operands
--  contain theirs: the radius accounts for every truncation the
--  computation made, so the digits a display derives from it are
--  guaranteed (Calc_Format shows the bound).
--
--  Elementary functions (exp, ln, log, sin, cos, tan and their inverses,
--  roots, powers, pi, e) are series on exact dyadic rationals, each term's
--  truncation counted. Exact where the value is rational and known:
--  sin 30 degrees is 1/2, ln 1 is 0, log 1000 is 3, 8 ^ (1/3) is 2.
--
--  Proved (level 2): no run-time errors, no leaks, every result within the
--  caller's budget. That the radius bounds the error is the design above,
--  checked against an independent high-precision reference by the
--  differential tests, not proved.
package Calc_Balls with SPARK_Mode is
   type Angle_Unit is (Radians, Degrees, Gradians);

   --  Decimal digits an approximation is computed to (its radius is about
   --  10 ** (-Digits) of its size, more after cancellation).
   MAXIMUM_DIGITS : constant := 10_000;
   subtype Precision_Digits is Positive range 1 .. MAXIMUM_DIGITS;

   type Status is
     (Done, Division_By_Zero, Too_Large, Not_An_Integer, Outside_Domain,
      --  The ball straddles a point where the answer changes (floor of
      --  2 - 1e-60 +- 1e-50, the sign of a divisor): no answer is
      --  guaranteed at this precision.
      Indeterminate);

   type Ball is private
     with Default_Initial_Condition => Is_Reclaimed (Ball);
   function Is_Reclaimed (X : Ball) return Boolean;
   --  Radius zero.
   function Is_Exact (X : Ball) return Boolean;
   function Within (X : Ball; Budget : Size_Budget) return Boolean;
   --  Exact and an integer.
   function Is_Integer (X : Ball) return Boolean;
   function Size (X : Ball) return Limb_Count;

   procedure Free (X : in out Ball)
     with Post => Is_Reclaimed (X);
   procedure Move (From, Into : in out Ball)
     with Pre => Is_Reclaimed (Into),
          Post => Size (Into) = Size (From)'Old and then Is_Reclaimed (From);
   procedure Copy (X : Ball; Result : in out Ball)
     with Pre => Is_Reclaimed (Result), Post => Size (Result) = Size (X);
   --  The exact ball of X (X is taken).
   procedure From_Exact (X : in out Number; Result : in out Ball)
     with Pre => Is_Reclaimed (Result),
          Post => Size (Result) = Calc_Numbers.Size (X)'Old and then Calc_Numbers.Is_Reclaimed (X)
                  and then Is_Exact (Result);
   --  Copies of the midpoint and the radius.
   procedure Midpoint (X : Ball; Result : in out Number)
     with Pre => Calc_Numbers.Is_Reclaimed (Result), Post => Calc_Numbers.Size (Result) <= Size (X);
   procedure Radius (X : Ball; Result : in out Number)
     with Pre => Calc_Numbers.Is_Reclaimed (Result), Post => Calc_Numbers.Size (Result) <= Size (X);

   type Arithmetic is (Add, Subtract, Multiply, Divide, Power);
   type Rounding is (Floor, Ceiling, Round, Truncate);
   type Elementary is
     (Square_Root, Exponential, Natural_Log, Common_Log, Sine, Cosine, Tangent, Arcsine, Arccosine,
      Arctangent);
   type Constant_Value is (Pi, Euler);

   procedure Apply
     (Operation : Arithmetic; A, B : Ball; Digits_Wanted : Precision_Digits; Budget : Size_Budget;
      Result : in out Ball; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Within (A, Budget) and then Within (B, Budget),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result));
   procedure Negate (X : Ball; Result : in out Ball)
     with Pre => Is_Reclaimed (Result), Post => Size (Result) = Size (X);
   procedure Absolute (X : Ball; Digits_Wanted : Precision_Digits; Budget : Size_Budget;
                       Result : in out Ball; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Within (X, Budget),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result));
   --  To an integer: exact, or Indeterminate when the ball straddles one.
   procedure Round_To_Integer
     (Mode : Rounding; X : Ball; Budget : Size_Budget; Result : in out Ball; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Within (X, Budget),
          Post => (if State = Done then Within (Result, Budget) and then Is_Exact (Result)
                   else Is_Reclaimed (Result));
   --  The Degree-th root (Degree a positive integer).
   procedure Root
     (X, Degree : Ball; Digits_Wanted : Precision_Digits; Budget : Size_Budget; Result : in out Ball; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Within (X, Budget) and then Within (Degree, Budget),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result));
   procedure Evaluate
     (Function_Kind : Elementary; X : Ball; Unit : Angle_Unit; Digits_Wanted : Precision_Digits;
      Budget : Size_Budget; Result : in out Ball; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Within (X, Budget),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result));
   procedure Make_Constant
     (Which : Constant_Value; Digits_Wanted : Precision_Digits; Budget : Size_Budget;
      Result : in out Ball; State : out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result));
private
   type Ball is record
      Mid, Rad : Number;
   end record;
   function Is_Reclaimed (X : Ball) return Boolean is
     (Calc_Numbers.Is_Reclaimed (X.Mid) and then Calc_Numbers.Is_Reclaimed (X.Rad));
   function Is_Exact (X : Ball) return Boolean is (Calc_Numbers.Is_Zero (X.Rad));
   function Is_Integer (X : Ball) return Boolean is (Is_Exact (X) and then Calc_Numbers.Is_Integer (X.Mid));
   function Size (X : Ball) return Limb_Count is
     (Natural'Max (Calc_Numbers.Size (X.Mid), Calc_Numbers.Size (X.Rad)));
   function Within (X : Ball; Budget : Size_Budget) return Boolean is (Size (X) <= Budget);
end Calc_Balls;
