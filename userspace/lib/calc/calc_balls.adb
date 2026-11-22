with Interfaces; use Interfaces;
with Calc_Naturals; use Calc_Naturals;

package body Calc_Balls with SPARK_Mode is
   pragma Warnings (GNATprove, Off, "statement has no effect", Reason => "frees end ownership");
   pragma Warnings (GNATprove, Off, "is set by ""Free"" but not used", Reason => "frees end ownership");
   pragma Warnings (GNATprove, Off, "is set by ""Move"" but not used", Reason => "moves end ownership");

   subtype Number is Calc_Numbers.Number;
   subtype Magnitude is Calc_Naturals.Magnitude;
   package N renames Calc_Numbers;

   --  log2 10 < 10/3: bits for a count of decimal digits.
   BITS_PER_DIGIT_NUMERATOR : constant := 10;
   BITS_PER_DIGIT_DENOMINATOR : constant := 3;
   --  Bits a series carries beyond those asked for, so its counted
   --  truncation errors stay below the digits shown.
   GUARD_BITS : constant := 64;
   --  Bits a midpoint keeps below its radius when a ball is trimmed.
   TRIM_BITS : constant := 32;
   --  Machin: pi = 16 arctan (1/5) - 4 arctan (1/239).
   MACHIN_FIRST : constant := 5;
   MACHIN_SECOND : constant := 239;
   MACHIN_FIRST_WEIGHT : constant := 16;
   MACHIN_SECOND_WEIGHT : constant := 4;
   --  Truncation errors counted per series term, and for the tail and the
   --  first term together (in units of the last bit; see each series).
   ULPS_PER_TERM : constant := 3;
   ULPS_FOR_TAIL : constant := 8;
   --  Degrees and gradians in a turn, and in a right angle.
   DEGREES_PER_TURN : constant := 360;
   DEGREES_PER_HALF_TURN : constant := 180;
   GRADIANS_PER_HALF_TURN : constant := 200;
   --  Gradians to degrees: 9/10.
   GRADIAN_NUMERATOR : constant := 9;
   GRADIAN_DENOMINATOR : constant := 10;
   DECIMAL_BASE : constant := 10;

   --  The most fractional bits a series carries: room for every count
   --  below (terms, truncation units) without overflow; a request beyond
   --  it is Too_Large.
   MAXIMUM_SERIES_BITS : constant := Natural'Last / 8;
   subtype Series_Bits is Natural range 0 .. MAXIMUM_SERIES_BITS;
   --  Truncation units a series counts: ULPS_PER_TERM per term, at most
   --  Bits + 2 terms, and one more.
   subtype Ulps_Count is Natural range 0 .. ULPS_PER_TERM * (MAXIMUM_SERIES_BITS + 3) + 1;
   --  With the tail's.
   subtype Ulps_Total is Natural range 0 .. Ulps_Count'Last + ULPS_FOR_TAIL;

   --  Bits of the series for Digits_Wanted decimal digits.
   MAXIMUM_WORK_BITS : constant := MAXIMUM_DIGITS * BITS_PER_DIGIT_NUMERATOR / BITS_PER_DIGIT_DENOMINATOR + GUARD_BITS;
   function Work_Bits (Digits_Wanted : Precision_Digits) return Series_Bits is
     (Digits_Wanted * BITS_PER_DIGIT_NUMERATOR / BITS_PER_DIGIT_DENOMINATOR + GUARD_BITS)
     with Post => Work_Bits'Result <= MAXIMUM_WORK_BITS;
   --  The most bits an argument's size may add to a series (the reduction
   --  of a large angle by pi).
   EXTRA_ROOM : constant := 8;
   MAXIMUM_ARGUMENT_BITS : constant := MAXIMUM_SERIES_BITS - MAXIMUM_WORK_BITS - EXTRA_ROOM;

   function Map (State : N.Status) return Status is
     (case State is
         when N.Done => Done, when N.Division_By_Zero => Division_By_Zero,
         when N.Too_Large => Too_Large, when N.Not_An_Integer => Not_An_Integer,
         when N.Outside_Domain => Outside_Domain, when N.Inexact => Indeterminate);

   ---------------------------------------------------------------------------
   --  Exact steps that run only while State is Done (a failed step leaves
   --  its result reclaimed and every later step a no-op).

   procedure Step
     (Operation : N.Binary_Operation; A, B : Number; Budget : Size_Budget;
      Result : in out Number; State : in out Status)
     with Pre => N.Is_Reclaimed (Result),
          Post => (if State = Done then N.Within (Result, Budget) else N.Is_Reclaimed (Result))
   is
      Outcome : N.Status;
   begin
      if State = Done and then not (N.Within (A, Budget) and then N.Within (B, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      if State = Done then
         N.Apply (Operation, A, B, Budget, Result, Outcome);
         State := Map (Outcome);
         if State /= Done then
            N.Free (Result);
         end if;
      end if;
   end Step;

   procedure Step
     (Operation : N.Unary_Operation; X : Number; Budget : Size_Budget;
      Result : in out Number; State : in out Status)
     with Pre => N.Is_Reclaimed (Result),
          Post => (if State = Done then N.Within (Result, Budget) else N.Is_Reclaimed (Result))
   is
      Outcome : N.Status;
   begin
      if State = Done and then not (N.Within (X, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      if State = Done then
         N.Apply (Operation, X, Budget, Result, Outcome);
         State := Map (Outcome);
         if State /= Done then
            N.Free (Result);
         end if;
      end if;
   end Step;

   procedure Integer_Number
     (Value : Integer_64; Budget : Size_Budget; Result : in out Number; State : in out Status)
     with Pre => N.Is_Reclaimed (Result),
          Post => (if State = Done then N.Within (Result, Budget) else N.Is_Reclaimed (Result))
   is
   begin
      if State = Done then
         N.From_Integer (Value, Result);
         if N.Size (Result) > Budget then
            N.Free (Result);  --  never: a budget holds two limbs... or more
            State := Too_Large;
         end if;
      end if;
   end Integer_Number;

   procedure Two_To
     (Exponent : Integer; Budget : Size_Budget; Result : in out Number; State : in out Status)
     with Pre => N.Is_Reclaimed (Result) and then Exponent /= Integer'First,
          Post => (if State = Done then N.Within (Result, Budget) else N.Is_Reclaimed (Result))
   is
      Outcome : N.Status;
   begin
      if State = Done then
         N.Power_Of_Two (Exponent, Budget, Result, Outcome);
         State := Map (Outcome);
      end if;
   end Two_To;

   --  X to Bits fractional bits, toward zero: within one unit of the last
   --  bit, and a term that shrinks reaches zero (floor would keep -1 unit).
   procedure Truncate
     (X : Number; Bits : Natural; Budget : Size_Budget; Result : in out Number; State : in out Status)
     with Pre => N.Is_Reclaimed (Result),
          Post => (if State = Done then N.Within (Result, Budget) else N.Is_Reclaimed (Result))
   is
      Scale, Scaled, Whole : Number;
   begin
      if State = Done and then not (N.Within (X, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      Two_To (Bits, Budget, Scale, State);
      Step (N.Multiply, X, Scale, Budget, Scaled, State);
      Step (N.Truncate, Scaled, Budget, Whole, State);
      Step (N.Divide, Whole, Scale, Budget, Result, State);
      N.Free (Scale);
      N.Free (Scaled);
      N.Free (Whole);
   end Truncate;

   --  Ulps units of the last of Bits fractional bits: Ulps * 2 ** (-Bits).
   procedure Ulps_Of
     (Ulps : Ulps_Total; Bits : Natural; Budget : Size_Budget; Result : in out Number; State : in out Status)
     with Pre => N.Is_Reclaimed (Result),
          Post => (if State = Done then N.Within (Result, Budget) else N.Is_Reclaimed (Result))
   is
      Count, Unit : Number;
   begin
      Integer_Number (Integer_64 (Ulps), Budget, Count, State);
      Two_To (-Bits, Budget, Unit, State);
      Step (N.Multiply, Count, Unit, Budget, Result, State);
      N.Free (Count);
      N.Free (Unit);
   end Ulps_Of;

   --  log2 |X| within one: bits of the numerator less bits of the denominator.
   function Scale_Of (X : Number) return Integer is
     (Integer (N.Numerator_Bits (X)) - Integer (N.Denominator_Bits (X)));

   ---------------------------------------------------------------------------
   --  Balls.

   procedure Free (X : in out Ball) is
   begin
      N.Free (X.Mid);
      N.Free (X.Rad);
   end Free;

   procedure Move (From, Into : in out Ball) is
   begin
      N.Move (From.Mid, Into.Mid);
      N.Move (From.Rad, Into.Rad);
   end Move;

   procedure Copy (X : Ball; Result : in out Ball) is
   begin
      N.Copy (X.Mid, Result.Mid);
      N.Copy (X.Rad, Result.Rad);
   end Copy;

   procedure From_Exact (X : in out Number; Result : in out Ball) is
   begin
      N.Move (X, Result.Mid);
   end From_Exact;

   procedure Midpoint (X : Ball; Result : in out Number) is
   begin
      N.Copy (X.Mid, Result);
   end Midpoint;

   procedure Radius (X : Ball; Result : in out Number) is
   begin
      N.Copy (X.Rad, Result);
   end Radius;

   --  A ball from a midpoint and a radius (both taken; the radius may be
   --  zero). On failure both are freed and Result stays reclaimed.
   procedure Make_Ball
     (Mid, Rad : in out Number; State : Status; Result : in out Ball)
     with Pre => Is_Reclaimed (Result),
          Post => Size (Result) <= Natural'Max (N.Size (Mid)'Old, N.Size (Rad)'Old)
                  and then N.Is_Reclaimed (Mid) and then N.Is_Reclaimed (Rad)
                  and then (if State /= Done then Is_Reclaimed (Result))
   is
   begin
      if State = Done then
         N.Move (Mid, Result.Mid);
         N.Move (Rad, Result.Rad);
      else
         N.Free (Mid);
         N.Free (Rad);
      end if;
   end Make_Ball;

   --  Keep a ball's midpoint short: round it to TRIM_BITS bits below its
   --  radius, and the radius up to TRIM_BITS significant bits; the
   --  rounding goes into the radius. An exact ball is unchanged.
   procedure Trim (X : in out Ball; Budget : Size_Budget; State : in out Status)
     with Post => (if State = Done then Within (X, Budget) else Is_Reclaimed (X))
   is
      Exponent : Integer;
      Unit, Half, Scaled, Shifted, Whole, Mid, Error, Widened, Rad_Scaled, Rad_Whole, Rad : Number;
   begin
      if State = Done and then not (Within (X, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      if State /= Done then
         Free (X);
         return;
      end if;
      if Is_Exact (X) then
         return;
      end if;
      Exponent := Scale_Of (X.Rad) - TRIM_BITS;
      if Exponent = Integer'First then
         return;  --  never: bit counts are small
      end if;
      Two_To (Exponent, Budget, Unit, State);
      --  The midpoint to a multiple of Unit, to nearest: error at most Unit/2.
      Step (N.Divide, X.Mid, Unit, Budget, Scaled, State);
      Step (N.Round, Scaled, Budget, Whole, State);
      Step (N.Multiply, Whole, Unit, Budget, Mid, State);
      Integer_Number (2, Budget, Half, State);
      Step (N.Divide, Unit, Half, Budget, Error, State);
      Step (N.Add, X.Rad, Error, Budget, Widened, State);
      --  The radius up to a multiple of Unit.
      Step (N.Divide, Widened, Unit, Budget, Rad_Scaled, State);
      Step (N.Ceiling, Rad_Scaled, Budget, Rad_Whole, State);
      Step (N.Multiply, Rad_Whole, Unit, Budget, Rad, State);
      N.Free (Unit);
      N.Free (Half);
      N.Free (Scaled);
      N.Free (Shifted);
      N.Free (Whole);
      N.Free (Error);
      N.Free (Widened);
      N.Free (Rad_Scaled);
      N.Free (Rad_Whole);
      Free (X);
      Make_Ball (Mid, Rad, State, X);
   end Trim;

   --  Result := Mid +- Rad, trimmed; Mid and Rad taken.
   procedure Finish
     (Mid, Rad : in out Number; Budget : Size_Budget; Result : in out Ball; State : in out Status)
     with Pre => Is_Reclaimed (Result),
          Post => N.Is_Reclaimed (Mid) and then N.Is_Reclaimed (Rad)
                  and then (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
   begin
      if State = Done and then not (N.Within (Mid, Budget) and then N.Within (Rad, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      Make_Ball (Mid, Rad, State, Result);
      Trim (Result, Budget, State);
   end Finish;

   procedure Negate (X : Ball; Result : in out Ball) is
   begin
      N.Copy (X.Rad, Result.Rad);
      N.Copy_Negated (X.Mid, Result.Mid);
   end Negate;

   --  Whether every point of X is above zero (Positive), below (Negative),
   --  the ball is exactly zero, or it straddles zero.
   type Sign is (Negative, Zero, Positive, Straddles);
   procedure Sign_Of (X : Ball; Budget : Size_Budget; Result : out Sign; State : in out Status)
   is
      Low, High : Number;
      Low_Order, High_Order : Ordering;
      Zero_Value : Number;
   begin
      if State = Done and then not (Within (X, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      Result := Straddles;
      if State /= Done then
         return;
      end if;
      if Is_Exact (X) then
         Result := (if N.Is_Zero (X.Mid) then Zero elsif N.Is_Negative (X.Mid) then Negative else Positive);
         return;
      end if;
      Step (N.Subtract, X.Mid, X.Rad, Budget, Low, State);
      Step (N.Add, X.Mid, X.Rad, Budget, High, State);
      if State = Done then
         N.Compare (Low, Zero_Value, Low_Order);
         N.Compare (High, Zero_Value, High_Order);
         Result := (if Low_Order = Greater then Positive elsif High_Order = Less then Negative else Straddles);
      end if;
      N.Free (Low);
      N.Free (High);
   end Sign_Of;

   ---------------------------------------------------------------------------
   --  Arithmetic.

   procedure Sum
     (A, B : Ball; Flip : Boolean; Budget : Size_Budget; Result : in out Ball; State : in out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Mid, Rad : Number;
   begin
      if State = Done and then not (Within (A, Budget) and then Within (B, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      Step ((if Flip then N.Subtract else N.Add), A.Mid, B.Mid, Budget, Mid, State);
      Step (N.Add, A.Rad, B.Rad, Budget, Rad, State);
      Finish (Mid, Rad, Budget, Result, State);
   end Sum;

   --  |a| rb + |b| ra + ra rb.
   procedure Product
     (A, B : Ball; Budget : Size_Budget; Result : in out Ball; State : in out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Mid, Rad, Abs_A, Abs_B, Part_A, Part_B, Part_AB, Partial : Number;
   begin
      if State = Done and then not (Within (A, Budget) and then Within (B, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      Step (N.Multiply, A.Mid, B.Mid, Budget, Mid, State);
      if State = Done and then Is_Exact (A) and then Is_Exact (B) then
         Finish (Mid, Rad, Budget, Result, State);
         return;
      end if;
      Step (N.Absolute, A.Mid, Budget, Abs_A, State);
      Step (N.Absolute, B.Mid, Budget, Abs_B, State);
      Step (N.Multiply, Abs_A, B.Rad, Budget, Part_A, State);
      Step (N.Multiply, Abs_B, A.Rad, Budget, Part_B, State);
      Step (N.Multiply, A.Rad, B.Rad, Budget, Part_AB, State);
      Step (N.Add, Part_A, Part_B, Budget, Partial, State);
      Step (N.Add, Partial, Part_AB, Budget, Rad, State);
      N.Free (Abs_A);
      N.Free (Abs_B);
      N.Free (Part_A);
      N.Free (Part_B);
      N.Free (Part_AB);
      N.Free (Partial);
      Finish (Mid, Rad, Budget, Result, State);
   end Product;

   --  a / b, b bounded away from zero: radius (|a| rb + |b| ra) / (|b| (|b| - rb)).
   procedure Quotient
     (A, B : Ball; Budget : Size_Budget; Result : in out Ball; State : in out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Divisor_Sign : Sign;
      Mid, Rad, Abs_A, Abs_B, Part_A, Part_B, Top, Gap, Bottom : Number;
   begin
      if State = Done and then not (Within (A, Budget) and then Within (B, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      Sign_Of (B, Budget, Divisor_Sign, State);
      if State /= Done then
         return;
      elsif Divisor_Sign = Zero then
         State := Division_By_Zero;
         return;
      elsif Divisor_Sign = Straddles then
         State := Indeterminate;
         return;
      end if;
      Step (N.Divide, A.Mid, B.Mid, Budget, Mid, State);
      if State = Done and then Is_Exact (A) and then Is_Exact (B) then
         Finish (Mid, Rad, Budget, Result, State);
         return;
      end if;
      Step (N.Absolute, A.Mid, Budget, Abs_A, State);
      Step (N.Absolute, B.Mid, Budget, Abs_B, State);
      Step (N.Multiply, Abs_A, B.Rad, Budget, Part_A, State);
      Step (N.Multiply, Abs_B, A.Rad, Budget, Part_B, State);
      Step (N.Add, Part_A, Part_B, Budget, Top, State);
      Step (N.Subtract, Abs_B, B.Rad, Budget, Gap, State);
      Step (N.Multiply, Abs_B, Gap, Budget, Bottom, State);
      Step (N.Divide, Top, Bottom, Budget, Rad, State);
      N.Free (Abs_A);
      N.Free (Abs_B);
      N.Free (Part_A);
      N.Free (Part_B);
      N.Free (Top);
      N.Free (Gap);
      N.Free (Bottom);
      Finish (Mid, Rad, Budget, Result, State);
   end Quotient;

   procedure Exact_Ball (Value : Integer_64; Budget : Size_Budget; Result : in out Ball; State : in out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Mid, Rad : Number;
   begin
      Integer_Number (Value, Budget, Mid, State);
      Make_Ball (Mid, Rad, State, Result);
   end Exact_Ball;

   --  A ball scaled by an exact rational Numerator / Denominator.
   procedure Scale
     (X : Ball; Numerator, Denominator : Integer_64; Budget : Size_Budget; Result : in out Ball;
      State : in out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Top, Bottom, Ratio : Ball;
   begin
      if State = Done and then not (Within (X, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      Exact_Ball (Numerator, Budget, Top, State);
      Exact_Ball (Denominator, Budget, Bottom, State);
      Quotient (Top, Bottom, Budget, Ratio, State);
      Product (X, Ratio, Budget, Result, State);
      Free (Top);
      Free (Bottom);
      Free (Ratio);
   end Scale;

   ---------------------------------------------------------------------------
   --  Series on exact rationals, Bits fractional bits. Each returns the sum
   --  as the midpoint and the counted truncation as the radius.

   --  Ends a series: Sum +- Ulps units of the last bit; Converged False
   --  (the terms never reached zero) is Indeterminate.
   procedure Series_Ball
     (Sum : in out Number; Ulps : Ulps_Count; Bits : Series_Bits; Converged : Boolean; Budget : Size_Budget;
      Result : in out Ball; State : in out Status)
     with Pre => Is_Reclaimed (Result),
          Post => N.Is_Reclaimed (Sum) and then (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Rad : Number;
   begin
      if State = Done and then not (N.Within (Sum, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      if State = Done and then not Converged then
         State := Indeterminate;
      end if;
      Ulps_Of (Ulps + ULPS_FOR_TAIL, Bits, Budget, Rad, State);
      Finish (Sum, Rad, Budget, Result, State);
   end Series_Ball;

   --  exp (U), |U| <= 1/2: terms U ** n / n!, each within 2 units.
   procedure Exp_Series (U : Number; Bits : Series_Bits; Budget : Size_Budget; Result : in out Ball; State : in out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Sum, Term, Product_Value, Count, Next, Total : Number;
      Ulps : Natural := 0;
      Converged : Boolean := False;
   begin
      if State = Done and then not (N.Within (U, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      Integer_Number (1, Budget, Sum, State);
      Integer_Number (1, Budget, Term, State);
      for K in 1 .. Bits + 2 loop
         exit when State /= Done;
         pragma Loop_Invariant (State = Done and then N.Is_Reclaimed (Product_Value) and then N.Is_Reclaimed (Count)
                                and then N.Is_Reclaimed (Next) and then N.Is_Reclaimed (Total)
                                and then N.Within (Sum, Budget) and then N.Within (Term, Budget));
         pragma Loop_Invariant (Ulps <= ULPS_PER_TERM * K);
         Step (N.Multiply, Term, U, Budget, Product_Value, State);
         Integer_Number (Integer_64 (K), Budget, Count, State);
         Step (N.Divide, Product_Value, Count, Budget, Next, State);
         N.Free (Term);
         Truncate (Next, Bits, Budget, Term, State);
         N.Free (Product_Value);
         N.Free (Count);
         N.Free (Next);
         if State = Done and then N.Is_Zero (Term) then
            Converged := True;
            exit;
         end if;
         Step (N.Add, Sum, Term, Budget, Total, State);
         N.Free (Sum);
         if State = Done then
            N.Move (Total, Sum);
         end if;
         Ulps := Ulps + ULPS_PER_TERM;
      end loop;
      N.Free (Term);
      Series_Ball (Sum, Ulps, Bits, Converged, Budget, Result, State);
   end Exp_Series;

   --  atanh (Z), |Z| <= 1/3: Z ** (2n+1) / (2n+1). Odd: alternating signs
   --  (atan) instead.
   procedure Odd_Series
     (Z : Number; Alternating : Boolean; Bits : Series_Bits; Budget : Size_Budget; Result : in out Ball;
      State : in out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Square, Sum, Power_Term, Next_Power, Divisor, Term, Total : Number;
      Ulps : Natural := 0;
      Converged : Boolean := False;
   begin
      if State = Done and then not (N.Within (Z, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      Step (N.Multiply, Z, Z, Budget, Square, State);
      Truncate (Z, Bits, Budget, Power_Term, State);
      N.Copy (Power_Term, Sum);
      for K in 1 .. Bits + 2 loop
         exit when State /= Done;
         pragma Loop_Invariant (State = Done and then N.Is_Reclaimed (Next_Power) and then N.Is_Reclaimed (Divisor)
                                and then N.Is_Reclaimed (Term) and then N.Is_Reclaimed (Total)
                                and then N.Within (Sum, Budget) and then N.Within (Power_Term, Budget)
                                and then N.Within (Square, Budget));
         pragma Loop_Invariant (Ulps <= ULPS_PER_TERM * K);
         Step (N.Multiply, Power_Term, Square, Budget, Term, State);
         Truncate (Term, Bits, Budget, Next_Power, State);
         N.Free (Term);
         N.Free (Power_Term);
         if State = Done then
            N.Move (Next_Power, Power_Term);
         end if;
         if State = Done and then N.Is_Zero (Power_Term) then
            Converged := True;
            exit;
         end if;
         Integer_Number (Integer_64 (2 * K + 1), Budget, Divisor, State);
         Step (N.Divide, Power_Term, Divisor, Budget, Total, State);
         Truncate (Total, Bits, Budget, Term, State);
         N.Free (Total);
         N.Free (Divisor);
         Step ((if Alternating and then K mod 2 = 1 then N.Subtract else N.Add), Sum, Term, Budget, Total, State);
         N.Free (Term);
         N.Free (Sum);
         if State = Done then
            N.Move (Total, Sum);
         end if;
         Ulps := Ulps + ULPS_PER_TERM;
      end loop;
      N.Free (Square);
      N.Free (Power_Term);
      N.Free (Next_Power);
      if State /= Done then
         N.Free (Sum);
      end if;
      Series_Ball (Sum, Ulps + 1, Bits, Converged, Budget, Result, State);
   end Odd_Series;

   --  sin (S) (Sine) or cos (S), |S| <= 1: S ** n / n! with alternating signs.
   procedure Trig_Series
     (S : Number; Sine_Wanted : Boolean; Bits : Series_Bits; Budget : Size_Budget; Result : in out Ball;
      State : in out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Square, Sum, Term, Product_Value, Divisor, Next, Total : Number;
      Ulps : Natural := 0;
      Converged : Boolean := False;
      Order : Natural := (if Sine_Wanted then 1 else 0);
   begin
      if State = Done and then not (N.Within (S, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      Step (N.Multiply, S, S, Budget, Square, State);
      if Sine_Wanted then
         Truncate (S, Bits, Budget, Term, State);
      else
         Integer_Number (1, Budget, Term, State);
      end if;
      if State = Done then
         N.Copy (Term, Sum);
      end if;
      for K in 1 .. Bits + 2 loop
         exit when State /= Done;
         pragma Loop_Invariant (State = Done and then N.Is_Reclaimed (Product_Value) and then N.Is_Reclaimed (Divisor)
                                and then N.Is_Reclaimed (Next) and then N.Is_Reclaimed (Total)
                                and then N.Within (Sum, Budget) and then N.Within (Term, Budget)
                                and then N.Within (Square, Budget));
         pragma Loop_Invariant (Ulps <= ULPS_PER_TERM * K and then Order <= 2 * K - 1);
         Step (N.Multiply, Term, Square, Budget, Product_Value, State);
         Integer_Number (Integer_64 (Order + 1) * Integer_64 (Order + 2), Budget, Divisor, State);
         Step (N.Divide, Product_Value, Divisor, Budget, Next, State);
         N.Free (Term);
         Truncate (Next, Bits, Budget, Term, State);
         N.Free (Product_Value);
         N.Free (Divisor);
         N.Free (Next);
         Order := Order + 2;
         if State = Done and then N.Is_Zero (Term) then
            Converged := True;
            exit;
         end if;
         Step ((if K mod 2 = 1 then N.Subtract else N.Add), Sum, Term, Budget, Total, State);
         N.Free (Sum);
         if State = Done then
            N.Move (Total, Sum);
         end if;
         Ulps := Ulps + ULPS_PER_TERM;
      end loop;
      N.Free (Square);
      N.Free (Term);
      if State /= Done then
         N.Free (Sum);
      end if;
      Series_Ball (Sum, Ulps, Bits, Converged, Budget, Result, State);
   end Trig_Series;

   --  arctan (1 / K) by its series in 1/K ** 2 (Machin's terms).
   procedure Inverse_Arctangent
     (K : Integer_64; Bits : Series_Bits; Budget : Size_Budget; Result : in out Ball; State : in out Status)
     with Pre => Is_Reclaimed (Result) and then K > 1,
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      One, Divisor, Inverse : Number;
   begin
      Integer_Number (1, Budget, One, State);
      Integer_Number (K, Budget, Divisor, State);
      Step (N.Divide, One, Divisor, Budget, Inverse, State);
      Odd_Series (Inverse, True, Bits, Budget, Result, State);
      N.Free (One);
      N.Free (Divisor);
      N.Free (Inverse);
   end Inverse_Arctangent;

   procedure Pi_Ball (Bits : Series_Bits; Budget : Size_Budget; Result : in out Ball; State : in out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      First, Second, Weighted_First, Weighted_Second : Ball;
   begin
      Inverse_Arctangent (MACHIN_FIRST, Bits, Budget, First, State);
      Inverse_Arctangent (MACHIN_SECOND, Bits, Budget, Second, State);
      Scale (First, MACHIN_FIRST_WEIGHT, 1, Budget, Weighted_First, State);
      Scale (Second, MACHIN_SECOND_WEIGHT, 1, Budget, Weighted_Second, State);
      Sum (Weighted_First, Weighted_Second, True, Budget, Result, State);
      Free (First);
      Free (Second);
      Free (Weighted_First);
      Free (Weighted_Second);
   end Pi_Ball;

   ---------------------------------------------------------------------------
   --  Elementary functions of an exact argument; a ball argument is its
   --  midpoint's value widened by a bound on the function's change.

   --  exp of an exact M: exp (M / 2 ** J) squared J times.
   procedure Exp_Exact
     (M : Number; Digits_Wanted : Precision_Digits; Budget : Size_Budget; Result : in out Ball; State : in out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Scale : constant Integer := Scale_Of (M);
      Halvings : constant Natural := Natural'Max (0, Integer'Min (Scale, LIMB_BITS - 2) + 2);
      Scale_Factor, Reduced : Number;
      Value, Squared : Ball;
   begin
      if State = Done and then not (N.Within (M, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      if State /= Done then
         return;
      end if;
      if N.Is_Zero (M) then
         Exact_Ball (1, Budget, Result, State);
         return;
      end if;
      --  exp (M) has about 1.44 |M| bits: refuse what the budget cannot hold.
      if Scale >= LIMB_BITS - 2
        or else (Scale > 0 and then Shift_Left (Unsigned_64 (1), Scale) > Unsigned_64 (Budget) * LIMB_BITS)
      then
         State := Too_Large;
         return;
      end if;
      Two_To (Halvings, Budget, Scale_Factor, State);
      Step (N.Divide, M, Scale_Factor, Budget, Reduced, State);
      Exp_Series (Reduced, Work_Bits (Digits_Wanted) + Halvings, Budget, Value, State);
      for J in 1 .. Halvings loop
         exit when State /= Done;
         pragma Loop_Invariant (State = Done and then Is_Reclaimed (Squared) and then Within (Value, Budget));
         Product (Value, Value, Budget, Squared, State);
         Free (Value);
         if State = Done then
            Move (Squared, Value);
         end if;
      end loop;
      N.Free (Scale_Factor);
      N.Free (Reduced);
      Free (Squared);
      if State = Done then
         Move (Value, Result);
      else
         Free (Value);
      end if;
   end Exp_Exact;

   --  ln of an exact M > 0: k ln 2 + 2 atanh ((y - 1) / (y + 1)), M = 2 ** k y.
   procedure Ln_Exact
     (M : Number; Digits_Wanted : Precision_Digits; Budget : Size_Budget; Result : in out Ball; State : in out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      K : constant Integer := Scale_Of (M);
      Bits : constant Natural := Work_Bits (Digits_Wanted) + Natural'Min (abs K, MAXIMUM_DIGITS) / 16 + 8;
      Shift, Y, One, Above, Below, Z, Third : Number;
      Atanh_Y, Atanh_Third, Ln_Y, Ln_Two, K_Ln_Two, K_Ball : Ball;
   begin
      if State = Done and then not (N.Within (M, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      if State /= Done then
         return;
      end if;
      if K = Integer'First then
         State := Too_Large;  --  never
         return;
      end if;
      Two_To (-K, Budget, Shift, State);
      Step (N.Multiply, M, Shift, Budget, Y, State);
      Integer_Number (1, Budget, One, State);
      Step (N.Subtract, Y, One, Budget, Below, State);
      Step (N.Add, Y, One, Budget, Above, State);
      Step (N.Divide, Below, Above, Budget, Z, State);
      Odd_Series (Z, False, Bits, Budget, Atanh_Y, State);
      Scale (Atanh_Y, 2, 1, Budget, Ln_Y, State);
      if K /= 0 then
         --  ln 2 = 2 atanh (1/3).
         Integer_Number (1, Budget, Third, State);
         declare
            Three, One_Third : Number;
         begin
            Integer_Number (3, Budget, Three, State);
            Step (N.Divide, Third, Three, Budget, One_Third, State);
            Odd_Series (One_Third, False, Bits, Budget, Atanh_Third, State);
            N.Free (Three);
            N.Free (One_Third);
         end;
         Scale (Atanh_Third, 2, 1, Budget, Ln_Two, State);
         Exact_Ball (Integer_64 (K), Budget, K_Ball, State);
         Product (K_Ball, Ln_Two, Budget, K_Ln_Two, State);
         Sum (Ln_Y, K_Ln_Two, False, Budget, Result, State);
      elsif State = Done then
         Move (Ln_Y, Result);
      end if;
      N.Free (Shift);
      N.Free (Y);
      N.Free (One);
      N.Free (Above);
      N.Free (Below);
      N.Free (Z);
      N.Free (Third);
      Free (Atanh_Y);
      Free (Atanh_Third);
      Free (Ln_Y);
      Free (Ln_Two);
      Free (K_Ln_Two);
      Free (K_Ball);
   end Ln_Exact;

   --  sin or cos of an exact M radians: M = q pi/2 + s, |s| <= pi/4 or a
   --  little more, by the quadrant q mod 4.
   procedure Sin_Cos_Exact
     (M : Number; Sine_Wanted : Boolean; Digits_Wanted : Precision_Digits; Budget : Size_Budget;
      Result : in out Ball; State : in out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Bits : constant Natural := Work_Bits (Digits_Wanted);
      Pi_Value, Half_Pi, M_Ball, Turns, Offset, S : Ball;
      Half_Pi_Mid, Ratio, Q, S_Mid, S_Rad, Four, Quadrant_Number : Number;
      Value, Shifted : Ball;
      Quadrant : Natural := 0;
      Use_Sine : Boolean;
      Negated : Boolean;
   begin
      if State = Done and then not (N.Within (M, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      if State /= Done then
         return;
      end if;
      if N.Is_Zero (M) then
         Exact_Ball ((if Sine_Wanted then 0 else 1), Budget, Result, State);
         return;
      end if;
      if Scale_Of (M) > MAXIMUM_ARGUMENT_BITS then
         State := Too_Large;
         return;
      end if;
      Pi_Ball (Bits + Natural'Max (0, Scale_Of (M)) + EXTRA_ROOM, Budget, Pi_Value, State);
      Scale (Pi_Value, 1, 2, Budget, Half_Pi, State);
      if State = Done then
         N.Copy (Half_Pi.Mid, Half_Pi_Mid);
      end if;
      Step (N.Divide, M, Half_Pi_Mid, Budget, Ratio, State);
      Step (N.Round, Ratio, Budget, Q, State);
      --  s = M - q pi/2, as a ball.
      Integer_Number (4, Budget, Four, State);
      Step (N.Modulo, Q, Four, Budget, Quadrant_Number, State);
      if State = Done and then N.Fits_Natural (Quadrant_Number) then
         Quadrant := N.To_Natural (Quadrant_Number) mod 4;
      end if;
      if State = Done then
         declare
            Q_Copy, M_Copy : Number;
         begin
            N.Copy (Q, Q_Copy);
            From_Exact (Q_Copy, Turns);
            N.Copy (M, M_Copy);
            From_Exact (M_Copy, M_Ball);
         end;
      end if;
      Product (Turns, Half_Pi, Budget, Offset, State);
      Sum (M_Ball, Offset, True, Budget, S, State);
      if State = Done then
         N.Copy (S.Mid, S_Mid);
         N.Copy (S.Rad, S_Rad);
      end if;
      --  Quadrants: sin (s), cos (s), -sin (s), -cos (s) for sin; shifted by
      --  one for cos.
      Use_Sine := (if Sine_Wanted then Quadrant mod 2 = 0 else Quadrant mod 2 = 1);
      Negated := (if Sine_Wanted then Quadrant >= 2 else Quadrant in 1 .. 2);
      Trig_Series (S_Mid, Use_Sine, Bits, Budget, Value, State);
      --  The midpoint's error: sin and cos change by at most |ds|.
      if State = Done then
         declare
            Widened : Number;
         begin
            Step (N.Add, Value.Rad, S_Rad, Budget, Widened, State);
            if State = Done then
               N.Free (Value.Rad);
               N.Move (Widened, Value.Rad);
            end if;
            N.Free (Widened);
         end;
      end if;
      if State = Done then
         if Negated then
            Negate (Value, Shifted);
            Move (Shifted, Result);
         else
            Move (Value, Result);
         end if;
         Trim (Result, Budget, State);
      end if;
      Free (Pi_Value);
      Free (Half_Pi);
      Free (M_Ball);
      Free (Turns);
      Free (Offset);
      Free (S);
      Free (Value);
      Free (Shifted);
      N.Free (Half_Pi_Mid);
      N.Free (Ratio);
      N.Free (Q);
      N.Free (S_Mid);
      N.Free (S_Rad);
      N.Free (Four);
      N.Free (Quadrant_Number);
   end Sin_Cos_Exact;

   ---------------------------------------------------------------------------
   --  Square roots.

   --  floor (sqrt (M) * 2 ** Bits) / 2 ** Bits for an exact M >= 0: within
   --  2 ** -Bits below the root.
   procedure Lower_Root
     (M : Number; Bits : Natural; Budget : Size_Budget; Result : in out Number; State : in out Status)
     with Pre => N.Is_Reclaimed (Result),
          Post => (if State = Done then N.Within (Result, Budget) else N.Is_Reclaimed (Result))
   is
      Num, Den, Scaled, Product_Value, Root_Value, Unit, Out_Den : Magnitude;
   begin
      if State = Done and then not (N.Within (M, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      if State /= Done then
         return;
      end if;
      if Bits > Bit_Count'Last / 4 then
         State := Too_Large;
         return;
      end if;
      N.Parts (M, Num, Den);
      --  sqrt (p/q) = sqrt (p q 2 ** 2B) / (q 2 ** B).
      Multiply (Num, Den, Product_Value);
      if Length (Product_Value) + (2 * Bits) / LIMB_BITS + 1 > Budget * 2 then
         State := Too_Large;
      else
         Shift_Left (Product_Value, 2 * Bits, Scaled);
         if Length (Scaled) < MAXIMUM_LIMBS / 4 then
            Integer_Root (Scaled, 2, Root_Value);
         else
            State := Too_Large;
         end if;
         Make (Unit, 1);
         if Length (Den) + Bits / LIMB_BITS + 1 <= MAXIMUM_LIMBS / 2 then
            Shift_Left (Den, Bits, Out_Den);
         else
            State := Too_Large;
         end if;
         Free (Unit);
      end if;
      if State = Done and then Length (Root_Value) < MAXIMUM_LIMBS and then Length (Out_Den) < MAXIMUM_LIMBS then
         declare
            Outcome : N.Status;
         begin
            N.From_Parts (False, Root_Value, Out_Den, Budget, Result, Outcome);
            State := Map (Outcome);
         end;
      elsif State = Done then
         State := Too_Large;
      end if;
      Free (Num);
      Free (Den);
      Free (Scaled);
      Free (Product_Value);
      Free (Root_Value);
      Free (Out_Den);
   end Lower_Root;

   procedure Sqrt_Ball
     (X : Ball; Digits_Wanted : Precision_Digits; Budget : Size_Budget; Result : in out Ball; State : in out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Bits : constant Natural := Work_Bits (Digits_Wanted);
      Where : Sign;
      Exact_Root, Mid, Rad, Unit, Low, Low_Root, Extra, Total : Number;
      Outcome : N.Status;
   begin
      if State = Done and then not (Within (X, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      Sign_Of (X, Budget, Where, State);
      if State /= Done then
         return;
      elsif Where = Negative then
         State := Outside_Domain;
         return;
      elsif Where = Zero then
         Exact_Ball (0, Budget, Result, State);
         return;
      elsif Where = Straddles then
         State := Indeterminate;
         return;
      end if;
      if Is_Exact (X) and then Within (X, Budget) then
         N.Apply (N.Square_Root, X.Mid, Budget, Exact_Root, Outcome);
         if Outcome = N.Done then
            From_Exact (Exact_Root, Result);
            return;
         end if;
         N.Free (Exact_Root);
      end if;
      --  The midpoint's root within 2 ** -Bits; the radius r moves the
      --  root by at most r / sqrt (m - r).
      Lower_Root (X.Mid, Bits, Budget, Mid, State);
      Ulps_Of (1, Bits, Budget, Rad, State);
      if State = Done and then not Is_Exact (X) then
         Step (N.Subtract, X.Mid, X.Rad, Budget, Low, State);
         Lower_Root (Low, Bits, Budget, Low_Root, State);
         if State = Done and then N.Is_Zero (Low_Root) then
            State := Indeterminate;
         end if;
         Step (N.Divide, X.Rad, Low_Root, Budget, Extra, State);
         Step (N.Add, Rad, Extra, Budget, Total, State);
         N.Free (Rad);
         if State = Done then
            N.Move (Total, Rad);
         end if;
      end if;
      N.Free (Unit);
      N.Free (Low);
      N.Free (Low_Root);
      N.Free (Extra);
      N.Free (Total);
      Finish (Mid, Rad, Budget, Result, State);
   end Sqrt_Ball;

   ---------------------------------------------------------------------------
   --  Exp, ln and the arctangent of balls.

   --  exp (m +- r) within exp (m) (1 +- 2 r) for r <= 1/2.
   procedure Exp_Ball
     (X : Ball; Digits_Wanted : Precision_Digits; Budget : Size_Budget; Result : in out Ball; State : in out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Value : Ball;
      Half, Order_Base : Number;
      Order : Ordering := Less;
      Abs_Mid, Upper, Twice, Extra, Total : Number;
   begin
      if State = Done and then not (Within (X, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      Exp_Exact (X.Mid, Digits_Wanted, Budget, Value, State);
      if State = Done and then not Is_Exact (X) and then Within (X, Budget) then
         Integer_Number (1, Budget, Half, State);
         Integer_Number (2, Budget, Order_Base, State);
         declare
            Half_Value : Number;
         begin
            Step (N.Divide, Half, Order_Base, Budget, Half_Value, State);
            if State = Done then
               N.Compare (X.Rad, Half_Value, Order);
            end if;
            N.Free (Half_Value);
         end;
         if State = Done and then Order = Greater then
            State := Indeterminate;
         end if;
         Step (N.Absolute, Value.Mid, Budget, Abs_Mid, State);
         Step (N.Add, Abs_Mid, Value.Rad, Budget, Upper, State);
         Step (N.Multiply, Upper, X.Rad, Budget, Twice, State);
         Step (N.Multiply, Twice, Order_Base, Budget, Extra, State);
         Step (N.Add, Value.Rad, Extra, Budget, Total, State);
         if State = Done then
            N.Free (Value.Rad);
            N.Move (Total, Value.Rad);
         end if;
      end if;
      if State = Done then
         Move (Value, Result);
         Trim (Result, Budget, State);
      end if;
      Free (Value);
      N.Free (Half);
      N.Free (Order_Base);
      N.Free (Abs_Mid);
      N.Free (Upper);
      N.Free (Twice);
      N.Free (Extra);
      N.Free (Total);
   end Exp_Ball;

   --  ln (m +- r) within ln (m) +- r / (m - r), for m - r > 0.
   procedure Ln_Ball
     (X : Ball; Digits_Wanted : Precision_Digits; Budget : Size_Budget; Result : in out Ball; State : in out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Where : Sign;
      Value : Ball;
      Low, Extra, Total, One : Number;
      Order : Ordering := Less;
   begin
      if State = Done and then not (Within (X, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      Sign_Of (X, Budget, Where, State);
      if State /= Done then
         return;
      elsif Where in Negative | Zero then
         State := Outside_Domain;
         return;
      elsif Where = Straddles then
         State := Indeterminate;
         return;
      end if;
      if Is_Exact (X) and then Within (X, Budget) then
         Integer_Number (1, Budget, One, State);
         if State = Done then
            N.Compare (X.Mid, One, Order);
         end if;
         N.Free (One);
         if State = Done and then Order = Equal then
            Exact_Ball (0, Budget, Result, State);
            return;
         end if;
      end if;
      Ln_Exact (X.Mid, Digits_Wanted, Budget, Value, State);
      if State = Done and then not Is_Exact (X) then
         Step (N.Subtract, X.Mid, X.Rad, Budget, Low, State);
         Step (N.Divide, X.Rad, Low, Budget, Extra, State);
         Step (N.Add, Value.Rad, Extra, Budget, Total, State);
         if State = Done then
            N.Free (Value.Rad);
            N.Move (Total, Value.Rad);
         end if;
      end if;
      if State = Done then
         Move (Value, Result);
         Trim (Result, Budget, State);
      end if;
      Free (Value);
      N.Free (Low);
      N.Free (Extra);
      N.Free (Total);
   end Ln_Ball;

   --  arctan in radians: for |v| > 1, sign pi/2 - arctan (1/v); then two
   --  halvings v / (1 + sqrt (1 + v ** 2)) bring |v| under 1/4 for the
   --  series; the argument's radius moves arctan by at most as much.
   procedure Atan_Core
     (X : Ball; Digits_Wanted : Precision_Digits; Budget : Size_Budget; Result : in out Ball; State : in out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget))
   is
      Bits : constant Natural := Work_Bits (Digits_Wanted);
      Mid_Copy, Abs_Mid, One_Number, X_Rad : Number;
      Order : Ordering := Less;
      Inverted : Boolean := False;
      V, One, Squared, Above, Root_Value, Denominator, Reduced, Series, Scaled_Series, Pi_Value, Half_Pi,
        Combined : Ball;
   begin
      if State = Done and then not (Within (X, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      if State /= Done then
         return;
      end if;
      if Is_Exact (X) and then N.Is_Zero (X.Mid) then
         Exact_Ball (0, Budget, Result, State);
         return;
      end if;
      Step (N.Absolute, X.Mid, Budget, Abs_Mid, State);
      Integer_Number (1, Budget, One_Number, State);
      if State = Done then
         N.Compare (Abs_Mid, One_Number, Order);
      end if;
      N.Copy (X.Mid, Mid_Copy);
      From_Exact (Mid_Copy, V);
      Exact_Ball (1, Budget, One, State);
      if State = Done and then Order = Greater then
         --  arctan (v) = sign (v) pi/2 - arctan (1/v).
         Inverted := True;
         declare
            Flipped : Ball;
         begin
            Quotient (One, V, Budget, Flipped, State);
            Free (V);
            if State = Done then
               Move (Flipped, V);
            end if;
            Free (Flipped);
         end;
      end if;
      for Halving in 1 .. 2 loop
         exit when State /= Done;
         pragma Loop_Invariant (State = Done and then Within (V, Budget) and then Within (One, Budget)
                                and then Is_Reclaimed (Result)
                                and then Is_Reclaimed (Squared) and then Is_Reclaimed (Above)
                                and then Is_Reclaimed (Root_Value) and then Is_Reclaimed (Denominator)
                                and then Is_Reclaimed (Reduced));
         Product (V, V, Budget, Squared, State);
         Sum (One, Squared, False, Budget, Above, State);
         Sqrt_Ball (Above, Digits_Wanted, Budget, Root_Value, State);
         Sum (One, Root_Value, False, Budget, Denominator, State);
         Quotient (V, Denominator, Budget, Reduced, State);
         Free (Squared);
         Free (Above);
         Free (Root_Value);
         Free (Denominator);
         Free (V);
         if State = Done then
            Move (Reduced, V);
         end if;
         Free (Reduced);
      end loop;
      if State = Done then
         Odd_Series (V.Mid, True, Bits, Budget, Series, State);
      end if;
      if State = Done then
         --  The reduced argument's radius.
         declare
            Total : Number;
         begin
            Step (N.Add, Series.Rad, V.Rad, Budget, Total, State);
            if State = Done then
               N.Free (Series.Rad);
               N.Move (Total, Series.Rad);
            end if;
            N.Free (Total);
         end;
      end if;
      Scale (Series, 4, 1, Budget, Scaled_Series, State);
      if Inverted then
         Pi_Ball (Bits, Budget, Pi_Value, State);
         Scale (Pi_Value, (if N.Is_Negative (X.Mid) then -1 else 1), 2, Budget, Half_Pi, State);
         Sum (Half_Pi, Scaled_Series, True, Budget, Combined, State);
      elsif State = Done then
         Move (Scaled_Series, Combined);
      end if;
      --  The original radius: arctan changes by at most as much.
      if State = Done and then not Is_Exact (X) then
         N.Copy (X.Rad, X_Rad);
         declare
            Total : Number;
         begin
            Step (N.Add, Combined.Rad, X_Rad, Budget, Total, State);
            if State = Done then
               N.Free (Combined.Rad);
               N.Move (Total, Combined.Rad);
            end if;
            N.Free (Total);
         end;
      end if;
      if State = Done then
         Move (Combined, Result);
         Trim (Result, Budget, State);
      end if;
      N.Free (Mid_Copy);
      N.Free (Abs_Mid);
      N.Free (One_Number);
      N.Free (X_Rad);
      Free (V);
      Free (One);
      Free (Squared);
      Free (Above);
      Free (Root_Value);
      Free (Denominator);
      Free (Reduced);
      Free (Series);
      Free (Scaled_Series);
      Free (Pi_Value);
      Free (Half_Pi);
      Free (Combined);
   end Atan_Core;

   procedure Atan_Ball
     (X : Ball; Digits_Wanted : Precision_Digits; Budget : Size_Budget; Result : in out Ball; State : in out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
   begin
      Atan_Core (X, Digits_Wanted, Budget, Result, State);
      if State /= Done then
         Free (Result);
      end if;
   end Atan_Ball;

   ---------------------------------------------------------------------------
   --  Angles.

   --  An exact angle in degrees (gradians converted), reduced to [0, 360).
   procedure Exact_Degrees
     (X : Number; Unit : Angle_Unit; Budget : Size_Budget; Result : in out Number; State : in out Status)
     with Pre => N.Is_Reclaimed (Result) and then Unit /= Radians,
          Post => (if State = Done then N.Within (Result, Budget) else N.Is_Reclaimed (Result))
   is
      Ratio_Top, Ratio_Bottom, Ratio, Degrees_Value, Turn : Number;
   begin
      if State = Done and then not (N.Within (X, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      if Unit = Gradians then
         Integer_Number (GRADIAN_NUMERATOR, Budget, Ratio_Top, State);
         Integer_Number (GRADIAN_DENOMINATOR, Budget, Ratio_Bottom, State);
         Step (N.Divide, Ratio_Top, Ratio_Bottom, Budget, Ratio, State);
         Step (N.Multiply, X, Ratio, Budget, Degrees_Value, State);
      elsif State = Done then
         N.Copy (X, Degrees_Value);
      end if;
      Integer_Number (DEGREES_PER_TURN, Budget, Turn, State);
      Step (N.Modulo, Degrees_Value, Turn, Budget, Result, State);
      N.Free (Ratio_Top);
      N.Free (Ratio_Bottom);
      N.Free (Ratio);
      N.Free (Degrees_Value);
      N.Free (Turn);
   end Exact_Degrees;

   --  Niven: sin, cos and tan of a rational number of degrees are rational
   --  only at these angles.
   type Exact_Value is (Not_Known, Zero, Half, One, Minus_Half, Minus_One, Undefined);
   function Table (Kind : Elementary; Angle : Natural) return Exact_Value is
     (case Kind is
         when Sine =>
           (case Angle is
               when 0 | 180 => Zero, when 30 | 150 => Half, when 90 => One,
               when 210 | 330 => Minus_Half, when 270 => Minus_One, when others => Not_Known),
         when Cosine =>
           (case Angle is
               when 90 | 270 => Zero, when 60 | 300 => Half, when 0 => One,
               when 120 | 240 => Minus_Half, when 180 => Minus_One, when others => Not_Known),
         when Tangent =>
           (case Angle is
               when 0 | 180 => Zero, when 45 | 225 => One, when 135 | 315 => Minus_One,
               when 90 | 270 => Undefined, when others => Not_Known),
         when others => Not_Known);

   procedure Exact_Of (Value : Exact_Value; Budget : Size_Budget; Result : in out Ball; State : in out Status)
     with Pre => Is_Reclaimed (Result) and then Value not in Not_Known | Undefined,
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Top, Bottom, Mid, Rad : Number;
   begin
      Integer_Number ((case Value is when Zero => 0, when Half | One => 1, when others => -1), Budget, Top, State);
      Integer_Number ((if Value in Half | Minus_Half then 2 else 1), Budget, Bottom, State);
      Step (N.Divide, Top, Bottom, Budget, Mid, State);
      N.Free (Top);
      N.Free (Bottom);
      Make_Ball (Mid, Rad, State, Result);
   end Exact_Of;

   --  X in Unit as radians (a ball unless Unit is Radians).
   procedure To_Radians
     (X : Ball; Unit : Angle_Unit; Digits_Wanted : Precision_Digits; Budget : Size_Budget;
      Result : in out Ball; State : in out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Pi_Value, Ratio : Ball;
   begin
      if State = Done and then not (Within (X, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      if Unit = Radians then
         if State = Done then
            Copy (X, Result);
         end if;
         return;
      end if;
      if State = Done and then Scale_Of (X.Mid) > MAXIMUM_ARGUMENT_BITS then
         State := Too_Large;
      end if;
      Pi_Ball (Work_Bits (Digits_Wanted) + Natural'Min (MAXIMUM_ARGUMENT_BITS, Natural'Max (0, Scale_Of (X.Mid)))
               + EXTRA_ROOM, Budget, Pi_Value, State);
      Scale (Pi_Value, 1, (if Unit = Degrees then DEGREES_PER_HALF_TURN else GRADIANS_PER_HALF_TURN),
             Budget, Ratio, State);
      Product (X, Ratio, Budget, Result, State);
      Free (Pi_Value);
      Free (Ratio);
   end To_Radians;

   --  Radians to Unit.
   procedure From_Radians
     (X : Ball; Unit : Angle_Unit; Digits_Wanted : Precision_Digits; Budget : Size_Budget;
      Result : in out Ball; State : in out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Pi_Value, Half_Turn : Ball;
   begin
      if State = Done and then not (Within (X, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      if Unit = Radians then
         if State = Done then
            Copy (X, Result);
         end if;
         return;
      end if;
      Pi_Ball (Work_Bits (Digits_Wanted) + EXTRA_ROOM, Budget, Pi_Value, State);
      Exact_Ball ((if Unit = Degrees then DEGREES_PER_HALF_TURN else GRADIANS_PER_HALF_TURN), Budget, Half_Turn, State);
      declare
         Turned : Ball;
      begin
         Product (X, Half_Turn, Budget, Turned, State);
         Quotient (Turned, Pi_Value, Budget, Result, State);
         Free (Turned);
      end;
      Free (Pi_Value);
      Free (Half_Turn);
   end From_Radians;

   --  The angle (in Unit) of a right angle times Count / Per (Count / Per
   --  of a half turn = 90 Count / Per degrees): exact in degrees and
   --  gradians, a ball of pi in radians.
   procedure Angle_Of
     (Numerator, Denominator : Integer_64; Unit : Angle_Unit; Digits_Wanted : Precision_Digits;
      Budget : Size_Budget; Result : in out Ball; State : in out Status)
     with Pre => Is_Reclaimed (Result) and then Denominator /= 0,
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Half_Turn : Ball;
   begin
      case Unit is
         when Radians => Pi_Ball (Work_Bits (Digits_Wanted), Budget, Half_Turn, State);
         when Degrees => Exact_Ball (DEGREES_PER_HALF_TURN, Budget, Half_Turn, State);
         when Gradians => Exact_Ball (GRADIANS_PER_HALF_TURN, Budget, Half_Turn, State);
      end case;
      Scale (Half_Turn, Numerator, Denominator, Budget, Result, State);
      Free (Half_Turn);
   end Angle_Of;

   procedure Trig
     (Kind : Elementary; X : Ball; Unit : Angle_Unit; Digits_Wanted : Precision_Digits; Budget : Size_Budget;
      Result : in out Ball; State : in out Status)
     with Pre => Is_Reclaimed (Result) and then Kind in Sine | Cosine | Tangent,
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Angle, Turn : Number;
      Known : Exact_Value := Not_Known;
      Radians_Value, Sine_Value, Cosine_Value, Widened : Ball;
   begin
      if State = Done and then not (Within (X, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      if State = Done and then Is_Exact (X) and then Unit /= Radians then
         Exact_Degrees (X.Mid, Unit, Budget, Angle, State);
         if State = Done and then N.Fits_Natural (Angle) and then N.To_Natural (Angle) < DEGREES_PER_TURN then
            Known := Table (Kind, N.To_Natural (Angle));
         end if;
      elsif State = Done and then Is_Exact (X) and then N.Is_Zero (X.Mid) then
         Known := (if Kind = Cosine then One else Zero);
      end if;
      N.Free (Angle);
      N.Free (Turn);
      if State /= Done then
         return;
      elsif Known = Undefined then
         State := Outside_Domain;
         return;
      elsif Known /= Not_Known then
         Exact_Of (Known, Budget, Result, State);
         return;
      end if;
      To_Radians (X, Unit, Digits_Wanted, Budget, Radians_Value, State);
      if Kind in Sine | Tangent then
         Sin_Cos_Exact (Radians_Value.Mid, True, Digits_Wanted, Budget, Sine_Value, State);
      end if;
      if Kind in Cosine | Tangent then
         Sin_Cos_Exact (Radians_Value.Mid, False, Digits_Wanted, Budget, Cosine_Value, State);
      end if;
      --  The argument's radius: sin and cos change by at most as much.
      if State = Done and then not Is_Exact (Radians_Value) then
         declare
            Total : Number;
         begin
            if Kind in Sine | Tangent then
               Step (N.Add, Sine_Value.Rad, Radians_Value.Rad, Budget, Total, State);
               if State = Done then
                  N.Free (Sine_Value.Rad);
                  N.Move (Total, Sine_Value.Rad);
               end if;
               N.Free (Total);
            end if;
            if Kind in Cosine | Tangent then
               Step (N.Add, Cosine_Value.Rad, Radians_Value.Rad, Budget, Total, State);
               if State = Done then
                  N.Free (Cosine_Value.Rad);
                  N.Move (Total, Cosine_Value.Rad);
               end if;
               N.Free (Total);
            end if;
         end;
      end if;
      case Kind is
         when Sine =>
            if State = Done then
               Move (Sine_Value, Widened);
            end if;
         when Cosine =>
            if State = Done then
               Move (Cosine_Value, Widened);
            end if;
         when others => Quotient (Sine_Value, Cosine_Value, Budget, Widened, State);
      end case;
      if State = Done then
         Move (Widened, Result);
         Trim (Result, Budget, State);
      end if;
      Free (Radians_Value);
      Free (Sine_Value);
      Free (Cosine_Value);
      Free (Widened);
   end Trig;

   --  arcsin, arccos, arctan, in Unit; exact at the angles Niven allows.
   procedure Inverse_Trig
     (Kind : Elementary; X : Ball; Unit : Angle_Unit; Digits_Wanted : Precision_Digits; Budget : Size_Budget;
      Result : in out Ball; State : in out Status)
     with Pre => Is_Reclaimed (Result) and then Kind in Arcsine | Arccosine | Arctangent,
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Two, Half_Value, Abs_Mid, One_Number : Number;
      Order : Ordering := Less;
      Is_Half, Is_One, Is_Zero_Value : Boolean := False;
      Negative : constant Boolean := N.Is_Negative (X.Mid);
      One, Squared, Rest, Root_Value, Ratio, Radians_Value, Right_Angle, Asin_Value : Ball;
   begin
      if State = Done and then not (Within (X, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      if State /= Done then
         return;
      end if;
      --  |x| for the exact cases and the domain.
      Step (N.Absolute, X.Mid, Budget, Abs_Mid, State);
      Integer_Number (1, Budget, One_Number, State);
      Integer_Number (2, Budget, Two, State);
      Step (N.Divide, One_Number, Two, Budget, Half_Value, State);
      if State = Done and then Is_Exact (X) then
         N.Compare (Abs_Mid, One_Number, Order);
         Is_One := Order = Equal;
         if Order = Greater and then Kind /= Arctangent then
            State := Outside_Domain;
         end if;
         if State = Done then
            N.Compare (Abs_Mid, Half_Value, Order);
            Is_Half := Order = Equal;
         end if;
         Is_Zero_Value := N.Is_Zero (X.Mid);
      end if;
      N.Free (Two);
      N.Free (Half_Value);
      N.Free (Abs_Mid);
      N.Free (One_Number);
      if State /= Done then
         return;
      end if;
      --  The exact angles: asin 0, 1/2, 1; acos the same; atan 0, 1 (in
      --  degrees and gradians; asin 1 is pi/2 in radians, a ball).
      if Is_Zero_Value and then Kind /= Arccosine then
         Exact_Ball (0, Budget, Result, State);
         return;
      elsif Kind = Arcsine and then (Is_One or else (Is_Half and then Unit /= Radians)) then
         Angle_Of ((if Negative then -1 else 1), (if Is_One then 2 else 6), Unit, Digits_Wanted, Budget, Result, State);
         return;
      elsif Kind = Arccosine and then (Is_One or else Is_Zero_Value or else (Is_Half and then Unit /= Radians)) then
         if Is_One and then not Negative then
            Exact_Ball (0, Budget, Result, State);
         else
            --  acos: pi/2 at 0; pi/3 and 2pi/3 at +-1/2; pi at -1.
            Angle_Of ((if Is_Zero_Value then 1 elsif Is_One then 2 elsif Negative then 4 else 2),
                      (if Is_Zero_Value then 2 elsif Is_One then 2 else 6), Unit, Digits_Wanted, Budget, Result,
                      State);
         end if;
         return;
      elsif Kind = Arctangent and then Is_One and then Unit /= Radians then
         Angle_Of ((if Negative then -1 else 1), 4, Unit, Digits_Wanted, Budget, Result, State);
         return;
      end if;
      if Kind = Arctangent then
         Atan_Ball (X, Digits_Wanted, Budget, Radians_Value, State);
      else
         --  asin x = atan (x / sqrt (1 - x ** 2)); acos x = pi/2 - asin x.
         Exact_Ball (1, Budget, One, State);
         Product (X, X, Budget, Squared, State);
         Sum (One, Squared, True, Budget, Rest, State);
         Sqrt_Ball (Rest, Digits_Wanted, Budget, Root_Value, State);
         Quotient (X, Root_Value, Budget, Ratio, State);
         if Kind = Arcsine then
            Atan_Ball (Ratio, Digits_Wanted, Budget, Radians_Value, State);
         else
            Atan_Ball (Ratio, Digits_Wanted, Budget, Asin_Value, State);
            Angle_Of (1, 2, Radians, Digits_Wanted, Budget, Right_Angle, State);
            Sum (Right_Angle, Asin_Value, True, Budget, Radians_Value, State);
         end if;
      end if;
      From_Radians (Radians_Value, Unit, Digits_Wanted, Budget, Result, State);
      Free (One);
      Free (Squared);
      Free (Rest);
      Free (Root_Value);
      Free (Ratio);
      Free (Radians_Value);
      Free (Right_Angle);
      Free (Asin_Value);
   end Inverse_Trig;

   --  log10 x: exact for a power of ten, else ln x / ln 10.
   procedure Log10_Ball
     (X : Ball; Digits_Wanted : Precision_Digits; Budget : Size_Budget; Result : in out Ball; State : in out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Ten, Probe, Next : Number;
      Order : Ordering := Less;
      Exponent : Integer_64 := 0;
      Ln_X, Ln_Ten, Ten_Ball : Ball;
      Inverted : Boolean := False;
   begin
      if State = Done and then not (Within (X, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      if State = Done and then Is_Exact (X) and then not N.Is_Negative (X.Mid) and then not N.Is_Zero (X.Mid) then
         --  A power of ten: X or 1/X is 10 ** k, k at most its digits.
         Integer_Number (DECIMAL_BASE, Budget, Ten, State);
         Integer_Number (1, Budget, Probe, State);
         if State = Done then
            N.Compare (X.Mid, Probe, Order);
            Inverted := Order = Less;
         end if;
         for K in 0 .. Natural'Min (Natural'Last / 4, N.Numerator_Bits (X.Mid))
                       + Natural'Min (Natural'Last / 4, N.Denominator_Bits (X.Mid))
         loop
            exit when State /= Done;
            pragma Loop_Invariant (State = Done and then N.Within (Probe, Budget) and then N.Is_Reclaimed (Next)
                                   and then N.Within (Ten, Budget));
            pragma Loop_Invariant (Exponent in 0 .. Integer_64 (K));
            declare
               Target : Number;
            begin
               if Inverted then
                  Step (N.Multiply, X.Mid, Probe, Budget, Target, State);
                  if State = Done then
                     declare
                        Unit : Number;
                     begin
                        Integer_Number (1, Budget, Unit, State);
                        if State = Done then
                           N.Compare (Target, Unit, Order);
                        end if;
                        N.Free (Unit);
                     end;
                  end if;
               elsif State = Done then
                  N.Compare (X.Mid, Probe, Order);
               end if;
               N.Free (Target);
            end;
            exit when State /= Done or else Order /= (if Inverted then Less else Greater);
            if Order = Equal then
               exit;
            end if;
            Exponent := Exponent + 1;
            Step (N.Multiply, Probe, Ten, Budget, Next, State);
            N.Free (Probe);
            if State = Done then
               N.Move (Next, Probe);
            end if;
         end loop;
         N.Free (Ten);
         N.Free (Probe);
         N.Free (Next);
         if State = Done and then Order = Equal then
            Exact_Ball ((if Inverted then -Exponent else Exponent), Budget, Result, State);
            return;
         end if;
      end if;
      Ln_Ball (X, Digits_Wanted, Budget, Ln_X, State);
      Exact_Ball (DECIMAL_BASE, Budget, Ten_Ball, State);
      Ln_Ball (Ten_Ball, Digits_Wanted, Budget, Ln_Ten, State);
      Quotient (Ln_X, Ln_Ten, Budget, Result, State);
      Free (Ln_X);
      Free (Ln_Ten);
      Free (Ten_Ball);
   end Log10_Ball;

   ---------------------------------------------------------------------------
   --  The public operations.

   procedure Integer_Power
     (A : Ball; Exponent : Number; Budget : Size_Budget; Result : in out Ball; State : in out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Count : Natural;
      Abs_Exponent : Number;
      Square, Accumulated, Next, One : Ball;
   begin
      if State = Done and then not (Within (A, Budget) and then N.Within (Exponent, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      Step (N.Absolute, Exponent, Budget, Abs_Exponent, State);
      if State = Done and then not N.Fits_Natural (Abs_Exponent) then
         State := Too_Large;
      end if;
      Count := (if State = Done then N.To_Natural (Abs_Exponent) else 0);
      N.Free (Abs_Exponent);
      Exact_Ball (1, Budget, Accumulated, State);
      if State = Done then
         Copy (A, Square);
      end if;
      if State = Done and then not Within (Square, Budget) then
         State := Too_Large;  --  never: A is within the budget
      end if;
      while State = Done and then Count > 0 loop
         pragma Loop_Invariant (State = Done and then Within (Accumulated, Budget) and then Within (Square, Budget)
                                and then Is_Reclaimed (Next));
         pragma Loop_Variant (Decreases => Count);
         if Count mod 2 = 1 then
            Product (Accumulated, Square, Budget, Next, State);
            Free (Accumulated);
            if State = Done then
               Move (Next, Accumulated);
            end if;
         end if;
         Count := Count / 2;
         if State = Done and then Count > 0 then
            Product (Square, Square, Budget, Next, State);
            Free (Square);
            if State = Done then
               Move (Next, Square);
            end if;
         end if;
      end loop;
      if State = Done and then N.Is_Negative (Exponent) then
         Exact_Ball (1, Budget, One, State);
         Quotient (One, Accumulated, Budget, Result, State);
      elsif State = Done then
         Move (Accumulated, Result);
      end if;
      Free (Square);
      Free (Accumulated);
      Free (Next);
      Free (One);
   end Integer_Power;

   --  x ** y = exp (y ln x) for x > 0; for x < 0 and y = p/q exactly with q
   --  odd, (-1) ** p |x| ** y.
   procedure General_Power
     (A, B : Ball; Digits_Wanted : Precision_Digits; Budget : Size_Budget; Result : in out Ball;
      State : in out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Where, Exponent_Sign : Sign;
      Base, Logarithm, Scaled, Value : Ball;
      Den, Num, Two, Parity : Number;
      Odd_Root, Odd_Power : Boolean := False;
   begin
      if State = Done and then not (Within (A, Budget) and then Within (B, Budget)) then
         State := Too_Large;  --  a caller past the budget
      end if;
      Sign_Of (A, Budget, Where, State);
      if State /= Done then
         return;
      end if;
      case Where is
         when Zero =>
            Sign_Of (B, Budget, Exponent_Sign, State);
            if State = Done then
               case Exponent_Sign is
                  when Positive => Exact_Ball (0, Budget, Result, State);
                  when Zero => Exact_Ball (1, Budget, Result, State);
                  when Negative => State := Division_By_Zero;
                  when Straddles => State := Indeterminate;
               end case;
            end if;
            return;
         when Straddles =>
            State := Indeterminate;
            return;
         when Negative =>
            --  Only an exact p/q with q odd has a real power.
            if not Is_Exact (B) then
               State := Indeterminate;
               return;
            end if;
            Step (N.Denominator_Of, B.Mid, Budget, Den, State);
            Step (N.Numerator_Of, B.Mid, Budget, Num, State);
            Integer_Number (2, Budget, Two, State);
            Step (N.Modulo, Den, Two, Budget, Parity, State);
            Odd_Root := State = Done and then not N.Is_Zero (Parity);
            N.Free (Parity);
            Step (N.Modulo, Num, Two, Budget, Parity, State);
            Odd_Power := State = Done and then not N.Is_Zero (Parity);
            N.Free (Parity);
            N.Free (Den);
            N.Free (Num);
            N.Free (Two);
            if State = Done and then not Odd_Root then
               State := Outside_Domain;
               return;
            end if;
            Negate (A, Base);
         when Positive =>
            if State = Done then
               Copy (A, Base);
            end if;
      end case;
      Ln_Ball (Base, Digits_Wanted, Budget, Logarithm, State);
      Product (B, Logarithm, Budget, Scaled, State);
      Exp_Ball (Scaled, Digits_Wanted, Budget, Value, State);
      if State = Done and then Odd_Power then
         Negate (Value, Result);
      elsif State = Done then
         Move (Value, Result);
      end if;
      Free (Base);
      Free (Logarithm);
      Free (Scaled);
      Free (Value);
   end General_Power;

   procedure Apply
     (Operation : Arithmetic; A, B : Ball; Digits_Wanted : Precision_Digits; Budget : Size_Budget;
      Result : in out Ball; State : out Status)
   is
      Exact, Mid, Rad : Number;
      Outcome : N.Status;
   begin
      State := Done;
      case Operation is
         when Add => Sum (A, B, False, Budget, Result, State);
         when Subtract => Sum (A, B, True, Budget, Result, State);
         when Multiply => Product (A, B, Budget, Result, State);
         when Divide => Quotient (A, B, Budget, Result, State);
         when Power =>
            if Is_Exact (A) and then Is_Exact (B) then
               N.Apply (N.Power, A.Mid, B.Mid, Budget, Exact, Outcome);
               if Outcome = N.Done then
                  From_Exact (Exact, Result);
                  return;
               end if;
               N.Free (Exact);
               if Outcome /= N.Inexact then
                  State := Map (Outcome);
                  return;
               end if;
               General_Power (A, B, Digits_Wanted, Budget, Result, State);
            elsif Is_Exact (B) and then N.Is_Integer (B.Mid) then
               Integer_Power (A, B.Mid, Budget, Result, State);
            else
               General_Power (A, B, Digits_Wanted, Budget, Result, State);
            end if;
      end case;
      N.Free (Mid);
      N.Free (Rad);
   end Apply;

   procedure Absolute (X : Ball; Digits_Wanted : Precision_Digits; Budget : Size_Budget;
                       Result : in out Ball; State : out Status)
   is
      pragma Unreferenced (Digits_Wanted);
      Where : Sign;
      Abs_Mid, Upper, Two, Half, Mid_Copy, Rad_Copy : Number;
   begin
      State := Done;
      Sign_Of (X, Budget, Where, State);
      case Where is
         when Negative => Negate (X, Result);
         when Zero | Positive => Copy (X, Result);
         when Straddles =>
            --  |x| lies in [0, |m| + r]: the ball of that interval.
            Step (N.Absolute, X.Mid, Budget, Abs_Mid, State);
            Step (N.Add, Abs_Mid, X.Rad, Budget, Upper, State);
            Integer_Number (2, Budget, Two, State);
            Step (N.Divide, Upper, Two, Budget, Half, State);
            if State = Done then
               N.Copy (Half, Mid_Copy);
               N.Copy (Half, Rad_Copy);
            end if;
            Finish (Mid_Copy, Rad_Copy, Budget, Result, State);
      end case;
      if State /= Done then
         Free (Result);
      elsif not Within (Result, Budget) then
         Free (Result);  --  never: a copy keeps the size
         State := Too_Large;
      end if;
      N.Free (Abs_Mid);
      N.Free (Upper);
      N.Free (Two);
      N.Free (Half);
      N.Free (Mid_Copy);
      N.Free (Rad_Copy);
   end Absolute;

   procedure Round_To_Integer
     (Mode : Rounding; X : Ball; Budget : Size_Budget; Result : in out Ball; State : out Status)
   is
      Operation : constant N.Unary_Operation :=
        (case Mode is
            when Floor => N.Floor, when Ceiling => N.Ceiling, when Round => N.Round, when Truncate => N.Truncate);
      Low, High, Low_Value, High_Value : Number;
   begin
      State := Done;
      if Is_Exact (X) then
         Step (Operation, X.Mid, Budget, Low_Value, State);
         if State = Done then
            From_Exact (Low_Value, Result);
         end if;
         N.Free (Low_Value);
         return;
      end if;
      --  The same integer at both ends (each mode is monotone), or no
      --  answer at this precision.
      Step (N.Subtract, X.Mid, X.Rad, Budget, Low, State);
      Step (N.Add, X.Mid, X.Rad, Budget, High, State);
      Step (Operation, Low, Budget, Low_Value, State);
      Step (Operation, High, Budget, High_Value, State);
      if State = Done and then not N.Same (Low_Value, High_Value) then
         State := Indeterminate;
      end if;
      if State = Done then
         From_Exact (Low_Value, Result);
      end if;
      N.Free (Low);
      N.Free (High);
      N.Free (Low_Value);
      N.Free (High_Value);
   end Round_To_Integer;

   procedure Root
     (X, Degree : Ball; Digits_Wanted : Precision_Digits; Budget : Size_Budget; Result : in out Ball;
      State : out Status)
   is
      Exact : Number;
      Outcome : N.Status;
      Inverse : Ball;
      One : Ball;
   begin
      State := Done;
      if not Is_Exact (Degree) or else not N.Is_Integer (Degree.Mid) then
         State := Not_An_Integer;
         return;
      elsif N.Is_Negative (Degree.Mid) or else N.Is_Zero (Degree.Mid) then
         State := Outside_Domain;
         return;
      end if;
      if Is_Exact (X) then
         N.Apply (N.Root, X.Mid, Degree.Mid, Budget, Exact, Outcome);
         if Outcome = N.Done then
            From_Exact (Exact, Result);
            return;
         end if;
         N.Free (Exact);
         if Outcome /= N.Inexact then
            State := Map (Outcome);
            return;
         end if;
      end if;
      --  x ** (1/n), an odd root of a negative number negative.
      Exact_Ball (1, Budget, One, State);
      Quotient (One, Degree, Budget, Inverse, State);
      General_Power (X, Inverse, Digits_Wanted, Budget, Result, State);
      Free (One);
      Free (Inverse);
   end Root;

   procedure Evaluate
     (Function_Kind : Elementary; X : Ball; Unit : Angle_Unit; Digits_Wanted : Precision_Digits;
      Budget : Size_Budget; Result : in out Ball; State : out Status)
   is
   begin
      State := Done;
      case Function_Kind is
         when Square_Root => Sqrt_Ball (X, Digits_Wanted, Budget, Result, State);
         when Exponential =>
            if Is_Exact (X) then
               Exp_Exact (X.Mid, Digits_Wanted, Budget, Result, State);
            else
               Exp_Ball (X, Digits_Wanted, Budget, Result, State);
            end if;
         when Natural_Log => Ln_Ball (X, Digits_Wanted, Budget, Result, State);
         when Common_Log => Log10_Ball (X, Digits_Wanted, Budget, Result, State);
         when Sine | Cosine | Tangent => Trig (Function_Kind, X, Unit, Digits_Wanted, Budget, Result, State);
         when Arcsine | Arccosine | Arctangent =>
            Inverse_Trig (Function_Kind, X, Unit, Digits_Wanted, Budget, Result, State);
      end case;
   end Evaluate;

   procedure Make_Constant
     (Which : Constant_Value; Digits_Wanted : Precision_Digits; Budget : Size_Budget;
      Result : in out Ball; State : out Status)
   is
      One : Number;
   begin
      State := Done;
      case Which is
         when Pi => Pi_Ball (Work_Bits (Digits_Wanted), Budget, Result, State);
         when Euler =>
            Integer_Number (1, Budget, One, State);
            Exp_Exact (One, Digits_Wanted, Budget, Result, State);
            N.Free (One);
      end case;
   end Make_Constant;
end Calc_Balls;
