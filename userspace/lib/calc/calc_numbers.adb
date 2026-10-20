package body Calc_Numbers with SPARK_Mode is
   --  Freeing a local before it goes out of scope is how ownership ends;
   --  flow analysis sees no effect on the outputs.
   pragma Warnings (GNATprove, Off, "statement has no effect", Reason => "frees end ownership");
   pragma Warnings (GNATprove, Off, "is set by ""Free"" but not used", Reason => "frees end ownership");
   pragma Warnings (GNATprove, Off, "is set by ""Build"" but not used", Reason => "its parts are consumed");
   pragma Warnings (GNATprove, Off, "is set by ""Replace"" but not used", Reason => "the replaced value is freed");
   pragma Warnings (GNATprove, Off, "is set by ""Divide"" but not used", Reason => "only the quotient is kept");
   pragma Warnings (GNATprove, Off, "is set by ""Move"" but not used", Reason => "moves end ownership");
   DECIMAL_BASE : constant := 10;
   --  Extra digits the constants' series carry, so their truncation
   --  errors (a unit per term) stay below the last digit kept.
   GUARD_DIGITS : constant := 12;
   --  Machin: pi = 16 arctan (1/5) - 4 arctan (1/239).
   MACHIN_FIRST : constant := 5;
   MACHIN_SECOND : constant := 239;
   MACHIN_FIRST_WEIGHT : constant := 16;
   MACHIN_SECOND_WEIGHT : constant := 4;
   --  A decimal digit is under this many bits (log2 10 < 10/3): a bound
   --  for a power of ten's size before making it.
   BITS_PER_DIGIT_NUMERATOR : constant := 10;
   BITS_PER_DIGIT_DENOMINATOR : constant := 3;

   function Kind (X : Number) return Exactness is (X.Kind);
   function Is_Zero (X : Number) return Boolean is (Calc_Naturals.Is_Zero (X.Numerator));
   function Is_Negative (X : Number) return Boolean is
     (X.Negative and then not Calc_Naturals.Is_Zero (X.Numerator));
   function Is_Integer (X : Number) return Boolean is (Calc_Naturals.Is_Zero (X.Denominator));
   function Numerator_Bits (X : Number) return Bit_Count is (Bits (X.Numerator));
   function Denominator_Bits (X : Number) return Bit_Count is
     (if Calc_Naturals.Is_Zero (X.Denominator) then 1 else Bits (X.Denominator));
   function Same (A, B : Number) return Boolean is
     (A.Kind = B.Kind and then Is_Negative (A) = Is_Negative (B)
      and then Compare (A.Numerator, B.Numerator) = Equal
      and then Compare (A.Denominator, B.Denominator) = Equal);

   --  Bits of 10 ** Count, at most.
   function Decimal_Bits (Count : Natural) return Unsigned_64 is
     (Unsigned_64 (Count) * BITS_PER_DIGIT_NUMERATOR / BITS_PER_DIGIT_DENOMINATOR + 1);

   function Budget_Bits (Budget : Size_Budget) return Bit_Count is (Budget * LIMB_BITS);

   procedure Free (X : in out Number) is
   begin
      Free (X.Numerator);
      Free (X.Denominator);
      X.Kind := Exact;
      X.Negative := False;
   end Free;

   procedure Move (From, Into : in out Number) is
   begin
      Into.Kind := From.Kind;
      Into.Negative := From.Negative;
      Move (From.Numerator, Into.Numerator);
      Move (From.Denominator, Into.Denominator);
      From.Kind := Exact;
      From.Negative := False;
   end Move;

   procedure Copy (X : Number; Result : in out Number) is
   begin
      Result.Kind := X.Kind;
      Result.Negative := X.Negative;
      Copy (X.Numerator, Result.Numerator);
      Copy (X.Denominator, Result.Denominator);
   end Copy;

   procedure From_Integer (Value : Integer_64; Result : in out Number) is
      Size : constant Unsigned_64 :=
        (if Value >= 0 then Unsigned_64 (Value) else Unsigned_64 (-(Value + 1)) + 1);
   begin
      Result.Kind := Exact;
      Result.Negative := Value < 0;
      Make (Result.Numerator, Size);
   end From_Integer;

   procedure Parts (X : Number; Numerator, Denominator : in out Magnitude) is
   begin
      Copy (X.Numerator, Numerator);
      if Calc_Naturals.Is_Zero (X.Denominator) then
         Make (Denominator, 1);
      else
         Copy (X.Denominator, Denominator);
      end if;
      if Calc_Naturals.Is_Zero (Denominator) then
         Make (Denominator, 1);  --  never: a stored denominator is not zero
      end if;
   end Parts;

   --  The denominator as a magnitude (1 for an integer).
   procedure Denominator_Copy (X : Number; Result : in out Magnitude)
     with Pre => Calc_Naturals.Is_Zero (Result),
          Post => Length (Result) <= Natural'Max (Length (X.Denominator), 2)
                  and then not Calc_Naturals.Is_Zero (Result)
   is
   begin
      if Calc_Naturals.Is_Zero (X.Denominator) then
         Make (Result, 1);
      else
         Copy (X.Denominator, Result);
      end if;
      if Calc_Naturals.Is_Zero (Result) then
         Make (Result, 1);
      end if;
   end Denominator_Copy;

   --  Result := (-1 if Negative) * Num / Den in lowest terms. Num and Den
   --  are consumed (left zero). Too_Large when the reduced parts exceed the
   --  budget; Division_By_Zero for a zero Den.
   procedure Build
     (Negative : Boolean; Of_Kind : Exactness; Num, Den : in out Magnitude; Budget : Size_Budget;
      Result : in out Number; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Length (Num) < MAXIMUM_LIMBS and then Length (Den) < MAXIMUM_LIMBS,
          Post => Calc_Naturals.Is_Zero (Num) and then Calc_Naturals.Is_Zero (Den)
                  and then (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Common, Quotient, Remainder : Magnitude;
   begin
      if Calc_Naturals.Is_Zero (Den) then
         Free (Num);
         State := Division_By_Zero;
         return;
      end if;
      State := Done;
      Result.Kind := Of_Kind;
      Result.Negative := False;
      if Calc_Naturals.Is_Zero (Num) then
         Free (Den);
         return;
      end if;
      Greatest_Common_Divisor (Num, Den, Common);
      if not Calc_Naturals.Is_Zero (Common) and then not Is_One (Common) then
         Divide (Num, Common, Quotient, Remainder);
         Free (Remainder);
         Replace (Num, Quotient);
         Divide (Den, Common, Quotient, Remainder);
         Free (Remainder);
         Replace (Den, Quotient);
      end if;
      Free (Common);
      if Is_One (Den) then
         Free (Den);
      end if;
      if Length (Num) > Budget or else Length (Den) > Budget then
         Free (Num);
         Free (Den);
         State := Too_Large;
         return;
      end if;
      Result.Negative := Negative;
      Move (Num, Result.Numerator);
      Move (Den, Result.Denominator);
   end Build;

   procedure Ten_To (Count : Natural; Budget : Size_Budget; Result : in out Magnitude; Fits : out Boolean)
     with Pre => Calc_Naturals.Is_Zero (Result),
          Post => (if Fits then Length (Result) <= Budget + 1 and then not Calc_Naturals.Is_Zero (Result)
                   else Calc_Naturals.Is_Zero (Result))
   is
      Ten : Magnitude;
   begin
      Make (Ten, DECIMAL_BASE);
      Power (Ten, Count, Budget_Bits (Budget), Result, Fits);
      Free (Ten);
      if Fits and then Calc_Naturals.Is_Zero (Result) then
         Fits := False;  --  never: a power of ten is not zero
      end if;
   end Ten_To;

   procedure From_Decimal
     (Whole, Fraction : String; Exponent : Decimal_Exponent; Budget : Size_Budget;
      Result : in out Number; State : out Status)
   is
      Num, Den, Scale, Next : Magnitude;
      Fits : Boolean;
      --  Value = Whole & Fraction * 10 ** Shift.
      Shift : constant Integer_64 := Exponent - Integer_64 (Fraction'Length);
      Limit : constant Integer_64 := Integer_64 (Budget_Bits (Budget));
   begin
      if Shift > Limit or else Shift < -Limit then
         --  10 ** |Shift| alone has more bits than the budget allows.
         State := (if Shift > 0 or else (for some C of Whole => C /= '0')
                     or else (for some C of Fraction => C /= '0') then Too_Large else Done);
         if State = Done then
            Result.Kind := Exact;
            Result.Negative := False;
         end if;
         return;
      end if;
      if Whole'Length + Fraction'Length > Budget * 10 then
         State := Too_Large;
         return;
      end if;
      declare
         --  Both parts' digits, on the stack.
         Joined : String (1 .. Whole'Length + Fraction'Length) := [others => '0'];
      begin
         Joined (1 .. Whole'Length) := Whole;
         Joined (Whole'Length + 1 .. Joined'Last) := Fraction;
         From_Decimal (Joined, Num);
      end;
      Make (Den, 1);
      Ten_To (Natural (abs Shift), Budget, Scale, Fits);
      if not Fits then
         Free (Num);
         Free (Den);
         State := Too_Large;
         return;
      end if;
      if Length (Num) + Length (Scale) > MAXIMUM_LIMBS / 2 then
         Free (Num);
         Free (Den);
         Free (Scale);
         State := Too_Large;
         return;
      end if;
      if Shift >= 0 then
         Multiply (Num, Scale, Next);
         Replace (Num, Next);
      else
         Replace (Den, Scale);
      end if;
      Free (Scale);
      Build (False, Exact, Num, Den, Budget, Result, State);
   end From_Decimal;

   --  a/b + (-1 if Flip) c/d.
   procedure Sum
     (A, B : Number; Flip : Boolean; Budget : Size_Budget; Result : in out Number; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Within (A, Budget) and then Within (B, Budget),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      A_Den, B_Den, Left, Right, Num, Den : Magnitude;
      A_Negative : constant Boolean := Is_Negative (A);
      B_Negative : constant Boolean := Is_Negative (B) /= Flip and then not Is_Zero (B);
      Negative : Boolean;
   begin
      Denominator_Copy (A, A_Den);
      Denominator_Copy (B, B_Den);
      Multiply (A.Numerator, B_Den, Left);
      Multiply (B.Numerator, A_Den, Right);
      Multiply (A_Den, B_Den, Den);
      Free (A_Den);
      Free (B_Den);
      Negative := A_Negative;
      if A_Negative = B_Negative then
         Add (Left, Right, Num);
      elsif Compare (Left, Right) /= Less then
         Subtract (Left, Right, Num);
      elsif Compare (Right, Left) /= Less then
         Subtract (Right, Left, Num);
         Negative := B_Negative;
      end if;
      Free (Left);
      Free (Right);
      Build (Negative, (if A.Kind = Approximate or else B.Kind = Approximate then Approximate else Exact),
             Num, Den, Budget, Result, State);
   end Sum;

   --  (a/b) * (c/d), or (a/b) / (c/d) when Invert.
   procedure Product
     (A, B : Number; Invert : Boolean; Budget : Size_Budget; Result : in out Number; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Within (A, Budget) and then Within (B, Budget),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      A_Den, B_Den, Num, Den : Magnitude;
   begin
      if Invert and then Is_Zero (B) then
         State := Division_By_Zero;
         return;
      end if;
      Denominator_Copy (A, A_Den);
      Denominator_Copy (B, B_Den);
      if Invert then
         Multiply (A.Numerator, B_Den, Num);
         Multiply (A_Den, B.Numerator, Den);
      else
         Multiply (A.Numerator, B.Numerator, Num);
         Multiply (A_Den, B_Den, Den);
      end if;
      Free (A_Den);
      Free (B_Den);
      Build (Is_Negative (A) /= Is_Negative (B),
             (if A.Kind = Approximate or else B.Kind = Approximate then Approximate else Exact),
             Num, Den, Budget, Result, State);
   end Product;

   type Rounding is (Toward_Negative, Toward_Positive, Toward_Zero, Nearest_Away);

   --  X rounded to an integer.
   procedure Round_Integer
     (X : Number; Mode : Rounding; Budget : Size_Budget; Result : in out Number; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Within (X, Budget),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Den, Quotient, Remainder, Twice, One, Next : Magnitude;
      Negative : constant Boolean := Is_Negative (X);
      Up : Boolean;
   begin
      Denominator_Copy (X, Den);
      Divide (X.Numerator, Den, Quotient, Remainder);
      if Calc_Naturals.Is_Zero (Remainder) then
         Up := False;
      else
         case Mode is
            when Toward_Negative => Up := Negative;
            when Toward_Positive => Up := not Negative;
            when Toward_Zero => Up := False;
            when Nearest_Away =>
               Add (Remainder, Remainder, Twice);
               Up := Compare (Twice, Den) /= Less;
               Free (Twice);
         end case;
      end if;
      Free (Remainder);
      Free (Den);
      if Up then
         Make (One, 1);
         Add (Quotient, One, Next);
         Free (One);
         Replace (Quotient, Next);
      end if;
      Make (Den, 1);
      Build (Negative, X.Kind, Quotient, Den, Budget, Result, State);
   end Round_Integer;

   --  X's value as a Natural, when X is a non-negative integer that fits.
   procedure Natural_Of (X : Number; Value : out Natural; State : out Status) is
   begin
      Value := 0;
      if not Is_Integer (X) then
         State := Not_An_Integer;
      elsif Is_Negative (X) then
         State := Outside_Domain;
      elsif not Fits_Natural (X.Numerator) then
         State := Too_Large;
      else
         Value := To_Natural (X.Numerator);
         State := Done;
      end if;
   end Natural_Of;

   procedure Raise_To
     (A, B : Number; Budget : Size_Budget; Result : in out Number; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Within (A, Budget) and then Within (B, Budget),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Num, Den, Base_Den : Magnitude;
      Exponent : Natural;
      Fits_Num, Fits_Den : Boolean;
      Inverse : constant Boolean := Is_Negative (B);
      Odd : Boolean;
      Of_Kind : constant Exactness :=
        (if A.Kind = Approximate or else B.Kind = Approximate then Approximate else Exact);
   begin
      if not Is_Integer (B) then
         State := Fractional_Exponent;
         return;
      end if;
      Odd := not Is_Even (B.Numerator);
      if Is_Zero (A) then
         if Inverse then
            State := Division_By_Zero;
         else
            --  0 ** 0 = 1; 0 ** n = 0.
            if Is_Zero (B) then
               Make (Num, 1);
            end if;
            Make (Den, 1);
            Build (False, Of_Kind, Num, Den, Budget, Result, State);
         end if;
         return;
      end if;
      if Is_Integer (A) and then Is_One (A.Numerator) then
         --  (+-1) ** n, for any n.
         Make (Num, 1);
         Make (Den, 1);
         Build (Is_Negative (A) and then Odd, Of_Kind, Num, Den, Budget, Result, State);
         return;
      end if;
      if not Fits_Natural (B.Numerator) then
         State := Too_Large;
         return;
      end if;
      Exponent := To_Natural (B.Numerator);
      Denominator_Copy (A, Base_Den);
      Power (A.Numerator, Exponent, Budget_Bits (Budget), Num, Fits_Num);
      Power (Base_Den, Exponent, Budget_Bits (Budget), Den, Fits_Den);
      Free (Base_Den);
      if not Fits_Num or else not Fits_Den then
         Free (Num);
         Free (Den);
         State := Too_Large;
         return;
      end if;
      if Inverse then
         Build (Is_Negative (A) and then Odd, Of_Kind, Den, Num, Budget, Result, State);
      else
         Build (Is_Negative (A) and then Odd, Of_Kind, Num, Den, Budget, Result, State);
      end if;
   end Raise_To;

   --  a - b * floor (a / b).
   procedure Modulo_Of
     (A, B : Number; Budget : Size_Budget; Result : in out Number; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Within (A, Budget) and then Within (B, Budget),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Ratio, Whole, Scaled : Number;
   begin
      Product (A, B, True, Budget, Ratio, State);
      if State /= Done then
         return;
      end if;
      Round_Integer (Ratio, Toward_Negative, Budget, Whole, State);
      Free (Ratio);
      if State /= Done then
         return;
      end if;
      Product (B, Whole, False, Budget, Scaled, State);
      Free (Whole);
      if State /= Done then
         return;
      end if;
      Sum (A, Scaled, True, Budget, Result, State);
      Free (Scaled);
   end Modulo_Of;

   procedure Integer_Divisor
     (A, B : Number; Multiple : Boolean; Budget : Size_Budget; Result : in out Number; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Within (A, Budget) and then Within (B, Budget),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Common, Num, Den, Quotient, Remainder : Magnitude;
   begin
      if not Is_Integer (A) or else not Is_Integer (B) then
         State := Not_An_Integer;
         return;
      end if;
      Greatest_Common_Divisor (A.Numerator, B.Numerator, Common);
      if Multiple then
         --  lcm (a, b) = |a b| / gcd (a, b); 0 when either is 0.
         if Calc_Naturals.Is_Zero (Common) then
            Make (Den, 1);
            Build (False, Exact, Num, Den, Budget, Result, State);
            return;
         end if;
         Divide (A.Numerator, Common, Quotient, Remainder);
         Free (Remainder);
         Free (Common);
         Multiply (Quotient, B.Numerator, Num);
         Free (Quotient);
      else
         Move (Common, Num);
      end if;
      Make (Den, 1);
      Build (False, Exact, Num, Den, Budget, Result, State);
   end Integer_Divisor;

   procedure Factorial_Of (X : Number; Budget : Size_Budget; Result : in out Number; State : out Status)
     with Pre => Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Count : Natural;
      Accumulated, Factor, Next, Den : Magnitude;
   begin
      Natural_Of (X, Count, State);
      if State /= Done then
         return;
      end if;
      --  n! < n ** n: at most n * bits (n) bits.
      if Unsigned_64 (Count) * Unsigned_64 (Bits_Of (Limb (Count))) > Unsigned_64 (Budget_Bits (Budget)) then
         State := Too_Large;
         return;
      end if;
      Make (Accumulated, 1);
      if Length (Accumulated) > Budget then
         Free (Accumulated);  --  never: a budget holds at least a limb
         State := Too_Large;
         return;
      end if;
      for K in 2 .. Count loop
         pragma Loop_Invariant (Calc_Naturals.Is_Zero (Factor) and then Calc_Naturals.Is_Zero (Next)
                                and then Calc_Naturals.Is_Zero (Den) and then Is_Reclaimed (Result));
         pragma Loop_Invariant (Length (Accumulated) <= Budget);
         Make (Factor, Unsigned_64 (K));
         Multiply (Accumulated, Factor, Next);
         Free (Factor);
         Replace (Accumulated, Next);
         --  Never taken after the bits check.
         if Length (Accumulated) > Budget then
            Free (Accumulated);
            State := Too_Large;
            return;
         end if;
      end loop;
      Make (Den, 1);
      Build (False, X.Kind, Accumulated, Den, Budget, Result, State);
   end Factorial_Of;

   procedure Root_Of
     (X : Number; Budget : Size_Budget; Precision : Approximate_Digits;
      Result : in out Number; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Within (X, Budget),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Den, Num_Root, Den_Root, Check, Scale, Scaled, Inner, Root, Out_Den : Magnitude;
      Fits : Boolean;
      Exact_Root : Boolean;
   begin
      if Is_Negative (X) then
         State := Outside_Domain;
         return;
      end if;
      Denominator_Copy (X, Den);
      --  Exact when numerator and denominator are both squares.
      Square_Root (X.Numerator, Num_Root);
      Square_Root (Den, Den_Root);
      Multiply (Num_Root, Num_Root, Check);
      Exact_Root := Compare (Check, X.Numerator) = Equal;
      Free (Check);
      Multiply (Den_Root, Den_Root, Check);
      Exact_Root := Exact_Root and then Compare (Check, Den) = Equal;
      Free (Check);
      if Exact_Root then
         Free (Den);
         Build (False, X.Kind, Num_Root, Den_Root, Budget, Result, State);
         return;
      end if;
      Free (Num_Root);
      Free (Den_Root);
      --  sqrt (p/q) = sqrt (p q 10 ** 2D) / (q 10 ** D), the root floored.
      if Decimal_Bits (2 * Precision) > Unsigned_64 (Budget_Bits (Budget)) then
         Free (Den);
         State := Too_Large;
         return;
      end if;
      Ten_To (2 * Precision, Budget, Scale, Fits);
      if not Fits then
         Free (Den);
         State := Too_Large;
         return;
      end if;
      Multiply (X.Numerator, Den, Inner);
      if Length (Inner) + Length (Scale) > MAXIMUM_LIMBS - 9 then
         Free (Inner);
         Free (Scale);
         Free (Den);
         State := Too_Large;
         return;
      end if;
      Multiply (Inner, Scale, Scaled);
      Free (Inner);
      Free (Scale);
      Square_Root (Scaled, Root);
      Free (Scaled);
      Ten_To (Precision, Budget, Scale, Fits);
      if not Fits then
         Free (Root);
         Free (Den);
         State := Too_Large;
         return;
      end if;
      Multiply (Den, Scale, Out_Den);
      Free (Den);
      Free (Scale);
      Build (False, Approximate, Root, Out_Den, Budget, Result, State);
   end Root_Of;

   procedure Apply
     (Operation : Unary_Operation; X : Number; Budget : Size_Budget; Precision : Approximate_Digits;
      Result : in out Number; State : out Status)
   is
      Num, Den : Magnitude;
   begin
      case Operation is
         when Negate | Absolute =>
            Copy (X, Result);
            Result.Negative := (if Operation = Negate then not X.Negative else False)
              and then not Calc_Naturals.Is_Zero (X.Numerator);
            State := Done;
         when Floor => Round_Integer (X, Toward_Negative, Budget, Result, State);
         when Ceiling => Round_Integer (X, Toward_Positive, Budget, Result, State);
         when Truncate => Round_Integer (X, Toward_Zero, Budget, Result, State);
         when Round => Round_Integer (X, Nearest_Away, Budget, Result, State);
         when Factorial => Factorial_Of (X, Budget, Result, State);
         when Square_Root => Root_Of (X, Budget, Precision, Result, State);
         when Numerator_Of =>
            Copy (X.Numerator, Num);
            Make (Den, 1);
            Build (Is_Negative (X), X.Kind, Num, Den, Budget, Result, State);
         when Denominator_Of =>
            Denominator_Copy (X, Num);
            Make (Den, 1);
            Build (False, X.Kind, Num, Den, Budget, Result, State);
      end case;
   end Apply;

   procedure Rounded_To
     (A, B : Number; Budget : Size_Budget; Result : in out Number; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Within (A, Budget) and then Within (B, Budget),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Places : Natural;
      Scale_Magnitude, Unit : Magnitude;
      Scale, Scaled, Whole : Number;
      Fits : Boolean;
   begin
      Natural_Of (B, Places, State);
      if State /= Done then
         return;
      end if;
      Ten_To (Places, Budget, Scale_Magnitude, Fits);
      if not Fits then
         Free (Scale_Magnitude);
         State := Too_Large;
         return;
      end if;
      Make (Unit, 1);
      Build (False, Exact, Scale_Magnitude, Unit, Budget, Scale, State);
      if State /= Done then
         return;
      end if;
      Product (A, Scale, False, Budget, Scaled, State);
      if State /= Done then
         Free (Scale);
         return;
      end if;
      Round_Integer (Scaled, Nearest_Away, Budget, Whole, State);
      Free (Scaled);
      if State /= Done then
         Free (Scale);
         return;
      end if;
      Product (Whole, Scale, True, Budget, Result, State);
      Free (Whole);
      Free (Scale);
   end Rounded_To;

   procedure Apply
     (Operation : Binary_Operation; A, B : Number; Budget : Size_Budget;
      Result : in out Number; State : out Status)
   is
   begin
      case Operation is
         when Add => Sum (A, B, False, Budget, Result, State);
         when Subtract => Sum (A, B, True, Budget, Result, State);
         when Multiply => Product (A, B, False, Budget, Result, State);
         when Divide => Product (A, B, True, Budget, Result, State);
         when Power => Raise_To (A, B, Budget, Result, State);
         when Modulo =>
            if Is_Zero (B) then
               State := Division_By_Zero;
            else
               Modulo_Of (A, B, Budget, Result, State);
            end if;
         when Greatest_Common_Divisor => Integer_Divisor (A, B, False, Budget, Result, State);
         when Least_Common_Multiple => Integer_Divisor (A, B, True, Budget, Result, State);
         when Round_To => Rounded_To (A, B, Budget, Result, State);
      end case;
   end Apply;

   --  X := X / Divisor (a small divisor).
   procedure Divide_By (X : in out Magnitude; Divisor : Unsigned_64)
     with Pre => Divisor /= 0 and then Length (X) < MAXIMUM_LIMBS,
          Post => Length (X) <= Length (X)'Old
   is
      D, Quotient, Remainder : Magnitude;
   begin
      Make (D, Divisor);
      if Calc_Naturals.Is_Zero (D) then
         return;  --  never: Divisor is not zero
      end if;
      Divide (X, D, Quotient, Remainder);
      Free (D);
      Free (Remainder);
      Replace (X, Quotient);
   end Divide_By;

   --  Into := Into + X.
   procedure Add_To (Into : in out Magnitude; X : Magnitude; Limit : Limb_Count)
     with Pre => Length (Into) <= Limit and then Length (X) <= Limit and then Limit < MAXIMUM_LIMBS - 1,
          Post => Length (Into) <= Limit + 1
   is
      Next : Magnitude;
   begin
      Add (Into, X, Next);
      Replace (Into, Next);
   end Add_To;

   --  Scale * arctan (1 / Base), truncated at each term.
   procedure Arctangent_Inverse
     (Base : Unsigned_64; Scale : Magnitude; Result : in out Magnitude)
     with Pre => Calc_Naturals.Is_Zero (Result) and then Base > 1 and then Base <= 2 ** 16
                 and then Length (Scale) < MAXIMUM_LIMBS / 4,
          Post => Length (Result) <= Length (Scale) + 1
   is
      Term, Piece, Positive_Sum, Negative_Sum : Magnitude;
      Odd : Unsigned_64 := 1;
      Subtracting : Boolean := False;
   begin
      Copy (Scale, Term);
      Divide_By (Term, Base);
      Copy (Term, Positive_Sum);
      loop
         pragma Loop_Invariant (Calc_Naturals.Is_Zero (Piece) and then Calc_Naturals.Is_Zero (Result));
         pragma Loop_Invariant (Length (Term) <= Length (Scale) and then Length (Positive_Sum) <= Length (Scale)
                                and then Length (Negative_Sum) <= Length (Scale));
         Divide_By (Term, Base * Base);
         exit when Calc_Naturals.Is_Zero (Term) or else Odd >= Unsigned_64'Last - 2;
         Odd := Odd + 2;
         Subtracting := not Subtracting;
         Copy (Term, Piece);
         Divide_By (Piece, Odd);
         --  Each term is smaller than the first: the sums stay at Scale's
         --  size (the guard below is never taken).
         if Subtracting then
            Add_To (Negative_Sum, Piece, Length (Scale));
            exit when Length (Negative_Sum) > Length (Scale);
         else
            Add_To (Positive_Sum, Piece, Length (Scale));
            exit when Length (Positive_Sum) > Length (Scale);
         end if;
         Free (Piece);
      end loop;
      Free (Piece);
      Free (Term);
      if Compare (Positive_Sum, Negative_Sum) /= Less then
         Subtract (Positive_Sum, Negative_Sum, Result);
      end if;
      Free (Positive_Sum);
      Free (Negative_Sum);
   end Arctangent_Inverse;

   procedure Make_Constant
     (Which : Constant_Value; Precision : Approximate_Digits; Budget : Size_Budget;
      Result : in out Number; State : out Status)
   is
      Scale, Value, Guard, Den, Term, Second, Next, Weight : Magnitude;
      Fits : Boolean;
      Count : Unsigned_64 := 0;
   begin
      if Decimal_Bits (Precision + GUARD_DIGITS) > Unsigned_64 (Budget_Bits (Budget)) then
         State := Too_Large;
         return;
      end if;
      Ten_To (Precision + GUARD_DIGITS, Budget, Scale, Fits);
      if not Fits or else Length (Scale) >= MAXIMUM_LIMBS / 4 - 2 then
         Free (Scale);
         State := Too_Large;
         return;
      end if;
      case Which is
         when Pi =>
            Arctangent_Inverse (MACHIN_FIRST, Scale, Value);
            Arctangent_Inverse (MACHIN_SECOND, Scale, Second);
            Make (Weight, MACHIN_FIRST_WEIGHT);
            Multiply (Value, Weight, Next);
            Free (Weight);
            Replace (Value, Next);
            Make (Weight, MACHIN_SECOND_WEIGHT);
            Multiply (Second, Weight, Next);
            Free (Weight);
            Replace (Second, Next);
            if Compare (Value, Second) /= Less then
               Subtract (Value, Second, Next);
               Replace (Value, Next);
            end if;
            Free (Second);
         when Euler =>
            --  e = sum 1/k!, each term Scale/k! truncated.
            Copy (Scale, Term);
            Copy (Scale, Value);
            loop
               pragma Loop_Invariant (Calc_Naturals.Is_Zero (Next) and then Calc_Naturals.Is_Zero (Second)
                                      and then Calc_Naturals.Is_Zero (Weight)
                                      and then Calc_Naturals.Is_Zero (Den) and then Calc_Naturals.Is_Zero (Guard));
               pragma Loop_Invariant (Length (Term) <= Length (Scale) and then Length (Value) <= Length (Scale) + 1);
               exit when Count >= Unsigned_64'Last - 1;
               Count := Count + 1;
               Divide_By (Term, Count);
               exit when Calc_Naturals.Is_Zero (Term);
               Add_To (Value, Term, Length (Scale) + 1);
               --  e < 3: the sum stays at Scale's size (never taken).
               exit when Length (Value) > Length (Scale) + 1;
            end loop;
            Free (Term);
      end case;
      Free (Scale);
      --  Drop the guard digits: Value / 10 ** GUARD over 10 ** Precision.
      Ten_To (GUARD_DIGITS, Budget, Guard, Fits);
      if not Fits or else Length (Value) >= MAXIMUM_LIMBS - 1 then
         Free (Guard);
         Free (Value);
         State := Too_Large;
         return;
      end if;
      Divide (Value, Guard, Next, Term);
      Free (Term);
      Free (Guard);
      Free (Value);
      Ten_To (Precision, Budget, Den, Fits);
      if not Fits then
         Free (Next);
         State := Too_Large;
         return;
      end if;
      Build (False, Approximate, Next, Den, Budget, Result, State);
   end Make_Constant;
end Calc_Numbers;
