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

   function Is_Zero (X : Number) return Boolean is (Calc_Naturals.Is_Zero (X.Numerator));
   function Is_Negative (X : Number) return Boolean is
     (X.Negative and then not Calc_Naturals.Is_Zero (X.Numerator));
   function Is_Integer (X : Number) return Boolean is (Calc_Naturals.Is_Zero (X.Denominator));
   function Numerator_Bits (X : Number) return Bit_Count is (Bits (X.Numerator));
   function Denominator_Bits (X : Number) return Bit_Count is
     (if Calc_Naturals.Is_Zero (X.Denominator) then 1 else Bits (X.Denominator));
   function Same (A, B : Number) return Boolean is
     (Is_Negative (A) = Is_Negative (B)
      and then Compare (A.Numerator, B.Numerator) = Equal
      and then Compare (A.Denominator, B.Denominator) = Equal);

   function Budget_Bits (Budget : Size_Budget) return Bit_Count is (Budget * LIMB_BITS);

   procedure Denominator_Copy (X : Number; Result : in out Magnitude)
     with Pre => Calc_Naturals.Is_Zero (Result),
          Post => Length (Result) <= Natural'Max (Length (X.Denominator), 2)
                  and then not Calc_Naturals.Is_Zero (Result);

   procedure Compare (A, B : Number; Order : out Ordering) is
      A_Den, B_Den, Left, Right : Magnitude;
      A_Negative : constant Boolean := Is_Negative (A);
   begin
      if A_Negative /= Is_Negative (B) then
         Order := (if A_Negative then Less else Greater);
         return;
      end if;
      Denominator_Copy (A, A_Den);
      Denominator_Copy (B, B_Den);
      Multiply (A.Numerator, B_Den, Left);
      Multiply (B.Numerator, A_Den, Right);
      Order := Compare (Left, Right);
      if A_Negative then
         Order := (case Order is when Less => Greater, when Greater => Less, when Equal => Equal);
      end if;
      Free (A_Den);
      Free (B_Den);
      Free (Left);
      Free (Right);
   end Compare;

   function Fits_Natural (X : Number) return Boolean is
     (Is_Integer (X) and then not Is_Negative (X) and then Fits_Natural (X.Numerator));
   function To_Natural (X : Number) return Natural is (To_Natural (X.Numerator));

   procedure Free (X : in out Number) is
   begin
      Free (X.Numerator);
      Free (X.Denominator);
      X.Negative := False;
   end Free;

   procedure Move (From, Into : in out Number) is
   begin
      Into.Negative := From.Negative;
      Move (From.Numerator, Into.Numerator);
      Move (From.Denominator, Into.Denominator);
      From.Negative := False;
   end Move;

   procedure Copy (X : Number; Result : in out Number) is
   begin
      Result.Negative := X.Negative;
      Copy (X.Numerator, Result.Numerator);
      Copy (X.Denominator, Result.Denominator);
   end Copy;

   procedure Copy_Negated (X : Number; Result : in out Number) is
   begin
      Copy (X, Result);
      --  Zero keeps no sign.
      Result.Negative := not X.Negative and then not Calc_Naturals.Is_Zero (X.Numerator);
   end Copy_Negated;

   procedure From_Integer (Value : Integer_64; Result : in out Number) is
      Size : constant Unsigned_64 :=
        (if Value >= 0 then Unsigned_64 (Value) else Unsigned_64 (-(Value + 1)) + 1);
   begin
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

   procedure Denominator_Copy (X : Number; Result : in out Magnitude)
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
     (Negative : Boolean; Num, Den : in out Magnitude; Budget : Size_Budget;
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

   procedure From_Parts
     (Negative : Boolean; Num, Den : in out Magnitude; Budget : Size_Budget;
      Result : in out Number; State : out Status)
   is
   begin
      Build (Negative, Num, Den, Budget, Result, State);
   end From_Parts;

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
      Build (False, Num, Den, Budget, Result, State);
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
      Build (Negative, Num, Den, Budget, Result, State);
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
      Build (Is_Negative (A) /= Is_Negative (B), Num, Den, Budget, Result, State);
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
      Build (Negative, Quotient, Den, Budget, Result, State);
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

   procedure Root_Of
     (X : Number; Degree : Positive; Budget : Size_Budget; Result : in out Number; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Within (X, Budget),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result));

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
   begin
      if not Is_Integer (B) then
         --  a ** (p/q) = (a ** (1/q)) ** p when the q-th root is exact.
         if not Fits_Natural (B.Denominator) or else To_Natural (B.Denominator) = 0 then
            State := Inexact;
            return;
         end if;
         declare
            Base, Whole : Number;
         begin
            Root_Of (A, To_Natural (B.Denominator), Budget, Base, State);
            if State /= Done then
               return;
            end if;
            Copy (B.Numerator, Num);
            Make (Den, 1);
            Build (Is_Negative (B), Num, Den, Budget, Whole, State);
            if State = Done then
               Raise_To (Base, Whole, Budget, Result, State);
            end if;
            Free (Base);
            Free (Whole);
         end;
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
            Build (False, Num, Den, Budget, Result, State);
         end if;
         return;
      end if;
      if Is_Integer (A) and then Is_One (A.Numerator) then
         --  (+-1) ** n, for any n.
         Make (Num, 1);
         Make (Den, 1);
         Build (Is_Negative (A) and then Odd, Num, Den, Budget, Result, State);
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
         Build (Is_Negative (A) and then Odd, Den, Num, Budget, Result, State);
      else
         Build (Is_Negative (A) and then Odd, Num, Den, Budget, Result, State);
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
            Build (False, Num, Den, Budget, Result, State);
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
      Build (False, Num, Den, Budget, Result, State);
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
      Build (False, Accumulated, Den, Budget, Result, State);
   end Factorial_Of;

   --  The exact Degree-th root of X, or Inexact; an odd root of a negative
   --  number is negative, an even one Outside_Domain.
   procedure Root_Of
     (X : Number; Degree : Positive; Budget : Size_Budget; Result : in out Number; State : out Status)
   is
      Den, Num_Root, Den_Root, Check : Magnitude;
      Fits, Exact_Root : Boolean;
   begin
      if Is_Negative (X) and then Degree mod 2 = 0 then
         State := Outside_Domain;
         return;
      end if;
      Denominator_Copy (X, Den);
      if Length (Den) >= MAXIMUM_LIMBS / 4 or else Length (X.Numerator) >= MAXIMUM_LIMBS / 4 then
         Free (Den);  --  never: a denominator of a number within the budget
         State := Too_Large;
         return;
      end if;
      --  Exact when numerator and denominator are both Degree-th powers.
      Integer_Root (X.Numerator, Degree, Num_Root);
      Integer_Root (Den, Degree, Den_Root);
      Power (Num_Root, Degree, Bits (X.Numerator) + 1, Check, Fits);
      Exact_Root := Fits and then Compare (Check, X.Numerator) = Equal;
      Free (Check);
      Power (Den_Root, Degree, Bits (Den) + 1, Check, Fits);
      Exact_Root := Exact_Root and then Fits and then Compare (Check, Den) = Equal;
      Free (Check);
      Free (Den);
      if not Exact_Root then
         Free (Num_Root);
         Free (Den_Root);
         State := Inexact;
         return;
      end if;
      Build (Is_Negative (X), Num_Root, Den_Root, Budget, Result, State);
   end Root_Of;

   --  The magnitude of a nonnegative integer, or of -X - 1 for a negative
   --  one: X in infinite two's complement is that magnitude, or its
   --  complement.
   procedure Complement_Of (X : Number; Result : in out Magnitude)
     with Pre => Calc_Naturals.Is_Zero (Result) and then Length (X.Numerator) < MAXIMUM_LIMBS,
          Post => Length (Result) <= Length (X.Numerator)
   is
      One : Magnitude;
   begin
      if Is_Negative (X) then
         Make (One, 1);
         if Compare (X.Numerator, One) /= Less then
            Subtract (X.Numerator, One, Result);
         end if;
         Free (One);
      else
         Copy (X.Numerator, Result);
      end if;
   end Complement_Of;

   --  The integer whose two's complement is Mag (Inverted: ~Mag = -Mag - 1).
   procedure From_Complement
     (Mag : in out Magnitude; Inverted : Boolean; Budget : Size_Budget; Result : in out Number; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Length (Mag) < MAXIMUM_LIMBS - 1,
          Post => Calc_Naturals.Is_Zero (Mag)
                  and then (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      One, Next, Den : Magnitude;
   begin
      if Inverted then
         Make (One, 1);
         Add (Mag, One, Next);
         Free (One);
         Replace (Mag, Next);
      end if;
      Make (Den, 1);
      Build (Inverted, Mag, Den, Budget, Result, State);
   end From_Complement;

   procedure Bitwise_Of
     (Operation : Bitwise_Operation; A, B : Number; Budget : Size_Budget; Result : in out Number; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Within (A, Budget) and then Within (B, Budget),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Left, Right, Bits_Out : Magnitude;
      Left_Not : constant Boolean := Is_Negative (A);
      Right_Not : constant Boolean := Is_Negative (B);
      Inverted : Boolean;
   begin
      if not Is_Integer (A) or else not Is_Integer (B) then
         State := Not_An_Integer;
         return;
      end if;
      Complement_Of (A, Left);
      Complement_Of (B, Right);
      --  With x = ~a for a negative x: a & ~b, ~(a | b), ~(b & ~a), ...
      case Operation is
         when Both =>
            Inverted := Left_Not and then Right_Not;
            if Inverted then
               Bitwise (Either, Left, Right, Bits_Out);
            elsif Right_Not then
               Bitwise (First_Only, Left, Right, Bits_Out);
            elsif Left_Not then
               Bitwise (First_Only, Right, Left, Bits_Out);
            else
               Bitwise (Both, Left, Right, Bits_Out);
            end if;
         when Either =>
            Inverted := Left_Not or else Right_Not;
            if Left_Not and then Right_Not then
               Bitwise (Both, Left, Right, Bits_Out);
            elsif Right_Not then
               Bitwise (First_Only, Right, Left, Bits_Out);
            elsif Left_Not then
               Bitwise (First_Only, Left, Right, Bits_Out);
            else
               Bitwise (Either, Left, Right, Bits_Out);
            end if;
         when Differ | First_Only =>
            Inverted := Left_Not /= Right_Not;
            Bitwise (Differ, Left, Right, Bits_Out);
      end case;
      Free (Left);
      Free (Right);
      From_Complement (Bits_Out, Inverted, Budget, Result, State);
   end Bitwise_Of;

   --  ~x = -x - 1.
   procedure Not_Of (X : Number; Budget : Size_Budget; Result : in out Number; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Within (X, Budget),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Mag : Magnitude;
   begin
      if not Is_Integer (X) then
         State := Not_An_Integer;
         return;
      end if;
      Complement_Of (X, Mag);
      From_Complement (Mag, not Is_Negative (X), Budget, Result, State);
   end Not_Of;

   procedure Power_Of_Two (Exponent : Integer; Budget : Size_Budget; Result : in out Number; State : out Status) is
      Count : constant Natural := abs Exponent;
      One, Scaled, Unit : Magnitude;
   begin
      if Count / LIMB_BITS + 1 > Budget then
         State := Too_Large;
         return;
      end if;
      Make (One, 1);
      Shift_Left (One, Count, Scaled);
      Free (One);
      Make (Unit, 1);
      if Exponent >= 0 then
         Build (False, Scaled, Unit, Budget, Result, State);
      else
         Build (False, Unit, Scaled, Budget, Result, State);
      end if;
   end Power_Of_Two;

   --  The shift count B: a nonnegative integer that fits.
   procedure Count_Of (B : Number; Count : out Natural; State : out Status) is
   begin
      Count := 0;
      if not Is_Integer (B) then
         State := Not_An_Integer;
      elsif Is_Negative (B) then
         State := Outside_Domain;
      elsif not Fits_Natural (B.Numerator) or else To_Natural (B.Numerator) = Natural'Last then
         State := Too_Large;
      else
         Count := To_Natural (B.Numerator);
         State := Done;
      end if;
   end Count_Of;

   --  A * 2 ** n (Left) or floor (A / 2 ** n), for integers.
   procedure Shift_Of
     (A, B : Number; Left : Boolean; Budget : Size_Budget; Result : in out Number; State : out Status)
     with Pre => Is_Reclaimed (Result) and then Within (A, Budget) and then Within (B, Budget),
          Post => (if State = Done then Within (Result, Budget) else Is_Reclaimed (Result))
   is
      Count : Natural;
      Scale, Scaled : Number;
   begin
      if not Is_Integer (A) then
         State := Not_An_Integer;
         return;
      end if;
      Count_Of (B, Count, State);
      if State /= Done then
         return;
      end if;
      Power_Of_Two (Count, Budget, Scale, State);
      if State /= Done then
         return;
      end if;
      if Left then
         Product (A, Scale, False, Budget, Result, State);
      else
         Product (A, Scale, True, Budget, Scaled, State);
         if State = Done then
            Round_Integer (Scaled, Toward_Negative, Budget, Result, State);
         end if;
         Free (Scaled);
      end if;
      Free (Scale);
   end Shift_Of;

   procedure Apply
     (Operation : Unary_Operation; X : Number; Budget : Size_Budget;
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
         when Square_Root => Root_Of (X, 2, Budget, Result, State);
         when Bit_Not => Not_Of (X, Budget, Result, State);
         when Numerator_Of =>
            Copy (X.Numerator, Num);
            Make (Den, 1);
            Build (Is_Negative (X), Num, Den, Budget, Result, State);
         when Denominator_Of =>
            Denominator_Copy (X, Num);
            Make (Den, 1);
            Build (False, Num, Den, Budget, Result, State);
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
      Build (False, Scale_Magnitude, Unit, Budget, Scale, State);
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
         when Root =>
            if not Is_Integer (B) then
               State := Not_An_Integer;
            elsif Is_Negative (B) then
               State := Outside_Domain;
            elsif not Fits_Natural (B.Numerator) then
               State := Inexact;
            elsif To_Natural (B.Numerator) = 0 then
               State := Outside_Domain;
            else
               Root_Of (A, To_Natural (B.Numerator), Budget, Result, State);
            end if;
         when Bit_And => Bitwise_Of (Both, A, B, Budget, Result, State);
         when Bit_Or => Bitwise_Of (Either, A, B, Budget, Result, State);
         when Bit_Xor => Bitwise_Of (Differ, A, B, Budget, Result, State);
         when Shift_Left => Shift_Of (A, B, True, Budget, Result, State);
         when Shift_Right => Shift_Of (A, B, False, Budget, Result, State);
         when Integer_Quotient =>
            declare
               Ratio : Number;
            begin
               Product (A, B, True, Budget, Ratio, State);
               if State = Done then
                  Round_Integer (Ratio, Toward_Zero, Budget, Result, State);
               end if;
               Free (Ratio);
            end;
      end case;
   end Apply;

end Calc_Numbers;
