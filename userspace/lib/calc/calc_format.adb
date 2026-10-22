with Interfaces; use Interfaces;
with Calc_Limbs; use Calc_Limbs;
with Calc_Naturals; use Calc_Naturals;

package body Calc_Format with SPARK_Mode is
   pragma Warnings (GNATprove, Off, "statement has no effect", Reason => "frees end ownership");
   pragma Warnings (GNATprove, Off, "is set by ""Free"" but not used", Reason => "frees end ownership");
   pragma Warnings (GNATprove, Off, "is set by ""Parts"" but not used", Reason => "the other part is freed");
   pragma Warnings (GNATprove, Off, "is set by ""Release"" but not used", Reason => "frees end ownership");
   pragma Warnings (GNATprove, Off, "is set by ""Replace"" but not used", Reason => "the replaced value is freed");
   pragma Warnings (GNATprove, Off, "is set by ""To_Decimal"" but not used", Reason => "only the count is needed");
   pragma Warnings (GNATprove, Off, "is set by ""Power"" but not used", Reason => "a power of ten always fits");
   pragma Warnings (GNATprove, Off, "is set by ""Scaled_Quotient"" but not used",
                    Reason => "exact digits need no rounding");

   DECIMAL_BASE : constant := 10;
   TWO : constant := 2;
   FIVE : constant := 5;
   --  10 ** n has fewer than 4 n bits.
   BITS_PER_DIGIT_BOUND : constant := 4;
   --  Decimal digits per limb, at most (a limb is under 10 ** 10).
   DIGITS_PER_LIMB : constant := 10;
   --  Sign, point, parentheses and an exponent's "e-" and digits.
   DECORATION : constant := 32;
   --  The most a power of ten here may have: its bits stay a Bit_Count.
   MAXIMUM_TEN_POWER : constant := Natural'Last / 16;
   --  Scientific forms up to this exponent (far beyond any budget's digits);
   --  past it the plain rounded form shows.
   MAXIMUM_SCIENTIFIC : constant := MAXIMUM_TEN_POWER / 4;
   --  A rounded decimal of a number below 1 whose leading zeros take more
   --  than half the places is shown in scientific form instead.
   ZEROS_SHARE : constant := 2;

   subtype Small_Magnitude is Natural range 0 .. MAXIMUM_LIMBS / 2;

   function Digit_Bound (X : Number) return Positive is (Size (X) * DIGITS_PER_LIMB + 2 * DIGITS_PER_LIMB);
   function Fraction_Bound (X : Number) return Positive is (2 * Digit_Bound (X) + DECORATION);
   function Decimal_Bound (X : Number; Places : Places_Count) return Positive is
     (2 * Digit_Bound (X) + 2 * Places + DECORATION);

   function Image (Value : Integer) return String is
      Text : constant String := Integer'Image (Value);
   begin
      return (if Value < 0 then Text else Text (Text'First + 1 .. Text'Last));
   end Image;

   --  The digits of M (at most Decimal_Bound (M) of them).
   procedure Digits_Of (M : Magnitude; Buffer : out String; Last : out Natural)
     with Pre => Buffer'First = 1 and then Buffer'Length >= Decimal_Bound (M),
          Post => Last in 1 .. Buffer'Last
   is
   begin
      To_Decimal (M, Buffer, Last);
   end Digits_Of;

   procedure Count_Digits (M : Magnitude; Count : out Positive)
     with Pre => Length (M) <= Small_Magnitude'Last,
          Post => Count <= Decimal_Bound (M)
   is
      Buffer : String (1 .. Decimal_Bound (M));
      Last : Natural;
   begin
      To_Decimal (M, Buffer, Last);
      Count := Positive'Max (Last, 1);
   end Count_Digits;

   --  Result := 10 ** Count.
   procedure Ten_To (Count : Natural; Result : in out Magnitude)
     with Pre => Is_Zero (Result) and then Count <= MAXIMUM_TEN_POWER,
          Post => Length (Result) <= (Count * BITS_PER_DIGIT_BOUND + BITS_PER_DIGIT_BOUND) / LIMB_BITS + 1
   is
      Ten : Magnitude;
      Fits : Boolean;
   begin
      Make (Ten, DECIMAL_BASE);
      Power (Ten, Count, Count * BITS_PER_DIGIT_BOUND + BITS_PER_DIGIT_BOUND, Result, Fits);
      Free (Ten);
   end Ten_To;

   --  Remove every factor Prime from X; Count: how many there were.
   procedure Remove_Factor (X : in out Magnitude; Prime : Unsigned_64; Count : out Natural)
     with Pre => Length (X) <= Small_Magnitude'Last and then Prime > 1,
          Post => Length (X) <= Length (X)'Old
   is
      Divisor, Quotient, Remainder : Magnitude;
   begin
      Count := 0;
      if Is_Zero (X) then
         return;
      end if;
      Make (Divisor, Prime);
      if Is_Zero (Divisor) then
         return;  --  never: Prime > 1
      end if;
      loop
         pragma Loop_Invariant (Is_Zero (Quotient) and then Is_Zero (Remainder) and then not Is_Zero (Divisor));
         pragma Loop_Invariant (Length (X) <= Length (X)'Loop_Entry);
         Divide (X, Divisor, Quotient, Remainder);
         if not Is_Zero (Remainder) or else Is_Zero (Quotient) or else Count = Natural'Last then
            Free (Quotient);
            Free (Remainder);
            exit;
         end if;
         Replace (X, Quotient);
         Count := Count + 1;
      end loop;
      Free (Divisor);
   end Remove_Factor;

   --  floor (A * 10 ** Count / B), and whether its remainder is at least
   --  half of B (Round_Up, for halves away from zero).
   procedure Scaled_Quotient
     (A, B : Magnitude; Count : Natural; Result : in out Magnitude; Round_Up : out Boolean)
     with Pre => Is_Zero (Result) and then not Is_Zero (B) and then Count <= MAXIMUM_TEN_POWER
                 and then Length (A) <= Small_Magnitude'Last and then Length (B) <= Small_Magnitude'Last,
          Post => Length (Result) <= Length (A) + (Count * BITS_PER_DIGIT_BOUND + BITS_PER_DIGIT_BOUND) / LIMB_BITS + 1
   is
      Scale, Scaled, Remainder, Twice : Magnitude;
   begin
      Ten_To (Count, Scale);
      Multiply (A, Scale, Scaled);
      Free (Scale);
      Divide (Scaled, B, Result, Remainder);
      Free (Scaled);
      Add (Remainder, Remainder, Twice);
      Round_Up := Compare (Twice, B) /= Less;
      Free (Twice);
      Free (Remainder);
   end Scaled_Quotient;

   procedure Fraction_Text (X : Number; Into : out String; Last : out Natural) is
      Num, Den : Magnitude;

      procedure Put (Text : String)
        with Global => (In_Out => (Into, Last)),
             Pre => Into'First = 1 and then Last <= Into'Last, Post => Last <= Into'Last
      is
      begin
         if Text'Length > 0 and then Text'Length <= Into'Last - Last then
            Into (Last + 1 .. Last + Text'Length) := Text;
            Last := Last + Text'Length;
         end if;
      end Put;

      procedure Put_Magnitude (M : Magnitude)
        with Global => (In_Out => (Into, Last)),
             Pre => Into'First = 1 and then Last <= Into'Last and then Length (M) <= Small_Magnitude'Last,
             Post => Last <= Into'Last
      is
         Buffer : String (1 .. Decimal_Bound (M));
         Count : Natural;
      begin
         Digits_Of (M, Buffer, Count);
         Put (Buffer (1 .. Count));
      end Put_Magnitude;
   begin
      Into := [others => ' '];
      Last := 0;
      Parts (X, Num, Den);
      if Is_Negative (X) then
         Put ("-");
      end if;
      Put_Magnitude (Num);
      if not Is_Integer (X) then
         Put ("/");
         Put_Magnitude (Den);
      end if;
      Free (Num);
      Free (Den);
   end Fraction_Text;

   procedure Numerator_Text (X : Number; Into : out String; Last : out Natural) is
      Num, Den : Magnitude;
      Buffer : String (1 .. Size (X) * DIGITS_PER_LIMB + DIGITS_PER_LIMB);
      Count : Natural;
   begin
      Into := [others => ' '];
      Last := 0;
      Parts (X, Num, Den);
      Free (Den);
      Digits_Of (Num, Buffer, Count);
      Free (Num);
      if Is_Negative (X) and then Count < Into'Length then
         Into (1) := '-';
         Last := 1;
      end if;
      if Count <= Into'Last - Last then
         Into (Last + 1 .. Last + Count) := Buffer (1 .. Count);
         Last := Last + Count;
      end if;
   end Numerator_Text;

   procedure Denominator_Text (X : Number; Into : out String; Last : out Natural) is
      Num, Den : Magnitude;
      Buffer : String (1 .. Size (X) * DIGITS_PER_LIMB + 3 * DIGITS_PER_LIMB);
      Count : Natural;
   begin
      Into := [others => ' '];
      Last := 0;
      Parts (X, Num, Den);
      Free (Num);
      Digits_Of (Den, Buffer, Count);
      Free (Den);
      if Count <= Into'Length then
         Into (1 .. Count) := Buffer (1 .. Count);
         Last := Count;
      end if;
   end Denominator_Text;

   procedure Decimal_Text
     (X : Number; Places : Places_Count; Into : out String; Last : out Natural; Form : out Decimal_Form)
   is
      Negative : constant Boolean := Is_Negative (X);
      Num, Den, Whole, Rest, Odd_Part, Digits_Value, Unit : Magnitude;
      Twos, Fives, Pre_Period, Zeros : Natural;
      Period : Natural := 0;
      Round_Up, Found : Boolean;
      --  The leading zeros of a number below 1 were counted (Zeros).
      Counted : Boolean;

      procedure Put (Text : String)
        with Global => (In_Out => (Into, Last)),
             Pre => Into'First = 1 and then Last <= Into'Last, Post => Last <= Into'Last
      is
      begin
         if Text'Length > 0 and then Text'Length <= Into'Last - Last then
            Into (Last + 1 .. Last + Text'Length) := Text;
            Last := Last + Text'Length;
         end if;
      end Put;

      procedure Put_Zeros (Count : Natural)
        with Global => (In_Out => (Into, Last)),
             Pre => Into'First = 1 and then Last <= Into'Last, Post => Last <= Into'Last
      is
      begin
         for K in 1 .. Count loop
            pragma Loop_Invariant (Last <= Into'Last);
            Put ("0");
         end loop;
      end Put_Zeros;

      procedure Put_Magnitude (M : Magnitude; Width : Natural := 0)
        with Global => (In_Out => (Into, Last)),
             Pre => Into'First = 1 and then Last <= Into'Last and then Length (M) <= Small_Magnitude'Last,
             Post => Last <= Into'Last
      is
         Buffer : String (1 .. Decimal_Bound (M));
         Count : Natural;
      begin
         Digits_Of (M, Buffer, Count);
         if Count < Width then
            Put_Zeros (Width - Count);
         end if;
         Put (Buffer (1 .. Count));
      end Put_Magnitude;

      --  N's digits as d.ddd (trailing zeros dropped) e Exponent.
      procedure Put_Scientific (N : Magnitude; Exponent : Integer)
        with Global => (In_Out => (Into, Last)),
             Pre => Into'First = 1 and then Last <= Into'Last and then Length (N) <= Small_Magnitude'Last,
             Post => Last <= Into'Last
      is
         Buffer : String (1 .. Decimal_Bound (N));
         Count : Natural;
      begin
         Digits_Of (N, Buffer, Count);
         while Count > 1 and then Buffer (Count) = '0' loop
            pragma Loop_Invariant (Count in 2 .. Buffer'Last);
            Count := Count - 1;
         end loop;
         Put (Buffer (1 .. 1));
         if Count > 1 then
            Put (".");
            Put (Buffer (2 .. Count));
         end if;
         Put ("e");
         Put (Image (Exponent));
      end Put_Scientific;

      --  N / 10 ** Places as a decimal, trailing fraction zeros dropped.
      procedure Put_Rounded (N : Magnitude)
        with Global => (In_Out => (Into, Last), Input => Places),
             Pre => Into'First = 1 and then Last <= Into'Last and then Length (N) <= Small_Magnitude'Last,
             Post => Last <= Into'Last
      is
         Buffer : String (1 .. Decimal_Bound (N));
         Count, Keep : Natural;
      begin
         Digits_Of (N, Buffer, Count);
         if Count <= Places then
            Put ("0.");
            Put_Zeros (Places - Count);
            Keep := Count;
            while Keep > 1 and then Buffer (Keep) = '0' loop
               pragma Loop_Invariant (Keep in 2 .. Count);
               Keep := Keep - 1;
            end loop;
            Put (Buffer (1 .. Keep));
         else
            Put (Buffer (1 .. Count - Places));
            Put (".");
            Keep := Count;
            while Keep > Count - Places + 1 and then Buffer (Keep) = '0' loop
               pragma Loop_Invariant (Keep in Count - Places + 2 .. Count);
               Keep := Keep - 1;
            end loop;
            Put (Buffer (Count - Places + 1 .. Keep));
         end if;
      end Put_Rounded;

      procedure Release
        with Global => (In_Out => (Num, Den, Whole, Rest, Odd_Part, Digits_Value, Unit)),
             Post => Is_Zero (Num) and then Is_Zero (Den) and then Is_Zero (Whole) and then Is_Zero (Rest)
                     and then Is_Zero (Odd_Part) and then Is_Zero (Digits_Value) and then Is_Zero (Unit)
      is
      begin
         Free (Num);
         Free (Den);
         Free (Whole);
         Free (Rest);
         Free (Odd_Part);
         Free (Digits_Value);
         Free (Unit);
      end Release;
   begin
      Into := [others => ' '];
      Last := 0;
      Form := Integer_Form;
      Parts (X, Num, Den);
      Divide (Num, Den, Whole, Rest);
      if Negative then
         Put ("-");
      end if;
      if Is_Zero (Rest) then
         Put_Magnitude (Whole);
         Release;
         return;
      end if;

      --  1/Den terminates after max (twos, fives) digits when Den has no
      --  other prime factor; otherwise it repeats after them.
      Copy (Den, Odd_Part);
      Remove_Factor (Odd_Part, TWO, Twos);
      Remove_Factor (Odd_Part, FIVE, Fives);
      Pre_Period := Natural'Max (Twos, Fives);

      if Is_One (Odd_Part) and then Pre_Period <= Places then
         Form := Terminating;
         Scaled_Quotient (Rest, Den, Pre_Period, Digits_Value, Round_Up);
         Put_Magnitude (Whole);
         Put (".");
         Put_Magnitude (Digits_Value, Pre_Period);
         Release;
         return;
      end if;

      if Is_One (Odd_Part) and then Pre_Period <= MAXIMUM_SCIENTIFIC and then Is_Zero (Whole) then
         --  Few significant digits: exact in scientific form.
         Scaled_Quotient (Rest, Den, Pre_Period, Digits_Value, Round_Up);
         declare
            Significant_Digits : Positive;
         begin
            Count_Digits (Digits_Value, Significant_Digits);
            if Significant_Digits <= Places then
               Form := Scientific_Exact;
               Put_Scientific (Digits_Value, Significant_Digits - 1 - Pre_Period);
               Release;
               return;
            end if;
         end;
         Free (Digits_Value);
      end if;

      if not Is_One (Odd_Part) and then Pre_Period < Places then
         --  The period is the order of 10 modulo the odd part: the first
         --  k with 10 ** k mod Odd_Part = 1. Look as far as fits.
         declare
            Power_Mod, Ten, Product, Quotient : Magnitude;
         begin
            Make (Ten, DECIMAL_BASE);
            Make (Unit, 1);
            Make (Power_Mod, 1);
            Found := False;
            for K in 1 .. Places - Pre_Period loop
               pragma Loop_Invariant (Is_Zero (Product) and then Is_Zero (Quotient) and then not Found);
               pragma Loop_Invariant (Length (Power_Mod) <= Length (Odd_Part) + 2 and then Length (Ten) <= 2);
               Multiply (Power_Mod, Ten, Product);
               Free (Power_Mod);
               if Is_Zero (Odd_Part) then
                  Free (Product);
                  exit;  --  never: Odd_Part divides Den
               end if;
               Divide (Product, Odd_Part, Quotient, Power_Mod);
               Free (Product);
               Free (Quotient);
               if Compare (Power_Mod, Unit) = Equal then
                  Found := True;
                  Period := K;
                  exit;
               end if;
            end loop;
            Free (Power_Mod);
            Free (Ten);
            Free (Unit);
         end;
         if Found then
            Form := Repeating;
            Scaled_Quotient (Rest, Den, Pre_Period + Period, Digits_Value, Round_Up);
            declare
               Full_Size : constant Positive := Decimal_Bound (Digits_Value);
               Buffer : String (1 .. Places + 2);
               Count : Natural := 0;
               Full : String (1 .. Full_Size);
               Full_Last : Natural;
            begin
               --  Pre_Period + Period digits, zero-padded on the left.
               Digits_Of (Digits_Value, Full, Full_Last);
               Buffer := [others => '0'];
               if Full_Last <= Pre_Period + Period then
                  Buffer (Pre_Period + Period - Full_Last + 1 .. Pre_Period + Period) := Full (1 .. Full_Last);
                  Count := Pre_Period + Period;
               end if;
               Put_Magnitude (Whole);
               Put (".");
               Put (Buffer (1 .. Natural'Min (Pre_Period, Count)));
               Put ("(");
               if Count > Pre_Period then
                  Put (Buffer (Pre_Period + 1 .. Count));
               end if;
               Put (")");
            end;
            Release;
            return;
         end if;
      end if;

      --  Rounded. Below 1, count the zeros after the point first: the
      --  smallest k with Rest * 10 ** k >= Den is the first nonzero digit.
      if Is_Zero (Whole) then
         declare
            Den_Digits, Rest_Digits : Positive;
            Guess : Natural;
            Scale, Scaled : Magnitude;
         begin
            Count_Digits (Den, Den_Digits);
            Count_Digits (Rest, Rest_Digits);
            Guess := Natural'Max (Den_Digits - Natural'Min (Rest_Digits, Den_Digits), 1);
            Zeros := Guess - 1;
            Counted := Den_Digits <= MAXIMUM_SCIENTIFIC;
            for Attempt in 1 .. (if Counted then 2 else 0) loop
               pragma Loop_Invariant (Is_Zero (Scale) and then Is_Zero (Scaled));
               pragma Loop_Invariant (Zeros = Guess - 1 + (Attempt - 1));
               Ten_To (Zeros + 1, Scale);
               Multiply (Rest, Scale, Scaled);
               Free (Scale);
               Found := Compare (Scaled, Den) /= Less;
               Free (Scaled);
               exit when Found;
               Zeros := Zeros + 1;
            end loop;
         end;
         if Counted and then Zeros > Places / ZEROS_SHARE then
            Form := Scientific_Rounded;
            Scaled_Quotient (Rest, Den, Zeros + Places, Digits_Value, Round_Up);
            if Round_Up then
               Make (Unit, 1);
               declare
                  Next : Magnitude;
               begin
                  Add (Digits_Value, Unit, Next);
                  Replace (Digits_Value, Next);
               end;
            end if;
            --  Digits_Value has Places digits, or Places + 1 after a carry.
            declare
               Mantissa_Digits : Positive;
            begin
               Count_Digits (Digits_Value, Mantissa_Digits);
               Put_Scientific (Digits_Value, Mantissa_Digits - 1 - Zeros - Places);
            end;
            Release;
            return;
         end if;
      end if;
      Form := Rounded;
      Scaled_Quotient (Num, Den, Places, Digits_Value, Round_Up);
      if Round_Up then
         Make (Unit, 1);
         declare
            Next : Magnitude;
         begin
            Add (Digits_Value, Unit, Next);
            Replace (Digits_Value, Next);
         end;
      end if;
      if Negative and then Is_Zero (Digits_Value) then
         Last := 0;  --  rounds to zero: no sign
      end if;
      Put_Rounded (Digits_Value);
      Release;
   end Decimal_Text;

   ---------------------------------------------------------------------------
   --  Balls and bases.

   --  The work of a display has the whole representation to itself.
   WORK : constant Size_Budget := Size_Budget'Last;

   procedure Step
     (Operation : Binary_Operation; A, B : Number; Result : in out Number; State : in out Status)
     with Pre => Calc_Numbers.Is_Reclaimed (Result)
                 and then (if State = Done then Within (A, WORK) and then Within (B, WORK)),
          Post => (if State = Done then Within (Result, WORK) else Calc_Numbers.Is_Reclaimed (Result))
   is
   begin
      if State = Done then
         Apply (Operation, A, B, WORK, Result, State);
      end if;
   end Step;

   procedure Step
     (Operation : Unary_Operation; X : Number; Result : in out Number; State : in out Status)
     with Pre => Calc_Numbers.Is_Reclaimed (Result) and then (if State = Done then Within (X, WORK)),
          Post => (if State = Done then Within (Result, WORK) else Calc_Numbers.Is_Reclaimed (Result))
   is
   begin
      if State = Done then
         Apply (Operation, X, WORK, Result, State);
      end if;
   end Step;

   --  10 ** K, exact (K may be negative).
   procedure Ten_Power (K : Integer; Result : in out Number; State : in out Status)
     with Pre => Calc_Numbers.Is_Reclaimed (Result),
          Post => (if State = Done then Within (Result, WORK) else Calc_Numbers.Is_Reclaimed (Result))
   is
      Ten, Exponent : Number;
   begin
      if State /= Done then
         return;
      end if;
      From_Integer (DECIMAL_BASE, Ten);
      From_Integer (Integer_64 (K), Exponent);
      Step (Power, Ten, Exponent, Result, State);
      Free (Ten);
      Free (Exponent);
   end Ten_Power;

   --  floor (log10 |V|) for V /= 0: from the digits of its numerator and
   --  denominator, then one comparison.
   procedure Decimal_Exponent_Of (V : Number; E : out Integer; State : in out Status)
     with Pre => (if State = Done then Within (V, WORK))
   is
      Num, Den : Magnitude;
      Num_Digits, Den_Digits : Positive;
      Abs_V, Power_Value : Number;
      Order : Ordering := Less;
   begin
      E := 0;
      if State /= Done or else Calc_Numbers.Is_Zero (V) then
         return;
      end if;
      Parts (V, Num, Den);
      Count_Digits (Num, Num_Digits);
      Count_Digits (Den, Den_Digits);
      Free (Num);
      Free (Den);
      E := Num_Digits - Den_Digits;
      Step (Absolute, V, Abs_V, State);
      Ten_Power (E, Power_Value, State);
      if State = Done then
         Compare (Abs_V, Power_Value, Order);
      end if;
      if Order = Less then
         E := E - 1;
      end if;
      Free (Abs_V);
      Free (Power_Value);
   end Decimal_Exponent_Of;

   function Ball_Bound (X : Calc_Balls.Ball; Places : Places_Count) return Positive is
     (Natural'Max (3 * Places + 2 * DECORATION,
                   2 * (Calc_Balls.Size (X) * DIGITS_PER_LIMB + 2 * DIGITS_PER_LIMB) + 2 * Places + DECORATION));

   procedure Ball_Text
     (X : Calc_Balls.Ball; Places : Places_Count; Into : out String; Last : out Natural;
      Bound_Into : out String; Bound_Last : out Natural; Form : out Decimal_Form)
   is
      Mid, Rad, Twice_Rad, Two, Scale, Scaled, Whole, Shown, Difference, Abs_Difference, Bound, Unit,
        Bound_Scaled, Bound_Units : Number;
      State : Status := Done;
      E, Error_Exponent, Q, Bound_Exponent : Integer := 0;
      Bound_Digit : Natural := 1;

      procedure Put (Text : String)
        with Global => (In_Out => (Into, Last)),
             Pre => Into'First = 1 and then Last <= Into'Last, Post => Last <= Into'Last
      is
      begin
         if Text'Length > 0 and then Text'Length <= Into'Last - Last then
            Into (Last + 1 .. Last + Text'Length) := Text;
            Last := Last + Text'Length;
         end if;
      end Put;

      procedure Put_Bound (Text : String)
        with Global => (In_Out => (Bound_Into, Bound_Last)),
             Pre => Bound_Into'First = 1 and then Bound_Last <= Bound_Into'Last, Post => Bound_Last <= Bound_Into'Last
      is
      begin
         if Text'Length > 0 and then Text'Length <= Bound_Into'Last - Bound_Last then
            Bound_Into (Bound_Last + 1 .. Bound_Last + Text'Length) := Text;
            Bound_Last := Bound_Last + Text'Length;
         end if;
      end Put_Bound;

      --  Num's digits, the sign and the point placed for Q and E.
      procedure Emit (Num : Magnitude; Negative, Zero : Boolean)
        with Global => (Input => (Q, E, Places), In_Out => (Into, Last), Output => Form),
             Pre => Into'First = 1 and then Last <= Into'Last and then Length (Num) <= Small_Magnitude'Last
                    and then Q > Integer'First,
             Post => Last <= Into'Last
      is
         Buffer : String (1 .. Decimal_Bound (Num));
         Count : Natural;
         Scientific : constant Boolean :=
           Q > 0 or else E >= Places or else E < -(Places / ZEROS_SHARE);
      begin
         Digits_Of (Num, Buffer, Count);
         if Negative then
            Put ("-");
         end if;
         if Zero then
            Form := Approximate_Decimal;
            Put ("0");
         elsif Scientific then
            Form := Approximate_Scientific;
            Put (Buffer (1 .. 1));
            if Count > 1 then
               Put (".");
               Put (Buffer (2 .. Count));
            end if;
            if Q < Integer'Last - Count then
               Put ("e");
               Put (Image (Q + Count - 1));
            end if;
         else
            Form := Approximate_Decimal;
            if Q = 0 then
               Put (Buffer (1 .. Count));
            elsif Count > -Q then
               Put (Buffer (1 .. Count + Q));
               Put (".");
               Put (Buffer (Count + Q + 1 .. Count));
            else
               Put ("0.");
               for K in 1 .. -Q - Count loop
                  pragma Loop_Invariant (Last <= Into'Last);
                  Put ("0");
               end loop;
               Put (Buffer (1 .. Count));
            end if;
         end if;
      end Emit;
   begin
      Into := [others => ' '];
      Last := 0;
      Bound_Into := [others => ' '];
      Bound_Last := 0;
      Calc_Balls.Midpoint (X, Mid);
      if Calc_Balls.Is_Exact (X) then
         if Into'Length >= Decimal_Bound (Mid, Places) then
            Decimal_Text (Mid, Places, Into, Last, Form);
         else
            Form := Rounded;  --  never: Ball_Bound is at least Decimal_Bound
         end if;
         Free (Mid);
         return;
      end if;
      Calc_Balls.Radius (X, Rad);
      --  The digits to show: Places significant ones at most, and none
      --  below what the radius leaves certain (0.5 10 ** q >= r).
      From_Integer (2, Two);
      Step (Multiply, Rad, Two, Twice_Rad, State);
      Decimal_Exponent_Of (Twice_Rad, Error_Exponent, State);
      Decimal_Exponent_Of (Mid, E, State);
      if Error_Exponent = Integer'Last or else E < Integer'First + Places then
         State := Too_Large;  --  never: exponents are digit counts
      end if;
      Q := (if State /= Done then 0
            elsif Calc_Numbers.Is_Zero (Mid) then Error_Exponent + 1
            else Integer'Max (E - Places + 1, Error_Exponent + 1));
      if Q = Integer'First then
         State := Too_Large;
      end if;
      Ten_Power ((if State = Done then -Q else 0), Scale, State);
      Step (Multiply, Mid, Scale, Scaled, State);
      Step (Round, Scaled, Whole, State);
      Step (Divide, Whole, Scale, Shown, State);
      Step (Subtract, Shown, Mid, Difference, State);
      Step (Absolute, Difference, Abs_Difference, State);
      Step (Add, Rad, Abs_Difference, Bound, State);
      --  The bound, up to one significant digit.
      Decimal_Exponent_Of (Bound, Bound_Exponent, State);
      Ten_Power ((if State = Done then Bound_Exponent else 0), Unit, State);
      Step (Divide, Bound, Unit, Bound_Scaled, State);
      Step (Ceiling, Bound_Scaled, Bound_Units, State);
      if State = Done and then Fits_Natural (Bound_Units) then
         Bound_Digit := To_Natural (Bound_Units);
      end if;
      if Bound_Digit >= DECIMAL_BASE and then Bound_Exponent < Integer'Last then
         Bound_Digit := 1;
         Bound_Exponent := Bound_Exponent + 1;
      end if;
      if State = Done then
         Put_Bound (Image (Bound_Digit));
         Put_Bound ("e");
         Put_Bound (Image (Bound_Exponent));
      end if;
      --  The guaranteed digits of Whole, scaled by 10 ** Q.
      if State = Done and then Calc_Numbers.Size (Whole) <= Small_Magnitude'Last then
         declare
            Num, Den : Magnitude;
            Negative : constant Boolean := Calc_Numbers.Is_Negative (Whole);
         begin
            Parts (Whole, Num, Den);
            Free (Den);
            if Q > Integer'First then
               Emit (Num, Negative, Calc_Numbers.Is_Zero (Whole));
            else
               Form := Approximate_Decimal;  --  never: Q = Integer'First fails above
            end if;
            Free (Num);
         end;
      else
         Form := Approximate_Decimal;
      end if;
      Free (Mid);
      Free (Rad);
      Free (Twice_Rad);
      Free (Two);
      Free (Scale);
      Free (Scaled);
      Free (Whole);
      Free (Shown);
      Free (Difference);
      Free (Abs_Difference);
      Free (Bound);
      Free (Unit);
      Free (Bound_Scaled);
      Free (Bound_Units);
   end Ball_Text;

   function Base_Bound (X : Number) return Positive is (Size (X) * LIMB_BITS + LIMB_BITS * 3 + DECORATION);

   procedure Base_Text
     (X : Number; Base : Display_Base; Word : Calc_Words.Word_Size; Into : out String; Last : out Natural)
   is
      use type Calc_Words.Word_Size;
      Value : Number;
      Num, Den : Magnitude;
      State : Status := Done;
      --  Digits a word of Width bits takes in Base.
      Width : constant Natural := Calc_Words.Width (Word);
      Bits_Per_Digit : constant Positive := (case Base is when 2 => 1, when 8 => 3, when 16 => 4, when others => 1);
      Pad : constant Natural :=
        (if Word = Calc_Words.Unbounded or else Base = DECIMAL_BASE then 0
         else (Width + Bits_Per_Digit - 1) / Bits_Per_Digit);

      --  Num's digits after the sign, padded with zeros to Pad digits.
      procedure Emit (Num : Magnitude; Negative : Boolean)
        with Global => (Input => (Base, Pad), In_Out => (Into, Last)),
             Pre => Into'First = 1 and then Last = 0 and then Into'Length > 0
                    and then Length (Num) <= Small_Magnitude'Last,
             Post => Last <= Into'Last
      is
         Buffer : String (1 .. Digits_Bound (Num) + 1);
         Count : Natural;
      begin
         To_Digits (Num, Base, Buffer, Count);
         if Negative then
            Into (1) := '-';
            Last := 1;
         end if;
         for K in Count + 1 .. Pad loop
            pragma Loop_Invariant (Last <= Into'Last);
            if Last < Into'Last then
               Last := Last + 1;
               Into (Last) := '0';
            end if;
         end loop;
         if Count <= Into'Last - Last then
            Into (Last + 1 .. Last + Count) := Buffer (1 .. Count);
            Last := Last + Count;
         end if;
      end Emit;
   begin
      Into := [others => ' '];
      Last := 0;
      if Word /= Calc_Words.Unbounded and then Base /= DECIMAL_BASE then
         Calc_Words.Pattern (X, Word, WORK, Value, State);
      else
         Copy (X, Value);
      end if;
      if State = Done and then Size (Value) <= Small_Magnitude'Last then
         Parts (Value, Num, Den);
         Emit (Num, Is_Negative (Value));
         Free (Num);
         Free (Den);
      end if;
      Free (Value);
   end Base_Text;
end Calc_Format;
