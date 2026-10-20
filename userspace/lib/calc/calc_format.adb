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
end Calc_Format;
