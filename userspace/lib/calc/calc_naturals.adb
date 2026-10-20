with Ada.Unchecked_Deallocation;

package body Calc_Naturals with SPARK_Mode is
   --  Freeing a local before it goes out of scope is how ownership ends;
   --  flow analysis sees no effect on the outputs.
   pragma Warnings (GNATprove, Off, "statement has no effect", Reason => "frees end ownership");
   pragma Warnings (GNATprove, Off, "is set by ""Free"" but not used", Reason => "frees end ownership");
   pragma Warnings (GNATprove, Off, "is set by ""Multiply_Add_Small"" but not used",
                    Reason => "the carry is zero: the scratch has room");
   --  Decimal conversion moves nine digits per limb operation.
   CHUNK_DIGITS : constant := 9;
   CHUNK_BASE : constant Limb := 1_000_000_000;
   DECIMAL_BASE : constant := 10;

   procedure Release is new Ada.Unchecked_Deallocation (Block, Block_Access);

   procedure Free (X : in out Magnitude) is
   begin
      Release (X.Store);
   end Free;

   procedure Move (From, Into : in out Magnitude) is
   begin
      Into.Store := From.Store;
      From.Store := null;
   end Move;

   procedure Replace (X, By : in out Magnitude) is
   begin
      Release (X.Store);
      X.Store := By.Store;
      By.Store := null;
   end Replace;

   --  Result takes the significant limbs of Scratch (none for zero).
   procedure Keep (Scratch : Limb_Array; Result : in out Magnitude)
     with Pre => Scratch'First = 1 and then Is_Zero (Result),
          Post => Length (Result) <= Scratch'Length
                  and then (if not Calc_Limbs.Is_Zero (Scratch) then not Is_Zero (Result))
   is
      Size : constant Limb_Count := Significant (Scratch);
   begin
      if Size > 0 then
         Result.Store := new Block'(Size => Size, Limbs => Scratch (1 .. Size));
      end if;
   end Keep;

   function Bits (X : Magnitude) return Bit_Count is
     (if X.Store = null then 0 else Calc_Limbs.Bits (X.Store.Limbs));

   function Compare (A, B : Magnitude) return Ordering is
     (if A.Store = null then (if B.Store = null or else Calc_Limbs.Is_Zero (B.Store.Limbs) then Equal else Less)
      elsif B.Store = null then (if Calc_Limbs.Is_Zero (A.Store.Limbs) then Equal else Greater)
      else Calc_Limbs.Compare (A.Store.Limbs, B.Store.Limbs));

   function Is_One (X : Magnitude) return Boolean is
     (X.Store /= null and then Significant (X.Store.Limbs) = 1 and then X.Store.Limbs (1) = 1);

   function Is_Even (X : Magnitude) return Boolean is
     (X.Store = null or else X.Store.Limbs (1) mod 2 = 0);

   function Fits_Natural (X : Magnitude) return Boolean is
     (X.Store = null or else
        (Significant (X.Store.Limbs) <= 1 and then X.Store.Limbs (1) <= Limb (Natural'Last)));

   function To_Natural (X : Magnitude) return Natural is
     (if X.Store = null then 0 else Natural (X.Store.Limbs (1)));

   function Low_Limb (X : Magnitude) return Limb is
     (if X.Store = null then 0 else X.Store.Limbs (1));

   procedure Make (Result : in out Magnitude; Value : Unsigned_64) is
   begin
      Keep ([Limb'Mod (Value), Limb'Mod (Shift_Right (Value, LIMB_BITS))], Result);
   end Make;

   procedure Copy (X : Magnitude; Result : in out Magnitude) is
   begin
      if X.Store /= null then
         Result.Store := new Block'(Size => X.Store.Size, Limbs => X.Store.Limbs);
      end if;
   end Copy;

   procedure From_Decimal (Text : String; Result : in out Magnitude) is
      --  Nine digits are under 30 bits: a limb per nine digits is room.
      Scratch : Limb_Array (1 .. Text'Length / CHUNK_DIGITS + 2) := [others => 0];
      Chunk : Limb := 0;
      Scale : Limb := 1;
      Carry : Limb;
   begin
      for C of Text loop
         Chunk := Chunk * DECIMAL_BASE + Limb (Character'Pos (C) - Character'Pos ('0'));
         Scale := Scale * DECIMAL_BASE;
         if Scale = CHUNK_BASE then
            Multiply_Add_Small (Scratch, Scale, Chunk, Carry);
            Chunk := 0;
            Scale := 1;
         end if;
      end loop;
      if Scale > 1 then
         Multiply_Add_Small (Scratch, Scale, Chunk, Carry);
      end if;
      Keep (Scratch, Result);
   end From_Decimal;

   procedure Add (A, B : Magnitude; Result : in out Magnitude) is
      Scratch : Limb_Array (1 .. Natural'Max (Length (A), Length (B)) + 1);
   begin
      if A.Store = null then
         Copy (B, Result);
      elsif B.Store = null then
         Copy (A, Result);
      else
         Calc_Limbs.Add (A.Store.Limbs, B.Store.Limbs, Scratch);
         Keep (Scratch, Result);
      end if;
   end Add;

   procedure Subtract (A, B : Magnitude; Result : in out Magnitude) is
   begin
      if A.Store = null then
         return;  --  0 - 0
      elsif B.Store = null then
         Copy (A, Result);
      else
         declare
            Scratch : Limb_Array (1 .. A.Store.Size);
         begin
            Calc_Limbs.Subtract (A.Store.Limbs, B.Store.Limbs, Scratch);
            Keep (Scratch, Result);
         end;
      end if;
   end Subtract;

   procedure Multiply (A, B : Magnitude; Result : in out Magnitude) is
   begin
      if A.Store /= null and then B.Store /= null then
         declare
            Scratch : Limb_Array (1 .. A.Store.Size + B.Store.Size);
         begin
            Calc_Limbs.Multiply (A.Store.Limbs, B.Store.Limbs, Scratch);
            Keep (Scratch, Result);
         end;
      end if;
   end Multiply;

   procedure Divide (A, B : Magnitude; Quotient, Remainder : in out Magnitude) is
   begin
      if A.Store /= null and then Significant (B.Store.Limbs) > 0 then
         declare
            Whole : Limb_Array (1 .. A.Store.Size);
            Rest : Limb_Array (1 .. B.Store.Size);
         begin
            Calc_Limbs.Divide (A.Store.Limbs, B.Store.Limbs, Whole, Rest);
            Keep (Whole, Quotient);
            Keep (Rest, Remainder);
         end;
      end if;
   end Divide;

   procedure Greatest_Common_Divisor (A, B : Magnitude; Result : in out Magnitude) is
      Left, Right, Quotient, Remainder : Magnitude;
   begin
      Copy (A, Left);
      Copy (B, Right);
      --  gcd (Left, Right) is invariant; Right shrinks each step.
      while not Is_Zero (Right) loop
         pragma Loop_Invariant (Is_Zero (Quotient) and then Is_Zero (Remainder) and then Is_Zero (Result));
         pragma Loop_Invariant (Length (Left) <= Natural'Max (Length (A), Length (B))
                                and then Length (Right) <= Natural'Max (Length (A), Length (B)));
         Divide (Left, Right, Quotient, Remainder);
         Free (Quotient);
         Free (Left);
         Left := Right;
         Right := Remainder;
         Remainder := (Store => null);
      end loop;
      Result := Left;
   end Greatest_Common_Divisor;

   procedure Power
     (Base : Magnitude; Exponent : Natural; Bits_Allowed : Bit_Count;
      Result : in out Magnitude; Fits : out Boolean)
   is
      Square, Next : Magnitude;
      Rest : Natural := Exponent;
   begin
      if Exponent = 0 or else Base.Store = null or else Is_One (Base) then
         --  1, 0 or 1.
         if Exponent = 0 then
            Make (Result, 1);
         else
            Copy (Base, Result);
         end if;
         Fits := Length (Result) <= Bits_Allowed / LIMB_BITS + 1;
         if not Fits then
            Free (Result);
         end if;
         return;
      end if;
      --  The result has more than (Bits (Base) - 1) * Exponent bits: refuse
      --  at once what cannot fit; the loop stops at the first step that
      --  outgrows the limbs Bits_Allowed fills.
      Fits := Unsigned_64 (Natural'Max (Bits (Base), 1) - 1) * Unsigned_64 (Exponent) < Unsigned_64 (Bits_Allowed);
      if not Fits then
         return;
      end if;
      Make (Result, 1);
      Copy (Base, Square);
      loop
         pragma Loop_Invariant (Is_Zero (Next));
         pragma Loop_Invariant (Rest > 0);
         --  Square and Result never exceed the final result.
         if Length (Result) > Bits_Allowed / LIMB_BITS + 1 or else Length (Square) > Bits_Allowed / LIMB_BITS + 1
           or else Length (Result) + Length (Square) > MAXIMUM_LIMBS or else Length (Square) > MAXIMUM_LIMBS / 2
         then
            Free (Result);
            Free (Square);
            Fits := False;
            return;
         end if;
         if Rest mod 2 = 1 then
            Multiply (Result, Square, Next);
            Free (Result);
            Result := Next;
            Next := (Store => null);
         end if;
         Rest := Rest / 2;
         exit when Rest = 0;
         Multiply (Square, Square, Next);
         Free (Square);
         Square := Next;
         Next := (Store => null);
      end loop;
      Free (Square);
      --  Never taken after the bits check.
      if Length (Result) > Bits_Allowed / LIMB_BITS + 1 then
         Free (Result);
         Fits := False;
      end if;
   end Power;

   procedure Square_Root (X : Magnitude; Result : in out Magnitude) is
      Guess, Next, Quotient, Remainder, Sum, Unit : Magnitude;
   begin
      if X.Store = null then
         return;
      end if;
      --  Start at 2 ** ceil (bits / 2) >= sqrt (X); Newton's steps then
      --  decrease to floor (sqrt (X)) and stop at the first non-decrease.
      Make (Unit, 1);
      Shift_Left (Unit, (Bits (X) + 1) / 2, Guess);
      Free (Unit);
      if Is_Zero (Guess) or else Length (Guess) > Length (X) + 3 then
         Free (Guess);
         return;  --  never: a power of two above sqrt (X)
      end if;
      loop
         pragma Loop_Invariant (Is_Zero (Next) and then Is_Zero (Quotient) and then Is_Zero (Remainder)
                                and then Is_Zero (Sum) and then Is_Zero (Result));
         pragma Loop_Invariant (not Is_Zero (Guess) and then Length (Guess) <= Length (X) + 3);
         Divide (X, Guess, Quotient, Remainder);
         Free (Remainder);
         Add (Guess, Quotient, Sum);
         Free (Quotient);
         Shift_Right_One : declare
            Size : constant Limb_Count := Length (Sum) + 1;
            Scratch : Limb_Array (1 .. Size) := [others => 0];
         begin
            if Sum.Store /= null then
               Calc_Limbs.Copy (Sum.Store.Limbs, Scratch);
               Calc_Limbs.Shift_Right (Scratch, 1);
            end if;
            Free (Sum);
            Keep (Scratch, Next);
         end Shift_Right_One;
         if Compare (Next, Guess) /= Less or else Is_Zero (Next) then
            Free (Next);
            exit;
         end if;
         --  Never taken: Newton's steps only decrease.
         if Length (Next) > Length (X) + 3 then
            Free (Next);
            exit;
         end if;
         Free (Guess);
         Guess := Next;
         Next := (Store => null);
      end loop;
      Result := Guess;
   end Square_Root;

   procedure Shift_Left (X : Magnitude; Shift : Bit_Count; Result : in out Magnitude) is
      Whole : constant Natural := Shift / LIMB_BITS;
   begin
      if X.Store = null then
         return;
      end if;
      declare
         Shifted : Limb_Array (1 .. X.Store.Size + 1);
         Scratch : Limb_Array (1 .. X.Store.Size + Whole + 1) := [others => 0];
      begin
         Calc_Limbs.Shift_Left (X.Store.Limbs, Shift mod LIMB_BITS, Shifted);
         Scratch (Whole + 1 .. Scratch'Last) := Shifted;
         Keep (Scratch, Result);
      end;
   end Shift_Left;

   function Decimal_Bound (X : Magnitude) return Positive is (Length (X) * 10 + CHUNK_DIGITS);

   procedure To_Decimal (X : Magnitude; Into : out String; Last : out Natural) is
      Scratch : Limb_Array (1 .. Length (X));
      Written : Natural := 0;  --  digits so far, filled from Into'Last down
      Chunk : Limb;
   begin
      Into := [others => '0'];
      if X.Store = null then
         Last := 1;
         return;
      end if;
      Calc_Limbs.Copy (X.Store.Limbs, Scratch);
      while not Calc_Limbs.Is_Zero (Scratch) loop
         pragma Loop_Invariant (Written <= Into'Length);
         --  Never taken: Decimal_Bound leaves room for every chunk.
         exit when Into'Length - Written < CHUNK_DIGITS;
         Divide_Small (Scratch, CHUNK_BASE, Chunk);
         for Digit in 1 .. CHUNK_DIGITS loop
            Into (Into'Last - Written) := Character'Val (Character'Pos ('0') + Natural (Chunk mod DECIMAL_BASE));
            Chunk := Chunk / DECIMAL_BASE;
            Written := Written + 1;
         end loop;
      end loop;
      --  Drop the leading zeros of the top chunk, then move to the front.
      if Written = 0 then
         Last := 1;  --  never: a nonzero magnitude has digits; Into (1) is '0'
         return;
      end if;
      while Written > 1 and then Into (Into'Last - Written + 1) = '0' loop
         pragma Loop_Invariant (Written in 1 .. Into'Length);
         Written := Written - 1;
      end loop;
      Into (1 .. Written) := Into (Into'Last - Written + 1 .. Into'Last);
      Last := Written;
   end To_Decimal;
end Calc_Naturals;
