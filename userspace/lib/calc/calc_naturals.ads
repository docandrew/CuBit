with Interfaces; use Interfaces;
with Calc_Limbs; use Calc_Limbs;

--  Arbitrary-precision natural numbers that own their limbs
--  (docs/calculator.md, "Numbers"). A magnitude's limbs live on the heap
--  (CuAlloc natively), allocated at the exact size of each result; zero
--  owns nothing. Nothing is shared: every result is a new magnitude, and
--  the owner frees it (SPARK ownership checks that none leaks).
--
--  There is no size limit here beyond the representation's: callers bound
--  their work up front (Calc_Numbers checks each step against the budget
--  its caller passed), so an allocation never asks for more than they
--  allowed. Scratch for one operation is a scoped local of its operands'
--  size.
package Calc_Naturals with SPARK_Mode is
   type Magnitude is private
     with Default_Initial_Condition => Is_Zero (Magnitude);

   --  Zero holds no storage: a reclaimed magnitude is zero.
   function Is_Zero (X : Magnitude) return Boolean;

   function Length (X : Magnitude) return Limb_Count
     with Post => (Length'Result = 0) = Is_Zero (X);
   function Bits (X : Magnitude) return Bit_Count
     with Post => Bits'Result <= Length (X) * LIMB_BITS
                  and then (if Is_Zero (X) then Bits'Result = 0);
   function Compare (A, B : Magnitude) return Ordering;
   function Is_One (X : Magnitude) return Boolean;
   function Is_Even (X : Magnitude) return Boolean;
   --  The value when it fits a Natural.
   function Fits_Natural (X : Magnitude) return Boolean;
   function To_Natural (X : Magnitude) return Natural
     with Pre => Fits_Natural (X);
   --  The low limb.
   function Low_Limb (X : Magnitude) return Limb;

   procedure Free (X : in out Magnitude)
     with Post => Is_Zero (X);
   --  Into takes From's limbs; From is left zero.
   procedure Move (From, Into : in out Magnitude)
     with Pre => Is_Zero (Into),
          Post => Length (Into) = Length (From)'Old and then Is_Zero (From);
   --  X := By (the old X freed); By is left zero.
   procedure Replace (X, By : in out Magnitude)
     with Post => Length (X) = Length (By)'Old and then Is_Zero (By);

   --  Result := Value.
   procedure Make (Result : in out Magnitude; Value : Unsigned_64)
     with Pre => Is_Zero (Result),
          Post => Length (Result) <= 2 and then (if Value /= 0 then not Is_Zero (Result));
   procedure Copy (X : Magnitude; Result : in out Magnitude)
     with Pre => Is_Zero (Result), Post => Length (Result) = Length (X);

   --  The digits of Text, which are '0' .. '9' only.
   procedure From_Decimal (Text : String; Result : in out Magnitude)
     with Pre => Is_Zero (Result)
                 and then Text'Length <= MAXIMUM_LIMBS - 1
                 and then (for all C of Text => C in '0' .. '9');

   procedure Add (A, B : Magnitude; Result : in out Magnitude)
     with Pre => Is_Zero (Result) and then Length (A) < MAXIMUM_LIMBS and then Length (B) < MAXIMUM_LIMBS,
          Post => Length (Result) <= Natural'Max (Length (A), Length (B)) + 1;
   --  Result := A - B, for A >= B.
   procedure Subtract (A, B : Magnitude; Result : in out Magnitude)
     with Pre => Is_Zero (Result) and then Compare (A, B) /= Less,
          Post => Length (Result) <= Length (A);
   procedure Multiply (A, B : Magnitude; Result : in out Magnitude)
     with Pre => Is_Zero (Result) and then Length (A) + Length (B) <= MAXIMUM_LIMBS,
          Post => Length (Result) <= Length (A) + Length (B);
   --  Quotient := A / B; Remainder := A mod B.
   procedure Divide (A, B : Magnitude; Quotient, Remainder : in out Magnitude)
     with Pre => Is_Zero (Quotient) and then Is_Zero (Remainder) and then not Is_Zero (B)
                 and then Length (A) < MAXIMUM_LIMBS,
          Post => Length (Quotient) <= Length (A) and then Length (Remainder) <= Length (B);
   --  The greatest common divisor (Euclid); zero only for gcd (0, 0).
   procedure Greatest_Common_Divisor (A, B : Magnitude; Result : in out Magnitude)
     with Pre => Is_Zero (Result) and then Length (A) < MAXIMUM_LIMBS and then Length (B) < MAXIMUM_LIMBS,
          Post => Length (Result) <= Natural'Max (Length (A), Length (B));
   --  Result := Base ** Exponent (0 ** 0 = 1), when it fits the limbs that
   --  Bits_Allowed bits fill; otherwise Fits is False and Result stays
   --  zero. The work stops at the first step past that size.
   procedure Power
     (Base : Magnitude; Exponent : Natural; Bits_Allowed : Bit_Count;
      Result : in out Magnitude; Fits : out Boolean)
     with Pre => Is_Zero (Result),
          Post => (if Fits then Length (Result) <= Bits_Allowed / LIMB_BITS + 1 else Is_Zero (Result));
   --  Result := floor (sqrt (X)) (Newton's iteration).
   procedure Square_Root (X : Magnitude; Result : in out Magnitude)
     with Pre => Is_Zero (Result) and then Length (X) < MAXIMUM_LIMBS - 8,
          Post => Length (Result) <= Length (X) + 3;
   --  Result := X * 2 ** Shift.
   procedure Shift_Left (X : Magnitude; Shift : Bit_Count; Result : in out Magnitude)
     with Pre => Is_Zero (Result) and then Length (X) + Shift / LIMB_BITS + 1 <= MAXIMUM_LIMBS,
          Post => Length (Result) <= Length (X) + Shift / LIMB_BITS + 1;

   --  Decimal digits: at most Decimal_Bound (X) of them ("0" for zero).
   function Decimal_Bound (X : Magnitude) return Positive
     with Post => Decimal_Bound'Result <= Length (X) * 10 + 9;
   procedure To_Decimal (X : Magnitude; Into : out String; Last : out Natural)
     with Pre => Into'First = 1 and then Into'Length >= Decimal_Bound (X),
          Post => Last in 1 .. Into'Last;
private
   type Block (Size : Limb_Index) is record
      Limbs : Limb_Array (1 .. Size);
   end record;
   type Block_Access is access Block;
   --  Store = null for zero; otherwise its top limb is not zero.
   type Magnitude is record
      Store : Block_Access;
   end record;
   function Is_Zero (X : Magnitude) return Boolean is (X.Store = null);
   function Length (X : Magnitude) return Limb_Count is
     (if X.Store = null then 0 else X.Store.Size);
end Calc_Naturals;
