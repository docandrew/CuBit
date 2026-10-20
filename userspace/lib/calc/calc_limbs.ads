with Interfaces; use Interfaces;

--  The arithmetic kernels of the calculator's exact numbers
--  (docs/calculator.md, "Numbers"): unsigned magnitudes as little-endian
--  arrays of 32-bit limbs, X (1) the least significant. Every array starts
--  at 1. Nothing here allocates: results go into arrays the caller sized
--  (Calc_Naturals owns the storage), and the one scratch the long division
--  needs is a scoped local sized by its operands.
--
--  Proved (tests/calculator/run.sh --prove, level 2): no run-time errors and
--  the length contracts below. The arithmetic itself (that Add adds) is
--  checked against Python's integers by the differential tests, not proved.
package Calc_Limbs with SPARK_Mode, Pure is
   LIMB_BITS : constant := 32;
   subtype Limb is Unsigned_32;
   --  A double limb: one limb times another plus a carry fits.
   subtype Wide is Unsigned_64;
   LIMB_BASE : constant Wide := 2 ** LIMB_BITS;
   LIMB_MASK : constant Wide := LIMB_BASE - 1;

   --  The most limbs one magnitude can have: its bit count is a Natural.
   --  A representation bound, not a policy: callers choose their budget.
   MAXIMUM_LIMBS : constant := Natural'Last / LIMB_BITS;
   subtype Limb_Count is Natural range 0 .. MAXIMUM_LIMBS;
   subtype Limb_Index is Limb_Count range 1 .. MAXIMUM_LIMBS;
   type Limb_Array is array (Limb_Index range <>) of Limb;
   subtype Bit_Count is Natural range 0 .. MAXIMUM_LIMBS * LIMB_BITS;
   subtype Bit_Shift is Natural range 0 .. LIMB_BITS - 1;

   type Ordering is (Less, Equal, Greater);

   --  The limbs below the highest nonzero one, inclusive: 0 for zero.
   function Significant (X : Limb_Array) return Limb_Count
     with Pre => X'First = 1,
          Post => Significant'Result <= X'Length
                  and then (for all I in X'Range => (if I > Significant'Result then X (I) = 0))
                  and then (if Significant'Result > 0 then X (Significant'Result) /= 0);

   --  The position of the highest set bit, counting from 1: 0 for zero.
   function Bits (X : Limb_Array) return Bit_Count
     with Pre => X'First = 1,
          Post => Bits'Result <= X'Length * LIMB_BITS;

   --  The bits of one limb (0 .. 32).
   function Bits_Of (Value : Limb) return Natural
     with Post => Bits_Of'Result <= LIMB_BITS
                  and then (Bits_Of'Result = 0) = (Value = 0);

   function Is_Zero (X : Limb_Array) return Boolean is
     (for all I in X'Range => X (I) = 0);

   function Compare (A, B : Limb_Array) return Ordering
     with Pre => A'First = 1 and then B'First = 1;

   procedure Clear (X : out Limb_Array)
     with Post => Is_Zero (X);

   --  Copy the low Into'Length limbs of X (zero beyond its end).
   procedure Copy (X : Limb_Array; Into : out Limb_Array)
     with Pre => X'First = 1 and then Into'First = 1;

   --  Sum := A + B.
   procedure Add (A, B : Limb_Array; Sum : out Limb_Array)
     with Pre => A'First = 1 and then B'First = 1 and then Sum'First = 1
                 and then Sum'Length > Natural'Max (A'Length, B'Length);

   --  Difference := A - B, for A >= B.
   procedure Subtract (A, B : Limb_Array; Difference : out Limb_Array)
     with Pre => A'First = 1 and then B'First = 1 and then Difference'First = 1
                 and then Compare (A, B) /= Less
                 and then Difference'Length >= Significant (A);

   --  Product := A * B.
   procedure Multiply (A, B : Limb_Array; Product : out Limb_Array)
     with Pre => A'First = 1 and then B'First = 1 and then Product'First = 1
                 and then Product'Length >= A'Length + B'Length;

   --  X := X * Factor + Addend; Carry is the limb that did not fit.
   procedure Multiply_Add_Small (X : in out Limb_Array; Factor, Addend : Limb; Carry : out Limb)
     with Pre => X'First = 1;

   --  X := X / Divisor; Remainder := X mod Divisor.
   procedure Divide_Small (X : in out Limb_Array; Divisor : Limb; Remainder : out Limb)
     with Pre => X'First = 1 and then Divisor /= 0;

   --  Into := X * 2**Shift (Shift less than a limb).
   procedure Shift_Left (X : Limb_Array; Shift : Bit_Shift; Into : out Limb_Array)
     with Pre => X'First = 1 and then Into'First = 1 and then Into'Length > X'Length;

   --  X := X / 2**Shift, in place.
   procedure Shift_Right (X : in out Limb_Array; Shift : Bit_Shift)
     with Pre => X'First = 1;

   --  Quotient := Numerator / Divisor; Remainder := Numerator mod Divisor
   --  (Knuth, TAOCP vol. 2, 4.3.1, Algorithm D).
   procedure Divide
     (Numerator, Divisor : Limb_Array; Quotient, Remainder : out Limb_Array)
     with Pre => Numerator'First = 1 and then Divisor'First = 1
                 and then Quotient'First = 1 and then Remainder'First = 1
                 and then Significant (Divisor) > 0
                 and then Numerator'Length < MAXIMUM_LIMBS
                 and then Quotient'Length >= Numerator'Length
                 and then Remainder'Length >= Significant (Divisor);
end Calc_Limbs;
