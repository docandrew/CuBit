package body Calc_Limbs with SPARK_Mode is

   function Low (Value : Wide) return Limb is (Limb'Mod (Value));
   function High (Value : Wide) return Limb is (Limb'Mod (Shift_Right (Value, LIMB_BITS)));

   function Significant (X : Limb_Array) return Limb_Count is
   begin
      for I in reverse X'Range loop
         pragma Loop_Invariant (for all J in X'Range => (if J > I then X (J) = 0));
         if X (I) /= 0 then
            return I;
         end if;
      end loop;
      return 0;
   end Significant;

   function Bits_Of (Value : Limb) return Natural is
   begin
      for Bit in reverse 1 .. LIMB_BITS loop
         pragma Loop_Invariant (Wide (Value) < Shift_Left (Wide'(1), Bit));
         if (Value and Shift_Left (Limb'(1), Bit - 1)) /= 0 then
            return Bit;
         end if;
      end loop;
      return 0;
   end Bits_Of;

   function Bits (X : Limb_Array) return Bit_Count is
      Top : constant Limb_Count := Significant (X);
   begin
      if Top = 0 then
         return 0;
      end if;
      return (Top - 1) * LIMB_BITS + Bits_Of (X (Top));
   end Bits;

   function Compare (A, B : Limb_Array) return Ordering is
      Length_A : constant Limb_Count := Significant (A);
      Length_B : constant Limb_Count := Significant (B);
   begin
      if Length_A /= Length_B then
         return (if Length_A < Length_B then Less else Greater);
      end if;
      for I in reverse 1 .. Length_A loop
         if A (I) /= B (I) then
            return (if A (I) < B (I) then Less else Greater);
         end if;
      end loop;
      return Equal;
   end Compare;

   procedure Clear (X : out Limb_Array) is
   begin
      X := [others => 0];
   end Clear;

   procedure Copy (X : Limb_Array; Into : out Limb_Array) is
   begin
      for I in Into'Range loop
         Into (I) := (if I <= X'Last then X (I) else 0);
      end loop;
   end Copy;

   procedure Add (A, B : Limb_Array; Sum : out Limb_Array) is
      Carry : Wide := 0;
      Total : Wide;
   begin
      --  Initialized whole for flow analysis; the loop writes every limb.
      pragma Warnings (GNATprove, Off, "unused assignment", Reason => "flow needs the whole array set");
      Sum := [others => 0];
      pragma Warnings (GNATprove, On, "unused assignment");
      for I in Sum'Range loop
         pragma Loop_Invariant (Carry <= 1);
         Total := Carry + Wide (if I <= A'Last then A (I) else 0) + Wide (if I <= B'Last then B (I) else 0);
         Sum (I) := Low (Total);
         Carry := Shift_Right (Total, LIMB_BITS);
      end loop;
   end Add;

   procedure Subtract (A, B : Limb_Array; Difference : out Limb_Array) is
      Borrow : Wide := 0;
      Total : Wide;
   begin
      pragma Warnings (GNATprove, Off, "unused assignment", Reason => "flow needs the whole array set");
      Difference := [others => 0];
      pragma Warnings (GNATprove, On, "unused assignment");
      for I in Difference'Range loop
         pragma Loop_Invariant (Borrow <= 1);
         --  Modular: a borrow out wraps the high half to all ones.
         Total := Wide (if I <= A'Last then A (I) else 0) - Wide (if I <= B'Last then B (I) else 0) - Borrow;
         Difference (I) := Low (Total);
         Borrow := (if High (Total) /= 0 then 1 else 0);
      end loop;
   end Subtract;

   procedure Multiply (A, B : Limb_Array; Product : out Limb_Array) is
      Carry, Total : Wide;
   begin
      Product := [others => 0];
      for I in A'Range loop
         if A (I) /= 0 then
            Carry := 0;
            for J in B'Range loop
               pragma Loop_Invariant (Carry < LIMB_BASE);
               --  (2^32-1)^2 + 2 (2^32-1) = 2^64 - 1: never wraps.
               Total := Wide (A (I)) * Wide (B (J)) + Wide (Product (I + J - 1)) + Carry;
               Product (I + J - 1) := Low (Total);
               Carry := Shift_Right (Total, LIMB_BITS);
            end loop;
            --  The row's top limb is I + B'Length, inside Product.
            Product (I + B'Length) := Low (Carry);
         end if;
      end loop;
   end Multiply;

   procedure Multiply_Add_Small (X : in out Limb_Array; Factor, Addend : Limb; Carry : out Limb) is
      Total : Wide;
      Running : Wide := Wide (Addend);
   begin
      for I in X'Range loop
         pragma Loop_Invariant (Running < LIMB_BASE);
         Total := Wide (X (I)) * Wide (Factor) + Running;
         X (I) := Low (Total);
         Running := Shift_Right (Total, LIMB_BITS);
      end loop;
      Carry := Low (Running);
   end Multiply_Add_Small;

   procedure Divide_Small (X : in out Limb_Array; Divisor : Limb; Remainder : out Limb) is
      Rest : Wide := 0;
      Current : Wide;
   begin
      for I in reverse X'Range loop
         Current := Shift_Left (Rest, LIMB_BITS) or Wide (X (I));
         X (I) := Low (Current / Wide (Divisor));
         Rest := Current mod Wide (Divisor);
      end loop;
      Remainder := Low (Rest);
   end Divide_Small;

   procedure Shift_Left (X : Limb_Array; Shift : Bit_Shift; Into : out Limb_Array) is
      Carry : Limb := 0;
   begin
      Into := [others => 0];
      for I in X'Range loop
         if Shift = 0 then
            Into (I) := X (I);
         else
            Into (I) := Shift_Left (X (I), Shift) or Carry;
            Carry := Shift_Right (X (I), LIMB_BITS - Shift);
         end if;
      end loop;
      Into (X'Length + 1) := Carry;
   end Shift_Left;

   procedure Shift_Right (X : in out Limb_Array; Shift : Bit_Shift) is
   begin
      if Shift = 0 then
         return;
      end if;
      for I in X'Range loop
         X (I) := Shift_Right (X (I), Shift) or
           (if I < X'Last then Shift_Left (X (I + 1), LIMB_BITS - Shift) else 0);
      end loop;
   end Shift_Right;

   --  The long division proper: Divisor has at least two significant limbs
   --  and its top bit set; Work holds the shifted numerator, one limb longer.
   procedure Divide_Normalized
     (Work : in out Limb_Array; Divisor : Limb_Array; Quotient : in out Limb_Array)
     with Pre => Work'First = 1 and then Divisor'First = 1 and then Quotient'First = 1
                 and then Divisor'Length >= 2
                 and then Work'Length > Divisor'Length
                 and then Quotient'Length >= Work'Length - Divisor'Length
   is
      N : constant Limb_Index := Divisor'Length;
      Top : constant Wide := Wide (Divisor (N));
      Next : constant Wide := Wide (Divisor (N - 1));
      Estimate, Rest, Product, Total : Wide;
      Carry, Borrow : Wide;
   begin
      for J in reverse 0 .. Work'Length - N - 1 loop
         --  Estimate this quotient limb from the top two limbs of the window
         --  Work (J + 1 .. J + N + 1); at most two too large.
         Total := Shift_Left (Wide (Work (J + N + 1)), LIMB_BITS) or Wide (Work (J + N));
         if Top = 0 then
            Estimate := 0;
            Rest := 0;
         else
            Estimate := Total / Top;
            Rest := Total mod Top;
         end if;
         for Correction in 1 .. 2 loop
            exit when Rest >= LIMB_BASE;
            exit when Estimate < LIMB_BASE
              and then Estimate * Next <= Shift_Left (Rest, LIMB_BITS) + Wide (Work (J + N - 1));
            if Estimate > 0 then
               Estimate := Estimate - 1;
            end if;
            Rest := Rest + Top;
         end loop;
         --  A third correction would never be needed for a normalized
         --  divisor; clamping keeps the multiply-subtract within a limb.
         if Estimate >= LIMB_BASE then
            Estimate := LIMB_MASK;
         end if;

         --  Work window -= Estimate * Divisor.
         Carry := 0;
         Borrow := 0;
         for I in 1 .. N loop
            pragma Loop_Invariant (Carry < LIMB_BASE and then Borrow <= 1 and then Estimate < LIMB_BASE);
            Product := Estimate * Wide (Divisor (I)) + Carry;
            Carry := Shift_Right (Product, LIMB_BITS);
            Total := Wide (Work (I + J)) - (Product and LIMB_MASK) - Borrow;
            Work (I + J) := Low (Total);
            Borrow := (if High (Total) /= 0 then 1 else 0);
         end loop;
         Total := Wide (Work (J + N + 1)) - Carry - Borrow;
         Work (J + N + 1) := Low (Total);

         if High (Total) /= 0 then
            --  Estimate was one too large: add the divisor back.
            if Estimate > 0 then
               Estimate := Estimate - 1;
            end if;
            Carry := 0;
            for I in 1 .. N loop
               pragma Loop_Invariant (Carry <= 1);
               Total := Wide (Work (I + J)) + Wide (Divisor (I)) + Carry;
               Work (I + J) := Low (Total);
               Carry := Shift_Right (Total, LIMB_BITS);
            end loop;
            Work (J + N + 1) := Low (Wide (Work (J + N + 1)) + Carry);
         end if;
         Quotient (J + 1) := Low (Estimate);
      end loop;
   end Divide_Normalized;

   procedure Divide
     (Numerator, Divisor : Limb_Array; Quotient, Remainder : out Limb_Array)
   is
      N : constant Limb_Count := Significant (Divisor);
      Small_Remainder : Limb;
   begin
      Quotient := [others => 0];
      Remainder := [others => 0];
      if N = 1 then
         Copy (Numerator, Quotient);
         Divide_Small (Quotient, Divisor (1), Small_Remainder);
         Remainder (1) := Small_Remainder;
         return;
      end if;
      if Significant (Numerator) < N then
         --  Numerator < Divisor: no quotient, all remainder.
         for I in 1 .. Significant (Numerator) loop
            Remainder (I) := Numerator (I);
         end loop;
         return;
      end if;
      declare
         Shift : constant Bit_Shift := LIMB_BITS - Bits_Of (Divisor (N));
         Normal_Divisor : Limb_Array (1 .. N + 1);
         Work : Limb_Array (1 .. Numerator'Length + 1);
      begin
         pragma Assert (Numerator'Length >= N);
         Shift_Left (Divisor (1 .. N), Shift, Normal_Divisor);
         Shift_Left (Numerator, Shift, Work);
         Divide_Normalized (Work, Normal_Divisor (1 .. N), Quotient);
         Shift_Right (Work (1 .. N), Shift);
         Remainder (1 .. N) := Work (1 .. N);
      end;
   end Divide;
end Calc_Limbs;
