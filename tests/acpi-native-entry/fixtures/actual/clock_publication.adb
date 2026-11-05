------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  The clock publication's conversion (clock_publication.ads).
------------------------------------------------------------------------------
pragma Ada_2022;

package body Clock_Publication with SPARK_Mode is

   --  Unsigned arithmetic GNATprove checks for wrap-around like signed
   --  overflow, so a proof of this body is a proof that nothing wraps.
   type Exact is mod 2 ** 64 with Annotate => (GNATprove, No_Wrap_Around);

   --  The two 32-bit halves of a tick count below 2**63.
   High_Limit : constant := 2 ** 31;

   function Elapsed_Nanoseconds
     (Ticks : Elapsed_Ticks; Scale : Tick_Scale) return Unsigned_64
   is
      S    : constant Exact := Exact (Scale);
      High : constant Exact := Exact (Ticks) / One_Nanosecond;
      Low  : constant Exact := Exact (Ticks) mod One_Nanosecond;
      Whole, Part : Exact;
   begin
      pragma Assert (High < High_Limit);
      pragma Assert (Exact (Ticks) = High * One_Nanosecond + Low);
      --  High * S <= High * 2**32 < 2**63.
      pragma Assert (High * S <= High * One_Nanosecond);
      Whole := High * S;
      --  Low * S <= (2**32 - 1) * 2**32 < 2**64.
      pragma Assert (Low * S <= Low * One_Nanosecond);
      Part := Low * S / One_Nanosecond;
      pragma Assert (Part <= Low);
      return Unsigned_64 (Whole + Part);
   end Elapsed_Nanoseconds;

   procedure Convert
     (P           : Parameters;
      Counter     : Unsigned_64;
      Nanoseconds : out Unsigned_64;
      Success     : out Boolean)
   is
   begin
      Success := Readable (P, Counter);
      Nanoseconds := (if Success then Time_At (P, Counter) else 0);
   end Convert;

   --  A product of a low half and a scale stays below 2**64.
   procedure Lemma_Bounded_Product (A, S : Exact)
   with Ghost,
        Pre  => A < One_Nanosecond and then S <= One_Nanosecond,
        Post => A * S <= A * One_Nanosecond;

   procedure Lemma_Bounded_Product (A, S : Exact) is
   begin
      --  Induction on A: each step adds S <= 2**32 to the product.
      for I in 0 .. A loop
         if I > 0 then
            pragma Assert (I * S = (I - 1) * S + S);
         end if;
         pragma Loop_Invariant (I * S <= I * One_Nanosecond);
      end loop;
   end Lemma_Bounded_Product;

   --  X * S <= Y * S for X <= Y, both products below 2**64.
   procedure Lemma_Scaled_Monotonic (X, Y, S : Exact)
   with Ghost,
        Pre  => X <= Y and then Y < One_Nanosecond and then S <= One_Nanosecond,
        Post => X * S <= Y * S;

   procedure Lemma_Scaled_Monotonic (X, Y, S : Exact) is
      D : constant Exact := Y - X;
   begin
      Lemma_Bounded_Product (Y, S);
      Lemma_Bounded_Product (X, S);
      Lemma_Bounded_Product (D, S);
      pragma Assert (Y * S = X * S + D * S);
   end Lemma_Scaled_Monotonic;

   --  The fraction a low half contributes is less than one whole step.
   procedure Lemma_Fraction_Below_Step (L, S : Exact)
   with Ghost,
        Pre  => L < One_Nanosecond and then S in 1 .. One_Nanosecond,
        Post => L * S / One_Nanosecond < S;

   procedure Lemma_Fraction_Below_Step (L, S : Exact) is
   begin
      if S = One_Nanosecond then
         pragma Assert (L * One_Nanosecond / One_Nanosecond = L);
         pragma Assert (L * S = L * One_Nanosecond);
      else
         Lemma_Scaled_Monotonic (L, One_Nanosecond - 1, S);
         pragma Assert ((One_Nanosecond - 1) * S + S = One_Nanosecond * S);
         pragma Assert (L * S < One_Nanosecond * S);
      end if;
   end Lemma_Fraction_Below_Step;

   procedure Lemma_Elapsed_Monotonic
     (Earlier, Later : Elapsed_Ticks; Scale : Tick_Scale)
   is
      S  : constant Exact := Exact (Scale);
      HE : constant Exact := Exact (Earlier) / One_Nanosecond;
      LE : constant Exact := Exact (Earlier) mod One_Nanosecond;
      HL : constant Exact := Exact (Later) / One_Nanosecond;
      LL : constant Exact := Exact (Later) mod One_Nanosecond;
   begin
      pragma Assert (HE <= HL and then HL < High_Limit);
      Lemma_Scaled_Monotonic (HE, HL, S);
      if HE = HL then
         Lemma_Scaled_Monotonic (LE, LL, S);
         pragma Assert (LE * S / One_Nanosecond <= LL * S / One_Nanosecond);
      else
         Lemma_Fraction_Below_Step (LE, S);
         Lemma_Scaled_Monotonic (HE + 1, HL, S);
         pragma Assert (HE * S + S = (HE + 1) * S);
         pragma Assert (HE * S + LE * S / One_Nanosecond < HL * S);
      end if;
   end Lemma_Elapsed_Monotonic;

   procedure Lemma_Time_Monotonic
     (P : Parameters; Earlier, Later : Unsigned_64)
   is
   begin
      pragma Assert (Ticks_Since (P, Earlier) <= Ticks_Since (P, Later));
      Lemma_Elapsed_Monotonic
        (Ticks_Since (P, Earlier), Ticks_Since (P, Later), P.Scale);
   end Lemma_Time_Monotonic;

end Clock_Publication;
