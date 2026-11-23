pragma Ada_2022;
package body HID_Axis_Resolution with SPARK_Mode is
   use type Interfaces.Unsigned_32;
   subtype Factor is Long_Long_Integer range 1 .. Long_Long_Integer'Last;
   type Factors is array (Positive range 1 .. 4) of Factor;
   Powers : constant array (Natural range 0 .. 8) of Factor :=
     [1, 10, 100, 1_000, 10_000, 100_000, 1_000_000, 10_000_000, 100_000_000];
   function GCD (A, B : Factor) return Factor
     with Post => GCD'Result <= A and then GCD'Result <= B;
   function GCD (A, B : Factor) return Factor is
      X : Factor := Long_Long_Integer'Max (A, B);
      Y : Long_Long_Integer := Long_Long_Integer'Min (A, B);
      R : Long_Long_Integer;
   begin
      while Y /= 0 loop
         R := X rem Y; X := Y; Y := R;
         pragma Loop_Invariant (X <= A and X <= B and Y >= 0 and Y < X);
         pragma Loop_Variant (Decreases => Y);
      end loop;
      return X;
   end GCD;
   function Quotient (A, B : Factor) return Factor
     with Pre => B <= A, Post => Quotient'Result <= A;
   function Quotient (A, B : Factor) return Factor is
   begin
      return A / B;
   end Quotient;
   function Product (A, B, Limit : Factor) return Factor
     with Pre => A <= Limit / B, Post => Product'Result <= Limit;
   function Product (A, B, Limit : Factor) return Factor is
      pragma Unreferenced (Limit); -- Used by the proof contract only.
   begin
      return A * B;
   end Product;
   procedure Resolve
     (Logical_Span, Physical_Span : Span; Unit : Interfaces.Unsigned_32;
      Exponent : Unit_Exponent; Multiplier : Gain;
      Value : out Gain; Outcome : out Status)
   is
      N : Factors := [Physical_Span, Units_Per_Inch, Multiplier.N, 1];
      D : Factors := [Logical_Span, 1, Multiplier.D, 1];
      Common : Factor;
      P : Numerator := 1;
      Q : Denominator := 1;
   begin
      Value := Multiplier; Outcome := Caller_Units;
      if Unit = 0 then return; end if;
      Outcome := Unsupported_Unit;
      if Unit = 16#11# then N (2) := 50_000; D (2) := 127;
      elsif Unit /= 16#13# then return; end if;
      for I in 1 .. abs Exponent loop
         pragma Loop_Invariant (N (4) <= Powers (I - 1) and D (4) <= Powers (I - 1));
         if Exponent >= 0 then N (4) := N (4) * 10;
         else D (4) := D (4) * 10; end if;
      end loop;
      for I in N'Range loop
         for J in D'Range loop
            Common := GCD (N (I), D (J));
            N (I) := Quotient (N (I), Common); D (J) := Quotient (D (J), Common);
         end loop;
      end loop;
      Outcome := Capacity_Exceeded;
      for I in N'Range loop
         if N (I) > Numerator'Last / P or else D (I) > Denominator'Last / Q then return; end if;
         P := Product (N (I), P, Numerator'Last);
         Q := Product (D (I), Q, Denominator'Last);
      end loop;
      Value := (P, Q); Outcome := Normalized;
   end Resolve;
end HID_Axis_Resolution;
