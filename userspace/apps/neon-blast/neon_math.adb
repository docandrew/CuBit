------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
package body Neon_Math with SPARK_Mode is

   function Smooth_Step (Edge0, Edge1, X : Float) return Float is
      T : constant Float := Saturate ((X - Edge0) / (Edge1 - Edge0));
   begin
      return T * T * (3.0 - 2.0 * T);
   end Smooth_Step;

   function Fraction (Phase : Float) return Float is
      --  Past 2**30 a float has no fraction left to keep.
      F : constant Float := (if abs Phase < 2.0**30 then Phase - Floor (Phase) else 0.0);
   begin
      return (if F >= 1.0 or else F < 0.0 then 0.0 else F);
   end Fraction;

   --  Taylor series of sin on -pi/2 .. pi/2: below 4e-6 absolute error.
   function Sin_Turns (Phase : Float) return Float is
      P : Float := Fraction (Phase);
      X, X2, S : Float;
   begin
      if P >= 0.5 then
         P := P - 1.0;
      end if;
      if P > 0.25 then
         P := 0.5 - P;
      elsif P < -0.25 then
         P := -0.5 - P;
      end if;
      X := P * Two_Pi;
      X2 := X * X;
      S := X * (1.0 - X2 / 6.0 * (1.0 - X2 / 20.0 * (1.0 - X2 / 42.0 * (1.0 - X2 / 72.0))));
      return Clamp (S, -1.0, 1.0);
   end Sin_Turns;

   --  2 ** F for F in 0 .. 1 by a degree-6 series of e ** (F ln 2).
   function Exp2 (X : Float) return Float is
      Whole : constant Float := Floor (X);
      Y : constant Float := (X - Whole) * 0.693_147_18;
      Part : constant Float :=
        1.0 + Y * (1.0 + Y / 2.0 * (1.0 + Y / 3.0 * (1.0 + Y / 4.0 * (1.0 + Y / 5.0 * (1.0 + Y / 6.0)))));
      --  Times 2 ** Whole by exact doublings (no 'Scaling: the run-time
      --  library CuBit apps link has no floating-point attribute support).
      Result : Float := Part;
   begin
      if Whole >= 0.0 then
         for I in 1 .. Integer (Whole) loop
            Result := Result * 2.0;
         end loop;
      else
         for I in 1 .. Integer (-Whole) loop
            Result := Result * 0.5;
         end loop;
      end if;
      return Result;
   end Exp2;

   function Soft_Clip (X : Float) return Float is
      C : constant Float := Clamp (X, -3.0, 3.0);
   begin
      --  x (27 + x^2) / (27 + 9 x^2): tanh's Pade shape, 1 at 3.
      return Clamp (C * (27.0 + C * C) / (27.0 + 9.0 * C * C), -1.0, 1.0);
   end Soft_Clip;

   function Normalize (A : Vec2; Fallback : Vec2) return Vec2 is
      L : constant Float := Length (A);
   begin
      return (if L < 1.0e-6 then Fallback else (A.X / L, A.Y / L));
   end Normalize;

end Neon_Math;
