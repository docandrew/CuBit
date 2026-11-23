------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Neon Blast: the small float math the game, the renderers and the synth
--  share.
--
--  @description
--  Sin, Cos and Exp2 are our own polynomials, not the C library's, so a run
--  computes the same bits on CuBit (musl) and on a Linux host (glibc): the
--  replay's frame and audio hashes depend on it. Sqrt is the SSE
--  instruction, which IEEE 754 defines exactly. The game's units are built
--  with -ffp-contract=off for the same reason (no fused multiply-add on one
--  side only).
------------------------------------------------------------------------------
package Neon_Math with SPARK_Mode, Pure is

   Pi : constant := 3.14159_26535_89793;
   Two_Pi : constant := 2.0 * Pi;

   function Clamp (X, Low, High : Float) return Float is
     (if X < Low then Low elsif X > High then High else X);
   function Saturate (X : Float) return Float is (Clamp (X, 0.0, 1.0));
   function Lerp (A, B, T : Float) return Float is (A + (B - A) * T);
   function Min (A, B : Float) return Float is (if A < B then A else B);
   function Max (A, B : Float) return Float is (if A > B then A else B);
   --  Hermite step between Edge0 and Edge1 (Edge0 < Edge1).
   function Smooth_Step (Edge0, Edge1, X : Float) return Float;

   --  Floor and nearest for |X| < 2**30, inline (the 'Floor and 'Rounding
   --  attributes are run-time library calls in GNAT). Exact for every float
   --  in range, so they keep replays bit-identical.
   function Floor_Int (X : Float) return Integer is
     (if Float (Integer (X)) > X then Integer (X) - 1 else Integer (X))
     with Inline, Pre => abs X < 2.0**30;
   function Floor (X : Float) return Float is (Float (Floor_Int (X)))
     with Inline, Pre => abs X < 2.0**30;
   function Nearest (X : Float) return Float is (Float (Floor_Int (X + 0.5)))
     with Inline, Pre => abs X < 2.0**30;

   function Sqrt (X : Float) return Float
     with Import, Convention => Intrinsic, External_Name => "__builtin_sqrtf",
          Global => null;

   --  Sine of a phase in turns (1.0 = one cycle). Any finite phase.
   function Sin_Turns (Phase : Float) return Float
     with Post => Sin_Turns'Result in -1.0 .. 1.0;
   function Cos_Turns (Phase : Float) return Float is (Sin_Turns (Phase + 0.25));
   function Sin (Radians : Float) return Float is (Sin_Turns (Radians / Two_Pi));
   function Cos (Radians : Float) return Float is (Sin_Turns (Radians / Two_Pi + 0.25));
   --  The phase's fraction in 0.0 .. 1.0 (excluded).
   function Fraction (Phase : Float) return Float;

   --  2 ** X for X in -64 .. 64 (pitch ratios, decays).
   function Exp2 (X : Float) return Float
     with Pre => X in -64.0 .. 64.0;
   --  e ** X, X in -40 .. 40.
   function Exp (X : Float) return Float is (Exp2 (Clamp (X * 1.442_695_04, -64.0, 64.0)));
   --  A smooth saturating curve shaped like tanh, exactly +-1 beyond +-3.
   function Soft_Clip (X : Float) return Float
     with Post => Soft_Clip'Result in -1.0 .. 1.0;
   --  The frequency of a MIDI note number (69 = A4 = 440 Hz).
   function Note_Hertz (Note : Float) return Float is
     (440.0 * Exp2 (Clamp ((Note - 69.0) / 12.0, -60.0, 60.0)));

   type Vec2 is record
      X, Y : Float := 0.0;
   end record;
   function "+" (A, B : Vec2) return Vec2 is ((A.X + B.X, A.Y + B.Y));
   function "-" (A, B : Vec2) return Vec2 is ((A.X - B.X, A.Y - B.Y));
   function "*" (S : Float; A : Vec2) return Vec2 is ((S * A.X, S * A.Y));
   function Dot (A, B : Vec2) return Float is (A.X * B.X + A.Y * B.Y);
   function Length (A : Vec2) return Float is (Sqrt (Dot (A, A)));
   --  A unit vector along A, or Fallback when A is too short.
   function Normalize (A : Vec2; Fallback : Vec2) return Vec2;
   function Rotate (A : Vec2; Turns : Float) return Vec2 is
     ((A.X * Cos_Turns (Turns) - A.Y * Sin_Turns (Turns),
       A.X * Sin_Turns (Turns) + A.Y * Cos_Turns (Turns)));

   type Vec3 is record
      X, Y, Z : Float := 0.0;
   end record;
   function "+" (A, B : Vec3) return Vec3 is ((A.X + B.X, A.Y + B.Y, A.Z + B.Z));
   function "-" (A, B : Vec3) return Vec3 is ((A.X - B.X, A.Y - B.Y, A.Z - B.Z));
   function "*" (S : Float; A : Vec3) return Vec3 is ((S * A.X, S * A.Y, S * A.Z));

   --  Linear RGB, 1.0 = full channel; values above 1.0 are allowed (glow).
   type RGB is record
      R, G, B : Float := 0.0;
   end record;
   function "+" (A, B : RGB) return RGB is ((A.R + B.R, A.G + B.G, A.B + B.B));
   function "*" (S : Float; A : RGB) return RGB is ((S * A.R, S * A.G, S * A.B));
   function Mix (A, B : RGB; T : Float) return RGB is
     ((Lerp (A.R, B.R, T), Lerp (A.G, B.G, T), Lerp (A.B, B.B, T)));
   --  A colour from 16#RRGGBB#.
   function From_Hex (Value : Natural) return RGB is
     ((Float (Value / 16#10000# mod 256) / 255.0, Float (Value / 16#100# mod 256) / 255.0,
       Float (Value mod 256) / 255.0))
     with Pre => Value <= 16#FF_FFFF#;

end Neon_Math;
