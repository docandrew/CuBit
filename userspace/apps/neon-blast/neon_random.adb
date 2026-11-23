------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
package body Neon_Random with SPARK_Mode is

   function Seeded (Seed : Unsigned_64; Stream : Unsigned_64 := 0) return Generator is
      G : Generator := (State => 0, Increment => Shift_Left (Stream, 1) or 1);
      Ignored : Unsigned_32;
   begin
      Next (G, Ignored);
      G.State := G.State + Seed;
      Next (G, Ignored);
      return G;
   end Seeded;

   procedure Next (G : in out Generator; Value : out Unsigned_32) is
      Old : constant Unsigned_64 := G.State;
      Shifted : constant Unsigned_32 :=
        Unsigned_32 (Shift_Right (Shift_Right (Old, 18) xor Old, 27) and 16#FFFF_FFFF#);
      Rotation : constant Natural := Natural (Shift_Right (Old, 59));
   begin
      G.State := Old * Multiplier + G.Increment;
      Value := Rotate_Right (Shifted, Rotation);
   end Next;

   procedure Uniform (G : in out Generator; Value : out Float) is
      Bits : Unsigned_32;
   begin
      Next (G, Bits);
      --  The top 24 bits: exactly representable, strictly below 1.0.
      Value := Float (Shift_Right (Bits, 8)) / 16_777_216.0;
      if Value >= 1.0 or else Value < 0.0 then
         Value := 0.0;
      end if;
   end Uniform;

   procedure Between (G : in out Generator; Low, High : Float; Value : out Float) is
      U : Float;
   begin
      Uniform (G, U);
      Value := Low + (High - Low) * U;
   end Between;

   procedure Pick (G : in out Generator; Count : Positive; Value : out Natural) is
      Bits : Unsigned_32;
   begin
      Next (G, Bits);
      Value := Natural (Unsigned_64 (Bits) * Unsigned_64 (Count) / 2**32);
      if Value >= Count then
         Value := Count - 1;
      end if;
   end Pick;

end Neon_Random;
