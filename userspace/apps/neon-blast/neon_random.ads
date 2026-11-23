------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Neon Blast: a seeded PCG32 generator (O'Neill, "PCG: A Family of Simple
--  Fast Space-Efficient Statistically Good Algorithms", 2014). Every random
--  choice in the game and the music comes from one of these, so a seed and
--  an input stream replay exactly.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Neon_Random with SPARK_Mode, Pure is

   type Generator is private;

   function Seeded (Seed : Unsigned_64; Stream : Unsigned_64 := 0) return Generator;
   procedure Next (G : in out Generator; Value : out Unsigned_32);
   --  Uniform in 0.0 .. 1.0 (excluded).
   procedure Uniform (G : in out Generator; Value : out Float)
     with Post => Value >= 0.0 and then Value < 1.0;
   --  Uniform in Low .. High.
   procedure Between (G : in out Generator; Low, High : Float; Value : out Float)
     with Pre => Low in -1.0e6 .. 1.0e6 and then High in -1.0e6 .. 1.0e6;
   --  Uniform integer in 0 .. Count - 1.
   procedure Pick (G : in out Generator; Count : Positive; Value : out Natural)
     with Post => Value < Count;

private
   --  PCG32's 64-bit LCG multiplier (the paper's default).
   Multiplier : constant Unsigned_64 := 6364136223846793005;
   type Generator is record
      State : Unsigned_64 := 16#853C_49E6_748F_EA9B#;
      Increment : Unsigned_64 := 16#DA3E_39CB_94B9_5BDB#;
   end record;
end Neon_Random;
