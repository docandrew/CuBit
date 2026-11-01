------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The work secondary-stack-check runs in its main thread and in its extra
--  threads at once: deep primary-stack use, then large unconstrained
--  results on the secondary stack, every byte checked.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;

package Stack_Work is

   KiB : constant := 1024;
   MiB : constant := 1024 * KiB;

   --  Primary stack each thread uses: past the 16 MiB at which the old
   --  fixed secondary stack sat below the main stack's top.
   Primary_Depth_Bytes : constant := 20 * MiB;
   Frame_Bytes : constant := 256 * KiB;
   Frames : constant := Primary_Depth_Bytes / Frame_Bytes;

   --  One unconstrained result; two of them concatenated make a third
   --  twice as large, all on the secondary stack at once.
   Result_Bytes : constant := 3 * MiB;

   --  What one run found (Run's result).
   type Outcome is record
      Passed : Boolean := False;
      --  Where its secondary-stack results were, and its stack's region.
      Result_Address : Unsigned_64 := 0;
      Region : Unsigned_64 := 0;
   end record;

   --  Use Frames frames of Frame_Bytes of the primary stack, each filled
   --  with a pattern from Seed; at the deepest, build results on the
   --  secondary stack and check them; on the way back, check every frame.
   function Run (Seed : Unsigned_64) return Outcome;

   --  Whether Where lies in [First, First + Bytes).
   function Inside (Where, First, Bytes : Unsigned_64) return Boolean is
     (Where >= First and then Where - First < Bytes);

   function Value_Of (Where : System.Address) return Unsigned_64;

end Stack_Work;
