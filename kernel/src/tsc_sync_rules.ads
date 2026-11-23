-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- @summary
-- Whether every CPU's time-stamp counter is one clock: what each CPU's
-- IA32_TSC_ADJUST becomes at start-up, the verdict of a warp check between
-- a secondary CPU and the boot CPU, and whether the clock stays shared
-- (TSC_Sync, Clock_Page.Shared).
--
-- @description
-- An invariant TSC runs at one constant rate; it need not read the same on
-- every CPU. On the N150 (image 37) a thread that moved between CPUs saw
-- the TSC go back about 1.4e9 ticks (1.75 s). The order follows Linux's
-- arch/x86/kernel/tsc_sync.c (behaviour only, no code): the boot CPU's
-- TSC_ADJUST is forced to 0 when firmware left it nonzero; a secondary
-- CPU's is set to the boot CPU's when it differs; then both CPUs read the
-- TSC in strict turns on one shared spin word, each checking that it never
-- reads a value below the last one the other published. A warp seen from one side only is
-- one offset, compensated once through TSC_ADJUST, then checked again. A
-- CPU that still warps leaves the clock CPU 0's alone.
--
-- This unit is the decision and the arithmetic; TSC_Sync is the glue.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package TSC_Sync_Rules with Pure, SPARK_Mode is

   subtype Ticks is Unsigned_64;

   ---------------------------------------------------------------------------
   -- IA32_TSC_ADJUST: a signed 64-bit offset added to the counter.
   ---------------------------------------------------------------------------
   type Adjust_Value is range -2 ** 63 .. 2 ** 63 - 1;
   Sign_Bit : constant Unsigned_64 := 2 ** 63;

   -- The register's bits as the value they mean (two's complement), and
   -- back.
   function To_Raw (Value : Adjust_Value) return Unsigned_64 is
     (if Value >= 0 then Unsigned_64 (Value)
      else Unsigned_64 (Value - Adjust_Value'First) + Sign_Bit);

   function To_Adjust (Raw : Unsigned_64) return Adjust_Value
   with Post => To_Raw (To_Adjust'Result) = Raw
                and then (To_Adjust'Result >= 0) = (Raw < Sign_Bit);

   -- What a CPU's TSC_ADJUST becomes: Value, written only when Write.
   type Adjust_Plan is record
      Write : Boolean;
      Value : Adjust_Value;
   end record;

   -- The boot CPU's value after start-up: a nonzero firmware value is a
   -- firmware bug and is forced to it.
   Boot_Value : constant Adjust_Value := 0;

   function Boot_Plan (Current : Adjust_Value) return Adjust_Plan is
     ((Write => Current /= Boot_Value, Value => Boot_Value))
   with Post => Boot_Plan'Result.Value = Boot_Value
                and then Boot_Plan'Result.Write = (Current /= Boot_Value);

   -- A secondary CPU (one package): the boot CPU's value, Reference.
   function Secondary_Plan (Current, Reference : Adjust_Value) return Adjust_Plan is
     ((Write => Current /= Reference, Value => Reference))
   with Post => Secondary_Plan'Result.Value = Reference
                and then Secondary_Plan'Result.Write = (Current /= Reference);

   ---------------------------------------------------------------------------
   -- The warp check: each CPU's own samples.
   ---------------------------------------------------------------------------

   -- Samples one CPU takes in one check, at most (the check also ends at
   -- its time limit): bounds the loop however fast the turns go.
   Maximum_Rounds : constant := 10_000_000;
   subtype Round_Count is Natural range 0 .. Maximum_Rounds;

   -- A check with fewer samples on either side shows nothing.
   Minimum_Rounds : constant := 64;

   -- How far a CPU's reading was below the other CPU's last one.
   function Warp (Previous, Now : Ticks) return Ticks is
     (if Previous > Now then Previous - Now else 0)
   with Post => (if Previous > Now then Warp'Result = Previous - Now
                 else Warp'Result = 0)
                and then (Warp'Result > 0) = (Previous > Now);

   -- What one CPU saw: its samples, how many of them were warps, the
   -- largest warp. No warp exactly when the largest is 0.
   type Observation is record
      Rounds   : Round_Count := 0;
      Warps    : Round_Count := 0;
      Max_Warp : Ticks := 0;
   end record
   with Predicate => Observation.Warps <= Observation.Rounds
                     and then (Observation.Warps = 0) = (Observation.Max_Warp = 0);

   No_Observation : constant Observation := (Rounds => 0, Warps => 0, Max_Warp => 0);

   -- One more sample: this CPU read Now after the other's last Previous.
   function Observed (O : Observation; Previous, Now : Ticks) return Observation is
     ((Rounds   => O.Rounds + 1,
       Warps    => (if Previous > Now then O.Warps + 1 else O.Warps),
       Max_Warp => Ticks'Max (O.Max_Warp, Warp (Previous, Now))))
   with Pre  => O.Rounds < Maximum_Rounds,
        Post => Observed'Result.Rounds = O.Rounds + 1
                and then Observed'Result.Warps =
                  (if Previous > Now then O.Warps + 1 else O.Warps)
                and then Observed'Result.Max_Warp =
                  Ticks'Max (O.Max_Warp, Warp (Previous, Now));

   ---------------------------------------------------------------------------
   -- Bounded waits on the CPU's own counter.
   ---------------------------------------------------------------------------
   subtype Wait_Milliseconds is Unsigned_64 range 1 .. 1_000;
   Milliseconds_Per_Second : constant := 1_000;

   -- Counter ticks in Ms at Frequency_Hz, at least Ms (Ms at most a
   -- second: the product never overflows).
   function Limit_Ticks (Frequency_Hz : Ticks; Ms : Wait_Milliseconds) return Ticks is
     (if Frequency_Hz / Milliseconds_Per_Second = 0 then Ms
      else Frequency_Hz / Milliseconds_Per_Second * Ms)
   with Post => Limit_Ticks'Result >= Ms
                and then (if Frequency_Hz >= Milliseconds_Per_Second
                          then Limit_Ticks'Result = Frequency_Hz / Milliseconds_Per_Second * Ms);

   -- A wait that began at Start is over at Now (one CPU's own counter,
   -- which never goes back; modular, so a wrap is still elapsed time).
   function Expired (Start, Now, Limit : Ticks) return Boolean is
     (Now - Start >= Limit)
   with Post => Expired'Result =
     (if Now >= Start then Now - Start >= Limit
      else (Ticks'Last - Start) + Now + 1 >= Limit);

   ---------------------------------------------------------------------------
   -- The verdict of one check, and what follows it.
   ---------------------------------------------------------------------------
   type Verdict is
     (Agree,       -- enough samples on both sides and no warp on either
      Disagree,    -- some CPU read the counter below the other's last value
      Incomplete); -- the CPUs did not meet, or too few samples

   function Verdict_Of (Completed : Boolean; Boot, Secondary : Observation) return Verdict is
     (if not Completed
         or else Boot.Rounds < Minimum_Rounds
         or else Secondary.Rounds < Minimum_Rounds
      then Incomplete
      elsif Boot.Warps = 0 and then Secondary.Warps = 0 then Agree
      else Disagree)
   with Post =>
     (Verdict_Of'Result = Agree) =
       (Completed and then Boot.Rounds >= Minimum_Rounds
        and then Secondary.Rounds >= Minimum_Rounds
        and then Boot.Warps = 0 and then Secondary.Warps = 0)
     and then (Verdict_Of'Result = Disagree) =
       (Completed and then Boot.Rounds >= Minimum_Rounds
        and then Secondary.Rounds >= Minimum_Rounds
        and then (Boot.Warps > 0 or else Secondary.Warps > 0));

   -- The CPUs' largest disagreement seen in one check.
   function Largest_Warp (Boot, Secondary : Observation) return Ticks is
     (Ticks'Max (Boot.Max_Warp, Secondary.Max_Warp))
   with Post => Largest_Warp'Result >= Boot.Max_Warp
                and then Largest_Warp'Result >= Secondary.Max_Warp
                and then Largest_Warp'Result in Boot.Max_Warp | Secondary.Max_Warp;

   -- One offset to compensate: the warps were seen from one side only, and
   -- the offset is within what TSC_ADJUST compensation may move (2**48
   -- ticks, days at any rate: more is no clock to fix).
   Maximum_Compensation : constant := 2 ** 48;

   function Compensable (Boot, Secondary : Observation) return Boolean is
     ((Secondary.Warps > 0) /= (Boot.Warps > 0)
      and then Largest_Warp (Boot, Secondary) <= Maximum_Compensation)
   with Post => Compensable'Result =
     (((Secondary.Warps > 0 and then Boot.Warps = 0)
       or else (Boot.Warps > 0 and then Secondary.Warps = 0))
      and then Boot.Max_Warp <= Maximum_Compensation
      and then Secondary.Max_Warp <= Maximum_Compensation);

   -- A + D and A - D, saturating at the register's range.
   function Plus (A : Adjust_Value; D : Ticks) return Adjust_Value is
     (if A > Adjust_Value'Last - Adjust_Value (D) then Adjust_Value'Last
      else A + Adjust_Value (D))
   with Pre  => D <= Maximum_Compensation,
        Post => (if A <= Adjust_Value'Last - Adjust_Value (D)
                 then Plus'Result = A + Adjust_Value (D)
                 else Plus'Result = Adjust_Value'Last);

   function Minus (A : Adjust_Value; D : Ticks) return Adjust_Value is
     (if A < Adjust_Value'First + Adjust_Value (D) then Adjust_Value'First
      else A - Adjust_Value (D))
   with Pre  => D <= Maximum_Compensation,
        Post => (if A >= Adjust_Value'First + Adjust_Value (D)
                 then Minus'Result = A - Adjust_Value (D)
                 else Minus'Result = Adjust_Value'First);

   -- The secondary CPU's new TSC_ADJUST. It saw warps: its counter is
   -- behind the boot CPU's, so it moves forward by the largest; the boot
   -- CPU saw them: it is ahead, so it moves back.
   function Compensated (Current : Adjust_Value; Boot, Secondary : Observation)
     return Adjust_Value is
     (if Secondary.Warps > 0 then Plus (Current, Secondary.Max_Warp)
      else Minus (Current, Boot.Max_Warp))
   with Pre  => Compensable (Boot, Secondary),
        Post => (if Secondary.Warps > 0
                 then Compensated'Result = Plus (Current, Secondary.Max_Warp)
                      and then Compensated'Result >= Current
                 else Compensated'Result = Minus (Current, Boot.Max_Warp)
                      and then Compensated'Result <= Current);

   -- Checks per secondary CPU: the first, and one after a compensation or
   -- an incomplete check.
   Maximum_Checks : constant := 2;
   subtype Check_Number is Positive range 1 .. Maximum_Checks;

   type Step is
     (Done_Agree,      -- one clock with the boot CPU
      Compensate,      -- write Compensated, check again
      Check_Again,     -- the check showed nothing: check again
      Done_Disagree);  -- not one clock: the clock is CPU 0's alone

   function Next_Step
     (Check      : Check_Number;
      Result     : Verdict;
      Can_Adjust : Boolean;
      Boot, Secondary : Observation) return Step is
     (if Result = Agree then Done_Agree
      elsif Check = Maximum_Checks then Done_Disagree
      elsif Result = Incomplete then Check_Again
      elsif Can_Adjust and then Compensable (Boot, Secondary) then Compensate
      else Done_Disagree)
   with Post =>
     (Next_Step'Result = Done_Agree) = (Result = Agree)
     and then (if Next_Step'Result = Compensate
               then Result = Disagree and then Can_Adjust
                    and then Compensable (Boot, Secondary))
     and then (if Next_Step'Result in Compensate | Check_Again
               then Check < Maximum_Checks)
     and then (if Check = Maximum_Checks
               then Next_Step'Result in Done_Agree | Done_Disagree);

   ---------------------------------------------------------------------------
   -- Whether the clock stays shared: every CPU so far agreed, and this one.
   -- Once not shared, never shared again.
   ---------------------------------------------------------------------------
   function Still_Shared (Shared_So_Far : Boolean; Final : Step) return Boolean is
     (Shared_So_Far and then Final = Done_Agree)
   with Pre  => Final in Done_Agree | Done_Disagree,
        Post => Still_Shared'Result = (Shared_So_Far and then Final = Done_Agree)
                and then (if not Shared_So_Far then not Still_Shared'Result);

end TSC_Sync_Rules;
