------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  SLPC waitboost (docs/gpu-async-submission.md "SLPC waitboost"), as i915
--  does it with GuC SLPC (gt/intel_rps.c intel_rps_boost and
--  intel_rps_dec_waiters, gt/uc/intel_guc_slpc.c slpc_boost_work and
--  intel_guc_slpc_dec_waiters):
--  - while a client waits on GPU work, SLPC's minimum frequency is raised to
--    the boost frequency, by default RP0 (slpc_get_rp_values);
--  - when no one waits any more, it returns to the minimum softlimit (RPe
--    here, the driver's explicit minimum).
--  CuBit adds an input hint: Desktop announces input before it submits the
--  frame, so the clock is already rising. And it holds the boost instead of
--  dropping it the moment the last job retires (i915): at 240 Hz a frame is
--  4.17 ms, and a ramp per frame would cost a large part of it. The boost
--  stays up while work is in flight, for Work_Hold_Us after the last job,
--  and for Input_Hold_Us after the last input hint (a typing or drag
--  session with pauses).
--
--  Ramp latency: from the accepted send of the boost to the first loop turn
--  that reads the actual frequency at the boost frequency.
--
--  Coalescing: the state names the minimum SLPC should have (Desired) and
--  the one last handed to the GuC (Sent). The driver sends at most one
--  non-blocking parameter set per loop turn, only when the two differ, and
--  always the newest Desired. Pure; no I/O.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Intel_GPU_Waitboost with Pure, SPARK_Mode is

   subtype Microseconds is Unsigned_64;
   subtype MHz is Unsigned_32 range 0 .. 16#1FF# * 50;

   --  How long the boost outlives the last job in flight, and the last
   --  input hint.
   Work_Hold_Us  : constant Microseconds := 50_000;
   Input_Hold_Us : constant Microseconds := 1_000_000;
   --  The actual frequency counts as reached within one 50/3 MHz ratio step.
   Reach_Tolerance_MHz : constant MHz := 17;

   type Phase is (Idle, Boosted);

   type State is record
      Current : Phase := Idle;
      Floor, Boost : MHz := 0;          --  RPe (softlimit) and RP0
      Last_Demand, Last_Hint : Microseconds := 0;
      Hinted : Boolean := False;        --  Last_Hint is meaningful
      Sent : MHz := 0;
      Since : Microseconds := 0;        --  start of the current boost
      Ramping : Boolean := False;       --  boost sent, frequency not there yet
      Ramp_Since : Microseconds := 0;
      --  Evidence since Take_Stats.
      Boosts, Hints, Sends : Unsigned_64 := 0;
      Boosted_Us : Microseconds := 0;
      Ramps, Unreached : Unsigned_64 := 0;
      Ramp_Total_Us, Ramp_Max_Us : Microseconds := 0;
   end record;

   function Valid (S : State) return Boolean is
     (S.Floor > 0 and then S.Floor <= S.Boost);

   function Desired (S : State) return MHz is
     (if S.Current = Boosted then S.Boost else S.Floor)
     with Pre => Valid (S),
          Post => Desired'Result in S.Floor .. S.Boost;

   function Need_Send (S : State) return Boolean is
     (Valid (S) and then S.Sent /= Desired (S));

   function Held (Last, Now, Hold : Microseconds) return Boolean is
     (Now < Last or else Now - Last < Hold);

   function Start (Floor, Boost : MHz; Now : Microseconds) return State
     with Pre => Floor > 0 and then Floor <= Boost,
          Post => Valid (Start'Result) and then Start'Result.Current = Idle and then
                  not Need_Send (Start'Result) and then
                  Start'Result.Floor = Floor and then Start'Result.Boost = Boost;

   --  One loop turn. Demand: GPU work in flight. Hint: an input hint
   --  arrived since the last turn.
   procedure Step (S : in out State; Demand, Hint : Boolean; Now : Microseconds)
     with Pre => Valid (S),
          Post => Valid (S) and then S.Floor = S.Floor'Old and then S.Boost = S.Boost'Old and then
                  S.Sent = S.Sent'Old and then
                  (if Demand or Hint then S.Current = Boosted) and then
                  (if Demand then S.Last_Demand = Now) and then
                  (if Hint then S.Last_Hint = Now and S.Hinted) and then
                  --  A boost ends only when both holds have run out.
                  (if S'Old.Current = Boosted and S.Current = Idle then
                     not Demand and not Hint and
                     not Held (S'Old.Last_Demand, Now, Work_Hold_Us) and
                     (not S'Old.Hinted or else not Held (S'Old.Last_Hint, Now, Input_Hold_Us))) and then
                  (if not Demand and not Hint and S'Old.Current = Idle then S.Current = Idle);

   --  The parameter set for Value was published (Accepted) or not. An
   --  accepted boost starts the ramp clock.
   procedure Note_Sent (S : in out State; Value : MHz; Accepted : Boolean; Now : Microseconds)
     with Pre => Valid (S),
          Post => Valid (S) and then S.Current = S.Current'Old and then
                  S.Floor = S.Floor'Old and then S.Boost = S.Boost'Old and then
                  S.Sent = (if Accepted then Value else S.Sent'Old) and then
                  (if Accepted and then Value = Desired (S) then not Need_Send (S)) and then
                  (if Accepted and then Value = S.Boost and then Value > S.Floor then S.Ramping);

   --  The actual frequency read this turn. Ends the ramp once it is at the
   --  boost frequency (within Reach_Tolerance_MHz), or when the boost was
   --  dropped first (counted as unreached).
   procedure Note_Actual (S : in out State; Actual : MHz; Now : Microseconds)
     with Pre => Valid (S),
          Post => Valid (S) and then S.Current = S.Current'Old and then S.Sent = S.Sent'Old and then
                  S.Floor = S.Floor'Old and then S.Boost = S.Boost'Old and then
                  (if not S'Old.Ramping then S.Ramping = False and S.Ramps = S'Old.Ramps) and then
                  (if S'Old.Ramping and then S.Current = Boosted and then
                      Actual + Reach_Tolerance_MHz >= S.Boost
                   then not S.Ramping);

   --  Waitboost turned off: back to the floor now, holds dropped.
   procedure Release (S : in out State; Now : Microseconds)
     with Pre => Valid (S),
          Post => Valid (S) and then S.Current = Idle and then Desired (S) = S.Floor and then
                  S.Floor = S.Floor'Old and then S.Boost = S.Boost'Old and then S.Sent = S.Sent'Old and then
                  not S.Hinted;

   procedure Take_Stats
     (S : in out State; Now : Microseconds;
      Boosts, Hints, Sends, Boosted_Us, Ramps, Unreached, Ramp_Avg_Us, Ramp_Max_Us : out Unsigned_64)
     with Pre => Valid (S),
          Post => Valid (S) and then S.Current = S.Current'Old and then
                  S.Sent = S.Sent'Old and then S.Floor = S.Floor'Old and then
                  S.Boost = S.Boost'Old and then S.Ramping = S.Ramping'Old and then
                  S.Boosts = 0 and then S.Hints = 0 and then S.Sends = 0 and then
                  S.Boosted_Us = 0 and then S.Ramps = 0 and then S.Unreached = 0 and then
                  S.Ramp_Total_Us = 0 and then S.Ramp_Max_Us = 0;

end Intel_GPU_Waitboost;
