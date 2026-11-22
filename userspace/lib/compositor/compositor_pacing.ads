------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  When to compose the next frame of an output (LAT-002): against the next
--  refresh latch, not as soon as the previous frame is released.
--
--  @description
--  A backend that releases a frame only once its refresh latched it
--  (intel-gpu) keeps one frame in flight per refresh while anything
--  animates. Composing at once on release puts every input that arrives
--  after it into the frame after next: on the laptop, typing next to
--  Teapots was one or two refreshes late. Composing at the latest moment
--  that still makes the next latch takes in everything that arrived before
--  it: Compose_At = next latch - (frame cost + Margin_Us), where the frame
--  cost (composition start to the backend's arm, display.svc's copy
--  included) is a decaying maximum of the recent frames'.
--
--  Composing at once is right, and Compose_At says Now, when there is no
--  known refresh (a virtual GPU's fence, Period 0), when the last latch is
--  older than Stale_Periods refreshes (an extrapolated grid drifts; a
--  quiet desktop composes the first input at once), or when the deadline
--  has passed (the frame then makes a later latch, as before).
--
--  Pure: microseconds of the published monotonic clock in, a time out.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
package Compositor_Pacing with SPARK_Mode, Pure is
   subtype Time_Us is Unsigned_64;

   --  Kept before the latch for what the cost estimate misses (scheduling,
   --  the millisecond wakeup of Desktop's wait).
   Margin_Us : constant := 2_500;
   --  A latch older than this many refreshes no longer predicts the grid.
   Stale_Periods : constant := 4;
   --  Periods the grid is believed for (refresh rates down to ~4 Hz).
   Maximum_Period_Us : constant := 250_000;
   --  A frame cost beyond this is not a pacing input (a stall, a fault).
   Maximum_Cost_Us : constant := 100_000;
   --  The cost estimate decays by 1/Decay_Divisor of itself per frame.
   Decay_Divisor : constant := 16;

   type State is record
      Latch_Us : Time_Us := 0;        --  the last latch seen; 0: none
      Period_Us : Time_Us := 0;       --  its refresh period; 0: none known
      Cost_Us : Time_Us := 0;         --  decaying maximum frame cost
   end record;

   --  A frame latched at Latch_Us on a refresh of Period_Us (0: none).
   procedure Latched (S : in out State; Latch_Us, Period_Us : Time_Us)
     with Post => S.Cost_Us = S'Old.Cost_Us;

   --  A frame whose composition began at Started_Us was armed at Armed_Us.
   procedure Frame_Cost (S : in out State; Started_Us, Armed_Us : Time_Us)
     with Post => S.Latch_Us = S'Old.Latch_Us and S.Period_Us = S'Old.Period_Us and
       S.Cost_Us <= Maximum_Cost_Us;

   --  The first latch after Now on the known grid (Now when none is known).
   function Next_Latch (S : State; Now : Time_Us) return Time_Us;

   --  When to start composing a frame whose damage is pending at Now: Now,
   --  or a later time that still makes the next latch.
   function Compose_At (S : State; Now : Time_Us) return Time_Us
     with Post => Compose_At'Result >= Now and then
       (if Compose_At'Result > Now then
          S.Period_Us in 1 .. Maximum_Period_Us and then
          Compose_At'Result + S.Cost_Us + Margin_Us = Next_Latch (S, Now));
end Compositor_Pacing;
