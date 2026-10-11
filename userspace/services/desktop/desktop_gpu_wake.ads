------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  How Desktop's event loop sleeps while its GPU work runs (GPU-001 step 3
--  follow-up, docs/gpu-async-submission.md): on a completion event, never by
--  looking again on a timer when it can avoid it, and never by waiting on
--  the GPU. The loop arms one OP_GPU_WAKE (through the Mesa glue) with a
--  fresh request token and sleeps in its own wait, which input and every
--  other completion also end; the driver answers when the submitted work
--  completes. Without an event (no GPU queue, the driver's one wake held
--  elsewhere) it keeps the 1 ms timer it had.
--
--  Evidence: once per second, while anything happened, one log line with
--  the arms by outcome, the longest armed sleep, and the Mesa glue's
--  blocking calls by call site (count/total/max).
------------------------------------------------------------------------------
with Interfaces;
with CuBit.Messages;
package Desktop_GPU_Wake is
   use type Interfaces.Unsigned_64;

   --  How the loop waits for its pending GPU work. Event: sleep until
   --  activity (the wake is armed), bounded by Event_Fallback_Ms against a
   --  lost answer. Look_Again: the work completed already; take the next
   --  turn without sleeping (at most once in a row). Timer: no event; sleep
   --  until the next millisecond, as without one.
   type Wait_Mode is (Event, Look_Again, Timer);
   Event_Fallback_Ms : constant := 8;
   Timer_Ms          : constant := 1;

   --  Rendering: the loop's GPU path has work pending. Sequence: the loop's
   --  request sequence (CuBit request tokens). Now_Ms: the millisecond clock.
   procedure Prepare_Wait
     (Rendering : Boolean; Sequence : in out Interfaces.Unsigned_64; Now_Ms : Interfaces.Unsigned_64;
      Mode : out Wait_Mode);
   --  The loop slept from Slept_At_Ms to Now_Ms in Mode (for the evidence).
   procedure Woke (Mode : Wait_Mode; Slept_At_Ms, Now_Ms : Interfaces.Unsigned_64);

   --  Input arrived: hint the GPU driver to raise its clock before the
   --  frame is submitted (one-way OP_GPU_BOOST, coalesced in the glue).
   --  Never waits.
   procedure Input_Hint;

   function Matches (Token : Interfaces.Unsigned_64) return Boolean;
   procedure Collect (C : CuBit.Messages.CompletionEntry; Now_Ms : Interfaces.Unsigned_64);
end Desktop_GPU_Wake;
