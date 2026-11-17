------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Desktop's frame ring to Display (IPC-009, second half): every frame
--  Desktop presents goes to Display through shared memory, and Display's
--  answer comes back the same way. The kernel only set the channel up and
--  rings a side that sleeps.
--
--  @description
--  Open is the one synchronous call (Desktop's endpoint to Display, the
--  Display_Frame_Ring contract). Submit queues a present and, when Display
--  sleeps and asked to be rung, rings it: the one place Desktop wakes
--  Display (IPC-010 replaces the doorbell with a shared futex here). Reap
--  gives each answer as the completion entry the rest of Desktop already
--  matches (its Token is the presentation's token), and, after the channel
--  ended, each present still outstanding once more as a failed completion:
--  every present ends in an answer or a typed failure. Display_Frame_Client
--  refuses an answer for an unknown present or another frame (Strays).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Control_Events;
with CuBit.Handles;
with CuBit.Messages;
with Display_Frame_Ring;

package Desktop_Frame_Ring is

   package FR renames Display_Frame_Ring;

   --  How long Open waits for Display's answer.
   Open_Limit_Ms : constant := 500;

   --  Open the ring if it is not open (one call). Opened: it is open now.
   procedure Open (Display : CuBit.Handles.Endpoint; Opened : out Boolean);
   function Is_Open return Boolean;

   --  Queue a frame. Accepted False: the ring is not open or full (the
   --  caller tries again on a later turn, as with a refused submission).
   procedure Submit (Item : FR.Present; Token, Frame : Unsigned_64; Accepted : out Boolean);

   --  The next answer, or (after the ring ended) the next failure.
   procedure Reap (Completion : out CuBit.Messages.CompletionEntry; Got : out Boolean);

   --  Before Desktop sleeps: declare it asleep for answers. False: one
   --  arrived meanwhile (do not sleep). Disarm after waking.
   function Quiet return Boolean;
   procedure Disarm;

   --  A kernel grant event: Ended when it ended the ring (Display gone).
   procedure Take_Event (Event : CuBit.Control_Events.Event; Ended : out Boolean);

   --  Answers refused: for no outstanding present, or another frame's.
   function Strays return Unsigned_64;

end Desktop_Frame_Ring;
