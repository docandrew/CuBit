------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Deskbar's Battery item talking to power.svc (PWR-004 slice 1),
--  never waiting on it: each request is submitted and its reply taken as a
--  completion in the event loop (the IPC graph stays a DAG).
--
--  @description
--  Desktop holds power.svc's endpoint badged for power sources (its
--  manifest's optional "power-sources"; CuBit.Power_Sources). At start it
--  asks Read_Now (power.svc reads its sources then), then keeps one Watch
--  parked with the generation it saw: power.svc answers it when the
--  snapshot changes, and every answer submits the next watch. Opening the
--  panel asks Read_Now again (query on read). Nothing is polled. A watch
--  that is refused No_Room, or not delivered, is watched again after
--  Desktop_Status_Machines.Retry_Ms; one not delivered also hides the item
--  (power.svc is gone) until an answer comes again.
------------------------------------------------------------------------------
with Interfaces;
with CuBit.Messages;
with CuBit.Power_Sources;

package Desktop_Power_Refresh is
   use type Interfaces.Unsigned_64;

   function Granted return Boolean;

   --  Keep the first read and the watch submitted (a failed watch again
   --  once Next_Due has passed). Now: monotonic milliseconds.
   procedure Start (Now : Interfaces.Unsigned_64; Sequence : in out Interfaces.Unsigned_64);

   --  The panel opened: read the sources again.
   procedure Read_Now (Sequence : in out Interfaces.Unsigned_64);

   function Matches (Token : Interfaces.Unsigned_64) return Boolean;
   procedure Collect (C_Entry : CuBit.Messages.CompletionEntry; Now : Interfaces.Unsigned_64;
                      Sequence : in out Interfaces.Unsigned_64);

   --  The newest summary. Have: one was decoded and power.svc still
   --  answers; Fresh is cleared by Take.
   procedure Take (Value : out CuBit.Power_Sources.Summary; Have, Fresh : out Boolean);

   --  When Start must run again for a retry (0: nothing due).
   function Next_Due return Interfaces.Unsigned_64;
end Desktop_Power_Refresh;
