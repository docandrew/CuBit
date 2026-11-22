------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Drain a queue class only when it has work (KERN-013): the kernel's
--  readiness page against the consumed counts a loop keeps itself.
--
--  @description
--  The kernel counts every service request, event and completion it queues
--  to this process on a read-only page (Readiness_Publication). A loop
--  keeps a Tracker and drains a class while it is Pending: it reads
--  Produced, polls once, and calls Caught_Up with the count it read when the
--  poll found nothing. Work that arrives meanwhile is above that count and
--  shows as Pending, so no entry is missed.
--
--  Every entry is counted at least once before it can be taken (requests
--  and completions once each; events once per notice, IRQ doorbell, wake
--  or published event; IPC-010), so the entries still queued never exceed
--  Produced minus what was consumed. A loop calls Took per entry it takes,
--  and a drain that took every counted entry stops with no empty poll. A
--  count with no entry behind it (a coalesced IRQ, an entry another thread
--  took) costs one empty poll. A wait that takes an entry itself (a kernel
--  notice, a wake of the wake word) says so with Note_Taken: every tracker
--  counts it consumed. Without the page (an older kernel) every class is
--  Pending, which is polling as before.
------------------------------------------------------------------------------
pragma Ada_2022;
with Readiness_Publication;

package CuBit.Readiness is

   package RP renames Readiness_Publication;
   subtype Queue_Class is RP.Queue_Class;
   subtype Count is RP.Count;
   Requests    : constant Queue_Class := RP.Requests;
   Events      : constant Queue_Class := RP.Events;
   Completions : constant Queue_Class := RP.Completions;

   type Tracker is record
      Consumed : RP.Produced_Counts := [others => 0];
   end record;

   --  What the kernel has queued to this process in Class, ever.
   function Produced (Class : Queue_Class) return Count;

   --  Class may have work this loop has not consumed.
   function Pending (T : Tracker; Class : Queue_Class) return Boolean;

   --  Some class has.
   function Any_Pending (T : Tracker) return Boolean is
     (for some C in Queue_Class => Pending (T, C));

   --  A poll of Class took one entry.
   procedure Took (T : in out Tracker; Class : Queue_Class);

   --  A wait took one entry of Class for this process (CuBit.Messages'
   --  activity waits: a notice or a wake); every tracker sees it consumed.
   procedure Note_Taken (Class : Queue_Class);

   --  A poll of Class found nothing after Produced returned Seen.
   procedure Caught_Up (T : in out Tracker; Class : Queue_Class; Seen : Count);

   --  Wakes of this process's waiting threads, ever (diagnostics).
   function Wake_Generation return Count;

end CuBit.Readiness;
