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
--  Requests and completions are counted exactly once each when queued, so
--  the entries still queued never exceed Produced minus Consumed. For them a
--  loop also calls Took per entry, and a drain that took every counted entry
--  stops with no empty poll. Events are not counted one to one (one doorbell
--  can stand for several grant notices, channel wakes coalesce), so an
--  event drain always runs until a poll finds nothing; Took does not accept
--  Events. A count with no entry behind it (an entry another thread took)
--  costs one empty poll. Without the page (an older kernel) every class is
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

   --  The classes whose every entry is counted once when queued.
   subtype Counted_Class is Queue_Class
     with Static_Predicate => Counted_Class in RP.Requests | RP.Completions;

   --  A poll of Class took one entry.
   procedure Took (T : in out Tracker; Class : Counted_Class);

   --  A poll of Class found nothing after Produced returned Seen.
   procedure Caught_Up (T : in out Tracker; Class : Queue_Class; Seen : Count);

   --  Wakes of this process's waiting threads, ever (diagnostics).
   function Wake_Generation return Count;

end CuBit.Readiness;
