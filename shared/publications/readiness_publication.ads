------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The readiness page (KERN-013): one kernel-written page per process,
--  mapped read-only, counting what was ever queued to the process.
--
--  @description
--  Each counter only goes up. The kernel adds one when it queues a service
--  request, an event (kernel notice, device doorbell, published event) or a
--  completion to the process, and Wake_Generation when it wakes one of the
--  process's waiting threads. The process keeps its own consumed count per
--  class (CuBit.Readiness): a class has work when its produced count moved
--  past the consumed count. It then drains the class until empty and sets
--  consumed to the produced count it read before draining, so an entry
--  queued meanwhile shows as work at the next check: at most one empty poll,
--  never a missed entry. Requests and completions count once per entry;
--  a call handed straight to a thread already blocked in its receive is not
--  queued and not counted (that thread takes it itself, never through a
--  poll). Events count once per doorbell, which may stand for several
--  entries, so an event drain runs until a poll finds nothing.
--
--  The clock publication is one page shared by every process; these counters
--  are per process, so they have their own page beside it (+6 pages).
--  The kernel writes each counter as one aligned 64-bit store under the
--  receiving mailbox's lock; readers need no lock.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Clock_Publication;

package Readiness_Publication with Pure, SPARK_Mode is

   Page_Bytes   : constant := Clock_Publication.Page_Bytes;
   Page_Address : constant := Clock_Publication.Page_Address + 6 * Page_Bytes;

   Layout_Version : constant := 1;

   --  A count of entries ever queued (or wakes). It never goes down; at
   --  its largest value it stays there (2**64 entries are never reached).
   type Count is new Unsigned_64;

   function Next (C : Count) return Count is
     (if C < Count'Last then C + 1 else Count'Last)
   with Post => Next'Result >= C
                and then (if C < Count'Last then Next'Result = C + 1);

   type Queue_Class is (Requests, Events, Completions);
   type Produced_Counts is array (Queue_Class) of Count;

   type Readiness_Page is record
      Version         : Unsigned_64     := Layout_Version;
      Produced        : Produced_Counts := [others => 0];
      Wake_Generation : Count           := 0;
   end record;
   for Readiness_Page use record
      Version         at 0  range 0 .. 63;
      Produced        at 8  range 0 .. 3 * 64 - 1;
      Wake_Generation at 32 range 0 .. 63;
   end record;
   pragma Compile_Time_Error (Readiness_Page'Size / 8 > Page_Bytes,
                              "the readiness page must fit one page");

   --  A class has work: produced moved past what the process consumed.
   function Behind (Produced, Consumed : Count) return Boolean is
     (Produced > Consumed);

end Readiness_Publication;
