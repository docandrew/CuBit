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
--
--  Identity is the process's own identity word (its PID and the
--  generation of this life), written once before the process runs: a
--  process learns who it is with a load, not a system call.
--
--  Owned_Bytes and Event_Drops are the process's own statistics (they
--  replace SYSCALL_READ_STATISTICS): its own physical frames in bytes,
--  stored whenever its frame list changes, and the events its mailbox
--  refused because a publisher's ring was full.
--
--  Thread counts: the two pages after the readiness page hold one 64-byte
--  line per thread slot (a cache line each, so no two threads' counters
--  share one). The kernel gives each thread the lowest free slot of its
--  process when it is made (the main thread slot 0, a THREAD_CREATE thread
--  the slot in its result and in its second entry argument) and frees it
--  when the thread exits; the next thread to take it starts from zero.
--  Only the thread itself writes its line (at each system call entry), so
--  a load reads a whole count.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Clock_Publication;

package Readiness_Publication with Pure, SPARK_Mode is

   Page_Bytes   : constant := Clock_Publication.Page_Bytes;
   Page_Address : constant := Clock_Publication.Page_Address + 6 * Page_Bytes;

   Layout_Version : constant := 3;

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
      Identity        : Unsigned_64     := 0;
      --  The process's own physical frames, in bytes: excludes borrowed
      --  mappings, page-table frames, these pages and other services'
      --  allocations.
      Owned_Bytes     : Unsigned_64     := 0;
      --  Events its mailbox refused because the publisher's ring was full.
      Event_Drops     : Count           := 0;
   end record;
   for Readiness_Page use record
      Version         at 0  range 0 .. 63;
      Produced        at 8  range 0 .. 3 * 64 - 1;
      Wake_Generation at 32 range 0 .. 63;
      Identity        at 40 range 0 .. 63;
      Owned_Bytes     at 48 range 0 .. 63;
      Event_Drops     at 56 range 0 .. 63;
   end record;
   --  Where a process reads its own identity word.
   Identity_Offset  : constant := 40;
   Identity_Address : constant := Page_Address + Identity_Offset;
   pragma Compile_Time_Error (Readiness_Page'Size / 8 > Page_Bytes,
                              "the readiness page must fit one page");

   ---------------------------------------------------------------------------
   --  Thread counts (the two pages after the readiness page)
   ---------------------------------------------------------------------------

   --  One slot per live thread: Maximum_Threads is the kernel's per-process
   --  thread limit (Process.MAX_THREADS_PER_PROCESS), so every live thread
   --  has one.
   Maximum_Threads : constant := 128;
   type Thread_Slot is range 0 .. Maximum_Threads - 1;
   Main_Thread_Slot : constant Thread_Slot := 0;

   Slot_Bytes      : constant := 64;   --  one cache line
   Slots_Per_Page  : constant := Page_Bytes / Slot_Bytes;
   Thread_Count_Pages : constant := Maximum_Threads / Slots_Per_Page;
   Thread_Counts_Address : constant := Page_Address + Page_Bytes;

   --  The rest of a line's cache line, kept zero (room for more counts).
   type Line_Padding is array (1 .. Slot_Bytes / 8 - 1) of Unsigned_64;

   type Thread_Line is record
      --  System calls the slot's thread has made, the current one included.
      System_Calls : Unsigned_64  := 0;
      Padding      : Line_Padding := [others => 0];
   end record;
   for Thread_Line use record
      System_Calls at 0 range 0 .. 63;
      Padding      at 8 range 0 .. (Slot_Bytes - 8) * 8 - 1;
   end record;
   for Thread_Line'Size use Slot_Bytes * 8;

   --  Which of the thread-count pages holds Slot, and where in it.
   function Page_Of (Slot : Thread_Slot) return Natural is
     (Natural (Slot) / Slots_Per_Page);
   function Offset_Of (Slot : Thread_Slot) return Natural is
     ((Natural (Slot) mod Slots_Per_Page) * Slot_Bytes);
   --  Where a process reads the line of one of its threads.
   function Line_Address (Slot : Thread_Slot) return Unsigned_64 is
     (Thread_Counts_Address + Unsigned_64 (Slot) * Slot_Bytes);

   pragma Compile_Time_Error
     (Maximum_Threads mod Slots_Per_Page /= 0,
      "the thread-count pages hold a whole number of lines");

   --  A class has work: produced moved past what the process consumed.
   function Behind (Produced, Consumed : Count) return Boolean is
     (Produced > Consumed);

end Readiness_Publication;
