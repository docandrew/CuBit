------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The kernel's system publications (KERN-002, docs/syscall-retirement.md):
--  system facts, the service registry and the wall clock, read from pages
--  the kernel maps read-only into every process; the process's own
--  statistics and its threads' system-call counts, read from its readiness
--  and thread-count pages (Readiness_Publication); and PUBLISH. Shared by
--  the Ada runtime and the libc's Ada, so it needs no run-time library and
--  calls the kernel through CuBit.Kernel_Calls.
--
--  @description
--  The page layouts are shared/publications/system_publications.ads; this
--  spec repeats only the reader's types, so units that use it need nothing
--  outside the runtime directory (the body checks they match). Every value
--  is one word the kernel writes with one store; a page not yet written
--  reads as zeros.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.System_Facts is

   package SP is
      --  The boot (firmware) framebuffer; Width 0: none.
      type Framebuffer_Geometry is record
         Width          : Unsigned_64 := 0;
         Height         : Unsigned_64 := 0;
         Pitch          : Unsigned_64 := 0;   --  bytes per row
         Bits_Per_Pixel : Unsigned_64 := 0;
      end record;

      --  The platform monotonic clock's startup diagnostic (kernel
      --  Platform_Monotonic), a snapshot at boot.
      type Clock_Diagnostic is record
         Status, HPET_ID, Period_Fs : Unsigned_64 := 0;
         Timer_Offset, Timer_Before, Timer_After : Unsigned_64 := 0;
      end record;

      --  A thread's line in the process's thread-count pages: the main
      --  thread's is Main_Thread_Slot; a THREAD_CREATE thread's is in the
      --  call's result and in the thread's second entry argument (RSI).
      Maximum_Threads : constant := 128;
      type Thread_Slot is range 0 .. Maximum_Threads - 1;
      Main_Thread_Slot : constant Thread_Slot := 0;

      --  What a service publishes through the kernel (PUBLISH).
      type Published_Value is (Wall_Clock_Offset, Boot_Display_Native);
      for Published_Value use (Wall_Clock_Offset => 1, Boot_Display_Native => 2);
   end SP;

   --  Logical processors the kernel found (ACPI MADT).
   function Processor_Count return Unsigned_64;

   --  RAM the kernel manages, in bytes.
   function Memory_Bytes return Unsigned_64;

   --  The boot (firmware) framebuffer's geometry; Width 0: none.
   function Framebuffer return SP.Framebuffer_Geometry;

   --  devmgr gave the boot display adapter to its native driver; the
   --  firmware framebuffer is not a fallback then.
   function Boot_Display_Native return Boolean;

   --  Device-memory requests the kernel has refused so far (CAP-001); what
   --  and why reach logstore as "kernel: device-memory denied #N" records.
   function Device_Memory_Denials return Unsigned_64;

   --  User processes the kernel has killed for a fault so far; who, why and
   --  where reach logstore as "kernel: process killed #N" records.
   function Process_Kills return Unsigned_64;

   --  Times the kernel's screen diagnostics erased part of the boot
   --  framebuffer after its handover: when this changes, the framebuffer's
   --  owner copies a whole frame again.
   function Scanout_Overwrites return Unsigned_64;

   --  The futex pause's limit in TSC cycles (IPC-010, kernel User_Wait);
   --  0: no pause (the build, the CPU or a CPU's MSR said no).
   function User_Wait_Cycles return Unsigned_64;

   --  The platform clock's startup diagnostic (snapshot at boot).
   function Clock return SP.Clock_Diagnostic;

   --  The identity word registered for Role (CuBit.Process_IDs wire form),
   --  or 0 for none.
   function Registered (Role : Unsigned_64) return Unsigned_64;

   --  UTC milliseconds since the Unix epoch at monotonic time zero, or 0
   --  while unknown (no clock service, or its time not current).
   function Wall_Clock_Offset return Unsigned_64;

   --  Free physical memory in the whole system, in bytes (a snapshot at
   --  most about 10 ms old); 0 before the page is written.
   function Free_Memory_Bytes return Unsigned_64;

   --  This process's own physical frames, in bytes: excludes borrowed
   --  mappings, page-table frames and other services' allocations.
   function Owned_Bytes return Unsigned_64;

   --  Events this process's mailbox refused because a publisher's ring
   --  was full.
   function Event_Drops return Unsigned_64;

   --  The system calls the thread in Slot has made so far (its current one
   --  included); a slot's count restarts from 0 for each thread that takes
   --  it. Reading one is a load, not a system call.
   function Thread_System_Calls (Slot : SP.Thread_Slot) return Unsigned_64;

   --  The calling thread's own count: Thread_System_Calls (its slot). A
   --  thread other than the main one first records the slot its entry
   --  received (Enter_Thread); until it does it reads the main thread's.
   --  Uses the thread pointer (thread-local storage).
   function Thread_System_Calls return Unsigned_64;
   procedure Enter_Thread (Slot : SP.Thread_Slot);

   --  Publish Word as Value through the kernel. Only the value's publisher
   --  may (the registered clock service, or the registered device manager).
   function Publish
     (Value : SP.Published_Value; Word : Unsigned_64) return Boolean;

end CuBit.System_Facts;
