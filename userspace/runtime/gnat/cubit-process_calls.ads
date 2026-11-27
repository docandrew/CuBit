------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The kernel calls a program makes about itself: end the process, give
--  up the CPU, grow the heap, write to the kernel console, and make or end
--  a thread without a thread area.
--
--  @description
--  Services, drivers and apps reach the kernel only through the library
--  units (IPC-013; tests/removed-abi/library-only.sh): this one for these
--  calls, CuBit.Waits, CuBit.Handles and the channel and message units for
--  the rest. A change of mechanism then touches only the library.
--
--  Every entry point is Inline_Always: a call compiles to the same call of
--  the system-call instruction (CuBit.Kernel_Calls.Call) the program would
--  make itself, with the same arguments, and no wrapper frame.
--
--  Threads that run Ada code using the secondary stack are made with a
--  thread area (CuBit.Thread_Areas.Create, then Create_Thread with it as
--  the thread pointer) and end with CuBit.Thread_Areas.Exit_Thread. The
--  raw Exit_Thread here is for a thread without one (its FS base is zero or
--  a value the program set for itself).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Child_Exits;
with CuBit.Kernel_ABI;

package CuBit.Process_Calls with Preelaborate is

   ---------------------------------------------------------------------------
   --  The process
   ---------------------------------------------------------------------------

   --  What the process reports when it ends (the kernel's exit report,
   --  CuBit.Child_Exits: 0 .. 255).
   subtype Exit_Code is CuBit.Child_Exits.Exit_Code;
   Success : constant Exit_Code := 0;
   Failure : constant Exit_Code := 1;

   --  End this process, every thread of it, with Code. Nothing is flushed
   --  first: a program that logs flushes its log itself.
   procedure Exit_Process (Code : Exit_Code := Success)
   with No_Return, Inline_Always;

   --  A count (failures, say) as an exit code, as the kernel reduces it.
   function Code_Of (Count : Unsigned_64) return Exit_Code is
     (Count mod CuBit.Child_Exits.Exit_Code_Modulus);

   --  Give the CPU to any other ready thread; run again when next picked.
   procedure Yield with Inline_Always;

   ---------------------------------------------------------------------------
   --  The heap
   ---------------------------------------------------------------------------

   --  Grow_Heap's refusal.
   Heap_Refused : constant Unsigned_64 := CuBit.Kernel_ABI.Failed;

   --  Move the heap's break up by Bytes (rounded up to pages, zero-filled):
   --  the old break, which is where the new bytes start, or Heap_Refused
   --  (the quota, or the next region in the address space). Bytes = 0
   --  reads the break.
   function Grow_Heap (Bytes : Unsigned_64) return Unsigned_64
   with Inline_Always;

   ---------------------------------------------------------------------------
   --  The kernel console
   ---------------------------------------------------------------------------

   --  Write Length bytes at Text to the kernel console (the serial line
   --  and boot log), bypassing logstore: the number written, or zero when
   --  the kernel cannot read the range. For probes and early start-up;
   --  programs log through CuBit.Logging.
   function Write_Console (Text : System.Address; Length : Unsigned_64)
     return Unsigned_64
   with Inline_Always;

   procedure Write_Console (Text : String) with Inline_Always;

   ---------------------------------------------------------------------------
   --  Threads
   ---------------------------------------------------------------------------

   --  A thread of this process (THREAD_CREATE): it starts at Start with RSP
   --  Stack, Argument in RDI (its thread-count slot in RSI) and FS base
   --  Thread_Pointer. When it ends, the kernel clears the 32-bit word at
   --  Exit_Word, then wakes its futex (0: none). The result is Thread_ABI's:
   --  the thread and its slot, or Thread_ABI.Refused.
   function Create_Thread
     (Start, Stack, Argument, Thread_Pointer, Exit_Word : Unsigned_64)
     return Unsigned_64
   with Inline_Always;

   --  End the calling thread, which has no thread area (above).
   procedure Exit_Thread with No_Return, Inline_Always;

end CuBit.Process_Calls;
