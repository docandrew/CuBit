------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Channel wakes (IPC-010 option B), shared by the kernel and the runtime:
--  the notice a process's wake word raises. No system call of its own.
--
--  @description
--  Each process has one wake word: a futex word in a page it owns
--  (CuBit.Wake_Words), which its Ready wait names (Wait_ABI's Ready_Word
--  form). Every channel peer holds a read-only grant of that page and wakes
--  the process with a futex wake of the word (FUTEX_WAKE, shared space): it
--  needs no request credit, never overflows and is never lost. The waker
--  must be a party of a live grant of the page; nobody can send the notice
--  (SEND_EVENT with Wake_Label is refused).
--
--  The kernel coalesces wakes until one is taken: a Ready_Word wait returns
--  Ready_Word, and any other receive or event poll takes one event: label
--  Wake_Label, sender NO_PROCESS, Notice_Words word(s), word 0
--  Wake_Word_Notice. The notice names no channel: the process looks at the
--  channels it serves.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Channel_Wake_ABI with Pure, SPARK_Mode is

   Wake_Label       : constant := 16#010F#;
   Notice_Words     : constant := 1;
   Wake_Word_Notice : constant Unsigned_64 := 0;
   --  The most words a wake notice carries (receivers check lengths with it).
   Wakes_Per_Event  : constant := 4;
   pragma Compile_Time_Error (Notice_Words > Wakes_Per_Event,
                              "a wake notice fits a receiver's bound");

end Channel_Wake_ABI;
