------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  tlb-probe's own pages and its reader thread: library-level, so every
--  life has Buffer at the same address (the image is linked at a fixed
--  base) and the thread's entry point needs no trampoline.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;

package Probe_Memory is
   Page_Bytes : constant := 4096;
   Words_Per_Page : constant := Page_Bytes / 8;
   Buffer_Pages : constant := 256;
   Granted_Pages : constant := 16;

   type Word_Array is array (Natural range <>) of Unsigned_64;

   --  Zero in every fresh life (.bss).
   Buffer : Word_Array (0 .. Buffer_Pages * Words_Per_Page - 1)
     with Alignment => Page_Bytes, Volatile;

   --  The unmap life's hand-off between the main thread and the reader.
   type Word is mod 2 ** 32 with Atomic, Size => 32;
   Started  : constant Word := 0;
   Read     : constant Word := 1;  --  the reader read the mapping, waits
   Unmapped : constant Word := 2;  --  the main thread unmapped it
   Survived : constant Word := 3;  --  the reader read it again (stale)
   Phase : aliased Word := Started;
   --  The kernel clears this, then wakes it, when the reader ends.
   Reader_Exit : aliased Word := 1;

   Mapped : System.Address := System.Null_Address with Volatile;
   Pattern : Unsigned_64 := 0 with Volatile;
   Seen_Before, Seen_After : Unsigned_64 := 0 with Volatile;

   --  Pages of the mapping whose first word holds Pattern.
   function Count_Pattern return Unsigned_64;

   --  The reader thread: reads the mapping (caching its translations on
   --  the CPU it runs on), says so, waits until the main thread unmapped
   --  it, and reads it again: that must fault.
   procedure Reader (Argument : Unsigned_64) with Convention => C;

   Stack_Size : constant := 16 * 1024;
   --  The reader's stack, from the heap; 0 when there is none.
   function New_Stack return Unsigned_64;
   --  Start the reader: CREATE_FAILED when the kernel refused.
   function Start_Reader (Stack : Unsigned_64) return Unsigned_64;
   CREATE_FAILED : constant Unsigned_64 := Unsigned_64'Last;
end Probe_Memory;
