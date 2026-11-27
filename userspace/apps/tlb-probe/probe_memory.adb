------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  tlb-probe's reader thread (no printing, no secondary stack).
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Process_Calls;
with CuBit.Waits;

package body Probe_Memory is
   package W renames CuBit.Waits;

   --  A SysV function expects RSP + 8 to be 16-byte aligned on entry.
   Stack_Alignment : constant := 16;
   Return_Slot : constant := 8;

   Ignore : Unsigned_64;

   function Count_Pattern return Unsigned_64 is
      --  The owner's granted pages, which it writes and revokes while this
      --  thread reads them (the tlb-tags test).
      Granted : Word_Array (0 .. Granted_Pages * Words_Per_Page - 1)
        with Import, Volatile, Async_Writers, Address => Mapped;
      Count : Unsigned_64 := 0;
   begin
      for P in 0 .. Granted_Pages - 1 loop
         if Granted (P * Words_Per_Page) = Pattern then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_Pattern;

   procedure Reader (Argument : Unsigned_64) is
      pragma Unreferenced (Argument);
   begin
      Seen_Before := Count_Pattern;
      Phase := Read;
      Ignore := W.Futex_Wake (W.Private_Word (Phase'Address), 1);
      while Phase = Read loop
         Ignore := W.Futex_Wait (W.Private_Word (Phase'Address),
                                 Unsigned_64 (Read), W.Forever);
      end loop;
      --  Unmapped: this read must fault, and the kernel ends the process.
      Seen_After := Count_Pattern;
      Phase := Survived;
      Ignore := W.Futex_Wake (W.Private_Word (Phase'Address), 1);
      CuBit.Process_Calls.Exit_Thread;
   end Reader;

   function New_Stack return Unsigned_64 is
      Base : constant Unsigned_64 := CuBit.Process_Calls.Grow_Heap (Stack_Size);
   begin
      if Base = Unsigned_64'Last or else Base = 0 then
         return 0;
      end if;
      return ((Base + Stack_Size) and not (Stack_Alignment - 1)) - Return_Slot;
   end New_Stack;

   function Start_Reader (Stack : Unsigned_64) return Unsigned_64 is
     (CuBit.Process_Calls.Create_Thread (Unsigned_64 (To_Integer (Reader'Address)), Stack, 0, 0,
                                         Unsigned_64 (To_Integer (Reader_Exit'Address))));
end Probe_Memory;
