------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  SYSCALL_PROCLIST entries, as the kernel writes them
--  (kernel/src/syscall-admin.adb, handleProclist). Every reader decodes
--  them here rather than by hand.
--
--  One 32-byte entry per process, little-endian:
--     0  u64 identity (KERN-003, docs/process-objects.md)
--     8  u8  state (the kernel's ProcessState position)
--     9  u8  cpu              10  i16 priority
--    12  u32 frames (4 KiB pages)
--    16  name (16 bytes, NUL padded)
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Process_IDs; use CuBit.Process_IDs;

package CuBit.Process_List is
   Entry_Bytes : constant := 32;
   Name_Bytes  : constant := 16;

   Identity_Offset : constant := 0;
   State_Offset    : constant := 8;
   CPU_Offset      : constant := 9;
   Priority_Offset : constant := 10;
   Frames_Offset   : constant := 12;
   Name_Offset     : constant := 16;

   type Process_Entry is record
      Identity : Process_ID := No_Process;
      State    : Unsigned_8 := 0;
      CPU      : Unsigned_8 := 0;
      Priority : Integer_16 := 0;
      Frames   : Unsigned_32 := 0;
      Name     : String (1 .. Name_Bytes) := [others => ASCII.NUL];
   end record;
   for Process_Entry use record
      Identity at Identity_Offset range 0 .. 63;
      State    at State_Offset range 0 .. 7;
      CPU      at CPU_Offset range 0 .. 7;
      Priority at Priority_Offset range 0 .. 15;
      Frames   at Frames_Offset range 0 .. 31;
      Name     at Name_Offset range 0 .. Name_Bytes * 8 - 1;
   end record;
   for Process_Entry'Size use Entry_Bytes * 8;

   --  Entry Index (0-based) of a buffer the kernel filled.
   function Get (Buffer : System.Address; Index : Natural) return Process_Entry;

   --  The name without its padding.
   function Name_Of (Item : Process_Entry) return String;
end CuBit.Process_List;
