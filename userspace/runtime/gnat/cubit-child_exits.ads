------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  EVENT_CHILD_EXIT, the kernel's notice that a process retired: sent to its
--  parent (the launcher procmgr named) and to procmgr. Must match
--  kernel/src/process_launch.ads and ipc_labels.ads.
--
--    words (0) = PID, (1) = Termination_Kind, (2) = exit code (0 .. 255),
--    (3) = the process's generation while it lived: with the PID, its
--    incarnation (OP_LAUNCH replies with the same), since PIDs are reused
--
--  Events are not unforgeable: a receiver that acts on one should confirm
--  that the PID is really gone (procmgr checks the process list).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Child_Exits with Pure, SPARK_Mode is

   Event_Label : constant := 16#0103#;
   Event_Words : constant := 4;

   --  Exited: the process asked to end (SYSCALL_EXIT) with a code.
   --  Stopped: it was killed, faulted, or its main thread ended.
   type Termination_Kind is (Exited, Stopped);
   for Termination_Kind use (Exited => 1, Stopped => 2);

   Exit_Code_Modulus : constant := 256;
   subtype Exit_Code is Unsigned_64 range 0 .. Exit_Code_Modulus - 1;

   type Report is record
      Process : Unsigned_64 := 0;
      Kind    : Termination_Kind := Stopped;
      Code    : Exit_Code := 0;
      Generation : Unsigned_64 := 0;
   end record;

   function Valid (Length : Unsigned_8; Process, Kind, Code : Unsigned_64)
     return Boolean is
     (Length = Event_Words and then Process /= 0
      and then (Kind = Termination_Kind'Enum_Rep (Exited)
                or else Kind = Termination_Kind'Enum_Rep (Stopped))
      and then Code < Exit_Code_Modulus
      and then (if Kind = Termination_Kind'Enum_Rep (Stopped) then Code = 0));

   function Decode (Process, Kind, Code, Generation : Unsigned_64)
     return Report is
     ((Process => Process,
       Kind    => (if Kind = Termination_Kind'Enum_Rep (Exited) then Exited
                   else Stopped),
       Code    => Code,
       Generation => Generation))
   with Pre => Valid (Event_Words, Process, Kind, Code);

end CuBit.Child_Exits;
