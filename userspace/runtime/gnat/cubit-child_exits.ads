------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  EVENT_CHILD_EXIT, the kernel's notice that a process retired: sent to its
--  parent (the launcher procmgr named) and to procmgr. Must match
--  kernel/src/process_launch.ads and ipc_labels.ads.
--
--    words (0) = the identity of the life that ended (KERN-003; OP_LAUNCH
--    replies with the same), (1) = Termination_Kind, (2) = exit code
--    (0 .. 255).
--
--  Only the kernel can send this label (IPC_Labels.Is_Kernel_Event).
--
--  A program launched into a session (Launch-into-S, CAP-012) has procmgr,
--  its session supervisor's process, as kernel parent; procmgr tells the
--  requester with Notice_Label, the same words, through its own badge
--  (Notice_Tag, CuBit.Launch_Handles). Is_Exit accepts either.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Process_IDs; use CuBit.Process_IDs;

package CuBit.Child_Exits with Pure, SPARK_Mode is

   Event_Label : constant := 16#0103#;
   Event_Words : constant := 3;

   --  Exited: the process asked to end (SYSCALL_EXIT) with a code.
   --  Stopped: it was killed, faulted, or its main thread ended.
   type Termination_Kind is (Exited, Stopped);
   for Termination_Kind use (Exited => 1, Stopped => 2);

   Exit_Code_Modulus : constant := 256;
   subtype Exit_Code is Unsigned_64 range 0 .. Exit_Code_Modulus - 1;

   type Report is record
      Process : Process_ID := No_Process;
      Kind    : Termination_Kind := Stopped;
      Code    : Exit_Code := 0;
   end record;

   function Valid (Length : Unsigned_8; Process, Kind, Code : Unsigned_64)
     return Boolean is
     (Length = Event_Words and then Process /= 0
      and then (Kind = Termination_Kind'Enum_Rep (Exited)
                or else Kind = Termination_Kind'Enum_Rep (Stopped))
      and then Code < Exit_Code_Modulus
      and then (if Kind = Termination_Kind'Enum_Rep (Stopped) then Code = 0));

   --  procmgr's notice (CuBit.Launch_Handles.Exit_Notice_Label, Notice_Tag).
   Notice_Label : constant := 16#011F#;
   Notice_Tag   : constant Unsigned_64 := 16#4C4E_4F54_4943_4501#;

   --  An exit report: the kernel's event, or procmgr's notice through its
   --  badge (Tag: the message's kernel-stamped authority tag).
   function Is_Exit
     (Label : Unsigned_32; Length : Unsigned_8; Tag : Unsigned_64;
      Process, Kind, Code : Unsigned_64) return Boolean is
     ((Label = Event_Label or else (Label = Notice_Label and then Tag = Notice_Tag))
      and then Valid (Length, Process, Kind, Code));

   function Decode (Process, Kind, Code : Unsigned_64)
     return Report is
     ((Process => From_Word (Process),
       Kind    => (if Kind = Termination_Kind'Enum_Rep (Exited) then Exited
                   else Stopped),
       Code    => Code))
   with Pre => Valid (Event_Words, Process, Kind, Code);

end CuBit.Child_Exits;
