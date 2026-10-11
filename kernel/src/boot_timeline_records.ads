-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The boot timeline's bookkeeping (BOOT-001, docs/boot-timeline.md): pure,
-- sequential, allocation-free updates of one Boot_Timeline_Layout.Image.
--
-- @description
-- Boot_Timeline (the kernel glue) serializes every call with one leaf
-- spinlock and supplies the TSC readings; nothing here reads a clock or
-- touches shared state. Proved (make prove-boot-timeline, level 2): no
-- run-time error for any call sequence; every process slot maps to a
-- recorded entry or to none; recorded entries are only appended, and each
-- per-entry time is written once (the first event wins); nothing changes
-- once recording has closed.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Boot_Timeline_Layout; use Boot_Timeline_Layout;

package Boot_Timeline_Records with
   SPARK_Mode => On
is
   --  The kernel's process slots (Process.ProcessID).
   Slot_Capacity : constant := 256;
   type Process_Slot is range 0 .. Slot_Capacity - 1;

   --  The entry recording each slot's current process; zero is none.
   type Slot_Map is array (Process_Slot) of Process_Count;

   --  Recording stops this long after the scheduling clock starts.
   Recording_Window_Seconds : constant := 30;
   --  Waits at least 1 / Wait_Fraction of a second get their own entry.
   Wait_Fraction : constant := 1_000;

   type State is record
      Wire : Image;
      Map  : Slot_Map := [others => 0];
   end record;

   function Recording (S : State) return Boolean is
     (S.Wire.Head.Recording = Open);

   --  Every mapped slot names an entry already recorded.
   function Valid (S : State) return Boolean is
     (for all P in Process_Slot => S.Map (P) <= S.Wire.Head.Processes);

   function Entry_Of (S : State; Slot : Process_Slot) return Process_Count is
     (S.Map (Slot));

   function Elapsed (Started, Finished : Ticks) return Ticks is
     (if Finished >= Started then Finished - Started else 0);

   function Saturating_Add (A, B : Ticks) return Ticks is
     (if A > Ticks'Last - B then Ticks'Last else A + B);

   function Saturating_Increment (C : Event_Count) return Event_Count is
     (if C = Event_Count'Last then C else C + 1);

   procedure Initialize (S : in out State)
   with Post => Valid (S) and then Recording (S)
                and then S.Wire.Head.Processes = 0
                and then S.Wire.Head.Waits = 0
                and then S.Wire.Head.Frequency = 0
                and then S.Wire.Head.Window_End = Not_Recorded;

   --  Close recording if Now is at or past the window's end.
   procedure Expire (S : in out State; Now : Ticks)
   with Pre  => Valid (S),
        Post => Valid (S)
                and then S.Wire.Stages = S.Wire.Stages'Old
                and then S.Wire.Milestones = S.Wire.Milestones'Old
                and then S.Wire.Processes = S.Wire.Processes'Old
                and then S.Wire.Waits = S.Wire.Waits'Old
                and then S.Wire.Head.Processes = S.Wire.Head.Processes'Old
                and then S.Wire.Head.Waits = S.Wire.Head.Waits'Old
                and then S.Map = S.Map'Old
                and then (if not Recording (S'Old) then S = S'Old);

   --  The first time Stage is reached; later marks change nothing.
   procedure Mark_Stage (S : in out State; Stage : Kernel_Stage; Now : Ticks)
   with Pre  => Valid (S),
        Post => Valid (S)
                and then (if S.Wire.Stages'Old (Slot_Of (Stage)) /= Not_Recorded
                          or else not Recording (S'Old)
                          then S.Wire.Stages = S.Wire.Stages'Old)
                and then S.Wire.Processes = S.Wire.Processes'Old
                and then S.Map = S.Map'Old;

   --  The admitted TSC rate (Hz) and the scheduling clock's start, from
   --  which the window and the long-wait threshold follow. Once only.
   procedure Set_Frequency (S : in out State; Frequency : Unsigned_64; Now : Ticks)
   with Pre  => Valid (S),
        Post => Valid (S)
                and then S.Wire.Processes = S.Wire.Processes'Old
                and then S.Map = S.Map'Old
                and then (if S.Wire.Head.Frequency'Old /= 0
                          then S = S'Old);

   --  A process loaded (suspended) in Slot by the process in Spawner_Slot
   --  (identity Spawner; No_Identity for the kernel). Takes the next entry,
   --  or counts a drop when none is left. A spawner that is still starting
   --  services is not ready yet: its Ready is cleared, and its next receive
   --  sets it again.
   procedure Note_Spawn
     (S            : in out State;
      Slot         : Process_Slot;
      Identity     : Identity_Word;
      Spawner      : Identity_Word;
      Spawner_Slot : Process_Slot;
      Name         : Name_Text;
      Started      : Ticks;
      Loaded       : Ticks)
   with Pre  => Valid (S),
        Post => Valid (S)
                and then S.Wire.Head.Processes >= S.Wire.Head.Processes'Old
                and then (for all N in 1 .. S.Wire.Head.Processes'Old =>
                            S.Wire.Processes (N).Identity = S.Wire.Processes'Old (N).Identity
                            and then S.Wire.Processes (N).Spawn_Started =
                                     S.Wire.Processes'Old (N).Spawn_Started
                            and then S.Wire.Processes (N).First_Run =
                                     S.Wire.Processes'Old (N).First_Run)
                and then (if not Recording (S'Old) then S = S'Old);

   type Process_Event is (Resumed, First_Run, Registered, Received);

   --  Event for the process in Slot. Resumed, First_Run and Registered keep
   --  their first time. Received sets First_Receive once, and Ready at the
   --  first receive after registration and after the process's last spawn.
   procedure Note_Event
     (S     : in out State;
      Slot  : Process_Slot;
      Event : Process_Event;
      Now   : Ticks)
   with Pre  => Valid (S),
        Post => Valid (S)
                and then S.Map = S.Map'Old
                and then S.Wire.Head.Processes = S.Wire.Head.Processes'Old
                and then S.Wire.Waits = S.Wire.Waits'Old
                and then (for all N in Process_Number =>
                            (if N /= S.Map (Slot)
                             then S.Wire.Processes (N) = S.Wire.Processes'Old (N)))
                and then (if not Recording (S'Old) then S = S'Old);

   --  The ready time of the process in Slot (Not_Recorded if none).
   function Ready_Of (S : State; Slot : Process_Slot) return Ticks is
     (if S.Map (Slot) in Process_Number
      then S.Wire.Processes (S.Map (Slot)).Ready else Not_Recorded)
   with Pre => Valid (S);

   --  A blocking call by the process in Slot from Started to Finished.
   --  Calls and sleeps always count; Receive, Completion and Futex waits
   --  count only before that process is Ready. A counted wait of at least
   --  Minimum_Wait also gets its own entry while any is left.
   procedure Note_Wait
     (S        : in out State;
      Slot     : Process_Slot;
      Waiter   : Identity_Word;
      Kind     : Wait_Kind;
      Label    : Unsigned_32;
      Target   : Identity_Word;
      Started  : Ticks;
      Finished : Ticks)
   with Pre  => Valid (S) and then Kind /= No_Wait,
        Post => Valid (S)
                and then S.Map = S.Map'Old
                and then S.Wire.Head.Processes = S.Wire.Head.Processes'Old
                and then S.Wire.Head.Waits >= S.Wire.Head.Waits'Old
                --  Recorded waits keep who, what and when; only their
                --  repeat count and total duration grow.
                and then (for all N in 1 .. S.Wire.Head.Waits'Old =>
                            S.Wire.Waits (N).Kind = S.Wire.Waits'Old (N).Kind
                            and then S.Wire.Waits (N).Waiter = S.Wire.Waits'Old (N).Waiter
                            and then S.Wire.Waits (N).Target = S.Wire.Waits'Old (N).Target
                            and then S.Wire.Waits (N).Started = S.Wire.Waits'Old (N).Started
                            and then S.Wire.Waits (N).Repeats >= S.Wire.Waits'Old (N).Repeats
                            and then S.Wire.Waits (N).Duration >= S.Wire.Waits'Old (N).Duration)
                and then (if not Recording (S'Old) then S = S'Old);

   --  The first mark of Item, by Marker; later marks change nothing.
   procedure Mark_Milestone
     (S      : in out State;
      Item   : Milestone;
      Marker : Identity_Word;
      Now    : Ticks)
   with Pre  => Valid (S),
        Post => Valid (S)
                and then S.Wire.Processes = S.Wire.Processes'Old
                and then S.Map = S.Map'Old
                and then (if S.Wire.Milestones'Old (Slot_Of (Item)).At_Ticks /= Not_Recorded
                          or else not Recording (S'Old)
                          then S.Wire.Milestones = S.Wire.Milestones'Old);
end Boot_Timeline_Records;
