------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Reading a boot timeline (BOOT-001, docs/boot-timeline.md): tick
--  conversion, the events in time order, the critical path to a milestone,
--  and the completeness check the headless test applies.
--
--  @description
--  Pure and allocation-free over one Boot_Timeline_Layout.Image whose
--  scalar fields the caller has validated ('Valid) after receiving it.
--  Proved (tests/boot-timeline/run.sh --prove, level 2): no run-time error
--  for any valid image, including cyclic or dangling spawner identities.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Boot_Timeline_Layout; use Boot_Timeline_Layout;

package Boot_Timeline_Report with
   SPARK_Mode
is
   subtype Microseconds is Unsigned_64;

   --  Admitted counter rates (the clock publication's range is 1-16 GHz;
   --  anything from 1 MHz converts without overflow).
   Minimum_Frequency : constant := 1_000_000;
   Maximum_Frequency : constant := 2**40;
   subtype Counter_Frequency is Unsigned_64
     range Minimum_Frequency .. Maximum_Frequency;

   Microseconds_Per_Second : constant := 1_000_000;

   --  T ticks at Frequency, rounded down; saturates at Microseconds'Last.
   function To_Microseconds (T : Ticks; Frequency : Counter_Frequency)
     return Microseconds;

   --  The entry recording Identity, or 0.
   function Index_Of (Img : Image; Identity : Identity_Word) return Process_Count
   with Post => Index_Of'Result <= Img.Head.Processes;

   --  The process's ready time: Ready, else its first receive.
   function Ready_Of (E : Process_Entry) return Ticks is
     (if E.Ready /= Not_Recorded then E.Ready else E.First_Receive);

   ---------------------------------------------------------------------------
   --  Events in time order
   ---------------------------------------------------------------------------
   type Event_Kind is
     (Stage_Reached, Spawned, Resumed, First_Run, Registered, Ready,
      Milestone_Marked);

   type Event is record
      Kind     : Event_Kind := Stage_Reached;
      At_Ticks : Ticks := Not_Recorded;
      Stage    : Kernel_Stage := Kernel_Entry;
      Process  : Process_Count := 0;
      Item     : Milestone := Milestone'First;
   end record;

   Events_Per_Process : constant := 5;   --  Spawned .. Ready
   Event_Capacity : constant :=
     Stage_Capacity + Milestone_Capacity + Process_Capacity * Events_Per_Process;
   type Listed_Count is range 0 .. Event_Capacity;
   subtype Listed_Index is Listed_Count range 1 .. Event_Capacity;
   type Event_List is array (Listed_Index) of Event;

   function Sorted (Events : Event_List; Count : Listed_Count) return Boolean is
     (for all I in 2 .. Count => Events (I - 1).At_Ticks <= Events (I).At_Ticks);

   --  Every recorded time, earliest first (equal times keep record order).
   procedure Collect (Img : Image; Events : out Event_List; Count : out Listed_Count)
   with Pre  => Img.Head.Processes <= Process_Capacity,
        Post => Sorted (Events, Count)
                and then (for all I in 1 .. Count =>
                            Events (I).Process <= Img.Head.Processes);

   ---------------------------------------------------------------------------
   --  Critical path: from power-on to Goal, following who spawned whom and
   --  the long waits each process on the way made before the next step.
   ---------------------------------------------------------------------------
   type Step_Kind is (Power_On, Stage_Step, Spawn_Step, Run_Step, Wait_Step, Goal_Step);

   type Step is record
      Kind     : Step_Kind := Power_On;
      At_Ticks : Ticks := Not_Recorded;
      Stage    : Kernel_Stage := Kernel_Entry;
      Process  : Process_Count := 0;
      Wait     : Wait_Count := 0;
   end record;

   Path_Capacity : constant := 96;
   type Path_Count is range 0 .. Path_Capacity;
   subtype Path_Index is Path_Count range 1 .. Path_Capacity;
   type Path_List is array (Path_Index) of Step;

   --  Found is False when Goal was not marked or its marker is unknown.
   procedure Critical_Path
     (Img   : Image;
      Goal  : Milestone;
      Path  : out Path_List;
      Count : out Path_Count;
      Found : out Boolean)
   with Pre  => Img.Head.Processes <= Process_Capacity
                and then Img.Head.Waits <= Wait_Capacity,
        Post => (for all I in 1 .. Count =>
                   Path (I).Process <= Img.Head.Processes
                   and then Path (I).Wait <= Img.Head.Waits);

   ---------------------------------------------------------------------------
   --  Completeness (the headless test's check)
   ---------------------------------------------------------------------------
   type Gap is
     (No_Frequency, Stage_Missing, No_Processes, Process_Missing,
      Process_Unstarted, Process_Not_Ready, Milestone_Missing, Dropped_Records);
   type Gap_Set is array (Gap) of Boolean;

   --  The services every desktop boot starts, by process name.
   Required_Count : constant := 4;
   type Required_Index is range 1 .. Required_Count;
   type Required_Names is array (Required_Index) of Name_Text;
   Required : constant Required_Names :=
     ["devmgr.svc      ", "procmgr.svc     ", "display.svc     ", "desktop.svc     "];

   --  The milestones a desktop boot must reach; input is optional (only
   --  a run that injects input can see it).
   type Milestone_Set is array (Milestone) of Boolean;
   Required_Milestones : constant Milestone_Set :=
     [First_Input_Accepted => False, others => True];

   function Name_Index (Img : Image; Name : Name_Text) return Process_Count
   with Post => Name_Index'Result <= Img.Head.Processes;

   procedure Check (Img : Image; Gaps : out Gap_Set; Complete : out Boolean)
   with Pre  => Img.Head.Processes <= Process_Capacity,
        Post => Complete = (for all G in Gap => not Gaps (G));
end Boot_Timeline_Report;
