------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  Boot_Timeline_Report body: see the specification.
------------------------------------------------------------------------------
pragma Ada_2022;

package body Boot_Timeline_Report with
   SPARK_Mode
is
   function To_Microseconds (T : Ticks; Frequency : Counter_Frequency)
     return Microseconds
   is
      Whole : constant Unsigned_64 := T / Frequency;
      Part  : constant Unsigned_64 := T mod Frequency;
   begin
      --  Part < Frequency <= 2**40, so Part * 10**6 < 2**60: no wrap.
      if Whole > Unsigned_64'Last / Microseconds_Per_Second - 1 then
         return Microseconds'Last;
      end if;
      return Whole * Microseconds_Per_Second
        + (Part * Microseconds_Per_Second) / Frequency;
   end To_Microseconds;

   function Index_Of (Img : Image; Identity : Identity_Word) return Process_Count is
   begin
      if Identity = No_Identity then
         return 0;
      end if;
      for N in 1 .. Img.Head.Processes loop
         if Img.Processes (N).Identity = Identity then
            return N;
         end if;
         pragma Loop_Invariant (N <= Img.Head.Processes);
      end loop;
      return 0;
   end Index_Of;

   function Name_Index (Img : Image; Name : Name_Text) return Process_Count is
   begin
      for N in 1 .. Img.Head.Processes loop
         if Img.Processes (N).Name = Name then
            return N;
         end if;
      end loop;
      return 0;
   end Name_Index;

   procedure Collect (Img : Image; Events : out Event_List; Count : out Listed_Count) is
      --  Insert keeping Events (1 .. Count) sorted by time.
      procedure Insert (Item : Event)
      with Pre  => Count < Event_Capacity and then Sorted (Events, Count)
                   and then Item.Process <= Img.Head.Processes
                   and then (for all I in 1 .. Count =>
                               Events (I).Process <= Img.Head.Processes),
           Post => Count = Count'Old + 1 and then Sorted (Events, Count)
                   and then (for all I in 1 .. Count =>
                               Events (I).Process <= Img.Head.Processes)
      is
         Position : Listed_Index := Count + 1;
      begin
         --  Shift later events up by one; Position ends where Item goes.
         while Position > 1 and then Events (Position - 1).At_Ticks > Item.At_Ticks loop
            pragma Loop_Invariant (Position in 2 .. Count + 1);
            pragma Loop_Invariant
              (for all I in 2 .. Position - 1 =>
                 Events (I - 1).At_Ticks <= Events (I).At_Ticks);
            pragma Loop_Invariant
              (for all I in Position + 1 .. Count + 1 =>
                 Events (I).At_Ticks > Item.At_Ticks);
            pragma Loop_Invariant
              (for all I in Position + 2 .. Count + 1 =>
                 Events (I - 1).At_Ticks <= Events (I).At_Ticks);
            pragma Loop_Invariant
              (if Position <= Count then
                 Events (Position - 1).At_Ticks <= Events (Position + 1).At_Ticks);
            pragma Loop_Invariant
              (for all I in 1 .. Count + 1 =>
                 (if I /= Position then Events (I).Process <= Img.Head.Processes));
            Events (Position) := Events (Position - 1);
            Position := Position - 1;
         end loop;
         Events (Position) := Item;
         Count := Count + 1;
      end Insert;

      procedure Add (Item : Event)
      with Pre  => Sorted (Events, Count)
                   and then Item.Process <= Img.Head.Processes
                   and then (for all I in 1 .. Count =>
                               Events (I).Process <= Img.Head.Processes),
           Post => Sorted (Events, Count) and then Count >= Count'Old
                   and then (for all I in 1 .. Count =>
                               Events (I).Process <= Img.Head.Processes)
      is
      begin
         if Item.At_Ticks /= Not_Recorded and then Count < Event_Capacity then
            Insert (Item);
         end if;
      end Add;
   begin
      Events := [others => (others => <>)];
      Count := 0;
      for Stage in Kernel_Stage loop
         pragma Loop_Invariant
           (Sorted (Events, Count)
            and then (for all I in 1 .. Count => Events (I).Process <= Img.Head.Processes));
         Add ((Kind => Stage_Reached, At_Ticks => Img.Stages (Slot_Of (Stage)),
               Stage => Stage, others => <>));
      end loop;
      for N in 1 .. Img.Head.Processes loop
         pragma Loop_Invariant
           (Sorted (Events, Count)
            and then (for all I in 1 .. Count => Events (I).Process <= Img.Head.Processes));
         declare
            E : Process_Entry renames Img.Processes (N);
         begin
            Add ((Kind => Spawned, At_Ticks => E.Spawn_Started, Process => N, others => <>));
            Add ((Kind => Resumed, At_Ticks => E.Resumed, Process => N, others => <>));
            Add ((Kind => First_Run, At_Ticks => E.First_Run, Process => N, others => <>));
            Add ((Kind => Registered, At_Ticks => E.Registered, Process => N, others => <>));
            Add ((Kind => Ready, At_Ticks => Ready_Of (E), Process => N, others => <>));
         end;
      end loop;
      for Item in Milestone loop
         pragma Loop_Invariant
           (Sorted (Events, Count)
            and then (for all I in 1 .. Count => Events (I).Process <= Img.Head.Processes));
         Add ((Kind => Milestone_Marked, At_Ticks => Img.Milestones (Slot_Of (Item)).At_Ticks,
               Item => Item, others => <>));
      end loop;
   end Collect;

   procedure Critical_Path
     (Img   : Image;
      Goal  : Milestone;
      Path  : out Path_List;
      Count : out Path_Count;
      Found : out Boolean)
   is
      --  Steps are gathered newest first, then reversed.
      Reversed : Path_List := [others => (others => <>)];
      Taken    : Path_Count := 0;

      procedure Push (Item : Step)
      with Pre  => Item.Process <= Img.Head.Processes
                   and then Item.Wait <= Img.Head.Waits
                   and then (for all I in 1 .. Taken =>
                               Reversed (I).Process <= Img.Head.Processes
                               and then Reversed (I).Wait <= Img.Head.Waits),
           Post => (for all I in 1 .. Taken =>
                      Reversed (I).Process <= Img.Head.Processes
                      and then Reversed (I).Wait <= Img.Head.Waits)
      is
      begin
         if Taken < Path_Capacity then
            Taken := Taken + 1;
            Reversed (Taken) := Item;
         end if;
      end Push;

      Mark    : constant Milestone_Entry := Img.Milestones (Slot_Of (Goal));
      Current : Process_Count;
      Until_T : Ticks;
   begin
      Path := [others => (others => <>)];
      Count := 0;
      Found := False;
      if Mark.At_Ticks = Not_Recorded then
         return;
      end if;
      Current := Index_Of (Img, Mark.Marker);
      if Current = 0 then
         return;
      end if;
      Found := True;
      Push ((Kind => Goal_Step, At_Ticks => Mark.At_Ticks, Process => Current, others => <>));
      Until_T := Mark.At_Ticks;
      --  Each round climbs one spawner; a cycle ends when rounds run out.
      for Round in 1 .. Process_Capacity loop
         pragma Loop_Invariant
           (Current in 1 .. Img.Head.Processes
            and then (for all I in 1 .. Taken =>
                        Reversed (I).Process <= Img.Head.Processes
                        and then Reversed (I).Wait <= Img.Head.Waits));
         declare
            E : constant Process_Entry := Img.Processes (Current);
            Spawner : constant Process_Count := Index_Of (Img, E.Spawner);
         begin
            --  Long waits between this process's start and the next step,
            --  newest first.
            for W in reverse 1 .. Img.Head.Waits loop
               pragma Loop_Invariant
                 (Current in 1 .. Img.Head.Processes
                  and then (for all I in 1 .. Taken =>
                              Reversed (I).Process <= Img.Head.Processes
                              and then Reversed (I).Wait <= Img.Head.Waits));
               if Img.Waits (W).Waiter = E.Identity
                 and then Img.Waits (W).Started >= E.First_Run
                 and then Img.Waits (W).Started < Until_T
               then
                  Push ((Kind => Wait_Step, At_Ticks => Img.Waits (W).Started,
                         Process => Current, Wait => W, others => <>));
               end if;
            end loop;
            if E.First_Run /= Not_Recorded then
               Push ((Kind => Run_Step, At_Ticks => E.First_Run, Process => Current,
                      others => <>));
            end if;
            Push ((Kind => Spawn_Step, At_Ticks => E.Spawn_Started, Process => Current,
                   others => <>));
            if Spawner = 0 or else Spawner = Current then
               --  Spawned by the kernel: its stages before this spawn.
               for Stage in reverse Kernel_Stage loop
                  pragma Loop_Invariant
                    (for all I in 1 .. Taken =>
                       Reversed (I).Process <= Img.Head.Processes
                       and then Reversed (I).Wait <= Img.Head.Waits);
                  if Img.Stages (Slot_Of (Stage)) /= Not_Recorded
                    and then Img.Stages (Slot_Of (Stage)) <= E.Spawn_Started
                  then
                     Push ((Kind => Stage_Step, At_Ticks => Img.Stages (Slot_Of (Stage)),
                            Stage => Stage, others => <>));
                  end if;
               end loop;
               exit;
            end if;
            Until_T := E.Spawn_Started;
            Current := Spawner;
         end;
      end loop;
      Push ((Kind => Power_On, others => <>));
      for I in 1 .. Taken loop
         pragma Loop_Invariant
           (for all J in 1 .. Count =>
              Path (J).Process <= Img.Head.Processes
              and then Path (J).Wait <= Img.Head.Waits);
         pragma Loop_Invariant (Count = I - 1);
         Count := Count + 1;
         Path (Count) := Reversed (Taken - I + 1);
      end loop;
   end Critical_Path;

   procedure Check (Img : Image; Gaps : out Gap_Set; Complete : out Boolean) is
   begin
      Gaps := [others => False];
      Gaps (No_Frequency) := Img.Head.Frequency < Minimum_Frequency
                             or else Img.Head.Frequency > Maximum_Frequency;
      for Stage in Kernel_Stage loop
         if Img.Stages (Slot_Of (Stage)) = Not_Recorded then
            Gaps (Stage_Missing) := True;
         end if;
      end loop;
      Gaps (No_Processes) := Img.Head.Processes = 0;
      for Name of Required loop
         declare
            N : constant Process_Count := Name_Index (Img, Name);
         begin
            if N = 0 then
               Gaps (Process_Missing) := True;
            else
               if Img.Processes (N).Loaded = Not_Recorded
                 or else Img.Processes (N).First_Run = Not_Recorded
               then
                  Gaps (Process_Unstarted) := True;
               end if;
               if Ready_Of (Img.Processes (N)) = Not_Recorded then
                  Gaps (Process_Not_Ready) := True;
               end if;
            end if;
         end;
      end loop;
      for Item in Milestone loop
         if Required_Milestones (Item)
           and then Img.Milestones (Slot_Of (Item)).At_Ticks = Not_Recorded
         then
            Gaps (Milestone_Missing) := True;
         end if;
      end loop;
      Gaps (Dropped_Records) := Img.Head.Processes_Dropped > 0
                                or else Img.Head.Waits_Dropped > 0;
      Complete := (for all G in Gap => not Gaps (G));
   end Check;
end Boot_Timeline_Report;
