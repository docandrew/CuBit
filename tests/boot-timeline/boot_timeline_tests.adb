--  Hosted checks for the boot timeline (docs/boot-timeline.md): the
--  kernel's bookkeeping (Boot_Timeline_Records), the reader's analysis
--  (Boot_Timeline_Report) and the agreement of the two milestone copies.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Boot_Timeline_Layout; use Boot_Timeline_Layout;
with Boot_Timeline_Records; use Boot_Timeline_Records;
with Boot_Timeline_Report; use Boot_Timeline_Report;
with CuBit.Boot_Milestone_Codes;

procedure Boot_Timeline_Tests is
   package Codes renames CuBit.Boot_Milestone_Codes;
   Failures : Natural := 0;
   Checks   : Natural := 0;

   procedure Expect (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Expect;

   function Name (Text : String) return Name_Text is
      Result : Name_Text := [others => ' '];
   begin
      Result (1 .. Text'Length) := Text;
      return Result;
   end Name;

   Hz : constant := 2_000_000_000;   --  2 GHz: 2000 ticks per microsecond
   function Ms (N : Unsigned_64) return Ticks is (N * (Hz / 1_000));

   S : State;
begin
   ---------------------------------------------------------------------------
   --  Layout and the two vocabularies
   ---------------------------------------------------------------------------
   Expect (Image'Size = Image_Bytes * 8, "image size");
   Expect (Image_Bytes = 10_304, "image bytes");
   Expect (Codes.System_Call = System_Call_Number, "system call number agrees");
   Expect (Codes.Read_Operation = Operation'Enum_Rep (Read), "read code agrees");
   Expect (Codes.Mark_Operation = Operation'Enum_Rep (Mark), "mark code agrees");
   Expect (Codes.Read_Chunk_Bytes = Read_Chunk_Bytes, "chunk size agrees");
   Expect (Codes.Milestone'Pos (Codes.Milestone'Last) = Milestone'Pos (Milestone'Last),
           "milestone count agrees");
   for M in Milestone loop
      Expect (Codes.Milestone'Enum_Rep (Codes.Milestone'Val (Milestone'Pos (M))) =
                Milestone'Enum_Rep (M)
              and then Codes.Milestone'Image (Codes.Milestone'Val (Milestone'Pos (M))) =
                Milestone'Image (M),
              "milestone " & M'Image & " agrees");
   end loop;
   Expect (Kernel_Stage'Pos (Kernel_Stage'Last) < Stage_Capacity, "stages fit");
   Expect (Milestone'Enum_Rep (Milestone'Last) < Milestone_Capacity, "milestones fit");

   ---------------------------------------------------------------------------
   --  Bookkeeping
   ---------------------------------------------------------------------------
   Initialize (S);
   Expect (Recording (S) and then Valid (S), "initialized and recording");
   Mark_Stage (S, Kernel_Entry, Ms (500));
   Mark_Stage (S, Kernel_Entry, Ms (900));
   Expect (S.Wire.Stages (Slot_Of (Kernel_Entry)) = Ms (500), "first stage mark wins");
   Set_Frequency (S, Hz, Ms (600));
   Expect (S.Wire.Head.Frequency = Hz
           and then S.Wire.Head.Window_End = Ms (600) + Ms (Recording_Window_Seconds * 1_000)
           and then S.Wire.Head.Minimum_Wait = Hz / Wait_Fraction, "frequency, window, threshold");
   Set_Frequency (S, 1_000_000_000, Ms (700));
   Expect (S.Wire.Head.Frequency = Hz, "frequency is set once");

   Note_Spawn (S, 3, 16#1_0000_0003#, No_Identity, 0, Name ("devmgr.svc"), Ms (610), Ms (611));
   Note_Spawn (S, 4, 16#1_0000_0004#, 16#1_0000_0003#, 3, Name ("procmgr.svc"), Ms (620), Ms (622));
   Expect (S.Wire.Head.Processes = 2 and then Entry_Of (S, 4) = 2, "spawns recorded");
   Note_Event (S, 4, Resumed, Ms (623));
   Note_Event (S, 4, First_Run, Ms (624));
   Note_Event (S, 4, First_Run, Ms (699));
   Expect (S.Wire.Processes (2).First_Run = Ms (624), "first run kept");
   Note_Event (S, 4, Received, Ms (630));
   Expect (S.Wire.Processes (2).First_Receive = Ms (630)
           and then S.Wire.Processes (2).Ready = Not_Recorded, "receive before registration");
   --  A startup receive wait before registration counts.
   Note_Wait (S, 4, 16#1_0000_0004#, Receive, 0, No_Identity, Ms (630), Ms (640));
   Note_Event (S, 4, Registered, Ms (641));
   Note_Event (S, 4, Received, Ms (650));
   Expect (S.Wire.Processes (2).Ready = Ms (650), "ready at first receive after registration");
   --  The request loop's idle receive does not count.
   Note_Wait (S, 4, 16#1_0000_0004#, Receive, 0, No_Identity, Ms (650), Ms (900));
   Expect (S.Wire.Processes (2).Wait_Ticks = Ms (10) and then S.Wire.Head.Waits = 1,
           "idle receive after ready ignored");
   Note_Wait (S, 4, 16#1_0000_0004#, Call, 16#0A04#, 16#1_0000_0003#, Ms (651), Ms (661));
   Note_Wait (S, 4, 16#1_0000_0004#, Call, 16#0A04#, 16#1_0000_0003#, Ms (662), Ms (662) + 1);
   Expect (S.Wire.Processes (2).Calls = 2 and then S.Wire.Head.Waits = 2
           and then S.Wire.Waits (2).Target = 16#1_0000_0003#
           and then S.Wire.Waits (2).Duration = Ms (10), "short calls aggregate, long ones listed");
   Note_Wait (S, 9, 16#1_0000_0009#, Sleep, 0, No_Identity, Ms (670), Ms (680));
   Expect (S.Wire.Head.Waits = 3, "unrecorded process's long sleep still listed");
   Note_Wait (S, 9, 16#1_0000_0009#, Futex, 0, No_Identity, Ms (670), Ms (680));
   Expect (S.Wire.Head.Waits = 3, "unrecorded process's futex wait ignored");

   --  A spawn by a ready process makes it not ready until its next receive.
   Note_Event (S, 3, Registered, Ms (612));
   Note_Event (S, 3, Received, Ms (613));
   Expect (S.Wire.Processes (1).Ready = Ms (613), "devmgr ready");
   Note_Spawn (S, 5, 16#1_0000_0005#, 16#1_0000_0003#, 3, Name ("nvme.drv"), Ms (681), Ms (682));
   Expect (S.Wire.Processes (1).Ready = Not_Recorded, "spawning clears ready");
   Note_Wait (S, 3, 16#1_0000_0003#, Receive, 0, No_Identity, Ms (683), Ms (685));
   Expect (S.Wire.Processes (1).Wait_Ticks = Ms (2), "spawner's receive counts as startup wait");
   Note_Event (S, 3, Received, Ms (686));
   Expect (S.Wire.Processes (1).Ready = Ms (686), "ready again after the last spawn");
   Mark_Milestone (S, First_Frame_Presented, 16#1_0000_0004#, Ms (700));
   Mark_Milestone (S, First_Frame_Presented, 16#1_0000_0009#, Ms (800));
   Expect (S.Wire.Milestones (Slot_Of (First_Frame_Presented)).Marker = 16#1_0000_0004#,
           "first milestone mark wins");

   --  Capacity: drops are counted, never written past the tables.
   for N in 1 .. Process_Capacity loop
      Note_Spawn (S, Process_Slot (10 + N), Unsigned_64 (100 + N), No_Identity, 0,
                  Name ("filler"), Ms (701), Ms (702));
   end loop;
   Expect (S.Wire.Head.Processes = Process_Capacity
           and then S.Wire.Head.Processes_Dropped = 3, "process drops counted");
   Expect (Valid (S), "slot map valid after drops");
   --  The same wait again joins its entry.
   Note_Wait (S, 9, 16#1_0000_0009#, Sleep, 0, No_Identity, Ms (690), Ms (695));
   Expect (S.Wire.Head.Waits = 4 and then S.Wire.Waits (3).Repeats = 2
           and then S.Wire.Waits (3).Duration = Ms (15)
           and then S.Wire.Waits (3).Started = Ms (670), "repeated wait merged");
   for N in 1 .. Wait_Capacity loop
      Note_Wait (S, 4, 16#1_0000_0004#, Call, 0, Unsigned_64 (1_000 + N), Ms (710), Ms (720));
   end loop;
   Expect (S.Wire.Head.Waits = Wait_Capacity and then S.Wire.Head.Waits_Dropped = 4,
           "wait drops counted");

   --  The window closes; nothing changes afterwards.
   Expire (S, S.Wire.Head.Window_End);
   Expect (not Recording (S) and then S.Wire.Head.Closed_At /= Not_Recorded, "window closed");
   declare
      Before : constant State := S;
   begin
      Mark_Stage (S, Scheduler_Started, Ms (40_000));
      Note_Event (S, 3, First_Run, Ms (40_000));
      Mark_Milestone (S, First_Input_Accepted, 1, Ms (40_000));
      Expect (S = Before, "closed timeline is frozen");
   end;

   ---------------------------------------------------------------------------
   --  Analysis
   ---------------------------------------------------------------------------
   Expect (To_Microseconds (Ms (1), Hz) = 1_000, "1 ms converts");
   Expect (To_Microseconds (Ticks'Last, Minimum_Frequency) = Microseconds'Last,
           "conversion saturates");
   Expect (To_Microseconds (Hz + Hz / 2, Hz) = 1_500_000, "fraction converts");

   declare
      Img : Image := (others => <>);
      Events : Event_List;
      Count : Listed_Count;
      Path : Path_List;
      Steps : Path_Count;
      Found, Complete : Boolean;
      Gaps : Gap_Set;
      procedure Add (N : Process_Number; Text : String; Id, Spawner : Identity_Word; Base : Unsigned_64) is
      begin
         Img.Processes (N) :=
           (Identity => Id, Spawner => Spawner, Name => Name (Text),
            Spawn_Started => Ms (Base), Loaded => Ms (Base + 1), Resumed => Ms (Base + 2),
            First_Run => Ms (Base + 3), Registered => Ms (Base + 4),
            First_Receive => Ms (Base + 5), Ready => Ms (Base + 5), others => <>);
      end Add;
   begin
      Img.Head.Frequency := Hz;
      for Stage in Kernel_Stage loop
         Img.Stages (Slot_Of (Stage)) := Ms (100 + Unsigned_64 (Kernel_Stage'Pos (Stage)));
      end loop;
      Add (1, "devmgr.svc", 11, No_Identity, 200);
      Add (2, "procmgr.svc", 12, 11, 300);
      Add (3, "display.svc", 13, 12, 400);
      Add (4, "desktop.svc", 14, 12, 500);
      Img.Head.Processes := 4;
      Img.Waits (1) := (Kind => Call, Waiter => 14, Target => 13, Label => 16#0A04#,
                        Started => Ms (510), Duration => Ms (40), others => <>);
      Img.Waits (2) := (Kind => Sleep, Waiter => 12, Started => Ms (310), Duration => Ms (100),
                        others => <>);
      Img.Head.Waits := 2;
      for M in Milestone loop
         Img.Milestones (Slot_Of (M)) := (At_Ticks => Ms (600 + Unsigned_64 (Milestone'Pos (M))),
                                          Marker => 14);
      end loop;

      Collect (Img, Events, Count);
      Expect (Count = Kernel_Stage'Pos (Kernel_Stage'Last) + 1 + 4 * Events_Per_Process
                + Milestone'Pos (Milestone'Last) + 1, "every recorded time collected");
      Expect (Sorted (Events, Count), "events sorted");

      Critical_Path (Img, First_Frame_Presented, Path, Steps, Found);
      Expect (Found and then Steps > 0 and then Path (1).Kind = Power_On
              and then Path (Steps).Kind = Goal_Step, "path from power-on to goal");
      declare
         Saw_Call, Saw_Sleep, Saw_Devmgr : Boolean := False;
      begin
         for I in 1 .. Steps loop
            if Path (I).Kind = Wait_Step and then Path (I).Wait = 1 then Saw_Call := True; end if;
            if Path (I).Kind = Wait_Step and then Path (I).Wait = 2 then Saw_Sleep := True; end if;
            if Path (I).Kind = Spawn_Step and then Path (I).Process = 1 then Saw_Devmgr := True; end if;
            if I > 1 then
               Expect (Path (I - 1).At_Ticks <= Path (I).At_Ticks, "path in time order");
            end if;
         end loop;
         Expect (Saw_Call and then Saw_Sleep and then Saw_Devmgr, "path has the waits and spawns");
      end;

      --  A spawner cycle ends; a missing goal is reported.
      Img.Processes (1).Spawner := 12;
      Critical_Path (Img, First_Frame_Presented, Path, Steps, Found);
      Expect (Found and then Steps <= Path_Capacity, "cyclic spawners terminate");
      Img.Milestones (Slot_Of (First_Input_Accepted)) := (others => <>);
      Critical_Path (Img, First_Input_Accepted, Path, Steps, Found);
      Expect (not Found and then Steps = 0, "unmarked goal");

      Check (Img, Gaps, Complete);
      Expect (Complete, "synthetic boot complete (input optional)");
      Img.Processes (3).First_Run := Not_Recorded;
      Img.Head.Waits_Dropped := 1;
      Check (Img, Gaps, Complete);
      Expect (not Complete and then Gaps (Process_Unstarted) and then Gaps (Dropped_Records),
              "gaps reported");
   end;

   if Failures = 0 then
      Put_Line ("boot-timeline: PASS" & Checks'Image & " checks");
   else
      Put_Line ("boot-timeline: FAIL" & Failures'Image & " of" & Checks'Image);
      raise Program_Error;
   end if;
end Boot_Timeline_Tests;
