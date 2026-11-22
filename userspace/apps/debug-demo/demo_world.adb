------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;
with CuBit.Debug_Stub;
with CuBit.Waits;

package body Demo_World is

   Job_Step_Ms    : constant := 200;
   Sensor_Step_Ms : constant := 150;
   Jobs_Wanted    : constant := 6;
   Sensor_Keys    : constant := 40;

   --  A small deterministic generator (each thread its own).
   type Seed is mod 2 ** 32;
   function Next (S : in out Seed) return Natural is
   begin
      S := S * 1_664_525 + 1_013_904_223;
      return Natural (S / 2 ** 8);
   end Next;

   Names : constant array (0 .. 7) of Job_Name :=
     ["index-files ", "compress    ", "send-report ", "resize-image",
      "spell-check ", "sync-mail   ", "render-page ", "backup-notes"];

   procedure Free is new Ada.Unchecked_Deallocation (Job, Job_Access);

   --  Statistics the threads keep after each checkpoint.
   Deepest_Insert : Natural := 0;
   Last_Updated   : Sensor_Key := 0;
   Progress_Made  : Natural := 0;
   Clipped_Steps  : Natural := 0;

   ---------------------------------------------------------------------------
   --  Jobs
   ---------------------------------------------------------------------------
   procedure Advance (J : in out Job; Step : Percent) with No_Inline;
   procedure Advance (J : in out Job; Step : Percent) is
      Before : constant Percent := J.Progress;
      After  : constant Natural := Before + Step;
   begin
      J.Progress := (if After > Percent'Last then Percent'Last else After);
      if J.Progress >= 90 and then J.Level < Critical then
         J.Level := Priority'Succ (J.Level);
      end if;
      CuBit.Debug_Stub.Checkpoint;
      --  Steps cut short at 100 %.
      if J.Progress - Before < Step then
         Clipped_Steps := Clipped_Steps + 1;
      end if;
      Progress_Made := Progress_Made + (J.Progress - Before);
   end Advance;

   procedure Add_Job (Name : Job_Name; Level : Priority) with No_Inline;
   procedure Add_Job (Name : Job_Name; Level : Priority) is
      Fresh : constant Job_Access :=
        new Job'(Id => The_World.Next_Id, Name => Name, Level => Level,
                 Progress => 0, Next => null);
      Tail : Job_Access := The_World.Jobs;
   begin
      The_World.Next_Id := The_World.Next_Id + 1;
      if Tail = null then
         The_World.Jobs := Fresh;
      else
         while Tail.Next /= null loop
            Tail := Tail.Next;
         end loop;
         Tail.Next := Fresh;
      end if;
      The_World.Job_Count := The_World.Job_Count + 1;
   end Add_Job;

   procedure Retire_Finished with No_Inline;
   procedure Retire_Finished is
      Done : Job_Access;
   begin
      while The_World.Jobs /= null and then The_World.Jobs.Progress = Percent'Last loop
         Done := The_World.Jobs;
         The_World.Jobs := Done.Next;
         Free (Done);
         The_World.Job_Count := The_World.Job_Count - 1;
      end loop;
   end Retire_Finished;

   procedure Run_Jobs is
      S : Seed := 7;
      Cursor : Job_Access;
   begin
      loop
         while The_World.Job_Count < Jobs_Wanted loop
            Add_Job (Names (Next (S) mod Names'Length),
                     Priority'Val (Next (S) mod (Priority'Pos (Priority'Last) + 1)));
         end loop;
         Cursor := The_World.Jobs;
         while Cursor /= null loop
            Advance (Cursor.all, Next (S) mod 23);
            Cursor := Cursor.Next;
         end loop;
         Retire_Finished;
         The_World.Generation := The_World.Generation + 1;
         CuBit.Waits.Sleep_For (Job_Step_Ms);
      end loop;
   end Run_Jobs;

   ---------------------------------------------------------------------------
   --  Sensors
   ---------------------------------------------------------------------------
   procedure Insert
     (Node : in out Sensor_Access; Key : Sensor_Key; Reading : Celsius; Depth : Natural)
   with No_Inline;
   procedure Insert
     (Node : in out Sensor_Access; Key : Sensor_Key; Reading : Celsius; Depth : Natural) is
   begin
      if Node = null then
         Node := new Sensor_Node'(Key => Key, Temperature => Reading, Readings => 1,
                                  Left => null, Right => null);
         The_World.Sensor_Count := The_World.Sensor_Count + 1;
         CuBit.Debug_Stub.Checkpoint;
         Deepest_Insert := Natural'Max (Deepest_Insert, Depth);
      elsif Key < Node.Key then
         Insert (Node.Left, Key, Reading, Depth + 1);
      elsif Key > Node.Key then
         Insert (Node.Right, Key, Reading, Depth + 1);
      else
         Node.Temperature := Reading;
         Node.Readings := Node.Readings + 1;
         CuBit.Debug_Stub.Checkpoint;
         Deepest_Insert := Natural'Max (Deepest_Insert, Depth);
         Last_Updated := Key;
      end if;
   end Insert;

   procedure Reshape (Tick : Natural) with No_Inline;
   procedure Reshape (Tick : Natural) is
      Side : constant Positive := 1 + Tick mod 9;
   begin
      case Tick / 4 mod 3 is
         when 0 =>
            The_World.Current := (Kind => Circle, X => Tick mod 50, Y => 10, Radius => Side);
         when 1 =>
            The_World.Current :=
              (Kind => Rectangle, X => 5, Y => Tick mod 30, Width => Side, Height => 2 * Side);
         when others =>
            The_World.Current :=
              (Kind => Triangle, X => 1, Y => 2, A => 3, B => 4, C => Side + 2);
      end case;
   end Reshape;

   procedure Record_Sample (Reading : Celsius) with No_Inline;
   procedure Record_Sample (Reading : Celsius) is
   begin
      for I in Window_Index'First .. Window_Index'Last - 1 loop
         The_World.Window (I) := The_World.Window (I + 1);
      end loop;
      The_World.Window (Window_Index'Last) := Reading;
   end Record_Sample;

   procedure Run_Sensors is
      S : Seed := 11;
      Tick : Natural := 0;
      Key : Sensor_Key;
      Reading : Celsius;
   begin
      loop
         Key := Next (S) mod Sensor_Keys;
         Reading := Celsius'First + 50 + Next (S) mod 40;
         --  A new sensor, or a new reading of a known one.
         Insert (The_World.Sensors, Key, Reading, 0);
         Record_Sample (Reading);
         Reshape (Tick);
         Tick := Tick + 1;
         CuBit.Waits.Sleep_For (Sensor_Step_Ms);
      end loop;
   end Run_Sensors;

end Demo_World;
