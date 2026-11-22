------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The debugger demo's live data (DBG-001): a job queue (a linked list), a
--  sensor map (a binary search tree), a discriminated shape and a window
--  of samples with bounds that do not start at 1. Two threads change them
--  all the time, so a debugger's live views have something to show.
------------------------------------------------------------------------------
pragma Ada_2022;

package Demo_World is

   subtype Celsius is Integer range -40 .. 125;
   subtype Percent is Integer range 0 .. 100;
   type Priority is (Low, Normal, High, Critical);

   --  The job queue: a singly linked list, newest last.
   subtype Job_Name is String (1 .. 12);
   type Job;
   type Job_Access is access Job;
   type Job is record
      Id       : Positive := 1;
      Name     : Job_Name := [others => ' '];
      Level    : Priority := Normal;
      Progress : Percent := 0;
      Next     : Job_Access;
   end record;

   --  The sensor map: a binary search tree keyed by sensor number.
   subtype Sensor_Key is Natural range 0 .. 999;
   type Sensor_Node;
   type Sensor_Access is access Sensor_Node;
   type Sensor_Node is record
      Key         : Sensor_Key := 0;
      Temperature : Celsius := 20;
      Readings    : Natural := 0;
      Left, Right : Sensor_Access;
   end record;

   --  A discriminated record whose variant changes over time.
   type Shape_Kind is (Circle, Rectangle, Triangle);
   type Shape (Kind : Shape_Kind := Circle) is record
      X, Y : Integer := 0;
      case Kind is
         when Circle    => Radius : Positive := 1;
         when Rectangle => Width, Height : Positive := 1;
         when Triangle  => A, B, C : Positive := 1;
      end case;
   end record;

   --  Samples around now: index 0 is the latest, negatives the past.
   subtype Window_Index is Integer range -4 .. 11;
   type Sample_Window is array (Window_Index) of Celsius;

   type World is record
      Generation : Natural := 0;
      Jobs       : Job_Access;
      Job_Count  : Natural := 0;
      Next_Id    : Positive := 1;
      Sensors    : Sensor_Access;
      Sensor_Count : Natural := 0;
      Current    : Shape;
      Window     : Sample_Window := [others => 20];
   end record;

   The_World : World;

   --  The two observed threads' bodies (CuBit.Debug_Stub.Spawn).
   procedure Run_Jobs;
   procedure Run_Sensors;

end Demo_World;
