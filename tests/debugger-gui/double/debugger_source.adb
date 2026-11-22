------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  TEST DOUBLE for the hosted Debugger tests (tests/debugger-gui). It is not
--  a debugger and reads no process: every snapshot it makes is named
--  "test double" so no picture of it can pass for a real session. It is
--  never linked into the native app and never used for screenshots.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package body Debugger_Source is

   Step : Natural := 0;
   Graph_On : Boolean := True;
   Extra_Followed : Boolean := False;

   SOURCE_TEXT : constant array (1 .. 40) of access constant String :=
     [new String'("--  Test double source: not read from any program."),
      new String'("package body Double_World is"),
      new String'(""),
      new String'("   procedure Advance (Q : in out Queue; Tick : Natural) is"),
      new String'("      Cursor : Job_Access := Q.Head;"),
      new String'("      Count  : Natural := 0;"),
      new String'("   begin"),
      new String'("      while Cursor /= null loop"),
      new String'("         Cursor.Progress := (Cursor.Progress + Tick) mod 101;"),
      new String'("         Count := Count + 1;"),
      new String'("         Cursor := Cursor.Next;"),
      new String'("      end loop;"),
      new String'("      Q.Length := Count;  -- 'Length of a list"),
      new String'("      Checkpoint (""advance"");"),
      new String'("   end Advance;"),
      new String'(""),
      new String'("   procedure Run is"),
      new String'("      Q : Queue;"),
      new String'("   begin"),
      new String'("      loop"),
      new String'("         Advance (Q, 7);"),
      new String'("      end loop;"),
      new String'("   end Run;"),
      others => new String'("")];

   function T (S : String) return Text renames To_Text;
   function Sh (S : String) return Short_Text renames To_Short;

   procedure Fill_Graph (S : in out Snapshot) is
      Index : Natural;
      procedure Job (Id : Natural; Name : String; Progress : Natural; Next : Node_Id; Root : Boolean) is
         N : Graph_Node;
      begin
         N.Id := 16#7000_1000# + Node_Id (Id) * 16#40#;
         N.Address := Target_Address (N.Id);
         N.Title := T ("Job");
         N.Label := (if Root then T ("Q.Head") else T (""));
         N.Kind := Record_Value;
         N.Field_Total := 4;
         N.Fields (1) := (Name => Sh ("Id"), Value => Sh (Natural'Image (Id)), others => <>);
         N.Fields (2) := (Name => Sh ("Name"), Value => Sh ("""" & Name & """"), others => <>);
         N.Fields (3) := (Name => Sh ("Progress"), Value => Sh (Natural'Image ((Progress + Step * 7) mod 101)),
                          others => <>);
         N.Fields (4) := (Name => Sh ("Next"),
                          Value => Sh (if Next = NO_NODE then "null" else "0x7000_1" & "..."),
                          Is_Pointer => True, Pointee => Value_Ref (Next), Target => Next, others => <>);
         N.Is_Root := Root;
         Add_Node (S, N, Index);
      end Job;
      function Id_Of (K : Natural) return Node_Id is (16#7000_1000# + Node_Id (K) * 16#40#);
   begin
      S.Node_Total := 0;
      S.Edge_Total := 0;
      if not Graph_On then
         return;
      end if;
      Job (1, "compile", 10, Id_Of (2), True);
      Job (2, "link", 40, Id_Of (3), False);
      Job (3, "sign", 70, (if Extra_Followed then Id_Of (4) else NO_NODE), False);
      if Extra_Followed then
         Job (4, "publish", 5, Id_Of (1), False);
      end if;
      for N in 1 .. S.Node_Total loop
         if S.Nodes (N).Fields (4).Target /= NO_NODE then
            declare
               To : constant Natural := Find_Node (S, S.Nodes (N).Fields (4).Target);
            begin
               if To /= 0 then
                  Add_Edge (S, (From => N, From_Field => 4, To => To, Back => False));
               else
                  S.Nodes (N).Fields (4).Target := NO_NODE;
               end if;
            end;
         end if;
      end loop;
      Classify_Edges (S);
   end Fill_Graph;

   procedure Fill (S : in out Snapshot) is
      Added : Boolean;
      procedure Local (Name, Type_Name, Value : String; Kind : Value_Kind; Bounds : String := "";
                       Children : Natural := 0; Address : Target_Address := 0; Size : Unsigned_64 := 0;
                       Out_Of_Range : Boolean := False) is
         E : Local_Entry;
      begin
         E.Ref := Value_Ref (S.Local_Total + 1);
         E.Name := T (Name);
         E.Type_Name := T (Type_Name);
         E.Value := T (Value);
         E.Kind := Kind;
         E.Bounds := T (Bounds);
         E.Child_Count := Children;
         E.Address := Address;
         E.Size := Size;
         E.Out_Of_Range := Out_Of_Range;
         Add_Local (S, E, Added);
      end Local;
   begin
      S.State := Stopped_Cooperatively;
      S.Target_Name := T ("test double");
      S.Target_Pid := 0;
      S.Checkpoint := T ("advance");
      S.Engine_Note := T ("TEST DOUBLE: no program is being read");
      S.Generation := S.Generation + 1;
      S.Can_Resume := True;
      S.Can_Step := False;
      S.Thread_Total := 2;
      S.Threads (1) := (Id => 1, Name => T ("main"), State => Thread_Parked, PC => 16#40_1A2C#, Wait => T (""));
      S.Threads (2) := (Id => 2, Name => T ("worker"), State => Thread_Running, PC => 16#40_2210#, Wait => T (""));
      S.Frame_Total := 3;
      S.Frames (1) := (Func => T ("Double_World.Advance"), File => T ("double_world.adb"), Line => 14,
                       Column => 7, PC => 16#40_1A2C#, CFA => 16#7FFF_E000#);
      S.Frames (2) := (Func => T ("Double_World.Run"), File => T ("double_world.adb"), Line => 21,
                       Column => 10, PC => 16#40_1B80#, CFA => 16#7FFF_E040#);
      S.Frames (3) := (Func => T ("main"), File => T ("main.adb"), Line => 9, Column => 4,
                       PC => 16#40_1010#, CFA => 16#7FFF_E080#);
      S.Local_Total := 0;
      Local ("Q", "Double_World.Queue", "(Head => 0x70001040, Length => 3)", Record_Value, Children => 2,
             Address => 16#7FFF_DF80#, Size => 16);
      Local ("Tick", "Natural", "7", Scalar, "0 .. 2147483647", Address => 16#7FFF_DF90#, Size => 4);
      Local ("Count", "Small", Natural'Image (3 + Step mod 3), Scalar, "2 .. 5", Address => 16#7FFF_DF94#, Size => 4);
      Local ("Cursor", "Job_Access", "null", Pointer, Address => 16#7FFF_DF98#, Size => 8);
      Local ("Bad", "Percent", "140", Scalar, "0 .. 100", Out_Of_Range => True);
      S.Mem_Base := 16#7FFF_DF80#;
      S.Mem_Length := 256;
      for I in 0 .. S.Mem_Length - 1 loop
         S.Mem_Bytes (I) := Unsigned_8 ((I * 37 + (if I in 20 .. 23 then Step else 0)) mod 256);
      end loop;
      S.Source_File := T ("double_world.adb");
      Clear_Source (S);
      for L of SOURCE_TEXT loop
         Add_Source_Line (S, L.all, Added);
      end loop;
      Fill_Graph (S);
   end Fill;

   procedure Attach (S : in out Snapshot; Result : out Attach_Result) is
   begin
      Fill (S);
      Result := Attached;
   end Attach;

   procedure Read (Before : Snapshot; S : in out Snapshot) is
   begin
      S.Generation := Before.Generation;
      Fill (S);
      for I in 1 .. S.Local_Total loop
         if Was_Expanded (Before, Name_Path (S, I)) then
            Load_Children (S, I);
         end if;
      end loop;
   end Read;

   procedure Continue (Before : Snapshot; S : in out Snapshot) is
   begin
      Step := Step + 1;
      Read (Before, S);
   end Continue;

   procedure Set_Live (Before : Snapshot; S : in out Snapshot; On : Boolean) is
   begin
      Continue (Before, S);
      S.Mode := (if On then Live else Stopped);
      S.State := (if On then Running else Stopped_Cooperatively);
   end Set_Live;

   procedure Load_Children (S : in out Snapshot; I : Local_Index) is
      Kids : Local_Batch (1 .. 2);
   begin
      if S.Locals (I).Name = "Q" then
         Kids (1) := (Name => T ("Head"), Type_Name => T ("Job_Access"), Value => T ("0x70001040"),
                      Kind => Pointer, Ref => 100, others => <>);
         Kids (2) := (Name => T ("Length"), Type_Name => T ("Natural"), Value => T ("3"), Kind => Scalar,
                      Ref => 101, others => <>);
         Insert_Children (S, I, Kids);
      else
         Insert_Children (S, I, Kids (1 .. 0));
      end if;
   end Load_Children;

   procedure Graph_Local (S : in out Snapshot; I : Local_Index) is
      pragma Unreferenced (I);
   begin
      Graph_On := True;
      Fill_Graph (S);
   end Graph_Local;

   procedure Follow (S : in out Snapshot; N : Node_Index; F : Field_Index) is
      pragma Unreferenced (N, F);
   begin
      Extra_Followed := True;
      Fill_Graph (S);
   end Follow;

   procedure Memory_At (S : in out Snapshot; Address : Target_Address) is
   begin
      S.Mem_Base := Address;
   end Memory_At;

   procedure Select_Thread (Before : Snapshot; S : in out Snapshot; T : Thread_Index) is
   begin
      S.Thread_Sel := T;
      Read (Before, S);
   end Select_Thread;

   procedure Select_Frame (Before : Snapshot; S : in out Snapshot; F : Frame_Index) is
   begin
      S.Frame_Sel := F;
      Read (Before, S);
   end Select_Frame;

   --  Attach to process...: two made-up entries; attaching to one says it
   --  is the test double (nothing is attached).
   procedure List_Processes (S : in out Snapshot) is
   begin
      S.Processes (1) := (Name => To_Text ("clock.svc (test double)"), Identity => 16#0100_0001#);
      S.Processes (2) := (Name => To_Text ("debug-plain.app (test double)"), Identity => 16#0100_0002#);
      S.Process_Total := 2;
   end List_Processes;

   procedure Attach_Process (S : in out Snapshot; P : Process_Index; Result : out Attach_Result) is
   begin
      S.Target_Name := To_Text ("test double: " & Image (S.Processes (P).Name));
      S.Through_Kernel := True;
      Result := Attached;
   end Attach_Process;

   procedure Detach is null;

   --  Tests reset between cases.
end Debugger_Source;
