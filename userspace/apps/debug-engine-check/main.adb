------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The debugger engine's native check (DBG-001, headless --test
--  debug-engine). Attaches to debug-demo (cooperative, read-only), pauses
--  it, and checks real threads, frames, source lines, locals, values and
--  the graph; resumes, pauses again and checks that live values moved.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Console_Route;
with CuBit.Debug; use CuBit.Debug;
with CuBit.Debug_Protocol;
with CuBit.Debug_Target;
with CuBit.Grant_References;
with CuBit.Memory_Grants;
with CuBit.Messages; use CuBit.Messages;
with System;
with CuBit.Process_Calls;
with CuBit.Waits;
with Startup;

procedure Main is
   Attach_Ms      : constant := 2_000;
   Attach_Tries   : constant := 30;
   Retry_Ms       : constant := 200;
   Pause_Ms       : constant := 1_500;
   Run_Between_Ms : constant := 700;
   Graph_Depth    : constant := 8;
   Graph_Nodes    : constant := 64;
   Exit_Success   : constant := 0;
   Exit_Failure   : constant := 1;

   Failures : Natural := 0;

   procedure Say (Text : String) is
   begin
      debugPrint ("debug-engine: " & Text & ASCII.LF);
   end Say;

   procedure Check (Name : String; Condition : Boolean; Detail : String := "") is
   begin
      if Condition then
         Say (Name & " PASS");
      else
         Failures := Failures + 1;
         Say (Name & " FAIL " & Detail);
      end if;
   end Check;

   function Has (S, Part : String) return Boolean is
   begin
      if Part'Length > S'Length then
         return False;
      end if;
      for I in S'First .. S'Last - Part'Length + 1 loop
         if S (I .. I + Part'Length - 1) = Part then
            return True;
         end if;
      end loop;
      return False;
   end Has;

   type Graph_Access is access Graph_Result;

   S : Session;
   R : Outcome;
   Info : Program_Info;
   Ts : Thread_List;
   Handle : constant CuBit.Debug_Target.Process_Handle :=
     (Target => Startup.Target, Filesystem => Startup.Filesystem, Session => <>);
   Jobs_Thread, Sensors_Thread : Thread_Id := 0;
   World : Value_Ref;
   Have_World : Boolean := False;
   Generation_Before : Display_Text;
   G : constant Graph_Access := new Graph_Result;

   procedure Show_Frames (Thread : Thread_Id; Expect : String; Expect_Local : String) is
      Fs : Frame_List;
      Ls : Ref_List;
      Node : Value_Node;
      Found_Function : Natural := Frame_Ceiling;
      Found_Local : Boolean := False;
      Local_Text : Display_Text;
   begin
      Frames (S, Thread, Fs, R);
      Check ("frames of thread" & Thread'Image, R = Ok and then Fs.Count >= 3, Outcome'Image (R) & Fs.Count'Image);
      for I in 0 .. Fs.Count - 1 loop
         Say ("  #" & I'Image & " " & Image (Fs.Items (I).Function_Name) & " at "
              & Image (Fs.Items (I).File) & ":" & Fs.Items (I).Line'Image
              & (if Fs.Items (I).In_Stub then " (stub)" else ""));
         if Image (Fs.Items (I).Function_Name) = Expect and then Found_Function = Frame_Ceiling then
            Found_Function := I;
         end if;
      end loop;
      Check ("frame 0 is the stub's checkpoint", Fs.Count > 0 and then Fs.Items (0).In_Stub
             and then Image (Fs.Items (0).Function_Name) = "CuBit.Debug_Stub.Checkpoint",
             (if Fs.Count > 0 then Image (Fs.Items (0).Function_Name) else ""));
      Check (Expect & " is on the stack in demo_world.adb", Found_Function < Fs.Count
             and then Image (Fs.Items (Found_Function).File) = "demo_world.adb"
             and then Fs.Items (Found_Function).Line > 0);
      if Found_Function < Fs.Count then
         Locals (S, Fs.Items (Found_Function), Ls, R);
         Check ("locals of " & Expect, R = Ok and then Ls.Count > 0, Outcome'Image (R) & Ls.Count'Image);
         for L of Ls.Items (1 .. Ls.Count) loop
            Value (S, L, Node, R);
            Say ("    " & Image (L.Name) & " : " & Image (Node.Type_Name) & " = " & Image (Node.Display));
            if Image (L.Name) = Expect_Local then
               Found_Local := True;
               Local_Text := Node.Display;
            end if;
         end loop;
         Check ("local " & Expect_Local & " of " & Expect & " has a value",
                Found_Local and then Local_Text.Length > 0 and then Image (Local_Text) (1) /= '<',
                Image (Local_Text));
      end if;
   end Show_Frames;

   function Generation_Text return Display_Text is
      Kids : Node_List;
   begin
      Children (S, World, 0, 1, Kids, R);
      return (if Kids.Count = 1 then Kids.Items (1).Display else To_Display (""));
   end Generation_Text;
begin
   CuBit.Console_Route.Route_Named ("logstore");
   Say ("starting");
   for Try in 1 .. Attach_Tries loop
      Attach (Handle, Observe, Deadline_After (Attach_Ms), S, R);
      exit when R = Ok;
      CuBit.Waits.Sleep_For (Retry_Ms);
   end loop;
   Check ("attach to debug-demo", R = Ok, Outcome'Image (R));
   if R /= Ok then
      Say ("FAIL");
      CuBit.Process_Calls.Exit_Process (Exit_Failure);
   end if;
   Describe (S, Info, R);
   Say ("program " & Image (Info.Program) & " build-id " & Info.Build_Id & " debug file "
        & Image (Info.Debug_File) & ", " & Image (Info.Attach_Mode));
   Check ("attach is cooperative", Image (Info.Attach_Mode) = "cooperative attach (preview)");

   --  Let both threads run for a while, then stop them.
   CuBit.Waits.Sleep_For (Run_Between_Ms);
   Threads (S, Ts, R);
   Check ("two observed threads", R = Ok and then Ts.Count = 2, Ts.Count'Image);
   for T of Ts.Items (1 .. Ts.Count) loop
      Say ("thread" & T.Id'Image & " " & Image (T.Name) & " " & Thread_State'Image (T.State));
      if Image (T.Name) = "jobs" then
         Jobs_Thread := T.Id;
      elsif Image (T.Name) = "sensors" then
         Sensors_Thread := T.Id;
      end if;
   end loop;
   Check ("threads named jobs and sensors", Jobs_Thread /= 0 and then Sensors_Thread /= 0);

   Pause (S, Deadline_After (Pause_Ms), R);
   Check ("pause: both threads parked at a checkpoint", R = Ok, Outcome'Image (R));
   Check ("state is stopped cooperatively", State (S) = Stopped_Cooperatively);
   Threads (S, Ts, R);
   Check ("threads report parked", Ts.Count = 2 and then Ts.Items (1).State = Parked
          and then Ts.Items (2).State = Parked);

   Show_Frames (Jobs_Thread, "Demo_World.Advance", "Step");
   Show_Frames (Sensors_Thread, "Demo_World.Insert", "Depth");

   --  Globals and the graph.
   declare
      Gs : Ref_List;
      Node : Value_Node;
      Roots : Ref_List;
      Jobs_Nodes, Sensor_Nodes : Natural := 0;
   begin
      Globals (S, "Demo_World", Gs, R);
      for X of Gs.Items (1 .. Gs.Count) loop
         if Image (X.Name) = "Demo_World.The_World" then
            World := X;
            Have_World := True;
         end if;
      end loop;
      Check ("global Demo_World.The_World", Have_World);
      Value (S, World, Node, R);
      Say ("The_World = " & Image (Node.Display));
      Check ("The_World read live", R = Ok and then Node.Kind = Record_Value and then Has (Image (Node.Display), "Jobs => 0x"),
             Image (Node.Display));
      Generation_Before := Generation_Text;
      Roots.Count := 1;
      Roots.Items (1) := World;
      Graph (S, Roots, Graph_Depth, Graph_Nodes, G.all, R);
      for I in 1 .. G.Node_Total loop
         if Image (G.Nodes (I).Value.Type_Name) = "Demo_World.Job" then
            Jobs_Nodes := Jobs_Nodes + 1;
         elsif Image (G.Nodes (I).Value.Type_Name) = "Demo_World.Sensor_Node" then
            Sensor_Nodes := Sensor_Nodes + 1;
         end if;
         Say ("  node" & I'Image & " " & Image (G.Nodes (I).Value.Type_Name) & " "
              & Image (G.Nodes (I).Value.Display));
      end loop;
      Say ("graph:" & G.Node_Total'Image & " nodes," & G.Edge_Total'Image & " edges");
      Check ("graph holds the job list", Jobs_Nodes >= 3, Jobs_Nodes'Image);
      Check ("graph holds the sensor tree", Sensor_Nodes >= 3, Sensor_Nodes'Image);
      Check ("graph edges follow the pointers", G.Edge_Total >= Jobs_Nodes + Sensor_Nodes,
             G.Edge_Total'Image);
   end;

   --  Security: what the stub shares is read-only. Ask it directly for the
   --  page holding The_World: acquiring it for writing must be refused,
   --  for reading accepted.
   declare
      package P renames CuBit.Debug_Protocol;
      package MG renames CuBit.Memory_Grants;
      Share_Ms : constant := 500;
      Page : constant Unsigned_64 := Unsigned_64 (World.Address) - Unsigned_64 (World.Address) mod P.Page_Bytes;
      M : Message := NULL_MESSAGE;
      Tag : MessageTag;
      Where : System.Address;
      Written, Read_Only : Boolean := False;
   begin
      M.tag := (label => P.Op_Share, length => 2, protocol => 0, ipc => <>, reserved => 0);
      M.words (0) := Page;
      M.words (1) := 1;
      Tag := capCall (Startup.Target, M, Deadline_After (Share_Ms));
      Check ("the stub shares a page on request", Ending_Of (Tag) = Call_Answered
             and then M.tag.label = P.Reply_Ok and then CuBit.Grant_References.Valid_Wire (M.words (0)));
      if Ending_Of (Tag) = Call_Answered and then M.tag.label = P.Reply_Ok
        and then CuBit.Grant_References.Valid_Wire (M.words (0))
      then
         MG.Acquire_Via_Capability (Startup.Target, CuBit.Grant_References.Decode (M.words (0)), 0,
                                    P.Page_Bytes, MG.Write_Access, Where, Written);
         MG.Acquire_Via_Capability (Startup.Target, CuBit.Grant_References.Decode (M.words (0)), 0,
                                    P.Page_Bytes, MG.Read_Access, Where, Read_Only);
      end if;
      Check ("a shared page cannot be acquired for writing", not Written);
      Check ("a shared page can be acquired for reading", Read_Only);
   end;

   --  Source through the build-id layout.
   declare
      Page : Source_Page;
   begin
      Source (S, "demo_world.adb", 1, Page, R);
      Check ("source demo_world.adb from @system/Debug/Source", (R = Ok or else R = More) and then Page.Count > 0
             and then Has (Image (Page.Lines (2)), "CuBit"), Outcome'Image (R));
   end;

   --  Live: resume, let it run, pause again: values moved.
   Resume (S, Deadline_After (Pause_Ms), R);
   Check ("resume", R = Ok, Outcome'Image (R));
   CuBit.Waits.Sleep_For (Run_Between_Ms);
   Pause (S, Deadline_After (Pause_Ms), R);
   Check ("pause again", R = Ok, Outcome'Image (R));
   declare
      After : constant Display_Text := Generation_Text;
      Kids : Node_List;
   begin
      Check ("The_World.Generation moved while running",
             Image (After) /= Image (Generation_Before), Image (Generation_Before) & " -> " & Image (After));
      Resume (S, Deadline_After (Pause_Ms), R);
      CuBit.Waits.Sleep_For (Run_Between_Ms);
      Children (S, World, 0, 1, Kids, R);
      Check ("Generation is marked changed since the last read",
             Kids.Count = 1 and then Kids.Items (1).Changed);
   end;
   Resume (S, Deadline_After (Pause_Ms), R);
   Detach (S);
   Check ("detach", State (S) = Detached);
   if Failures = 0 then
      Say ("ALL PASS");
      CuBit.Process_Calls.Exit_Process (Exit_Success);
   else
      Say ("FAIL" & Failures'Image);
      CuBit.Process_Calls.Exit_Process (Exit_Failure);
   end if;
end Main;
