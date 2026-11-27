------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The debugger engine's native Rust check (DBG-001, headless --test
--  debug-engine-rs). Attaches to debug-demo-rs (cooperative, read-only),
--  pauses it, and checks real Rust frames, lines, locals (Vec, Option<Box>,
--  Arc, enums, arrays, &str), the graph of its tree and queue, and that
--  live values move between stops.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Console_Route;
with CuBit.Debug; use CuBit.Debug;
with CuBit.Debug_Protocol;
with CuBit.Debug_Target;
with CuBit.Grant_References;
with CuBit.Memory_Grants;
with CuBit.Messages; use CuBit.Messages;
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
   Graph_Nodes    : constant := 96;
   Exit_Success   : constant := 0;
   Exit_Failure   : constant := 1;

   Failures : Natural := 0;

   procedure Say (Text : String) is
   begin
      debugPrint ("debug-engine-rs: " & Text & ASCII.LF);
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
   G : constant Graph_Access := new Graph_Result;

   --  The thread's frames; the index of the first frame named Expect.
   procedure Show_Frames (Thread : Thread_Id; Fs : out Frame_List; Expect : String; Found : out Natural) is
   begin
      Found := Frame_Ceiling;
      Frames (S, Thread, Fs, R);
      Check ("frames of thread" & Thread'Image, R = Ok and then Fs.Count >= 3, Outcome'Image (R) & Fs.Count'Image);
      for I in 0 .. Fs.Count - 1 loop
         Say ("  #" & I'Image & " " & Image (Fs.Items (I).Function_Name) & " at "
              & Image (Fs.Items (I).File) & ":" & Fs.Items (I).Line'Image
              & (if Fs.Items (I).In_Stub then " (stub)" else ""));
         if Image (Fs.Items (I).Function_Name) = Expect and then Found = Frame_Ceiling then
            Found := I;
         end if;
      end loop;
      Check ("frame 0 is the Rust stub's checkpoint", Fs.Count > 0 and then Fs.Items (0).In_Stub
             and then Image (Fs.Items (0).Function_Name) = "cubit_debug_stub::checkpoint",
             (if Fs.Count > 0 then Image (Fs.Items (0).Function_Name) else ""));
      Check (Expect & " is on the stack in main.rs", Found < Fs.Count
             and then Image (Fs.Items (Found).File) = "main.rs" and then Fs.Items (Found).Line > 0);
   end Show_Frames;

   --  The frame's locals, shown; the one named Name.
   function Local (F : Frame; Name : String; Node : out Value_Node; Show : Boolean := True) return Value_Ref is
      Ls : Ref_List;
      Result : Value_Ref := Null_Ref;
      N : Value_Node;
   begin
      Node := (others => <>);
      Locals (S, F, Ls, R);
      for L of Ls.Items (1 .. Ls.Count) loop
         Value (S, L, N, R);
         if Show then
            Say ("    " & Image (L.Name) & " : " & Image (N.Type_Name) & " = " & Image (N.Display));
         end if;
         if Image (L.Name) = Name and then Result = Null_Ref then
            Result := L;
            Node := N;
         end if;
      end loop;
      return Result;
   end Local;

   function Readable (N : Value_Node) return Boolean is
     (N.Display.Length > 0 and then Image (N.Display) (1) /= '<');

   Generation_Before : Display_Text;
   function Generation_Text return Display_Text is
      Gs : Ref_List;
      N : Value_Node;
   begin
      Globals (S, "", Gs, R);
      for X of Gs.Items (1 .. Gs.Count) loop
         if Image (X.Name) = "debug_demo_rs::GENERATION" then
            Value (S, X, N, R);
            return N.Display;
         end if;
      end loop;
      return To_Display ("");
   end Generation_Text;
begin
   CuBit.Console_Route.Route_Named ("logstore");
   Say ("starting");
   for Try in 1 .. Attach_Tries loop
      Attach (Handle, Observe, Deadline_After (Attach_Ms), S, R);
      exit when R = Ok;
      CuBit.Waits.Sleep_For (Retry_Ms);
   end loop;
   Check ("attach to debug-demo-rs", R = Ok, Outcome'Image (R));
   if R /= Ok then
      Say ("FAIL");
      CuBit.Process_Calls.Exit_Process (Exit_Failure);
   end if;
   Describe (S, Info, R);
   Say ("program " & Image (Info.Program) & " build-id " & Info.Build_Id & " debug file "
        & Image (Info.Debug_File) & ", " & Image (Info.Attach_Mode));
   Check ("the target is the Rust demo", Image (Info.Program) = "debug-demo-rs", Image (Info.Program));

   CuBit.Waits.Sleep_For (Run_Between_Ms);
   Threads (S, Ts, R);
   for T of Ts.Items (1 .. Ts.Count) loop
      Say ("thread" & T.Id'Image & " " & Image (T.Name) & " " & Thread_State'Image (T.State));
      if Image (T.Name) = "jobs" then
         Jobs_Thread := T.Id;
      elsif Image (T.Name) = "sensors" then
         Sensors_Thread := T.Id;
      end if;
   end loop;
   Check ("two observed Rust threads, jobs and sensors", Ts.Count = 2 and then Jobs_Thread /= 0
          and then Sensors_Thread /= 0, Ts.Count'Image);
   Generation_Before := Generation_Text;

   Pause (S, Deadline_After (Pause_Ms), R);
   Check ("pause: both Rust threads parked at a checkpoint", R = Ok, Outcome'Image (R));

   --  The jobs thread: advance, called from run_jobs.
   declare
      Fs : Frame_List;
      At_Advance, At_Run : Natural;
      N : Value_Node;
      Kids : Node_List;
      Queue : Value_Ref;
   begin
      Show_Frames (Jobs_Thread, Fs, "debug_demo_rs::advance", At_Advance);
      if At_Advance < Fs.Count then
         Say ("  locals of advance:");
         if Local (Fs.Items (At_Advance), "step", N) /= Null_Ref then
            Check ("advance's step has a value", Readable (N), Image (N.Display));
         else
            Check ("advance's step has a value", False, "no local step");
         end if;
      end if;
      At_Run := Frame_Ceiling;
      for I in 0 .. Fs.Count - 1 loop
         if Image (Fs.Items (I).Function_Name) = "debug_demo_rs::run_jobs" then
            At_Run := I;
         end if;
      end loop;
      Check ("run_jobs calls advance", At_Run < Fs.Count and then At_Run > At_Advance);
      if At_Run < Fs.Count then
         Say ("  locals of run_jobs:");
         Queue := Local (Fs.Items (At_Run), "queue", N);
         Check ("queue is a Vec<Job> of its jobs", Queue /= Null_Ref and then N.Kind = Slice
                and then Image (N.Type_Name) = "Vec<Job>" and then N.Child_Count in 1 .. 6,
                Image (N.Type_Name) & N.Child_Count'Image);
         declare
            Owners_Node : Value_Node;
            Owners : constant Value_Ref := Local (Fs.Items (At_Run), "owners", Owners_Node, Show => False);
         begin
            Check ("owners is a HashMap with an entry per queued job", Owners /= Null_Ref
                   and then Owners_Node.Kind = Slice and then Owners_Node.Child_Count = N.Child_Count
                   and then Has (Image (Owners_Node.Display), "@cubit"),
                   Image (Owners_Node.Display) & Owners_Node.Child_Count'Image);
         end;
         Children (S, Queue, 0, 2, Kids, R);
         Check ("a job's fields: its String name, &str owner, enum priority",
                Kids.Count >= 1 and then Has (Image (Kids.Items (1).Display), "name: """)
                and then Has (Image (Kids.Items (1).Display), "owner: """)
                and then Has (Image (Kids.Items (1).Display), "priority: "),
                (if Kids.Count >= 1 then Image (Kids.Items (1).Display) else ""));
         declare
            Roots : Ref_List;
            Jobs : Natural := 0;
         begin
            Roots.Count := 1;
            Roots.Items (1) := Queue;
            Graph (S, Roots, Graph_Depth, Graph_Nodes, G.all, R);
            for I in 1 .. G.Node_Total loop
               if Image (G.Nodes (I).Value.Type_Name) = "Job" then
                  Jobs := Jobs + 1;
               end if;
            end loop;
            Say ("queue graph:" & G.Node_Total'Image & " nodes," & G.Edge_Total'Image & " edges");
            Check ("graph of queue: the Vec and its Job nodes", Jobs >= 1 and then Jobs = N.Child_Count
                   and then G.Edge_Total >= Jobs, Jobs'Image & G.Edge_Total'Image);
         end;
      end if;
   end;

   --  The sensors thread: insert's recursion, from run_sensors.
   declare
      Fs : Frame_List;
      At_Insert, At_Run, Inserts : Natural := 0;
      N : Value_Node;
      Tree, History : Value_Ref;
      Roots : Ref_List;
   begin
      Show_Frames (Sensors_Thread, Fs, "debug_demo_rs::insert", At_Insert);
      for I in 0 .. Fs.Count - 1 loop
         if Image (Fs.Items (I).Function_Name) = "debug_demo_rs::insert" then
            Inserts := Inserts + 1;
         elsif Image (Fs.Items (I).Function_Name) = "debug_demo_rs::run_sensors" then
            At_Run := I;
         end if;
      end loop;
      Check ("insert recursion on the stack", Inserts >= 1 and then At_Run > At_Insert, Inserts'Image);
      if At_Insert < Fs.Count then
         Say ("  locals of insert:");
         if Local (Fs.Items (At_Insert), "depth", N) /= Null_Ref then
            Check ("insert's depth has a value", Readable (N), Image (N.Display));
         else
            Check ("insert's depth has a value", False, "no local depth");
         end if;
      end if;
      if At_Run > 0 and then At_Run < Fs.Count then
         Say ("  locals of run_sensors:");
         Tree := Local (Fs.Items (At_Run), "tree", N);
         Check ("tree is Some(Box<Sensor>) (a niche enum)", Tree /= Null_Ref and then N.Kind = Rust_Enum
                and then Image (N.Variant) = "Some" and then Image (N.Type_Name) = "Option<Box<Sensor>>",
                Image (N.Type_Name) & " " & Image (N.Variant));
         History := Local (Fs.Items (At_Run), "history", N, Show => False);
         Check ("history is a Vec of Arc<Reading>, counted", History /= Null_Ref and then N.Kind = Slice
                and then Has (Image (N.Display), "strong"), Image (N.Display));
         declare
            Ignore_Ref : constant Value_Ref := Local (Fs.Items (At_Run), "window", N, Show => False);
         begin
            Check ("window is an [i32; 8]", Image (N.Type_Name) = "[i32; 8]" and then N.Child_Count = 8,
                   Image (N.Type_Name));
         end;
         Roots.Count := 2;
         Roots.Items (1) := Tree;
         Roots.Items (2) := History;
         Graph (S, Roots, Graph_Depth, Graph_Nodes, G.all, R);
         declare
            Sensors, Readings, Shared : Natural := 0;
         begin
            for I in 1 .. G.Node_Total loop
               Say ("  node" & I'Image & " " & Image (G.Nodes (I).Value.Type_Name) & " "
                    & Image (G.Nodes (I).Value.Display));
               if Image (G.Nodes (I).Value.Type_Name) = "Sensor" then
                  Sensors := Sensors + 1;
               elsif Image (G.Nodes (I).Value.Type_Name) = "Reading" then
                  Readings := Readings + 1;
               end if;
            end loop;
            for E in 1 .. G.Edge_Total loop
               if G.Edges (E).Kind = Shared_Edge then
                  Shared := Shared + 1;
               end if;
            end loop;
            Say ("tree graph:" & G.Node_Total'Image & " nodes," & G.Edge_Total'Image & " edges,"
                 & Shared'Image & " shared");
            Check ("graph holds the sensor tree", Sensors >= 3, Sensors'Image);
            Check ("graph holds the readings", Readings >= 2, Readings'Image);
            Check ("a reading shared by the tree and the history is one node", Shared >= 1, Shared'Image);
         end;
      end if;
   end;

   --  Security: what the stub shares is read-only.
   declare
      package P renames CuBit.Debug_Protocol;
      package MG renames CuBit.Memory_Grants;
      Share_Ms : constant := 500;
      Gs : Ref_List;
      Page : Unsigned_64 := 0;
      M : Message := NULL_MESSAGE;
      Tag : MessageTag;
      Where : System.Address;
      Written, Read_Only : Boolean := False;
   begin
      Globals (S, "", Gs, R);
      for X of Gs.Items (1 .. Gs.Count) loop
         if Image (X.Name) = "debug_demo_rs::GENERATION" then
            Page := Unsigned_64 (X.Address) - Unsigned_64 (X.Address) mod P.Page_Bytes;
         end if;
      end loop;
      M.tag := (label => P.Op_Share, length => 2, protocol => 0, ipc => <>, reserved => 0);
      M.words (0) := Page;
      M.words (1) := 1;
      Tag := capCall (Startup.Target, M, Deadline_After (Share_Ms));
      if Ending_Of (Tag) = Call_Answered and then M.tag.label = P.Reply_Ok
        and then CuBit.Grant_References.Valid_Wire (M.words (0))
      then
         MG.Acquire_Via_Capability (Startup.Target, CuBit.Grant_References.Decode (M.words (0)), 0,
                                    P.Page_Bytes, MG.Write_Access, Where, Written);
         MG.Acquire_Via_Capability (Startup.Target, CuBit.Grant_References.Decode (M.words (0)), 0,
                                    P.Page_Bytes, MG.Read_Access, Where, Read_Only);
      end if;
      Check ("the Rust stub's pages cannot be acquired for writing", Page /= 0 and then not Written);
      Check ("the Rust stub's pages can be acquired for reading", Read_Only);
   end;

   Resume (S, Deadline_After (Pause_Ms), R);
   Check ("resume", R = Ok, Outcome'Image (R));
   CuBit.Waits.Sleep_For (Run_Between_Ms);
   declare
      After : constant Display_Text := Generation_Text;
   begin
      Check ("debug_demo_rs::GENERATION moved while running",
             After.Length > 0 and then Image (After) /= Image (Generation_Before),
             Image (Generation_Before) & " -> " & Image (After));
   end;
   Detach (S);
   if Failures = 0 then
      Say ("ALL PASS");
      CuBit.Process_Calls.Exit_Process (Exit_Success);
   else
      Say ("FAIL" & Failures'Image);
      CuBit.Process_Calls.Exit_Process (Exit_Failure);
   end if;
end Main;
