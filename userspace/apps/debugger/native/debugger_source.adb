------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Debugger (DBG-001): the snapshot, read from a real target by the engine
------------------------------------------------------------------------------
--  Everything here comes from CuBit.Debug: the cooperative preview session
--  over the target's read-only grants (docs/debugging.md, "cooperative
--  attach (preview)"). An engine outcome other than Ok is shown verbatim
--  in the snapshot's notes; nothing is filled in when a read fails.
with Interfaces; use Interfaces;
with CuBit.Debug;
with CuBit.Debug_Target;
with CuBit.Grant_References;
with CuBit.Handles;
with CuBit.Handles.Debug;
with CuBit.Handles.IPC;
with CuBit.Handles.Services;
with CuBit.Kernel_ABI;
with CuBit.Launch_Handles;
with CuBit.Memory_Grants;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Process_Observer;
with Debug_ABI;
with CuBit.Monotonic;
with CuBit.Waits;
with Startup;

package body Debugger_Source is
   package D renames CuBit.Debug;
   use type D.Outcome;
   use type D.Value_Kind;
   use type D.Target_Address;
   use type D.Edge_Kind;

   --  Deadlines for calls to the target (explicit, never Forever).
   ATTACH_MS   : constant := 2_000;
   PAUSE_MS    : constant := 1_000;
   RESUME_MS   : constant := 500;
   --  How long Continue lets the target run before parking it again.
   RUN_SLICE_MS : constant := 120;
   GRAPH_DEPTH_START : constant D.Graph_Depth := 8;
   GRAPH_MAX_NODES   : constant D.Node_Count := 64;
   CHILDREN_PAGE     : constant D.Child_Count := D.Children_Page_Ceiling;
   MAX_ROOTS         : constant := 8;

   Session : D.Session := D.No_Session;
   Info    : D.Program_Info;

   --  The engine's references behind the snapshot's Value_Ref numbers.
   MAX_REFS : constant := 4_096;
   type Ref_Table is array (1 .. MAX_REFS) of D.Value_Ref;
   Refs : Ref_Table;
   Ref_Total : Natural range 0 .. MAX_REFS := 0;

   Thread_Ids : array (Thread_Index) of D.Thread_Id := [others => 0];
   Frame_Data : D.Frame_List;
   User_Frame : Boolean := False;

   --  Graph roots, by the name of the top-level variable they came from.
   type Root_Name is record
      Name : Text;
   end record;
   Roots : array (1 .. MAX_ROOTS) of Root_Name;
   Root_Total : Natural range 0 .. MAX_ROOTS := 0;
   --  The first root was chosen by the app, not the user: the user's first
   --  G replaces it.
   Auto_Root : Boolean := False;
   Depth : D.Graph_Depth := GRAPH_DEPTH_START;
   type Graph_Access is access D.Graph_Result;
   Graph_Data : constant Graph_Access := new D.Graph_Result;

   Loaded_File : Text;
   --  The package whose globals are shown (GNAT's lower-case unit name).
   Unit : Text;

   function Unit_Of (Func : String) return String is
      LOWER_OFFSET : constant := Character'Pos ('a') - Character'Pos ('A');
      Last : Natural := Func'First - 1;
   begin
      for I in Func'Range loop
         exit when Func (I) = '.' or else Func (I) = ' ';
         Last := I;
      end loop;
      if Last = Func'Last or else Last < Func'First then
         return "";
      end if;
      declare
         R : String := Func (Func'First .. Last);
      begin
         for C of R loop
            if C in 'A' .. 'Z' then
               C := Character'Val (Character'Pos (C) + LOWER_OFFSET);
            end if;
         end loop;
         return R;
      end;
   end Unit_Of;

   function Now return Microseconds is (CuBit.Monotonic.Microseconds);

   function Outcome_Image (R : D.Outcome) return String is
     (case R is
        when D.Ok => "ok", when D.More => "more past a ceiling", when D.Not_Attached => "not attached",
        when D.Refused => "refused", when D.No_Target => "no debug-target answered",
        when D.Timed_Out => "timed out", when D.Not_Stopped => "target not parked",
        when D.Stale => "stale reference", when D.No_Debug_Info => "no debug info for this build",
        when D.Unreadable => "memory not shared", when D.Not_Found => "not found",
        when D.Malformed => "malformed debug info", when D.Exited => "target exited");

   procedure Note (S : in out Snapshot; What : String; R : D.Outcome) is
   begin
      S.Engine_Note := To_Text (What & ": " & Outcome_Image (R));
      debugPrint ("debugger: " & What & ": " & Outcome_Image (R) & ASCII.LF);
   end Note;

   function Keep (R : D.Value_Ref) return Value_Ref is
   begin
      if Ref_Total = MAX_REFS then
         return NO_REF;
      end if;
      Ref_Total := Ref_Total + 1;
      Refs (Ref_Total) := R;
      return Value_Ref (Ref_Total);
   end Keep;

   function Kind_Of (K : D.Value_Kind) return Value_Kind is
     (case K is
        when D.Scalar => Scalar, when D.Enum => Enum, when D.Record_Value => Record_Value,
        when D.Variant_Record => Variant_Record, when D.Array_Value => Array_Value,
        when D.Pointer => Pointer, when D.Rust_Enum => Rust_Enum, when D.Slice => Slice,
        when D.Unknown => Unknown);

   function Int_Image (V : Integer_64) return String is
      S : constant String := Integer_64'Image (V);
   begin
      return (if V < 0 then S else S (S'First + 1 .. S'Last));
   end Int_Image;

   function Entry_Of (N : D.Value_Node) return Local_Entry is
      E : Local_Entry;
   begin
      E.Ref := Keep (N.Self);
      E.Name := To_Text (D.Image (N.Name));
      E.Type_Name := To_Text (D.Image (N.Type_Name)
                              & (if N.Variant.Length > 0 then " (" & D.Image (N.Variant) & ")" else ""));
      E.Value := To_Text (D.Image (N.Display));
      E.Kind := Kind_Of (N.Kind);
      E.Address := Target_Address (N.Address);
      E.Size := N.Size;
      E.Child_Count := N.Child_Count;
      E.Target := (if N.Has_Target then Keep (N.Target) else NO_REF);
      if N.Has_Range then
         E.Bounds := To_Text (Int_Image (N.Range_Low) & " .. " & Int_Image (N.Range_High));
      end if;
      E.Out_Of_Range := N.Out_Of_Range;
      E.Changed_At := (if N.Changed then Now else NEVER);
      return E;
   end Entry_Of;

   procedure Read_Source (S : in out Snapshot; File : String) is
      Page : D.Source_Page;
      R : D.Outcome;
      First : D.Line_Number := 1;
      Added : Boolean;
   begin
      if File = Image (Loaded_File) and then S.Line_Total > 0 then
         return;
      end if;
      Clear_Source (S);
      S.Source_File := To_Text (File);
      S.Source_Note := To_Text ("");
      loop
         D.Source (Session, File, First, Page, R);
         exit when R not in D.Ok | D.More;
         for I in 1 .. Page.Count loop
            Add_Source_Line (S, D.Image (Page.Lines (I)), Added);
            exit when not Added;
         end loop;
         exit when Page.Count = 0 or else First + Page.Count > Page.Total or else S.Line_Total >= MAX_SOURCE_LINES;
         First := First + Page.Count;
      end loop;
      if S.Line_Total = 0 then
         S.Source_Note := To_Text ("No source for " & File & ": " & Outcome_Image (R));
         Loaded_File := To_Text ("");
      else
         Loaded_File := To_Text (File);
      end if;
   end Read_Source;

   procedure Read_Memory (S : in out Snapshot) is
      Bytes : D.Byte_Array (1 .. MEMORY_WINDOW);
      Got : D.Memory_Length;
      R : D.Outcome;
   begin
      S.Mem_Length := 0;
      if S.Mem_Base = NULL_ADDRESS then
         S.Mem_Note := To_Text ("Select a variable and press M to read its memory");
         return;
      end if;
      D.Memory (Session, D.Target_Address (S.Mem_Base), MEMORY_WINDOW, Bytes, Got, R);
      if R not in D.Ok | D.More or else Got = 0 then
         S.Mem_Note := To_Text ("Memory at this address: " & Outcome_Image (R));
         return;
      end if;
      for I in 1 .. Natural (Got) loop
         S.Mem_Bytes (I - 1) := Bytes (I);
      end loop;
      S.Mem_Length := Natural (Got);
   end Read_Memory;

   --  A pointer's display without its " -> Type" tail: the edge and the
   --  node it points at say that already.
   --  Reference counts (Rc, Arc: "(strong 2, weak 1)") are kept.
   function Address_Only (Display : String) return String is
      ARROW : constant String := " -> ";
      COUNTS : constant String := "(strong";
   begin
      for I in Display'First .. Display'Last - ARROW'Length + 1 loop
         if Display (I .. I + ARROW'Length - 1) = ARROW then
            for J in I .. Display'Last - COUNTS'Length + 1 loop
               if Display (J .. J + COUNTS'Length - 1) = COUNTS then
                  return Display (Display'First .. I - 1) & " " & Display (J .. Display'Last);
               end if;
            end loop;
            return Display (Display'First .. I - 1);
         end if;
      end loop;
      return Display;
   end Address_Only;

   function Node_Key (Id : D.Node_Id) return Node_Id is
     (Node_Id (Id.Address) xor Rotate_Left (Unsigned_64 (Id.Of_Type), 29));

   procedure Read_Graph (S : in out Snapshot) is
      List : D.Ref_List;
      R : D.Outcome;
      Index : Natural;
   begin
      S.Node_Total := 0;
      S.Edge_Total := 0;
      S.Graph_Cut := False;
      --  Resolve the roots by name among the top-level variables.
      for K in 1 .. Root_Total loop
         for I in 1 .. S.Local_Total loop
            if S.Locals (I).Depth = 0 and then Image (S.Locals (I).Name) = Image (Roots (K).Name)
              and then S.Locals (I).Ref /= NO_REF and then List.Count < D.Ref_List_Ceiling
            then
               List.Count := List.Count + 1;
               List.Items (List.Count) := Refs (Natural (S.Locals (I).Ref));
               exit;
            end if;
         end loop;
      end loop;
      if List.Count = 0 then
         return;
      end if;
      D.Graph (Session, List, Depth, GRAPH_MAX_NODES, Graph_Data.all, R);
      if R not in D.Ok | D.More then
         Note (S, "graph", R);
         return;
      end if;
      S.Graph_Cut := Graph_Data.Truncated or else R = D.More;
      for N in 1 .. Graph_Data.Node_Total loop
         declare
            G : D.Graph_Node renames Graph_Data.Nodes (N);
            M : Graph_Node;
         begin
            M.Id := Node_Key (G.Id);
            M.Title := To_Text (D.Image (G.Value.Type_Name));
            M.Label := (if G.Is_Root then To_Text (D.Image (G.Value.Name)) else To_Text (""));
            M.Address := Target_Address (G.Id.Address);
            M.Kind := Kind_Of (G.Value.Kind);
            M.Is_Root := G.Is_Root;
            M.Field_Total := Natural'Min (G.Fields_Shown, MAX_NODE_FIELDS);
            M.More_Fields := (if G.Field_Total > M.Field_Total then G.Field_Total - M.Field_Total else 0);
            for F in 1 .. M.Field_Total loop
               M.Fields (F) :=
                 (Name       => To_Short (D.Image (G.Fields (F).Name)),
                  Value      => To_Short (if G.Fields (F).Is_Null then "null"
                                          elsif G.Fields (F).Is_Edge then Address_Only (D.Image (G.Fields (F).Display))
                                          else D.Image (G.Fields (F).Display)),
                  Is_Pointer => G.Fields (F).Is_Edge or else G.Fields (F).Is_Null,
                  --  Non-null pointers can be followed (one level deeper).
                  Pointee    => (if G.Fields (F).Is_Edge and then not G.Fields (F).Is_Null then 1 else NO_REF),
                  Target     => NO_NODE,
                  Dangling   => False,
                  Changed_At => (if G.Fields (F).Changed then Now else NEVER));
            end loop;
            Add_Node (S, M, Index);
         end;
      end loop;
      for E in 1 .. Graph_Data.Edge_Total loop
         declare
            G : D.Graph_Edge renames Graph_Data.Edges (E);
         begin
            if G.From <= S.Node_Total and then G.To <= S.Node_Total and then G.From_Field in 1 .. MAX_NODE_FIELDS
              and then G.From_Field <= S.Nodes (G.From).Field_Total
            then
               S.Nodes (G.From).Fields (G.From_Field).Target := S.Nodes (G.To).Id;
               Add_Edge (S, (From => G.From, From_Field => G.From_Field, To => G.To,
                             Back => G.Kind = D.Back_Edge));
            end if;
         end;
      end loop;
   end Read_Graph;

   --  A wait reason as the panes show it: lower case, words spaced.
   function Lower_Name (Raw : String) return String is
      LOWER_OFFSET : constant := Character'Pos ('a') - Character'Pos ('A');
      R : String := Raw;
   begin
      for C of R loop
         if C in 'A' .. 'Z' then
            C := Character'Val (Character'Pos (C) + LOWER_OFFSET);
         elsif C = '_' then
            C := ' ';
         end if;
      end loop;
      return R;
   end Lower_Name;

   procedure Read_Everything (Before : Snapshot; S : in out Snapshot) is
      Threads : D.Thread_List;
      R : D.Outcome;
      Started : constant Microseconds := Now;
      Added : Boolean;
      List : D.Ref_List;
   begin
      Ref_Total := 0;
      S.Engine_Note := To_Text ("");
      S.Generation := Before.Generation + 1;
      S.Read_At := Started;
      S.Target_Name := To_Text (D.Image (Info.Program));
      S.Target_Pid := Info.Process;
      S.Can_Step := False;
      S.Can_Resume := True;
      S.State := (case D.State (Session) is
                    when D.Running => Running,
                    when D.Stopped_Cooperatively => Stopped_Cooperatively,
                    when D.Exited => Exited,
                    when D.Detached => Not_Attached);
      S.Mode := (if S.State = Running then Live else Stopped);
      S.Through_Kernel := D.Through_Kernel (Session);
      --  Threads.
      D.Threads (Session, Threads, R);
      S.Thread_Total := 0;
      if R in D.Ok | D.More then
         for I in 1 .. Natural'Min (Threads.Count, MAX_THREADS) loop
            S.Thread_Total := I;
            Thread_Ids (I) := Threads.Items (I).Id;
            S.Threads (I) :=
              (Id    => Unsigned_64 (Threads.Items (I).Id),
               Name  => To_Text (D.Image (Threads.Items (I).Name)),
               State => (case Threads.Items (I).State is
                           when D.Running => Thread_Running, when D.Parked => Thread_Parked,
                           when D.Ended => Thread_Exited),
               PC    => Target_Address (Threads.Items (I).PC),
               --  Through the kernel: what it waits on (sleeping, in call ...).
               Wait  => To_Text (if S.Through_Kernel
                                 then Lower_Name (CuBit.Debug_Target.Wait_Code'Image
                                                    (Threads.Items (I).Wait))
                                 else ""));
         end loop;
      else
         Note (S, "threads", R);
      end if;
      if S.Thread_Sel > S.Thread_Total then
         S.Thread_Sel := 1;
      end if;
      --  Frames of the selected thread: while parked (cooperative; running,
      --  the last stop's frames stay, and their locals are not read), or
      --  through the kernel, from the registers it saved, each read anew.
      S.Local_Total := 0;
      S.Locals_Cut := False;
      if S.Thread_Total > 0 and then (S.State = Stopped_Cooperatively or else S.Through_Kernel) then
         D.Frames (Session, Thread_Ids (S.Thread_Sel), Frame_Data, R);
         S.Frame_Total := 0;
         if R in D.Ok | D.More then
            for I in 1 .. Natural'Min (Frame_Data.Count, MAX_FRAMES) loop
               declare
                  F : D.Frame renames Frame_Data.Items (I - 1);
               begin
                  S.Frame_Total := I;
                  S.Frames (I) :=
                    (Func => To_Text (D.Image (F.Function_Name)
                                      & (if F.In_Stub then "  (debug stub)" else "")),
                     File => To_Text (D.Image (F.File)), Line => F.Line, Column => F.Column,
                     PC => Target_Address (F.PC), CFA => Target_Address (F.CFA));
               end;
            end loop;
            --  The innermost frame of the program's own code, unless the
            --  user picked one.
            if not User_Frame or else S.Frame_Sel > S.Frame_Total then
               S.Frame_Sel := 1;
               for I in 1 .. S.Frame_Total loop
                  if not Frame_Data.Items (I - 1).In_Stub and then Frame_Data.Items (I - 1).Has_Debug_Info then
                     S.Frame_Sel := I;
                     exit;
                  end if;
               end loop;
            end if;
         else
            Note (S, "frames", R);
         end if;
      end if;
      --  Locals of the selected frame, then the program's globals.
      if (S.State = Stopped_Cooperatively or else S.Through_Kernel)
        and then S.Frame_Total >= S.Frame_Sel and then S.Frame_Total > 0
      then
         D.Locals (Session, Frame_Data.Items (S.Frame_Sel - 1), List, R);
         if R in D.Ok | D.More then
            for I in 1 .. List.Count loop
               declare
                  N : D.Value_Node;
               begin
                  D.Value (Session, List.Items (I), N, R);
                  if R = D.Ok then
                     Add_Local (S, Entry_Of (N), Added);
                  end if;
               end;
            end loop;
         else
            Note (S, "locals", R);
         end if;
      end if;
      --  The globals of the selected frame's own package (the runtime's
      --  and the allocator's are not what the user is looking at).
      if S.Frame_Total >= S.Frame_Sel and then S.Frame_Total > 0 then
         Unit := To_Text (Unit_Of (Image (S.Frames (S.Frame_Sel).Func)));
      end if;
      D.Globals (Session, Image (Unit), List, R);
      if R in D.Ok | D.More then
         for I in 1 .. List.Count loop
            declare
               N : D.Value_Node;
            begin
               D.Value (Session, List.Items (I), N, R);
               if R = D.Ok then
                  Add_Local (S, Entry_Of (N), Added);
               end if;
            end;
         end loop;
      end if;
      --  Keep what was open.
      declare
         I : Natural := 1;
      begin
         while I <= S.Local_Total loop
            if Kind_Has_Children (S.Locals (I).Kind) and then Was_Expanded (Before, Name_Path (S, I)) then
               Load_Children (S, I);
            end if;
            I := I + 1;
         end loop;
      end;
      --  The first time: draw the first record-like global as the graph.
      if Root_Total = 0 then
         --  A record first (a whole structure), else a pointer.
         for Wanted in Boolean loop
            for I in 1 .. S.Local_Total loop
               if Root_Total = 0 and then S.Locals (I).Depth = 0 and then
                 (if Wanted then S.Locals (I).Kind = Pointer
                  else S.Locals (I).Kind in Record_Value | Variant_Record)
               then
                  Root_Total := 1;
                  Roots (1).Name := S.Locals (I).Name;
                  Auto_Root := True;
               end if;
            end loop;
         end loop;
      end if;
      --  Source of the selected frame.
      if S.Frame_Total >= S.Frame_Sel and then S.Frame_Total > 0 and then S.Frames (S.Frame_Sel).File.Length > 0 then
         Read_Source (S, Image (S.Frames (S.Frame_Sel).File));
      end if;
      Read_Graph (S);
      if S.Mem_Base = NULL_ADDRESS and then Root_Total > 0 then
         for I in 1 .. S.Local_Total loop
            if S.Locals (I).Depth = 0 and then Image (S.Locals (I).Name) = Image (Roots (1).Name) then
               S.Mem_Base := S.Locals (I).Address;
               exit;
            end if;
         end loop;
      end if;
      if S.Mem_Base = NULL_ADDRESS then
         for I in 1 .. S.Local_Total loop
            if S.Locals (I).Address /= NULL_ADDRESS then
               S.Mem_Base := S.Locals (I).Address;
               exit;
            end if;
         end loop;
      end if;
      Read_Memory (S);
      S.Read_Cost_Us := Now - Started;
      --  Live reads come ten a second: only stops are logged.
      if S.Mode = Stopped then
         debugPrint ("debugger: read" & Unsigned_64'Image (S.Generation) & " threads=" & Natural'Image (S.Thread_Total)
                  & " frames=" & Natural'Image (S.Frame_Total) & " locals=" & Natural'Image (S.Local_Total)
                  & " nodes=" & Natural'Image (S.Node_Total) & " us=" & Unsigned_64'Image (S.Read_Cost_Us)
                  & ASCII.LF);
      end if;
   end Read_Everything;

   procedure Attach (S : in out Snapshot; Result : out Attach_Result) is
      R : D.Outcome;
      Empty : Snapshot renames S;
   begin
      D.Attach ((Target => (if Startup.Target.Present then Startup.Target.Value
                            else CuBit.Handles.No_Endpoint),
                 Filesystem => Startup.Filesystem, Session => <>), D.Observe,
                Deadline_After (ATTACH_MS), Session, R);
      if R /= D.Ok then
         Note (S, "attach", R);
         Result := (case R is when D.No_Target => No_Target, when D.Refused => Refused, when others => Engine_Failed);
         return;
      end if;
      D.Describe (Session, Info, R);
      D.Pause (Session, Deadline_After (PAUSE_MS), R);
      if R /= D.Ok then
         Note (S, "pause", R);
      end if;
      Read_Everything (Empty, S);
      Result := Attached;
   end Attach;

   procedure Read (Before : Snapshot; S : in out Snapshot) is
   begin
      Read_Everything (Before, S);
   end Read;

   procedure Continue (Before : Snapshot; S : in out Snapshot) is
      R : D.Outcome;
   begin
      D.Resume (Session, Deadline_After (RESUME_MS), R);
      if R /= D.Ok then
         Note (S, "resume", R);
      else
         CuBit.Waits.Sleep_For (RUN_SLICE_MS);
         D.Pause (Session, Deadline_After (PAUSE_MS), R);
         if R /= D.Ok then
            Note (S, "pause", R);
         end if;
      end if;
      Read_Everything (Before, S);
   end Continue;

   procedure Set_Live (Before : Snapshot; S : in out Snapshot; On : Boolean) is
      R : D.Outcome;
   begin
      if On then
         D.Resume (Session, Deadline_After (RESUME_MS), R);
         if R /= D.Ok then
            Note (S, "resume", R);
         end if;
      else
         D.Pause (Session, Deadline_After (PAUSE_MS), R);
         if R /= D.Ok then
            Note (S, "pause", R);
         end if;
      end if;
      Read_Everything (Before, S);
   end Set_Live;

   procedure Load_Children (S : in out Snapshot; I : Local_Index) is
      List : D.Node_List;
      R : D.Outcome;
      Kids : Local_Batch (1 .. D.Children_Page_Ceiling);
      Ref : constant Value_Ref := S.Locals (I).Ref;
   begin
      if Ref = NO_REF or else Natural (Ref) > Ref_Total then
         return;
      end if;
      --  A pointer opens onto what it points at.
      D.Children (Session,
                  (if S.Locals (I).Kind = Pointer and then S.Locals (I).Target /= NO_REF
                   then Refs (Natural (S.Locals (I).Target)) else Refs (Natural (Ref))),
                  0, CHILDREN_PAGE, List, R);
      if R not in D.Ok | D.More then
         Note (S, "children of " & Image (S.Locals (I).Name), R);
         return;
      end if;
      for K in 1 .. List.Count loop
         Kids (K) := Entry_Of (List.Items (K));
      end loop;
      Insert_Children (S, I, Kids (1 .. List.Count));
   end Load_Children;

   procedure Graph_Local (S : in out Snapshot; I : Local_Index) is
      Top : Local_Index := I;
   begin
      --  Roots are top-level variables (they are found again by name).
      while S.Locals (Top).Depth > 0 and then Top > 1 loop
         Top := Top - 1;
      end loop;
      for K in 1 .. Root_Total loop
         if Image (Roots (K).Name) = Image (S.Locals (Top).Name) then
            return;
         end if;
      end loop;
      if Auto_Root then
         Root_Total := 0;
         Auto_Root := False;
      end if;
      if Root_Total < MAX_ROOTS then
         Root_Total := Root_Total + 1;
         Roots (Root_Total).Name := S.Locals (Top).Name;
      end if;
      Read_Graph (S);
   end Graph_Local;

   procedure Follow (S : in out Snapshot; N : Node_Index; F : Field_Index) is
      pragma Unreferenced (N, F);
   begin
      if Depth < D.Graph_Depth'Last then
         Depth := Depth + 1;
      end if;
      Read_Graph (S);
   end Follow;

   procedure Memory_At (S : in out Snapshot; Address : Target_Address) is
   begin
      S.Mem_Base := Address;
      Read_Memory (S);
   end Memory_At;

   procedure Select_Thread (Before : Snapshot; S : in out Snapshot; T : Thread_Index) is
   begin
      S.Thread_Sel := T;
      User_Frame := False;
      Read_Everything (Before, S);
   end Select_Thread;

   procedure Select_Frame (Before : Snapshot; S : in out Snapshot; F : Frame_Index) is
   begin
      S.Frame_Sel := F;
      User_Frame := True;
      Read_Everything (Before, S);
   end Select_Frame;

   ---------------------------------------------------------------------------
   --  Attach to process...
   ---------------------------------------------------------------------------
   package PO renames CuBit.Process_Observer;
   package LH renames CuBit.Launch_Handles;
   package HD renames CuBit.Handles.Debug;
   package IPC renames CuBit.Handles.IPC;
   use type LH.Outcome;
   LIST_MS : constant := 1_000;
   Listing : array (0 .. PO.Page_Bytes - 1) of Unsigned_8 := [others => 0]
     with Alignment => PO.Page_Bytes;

   function Observer return CuBit.Handles.Endpoint is
     (if Startup.Process_Observer.Present then Startup.Process_Observer.Value
      else CuBit.Handles.No_Endpoint);

   procedure List_Processes (S : in out Snapshot) is
      First : Natural := 0;
      Written, Total : Natural;
      function U64 (At_Byte : Natural) return Unsigned_64 is
         V : Unsigned_64 := 0;
      begin
         for B in reverse 0 .. 7 loop
            V := Shift_Left (V, 8) or Unsigned_64 (Listing (At_Byte + B));
         end loop;
         return V;
      end U64;
   begin
      S.Process_Total := 0;
      loop
         declare
            Loan : CuBit.Memory_Grants.Grant_Reference;
            Lent, Revoked : Boolean;
            Request : Message := NULL_MESSAGE;
            Reply_Tag : MessageTag;
         begin
            CuBit.Memory_Grants.Create_Via_Capability (Observer, Listing'Address, 1, True, Loan, Lent);
            if not Lent then
               Note (S, "processes", D.Refused);
               return;
            end if;
            Request.tag := (label => PO.List_Label, length => PO.Paged_Request_Words, protocol => 0,
                            ipc => <>, reserved => 0);
            Request.words (0) := CuBit.Grant_References.Encode (Loan);
            Request.words (PO.First_Record_Word) := Unsigned_64 (First);
            Reply_Tag := capCall (Observer, Request, Deadline_After (LIST_MS));
            CuBit.Memory_Grants.Revoke (Loan, Revoked);
            exit when Reply_Tag.label /= Unsigned_32 (PO.Status'Enum_Rep (PO.OK));
            Total := Natural (Unsigned_64'Min (Request.words (2), Unsigned_64 (Natural'Last)));
            Written := Natural (Unsigned_64'Min (Request.words (1), PO.Page_Records));
         end;
         for Index in 0 .. Written - 1 loop
            exit when S.Process_Total = MAX_PROCESSES;
            declare
               Base : constant Natural := Index * PO.Record_Bytes;
               Length : constant Natural :=
                 Natural'Min (Natural (Listing (Base + PO.Name_Length_Offset)), PO.Name_Bytes);
               Name : String (1 .. Length);
            begin
               for I in Name'Range loop
                  Name (I) := Character'Val (Listing (Base + PO.Name_Offset + I - 1));
               end loop;
               S.Process_Total := S.Process_Total + 1;
               S.Processes (S.Process_Total) := (Name => To_Text (Name), Identity => U64 (Base + PO.Pid_Offset));
            end;
         end loop;
         First := First + Written;
         exit when First >= Total or else Written = 0 or else S.Process_Total = MAX_PROCESSES;
      end loop;
      debugPrint ("debugger: attach to process: " & Natural'Image (S.Process_Total) & " processes listed" & ASCII.LF);
   end List_Processes;

   procedure Attach_Process (S : in out Snapshot; P : Process_Index; Result : out Attach_Result) is
      Badge : constant CuBit.Handles.Endpoint :=
        (if Startup.Debug_Session.Present
         then CuBit.Handles.Services.Process_Manager.Handle (Startup.Debug_Session.Value)
         else CuBit.Handles.No_Endpoint);
      Target : constant Process_Entry := S.Processes (P);
      Request : Message := NULL_MESSAGE;
      Received : IPC.Carried;
      Answer : MessageTag;
      Minted : LH.Outcome;
      R : D.Outcome;
      Fresh : Snapshot;
   begin
      Result := Refused;
      Request.tag := (label => LH.Session_Debug_Operation, length => LH.Session_Debug_Words,
                      protocol => 0, ipc => <>, reserved => 0);
      Request.words := [Target.Identity, Debug_ABI.Kind_Word (Debug_ABI.Observe), 0, 0];
      Answer := IPC.Call_Accepting (Badge, Request, Deadline_After (ATTACH_MS), Received);
      Minted := (if Answer.label = CuBit.Kernel_ABI.Reply_OK and then LH.Is_Outcome_Word (Request.words (0))
                 then LH.To_Outcome (Request.words (0)) else LH.Failed);
      if Minted /= LH.Done then
         declare
            Ignore : IPC.Close_Result := IPC.Close (Received);
         begin
            S.Engine_Note := To_Text ("procmgr refused to debug " & Image (Target.Name) & ": "
                                      & LH.Outcome'Image (Minted));
            debugPrint ("debugger: " & Image (S.Engine_Note) & ASCII.LF);
         end;
         return;
      end if;
      declare
         Taken : constant HD.Accepted_Session := HD.Accept_Session (Received, Debug_ABI.Observe);
      begin
         if not Taken.OK then
            return;
         end if;
         D.Detach (Session);
         Fresh.Processes := S.Processes;
         Fresh.Process_Total := S.Process_Total;
         S := Fresh;
         Ref_Total := 0;
         Root_Total := 0;
         Auto_Root := False;
         Loaded_File := To_Text ("");
         Unit := To_Text ("");
         D.Attach ((Target => CuBit.Handles.No_Endpoint, Session => Taken.Value,
                    Filesystem => Startup.Filesystem), D.Observe, Deadline_After (ATTACH_MS), Session, R);
      end;
      if R /= D.Ok then
         Note (S, "attach", R);
         Result := Engine_Failed;
         return;
      end if;
      D.Describe (Session, Info, R);
      S.Through_Kernel := D.Through_Kernel (Session);
      debugPrint ("debugger: attached to " & Image (Target.Name) & " through the kernel ("
                  & D.Image (Info.Attach_Mode) & ")" & ASCII.LF);
      Read_Everything (Fresh, S);
      Result := Attached;
   end Attach_Process;

   procedure Detach is
   begin
      D.Detach (Session);
   end Detach;
end Debugger_Source;
