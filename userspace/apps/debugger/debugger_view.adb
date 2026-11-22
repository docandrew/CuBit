------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with CuBit.UI_Metrics;
with CuBit.UI.Instrument; use CuBit.UI.Instrument;
with Debugger_Code;
with Debugger_Locals;
with CuBit.UI.Graphs;
with CuBit.UI.Hex_Views;
with CuBit.UI.Timelines;

package body Debugger_View is
   use CuBit.UI;
   package GV renames Debugger_Graph.Views;
   use type GV.Hit_Kind;
   subtype Color is CuBit.UI.Instrument.Color;
   P : CuBit.UI.Instrument.Palette renames CuBit.UI.Instrument.Instrument;

   ---------------------------------------------------------------------------
   --  Geometry (logical pixels).
   LINE_H       : constant := CuBit.UI_Metrics.Line_Height;
   CHAR_W       : constant := CuBit.UI_Metrics.Mono_Advance;
   TOOLBAR_H    : constant := 44;
   TIMELINE_H   : constant := 70;
   STATUS_H     : constant := 26;
   GAP          : constant := 2;
   HEADER_H     : constant := 28;
   ROW_H        : constant := LINE_H + 2;
   INSET        : constant := 10;
   EDGE_W       : constant := 2;
   CHANGE_EDGE  : constant := 3;
   BUTTON_H     : constant := 28;
   --  Percentages of the window the panes take.
   TOP_SHARE     : constant := 50;
   LEFT_SHARE    : constant := 19;
   LOCALS_SHARE  : constant := 33;
   THREADS_SHARE : constant := 36;
   GRAPH_SHARE   : constant := 61;
   PERCENT       : constant := 100;
   LEFT_MIN      : constant := 250;
   LEFT_MAX      : constant := 470;
   US_PER_MS : constant := 1_000;


   ---------------------------------------------------------------------------
   --  Text helpers.
   function Dec (N : Natural) return String is (Dec (Unsigned_64 (N)));

   SHORT_ADDRESS_DIGITS : constant := 8;

   --  Milliseconds with two decimals, from microseconds.
   function Ms (Us : Unsigned_64) return String is
      Whole : constant Unsigned_64 := Us / US_PER_MS;
      Hundredths : constant Unsigned_64 := (Us mod US_PER_MS) / 10;
   begin
      return Dec (Whole) & "." & (if Hundredths < 10 then "0" else "") & Dec (Hundredths);
   end Ms;






   ---------------------------------------------------------------------------
   procedure Arrange (V : in out View_State; Width, Height : Natural) is
      Body_Y : constant Natural := TOOLBAR_H + GAP;
      Body_H : constant Integer := Integer (Height) - TOOLBAR_H - TIMELINE_H - STATUS_H - 2 * GAP;
      Top_H  : Integer;
      Low_Y, Low_H : Integer;
      Left_W, Right_W, Mid_W, Threads_H, Graph_W : Integer;
      function R (X, Y, W, H : Integer) return Rect is
        (Natural'Max (X, 0), Natural'Max (Y, 0), Natural'Max (W, 0), Natural'Max (H, 0));
   begin
      V.Width := Width;
      V.Height := Height;
      Top_H := Integer'Max (0, Body_H * TOP_SHARE / PERCENT);
      Low_Y := Body_Y + Top_H + GAP;
      Low_H := Integer'Max (0, Body_H - Top_H - GAP);
      Left_W := Integer'Max (LEFT_MIN, Integer'Min (LEFT_MAX, Integer (Width) * LEFT_SHARE / PERCENT));
      Right_W := Integer (Width) * LOCALS_SHARE / PERCENT;
      Mid_W := Integer (Width) - Left_W - Right_W - 2 * GAP;
      Threads_H := Top_H * THREADS_SHARE / PERCENT;
      Graph_W := Integer (Width) * GRAPH_SHARE / PERCENT;
      V.Regions (Toolbar_Region) := R (0, 0, Width, TOOLBAR_H);
      V.Panes (Threads_Pane) := R (0, Body_Y, Left_W, Threads_H);
      V.Panes (Stack_Pane) := R (0, Body_Y + Threads_H + GAP, Left_W, Top_H - Threads_H - GAP);
      V.Panes (Source_Pane) := R (Left_W + GAP, Body_Y, Mid_W, Top_H);
      V.Panes (Locals_Pane) := R (Left_W + Mid_W + 2 * GAP, Body_Y, Right_W, Top_H);
      V.Panes (Graph_Pane) := R (0, Low_Y, Graph_W, Low_H);
      V.Panes (Memory_Pane) := R (Graph_W + GAP, Low_Y, Integer (Width) - Graph_W - GAP, Low_H);
      V.Regions (Timeline_Region) := R (0, Low_Y + Low_H + GAP, Width, TIMELINE_H);
      if V.Maximized then
         --  The focused pane alone, over the whole panes' area.
         V.Panes := [others => (others => 0)];
         V.Panes (V.Focus) := R (0, Body_Y, Width, Low_Y + Low_H - Body_Y);
      end if;
      V.Regions (Status_Region) := R (0, Integer (Height) - STATUS_H, Width, STATUS_H);
   end Arrange;

   function Body_Of (R : Rect) return Rect is
     ((R.x, R.y + HEADER_H, R.w, (if R.h > HEADER_H then R.h - HEADER_H else 0)));

   function Rows_In (R : Rect; Row : Positive) return Natural is (Body_Of (R).h / Row);

   ---------------------------------------------------------------------------
   --  Locals tree: which entries show.
   procedure Rebuild_Visible (V : in out View_State; S : Snapshot) is
   begin
      Debugger_Locals.Table.Refresh (V.Locals, S);
   end Rebuild_Visible;

   function Visible_Position (V : View_State; Index : Local_Index) return Natural is
     (CuBit.UI.Tree_Tables.Position (V.Locals, Index));



   procedure Absorb (V : in out View_State; S : Snapshot) is
      Current : Natural := 0;
   begin
      Rebuild_Visible (V, S);
      if Visible_Position (V, V.Locals.Selected) = 0 then
         V.Locals.Selected := (if V.Locals.Visible_Total > 0 then V.Locals.Visible (1) else 1);
      end if;
      GV.Update (V.Graph, S, Body_Of (V.Panes (Graph_Pane)));
      if not V.Graph_Fitted and then S.Node_Total > 0 then
         GV.Fit (V.Graph, S, Body_Of (V.Panes (Graph_Pane)));
         V.Graph_Fitted := True;
      end if;
      V.Thread_Row := S.Thread_Sel;
      V.Frame_Row := S.Frame_Sel;
      if S.Frame_Total >= S.Frame_Sel then
         Current := S.Frames (S.Frame_Sel).Line;
      end if;
      CuBit.UI.Code_Views.Show (V.Code, Current, Body_Of (V.Panes (Source_Pane)));
   end Absorb;

   ---------------------------------------------------------------------------
   --  Pane chrome.
   procedure Pane_Frame (C : Canvas; R : Rect; Title, Detail, Hint : String; Focused : Boolean) is
      Title_W : constant Natural := Emphasis_Text_Width (Title);
      Y : constant Integer := Text_Y (R.y, HEADER_H);
   begin
      Fill (C, R.x, R.y, R.w, R.h, P.Pane);
      Fill (C, R.x, R.y, R.w, HEADER_H, (if Focused then P.Header_Focus else P.Header));
      Fill (C, R.x, R.y + HEADER_H - 1, R.w, 1, P.Rule);
      if Focused then
         Fill (C, R.x, R.y, R.w, EDGE_W, P.Focus_Mark);
      end if;
      Bold (C, R.x + INSET, Y, Title, (if Focused then P.Text else P.Muted));
      if Detail'Length > 0 then
         Mono (C, R.x + INSET + Title_W + INSET, Y,
               Fits (Detail, Integer (R.w) - Title_W - 3 * INSET - Code_Text_Width (Hint)), P.Text);
      end if;
      if Hint'Length > 0 then
         Mono (C, R.x + Integer (R.w) - INSET - Code_Text_Width (Hint), Y, Hint, P.Dim);
      end if;
   end Pane_Frame;

   procedure Empty_Note (C : Canvas; R : Rect; Note : String) is
      B : constant Rect := Body_Of (R);
   begin
      Sans (C, B.x + INSET, B.y + INSET, Fit_UI_Text (Note, Natural'Max (B.w, 2 * INSET) - 2 * INSET), P.Dim);
   end Empty_Note;

   ---------------------------------------------------------------------------
   procedure Draw_Toolbar (C : Canvas; Colors : Theme; S : Snapshot; V : View_State) is
      R : constant Rect := V.Regions (Toolbar_Region);
      X : Natural := INSET;
      BY : constant Natural := R.y + (TOOLBAR_H - BUTTON_H) / 2;
      procedure Button (Label : String; Enabled : Boolean) is
         W : constant Natural := UI_Text_Width (Label) + 2 * BUTTON_TEXT_INSET;
      begin
         Draw_Button (C, (X, BY, W, BUTTON_H), Colors, (if Enabled then Button_Normal else Button_Disabled), Label);
         X := X + W + CuBit.UI_Metrics.Space_S;
      end Button;
      procedure Separator is
      begin
         X := X + CuBit.UI_Metrics.Space_S;
         Fill (C, X, BY + 4, 1, BUTTON_H - 8, Colors.menuRule);
         X := X + CuBit.UI_Metrics.Space_M;
      end Separator;
      State_Text : constant String :=
        (case S.State is
           when Not_Attached => "Not attached",
           when Running => (if S.Mode = Live then "Running: live reads" else "Running"),
           when Stopped_Cooperatively =>
             (if S.Checkpoint.Length > 0 then "Parked at checkpoint " & Image (S.Checkpoint) else "Parked"),
           when Exited => "Exited");
      State_Col : constant Color :=
        (case S.State is
           when Running => P.Good, when Stopped_Cooperatively => P.Warn,
           when Exited | Not_Attached => P.Dim);
      Target : constant String :=
        (if S.Target_Name.Length > 0 then Image (S.Target_Name) & "   process 0x" & Hex (S.Target_Pid, SHORT_ADDRESS_DIGITS)
         else "no target");
      SQUARE : constant := 10;
      Right : Integer;
   begin
      Fill (C, R.x, R.y, R.w, R.h, Colors.titleBar);
      Fill (C, R.x, R.y + R.h - 1, R.w, 1, Colors.menuRule);
      Button ((if S.Can_Resume then "Continue   F5" else "Refresh   F5"), S.State /= Not_Attached);
      --  Live: the target runs and the views re-read it through the grants.
      declare
         W : constant Natural := UI_Text_Width ("Live   L") + 2 * BUTTON_TEXT_INSET;
      begin
         Draw_Button (C, (X, BY, W, BUTTON_H), Colors,
                      (if S.Mode = Live then Button_Active
                       elsif S.State = Not_Attached then Button_Disabled else Button_Normal), "Live   L");
         X := X + W + CuBit.UI_Metrics.Space_S;
      end;
      Separator;
      Button ("Step over   F10", S.Can_Step);
      Button ("Step in   F11", S.Can_Step);
      Button ("Step out   Shift+F11", S.Can_Step);
      Separator;
      Button ("Reverse   Alt+F5", False);
      --  Target and state, right aligned.
      Right := Integer (R.w) - INSET - UI_Text_Width (State_Text);
      Sans (C, Right, Text_Y (R.y, TOOLBAR_H), State_Text, Colors.text);
      Right := Right - CuBit.UI_Metrics.Space_S - SQUARE;
      Fill (C, Right, R.y + (TOOLBAR_H - SQUARE) / 2, SQUARE, SQUARE, State_Col);
      Right := Right - 3 * INSET - Emphasis_Text_Width (Target);
      if Right > Integer (X) then
         Bold (C, Right, Text_Y (R.y, TOOLBAR_H), Target, Colors.text);
      end if;
   end Draw_Toolbar;

   ---------------------------------------------------------------------------
   --  Attach to process...: what runs, in the Threads pane.
   procedure Draw_Processes (C : Canvas; S : Snapshot; V : View_State) is
      R : constant Rect := V.Panes (Threads_Pane);
      B : constant Rect := Body_Of (R);
      Rows : constant Natural := Rows_In (R, ROW_H);
      First : constant Positive := (if V.Pick_Row > Rows and then Rows > 0 then V.Pick_Row - Rows + 1 else 1);
   begin
      Pane_Frame (C, R, "ATTACH TO PROCESS", "", Dec (S.Process_Total), True);
      if S.Process_Total = 0 then
         Empty_Note (C, R, "No processes listed (process-observer)");
         return;
      end if;
      for I in First .. Natural'Min (S.Process_Total, First + Rows - 1) loop
         declare
            P_Entry : Process_Entry renames S.Processes (I);
            Y : constant Integer := B.y + (I - First) * ROW_H;
            Id : constant String := Address_Image (Target_Address (P_Entry.Identity));
         begin
            if I = V.Pick_Row then
               Fill (C, B.x, Y, B.w, ROW_H, P.Selection);
            end if;
            Mono (C, B.x + INSET, Text_Y (Y, ROW_H),
                  Fits (Image (P_Entry.Name), Integer (B.w) - Code_Text_Width (Id) - 3 * INSET), P.Text);
            Mono (C, B.x + Integer (B.w) - INSET - Code_Text_Width (Id), Text_Y (Y, ROW_H), Id, P.Dim);
         end;
      end loop;
   end Draw_Processes;

   procedure Draw_Threads (C : Canvas; S : Snapshot; V : View_State) is
      R : constant Rect := V.Panes (Threads_Pane);
      B : constant Rect := Body_Of (R);
      Rows : constant Natural := Rows_In (R, ROW_H);
      SQUARE : constant := 8;
   begin
      if V.Picking then
         Draw_Processes (C, S, V);
         return;
      end if;
      Pane_Frame (C, R, "THREADS", "", Dec (S.Thread_Total), V.Focus = Threads_Pane);
      if S.Thread_Total = 0 then
         Empty_Note (C, R, (if S.State = Not_Attached then "Not attached" else "No threads reported"));
         return;
      end if;
      for I in 1 .. Natural'Min (S.Thread_Total, Rows) loop
         declare
            T : Thread_Info renames S.Threads (I);
            Y : constant Integer := B.y + (I - 1) * ROW_H;
            Sel : constant Boolean := I = V.Thread_Row;
            State_Name : constant String :=
              (case T.State is
                 when Thread_Running => "running", when Thread_Parked => "parked",
                 when Thread_Blocked => "blocked", when Thread_Exited => "exited",
                 when Thread_Unknown => "?");
            Col : constant Color :=
              (case T.State is
                 when Thread_Running => P.Good, when Thread_Parked => P.Warn,
                 when others => P.Dim);
            PC : constant String := Address_Image (T.PC);
            Name_X : constant Integer := B.x + INSET + SQUARE + INSET + 3 * CHAR_W;
         begin
            if Sel then
               Fill (C, B.x, Y, B.w, ROW_H, (if V.Focus = Threads_Pane then P.Selection else P.Caller_Line));
            end if;
            Fill (C, B.x + INSET, Y + (ROW_H - SQUARE) / 2, SQUARE, SQUARE, Col);
            Mono (C, B.x + INSET + SQUARE + INSET, Text_Y (Y, ROW_H), Dec (T.Id), P.Muted);
            Mono (C, Name_X, Text_Y (Y, ROW_H),
                  Fits (Image (T.Name) & "  " & State_Name,
                        Integer (B.w) - (Name_X - B.x) - Code_Text_Width (PC) - 2 * INSET),
                  P.Text);
            Mono (C, B.x + Integer (B.w) - INSET - Code_Text_Width (PC), Text_Y (Y, ROW_H), PC, P.Dim);
         end;
      end loop;
   end Draw_Threads;

   STACK_ROW_H : constant := 2 * ROW_H;

   procedure Draw_Stack (C : Canvas; S : Snapshot; V : View_State) is
      R : constant Rect := V.Panes (Stack_Pane);
      B : constant Rect := Body_Of (R);
      Rows : constant Natural := B.h / STACK_ROW_H;
      Number_W : constant := 4 * CHAR_W;
   begin
      Pane_Frame (C, R, "CALL STACK", "", Dec (S.Frame_Total) & " frames", V.Focus = Stack_Pane);
      if S.Frame_Total = 0 then
         Empty_Note (C, R, (if S.State = Not_Attached then "Not attached" else "No frames unwound"));
         return;
      end if;
      for I in 1 .. Natural'Min (S.Frame_Total, Rows) loop
         declare
            F : Frame_Info renames S.Frames (I);
            Y : constant Integer := B.y + (I - 1) * STACK_ROW_H;
            Where : constant String :=
              (if F.File.Length > 0 then Image (F.File) & ":" & Dec (F.Line) else "no line information");
            PC : constant String := Address_Image (F.PC);
            Text_X : constant Integer := B.x + INSET + Number_W;
            Room : constant Integer := Integer (B.w) - (Text_X - B.x) - INSET;
         begin
            if I = V.Frame_Row then
               Fill (C, B.x, Y, B.w, STACK_ROW_H, (if V.Focus = Stack_Pane then P.Selection else P.Caller_Line));
            end if;
            if I = S.Frame_Sel then
               Fill (C, B.x, Y, CHANGE_EDGE, STACK_ROW_H, P.Current_Mark);
            end if;
            Mono (C, B.x + INSET, Text_Y (Y, ROW_H), "#" & Dec (I - 1), P.Dim);
            Mono (C, Text_X, Text_Y (Y, ROW_H), Fits (Image (F.Func), Room), (if I = 1 then P.Text else P.Type_Name));
            Mono (C, Text_X, Text_Y (Y + ROW_H - 3, ROW_H), Fits (Where, Room), P.Muted);
            if Room > Code_Text_Width (Where) + Code_Text_Width (PC) + INSET then
               Mono (C, B.x + Integer (B.w) - INSET - Code_Text_Width (PC), Text_Y (Y + ROW_H - 3, ROW_H), PC, P.Dim);
            end if;
            Fill (C, B.x + INSET, Y + STACK_ROW_H - 1, Integer (B.w) - 2 * INSET, 1, P.Rule);
         end;
      end loop;
   end Draw_Stack;

   ---------------------------------------------------------------------------
   function Current_Line_Of (S : Snapshot) return Natural renames Debugger_Code.Current_Line;

   function Current_Line_Text (S : Snapshot) return String is
      N : constant Natural := Current_Line_Of (S);
   begin
      return (if N in 1 .. S.Line_Total then Source_Line (S, N) else "");
   end Current_Line_Text;

   procedure Draw_Source (C : Canvas; S : Snapshot; V : View_State; Now : Microseconds) is
      R : constant Rect := V.Panes (Source_Pane);
      Current : constant Natural := Current_Line_Of (S);
      Hint : constant String := Debugger_Code.Language_Name (S) & (if Current > 0 then "  line " & Dec (Current) else "");
   begin
      Pane_Frame (C, R, "SOURCE", Image (S.Source_File), Hint, V.Focus = Source_Pane);
      if S.Line_Total = 0 then
         Empty_Note (C, R, (if S.Source_Note.Length > 0 then Image (S.Source_Note) else "No source for this frame"));
         return;
      end if;
      Debugger_Code.Set_Now (Now);
      Debugger_Code.Lines.Draw (C, Body_Of (R), S, V.Code, P);
   end Draw_Source;

   ---------------------------------------------------------------------------

   procedure Draw_Locals (C : Canvas; S : Snapshot; V : View_State; Now : Microseconds) is
      R : constant Rect := V.Panes (Locals_Pane);
      Func : constant String := (if S.Frame_Total >= S.Frame_Sel then Image (S.Frames (S.Frame_Sel).Func) else "");
   begin
      Pane_Frame (C, R, "LOCALS", Func, (if S.Locals_Cut then "cut at ceiling" else ""), V.Focus = Locals_Pane);
      Debugger_Locals.Set_Now (Now);
      Debugger_Locals.Table.Draw (C, Body_Of (R), S, V.Locals, "Name|Value|Type", V.Focus = Locals_Pane, P);
      if V.Locals.Visible_Total = 0 then
         Empty_Note (C, (R.x, R.y + CuBit.UI.Tree_Tables.ROW_HEIGHT, R.w, R.h), "No locals in this frame");
      end if;
   end Draw_Locals;

   procedure Draw_Graph (C : Canvas; S : Snapshot; V : View_State; Now : Microseconds) is
      R : constant Rect := V.Panes (Graph_Pane);
      Hint : constant String :=
        Dec (CuBit.UI.Graphs.Zoom_Percent (V.Graph.Zoom)) & "%  " & Dec (S.Node_Total) & " nodes  "
        & Dec (S.Edge_Total) & " edges" & (if S.Graph_Cut then "  (cut)" else "");
   begin
      Pane_Frame (C, R, "DATA STRUCTURES", "", Hint, V.Focus = Graph_Pane);
      if S.Node_Total = 0 then
         Empty_Note (C, R, "No structures yet. Press G on a local to draw it here.");
         return;
      end if;
      Debugger_Graph.Set_Now (Now);
      GV.Draw (C, Body_Of (R), S, V.Graph, P);
   end Draw_Graph;

   ---------------------------------------------------------------------------
   function Bytes_Per_Row (V : View_State) return Positive is
     (CuBit.UI.Hex_Views.Bytes_Per_Row (V.Panes (Memory_Pane).w));

   procedure Draw_Memory (C : Canvas; S : Snapshot; V : View_State; Now : Microseconds) is
      R : constant Rect := V.Panes (Memory_Pane);
      Sel_First, Sel_Last : Target_Address := 1;
   begin
      if S.Local_Total >= V.Locals.Selected and then S.Locals (V.Locals.Selected).Address /= NULL_ADDRESS
        and then S.Locals (V.Locals.Selected).Size > 0
      then
         Sel_First := S.Locals (V.Locals.Selected).Address;
         Sel_Last := Sel_First + S.Locals (V.Locals.Selected).Size - 1;
      else
         Sel_First := 1;
         Sel_Last := 0;
      end if;
      Pane_Frame (C, R, "MEMORY",
                  (if S.Mem_Length > 0 then Address_Image (S.Mem_Base) else ""),
                  (if S.Mem_Length > 0 then Dec (S.Mem_Length) & " bytes, read-only" else ""),
                  V.Focus = Memory_Pane);
      if S.Mem_Length = 0 then
         Empty_Note (C, R, (if S.Mem_Note.Length > 0 then Image (S.Mem_Note) else "No memory read yet"));
         return;
      end if;
      declare
         Bytes, Previous : CuBit.UI.Hex_Views.Byte_Array (0 .. S.Mem_Length - 1);
      begin
         for I in Bytes'Range loop
            Bytes (I) := S.Mem_Bytes (I);
            Previous (I) := S.Mem_Prev (I);
         end loop;
         CuBit.UI.Hex_Views.Draw
           (C, Body_Of (R), S.Mem_Base, Bytes, Previous, Marked (S.Mem_Changed_At, S, Now), Sel_First, Sel_Last,
            (Top => V.Mem_Top, others => <>), P);
      end;
   end Draw_Memory;

   ---------------------------------------------------------------------------
   --  The replay timeline. It is not built (slice 4): the strip is here,
   --  ruled and labelled, a lane per thread, with nothing on it.
   procedure Draw_Timeline (C : Canvas; S : Snapshot; V : View_State) is
      Strip : CuBit.UI.Timelines.Strip;
   begin
      CuBit.UI.Timelines.Set_Title (Strip, "REPLAY", "coming");
      CuBit.UI.Timelines.Set_Notice
        (Strip, "Replay: coming (slice 4). Nothing is recorded yet; live reads only.",
         "J K L  scrub    [ ]  writes");
      for T in 1 .. Natural'Min (S.Thread_Total, CuBit.UI.Timelines.MAX_LANES) loop
         Strip.Lanes (T) := CuBit.UI.Timelines.Lane_Of (Image (S.Threads (T).Name));
         Strip.Lane_Total := T;
      end loop;
      CuBit.UI.Timelines.Draw (C, V.Regions (Timeline_Region), Strip, P);
   end Draw_Timeline;

   procedure Draw_Status (C : Canvas; Colors : Theme; S : Snapshot; V : View_State) is
      R : constant Rect := V.Regions (Status_Region);
      Left : constant String :=
        "Cooperative attach (preview)  |  Observe only  |  memory through a read-only grant"
        & (if S.Engine_Note.Length > 0 then "  |  " & Image (S.Engine_Note) else "");
      Right : constant String :=
        (if S.Generation > 0 then "read " & Ms (S.Read_Cost_Us) & " ms  |  " else "")
        & "frame " & Ms (V.Stats.Last_Us) & " ms, worst " & Ms (V.Stats.Worst_Us) & " ms";
   begin
      Draw_Status_Bar (C, R, Colors, Left, Right);
   end Draw_Status;

   ---------------------------------------------------------------------------
   procedure Render
     (C : Canvas; Damage : Rect; Colors : Theme; S : Snapshot; V : in out View_State; Now : Microseconds)
   is
      function Hit (R : Rect) return Boolean is (Intersects (R, Damage));
   begin
      if Hit ((0, 0, V.Width, V.Height)) then
         --  The gaps between panes.
         Fill (C, 0, TOOLBAR_H, V.Width, Integer (V.Height) - TOOLBAR_H - STATUS_H, P.Workspace);
      end if;
      if Hit (V.Regions (Toolbar_Region)) then Draw_Toolbar (C, Colors, S, V); end if;
      if Hit (V.Panes (Threads_Pane)) then Draw_Threads (C, S, V); end if;
      if Hit (V.Panes (Stack_Pane)) then Draw_Stack (C, S, V); end if;
      if Hit (V.Panes (Source_Pane)) then Draw_Source (With_Clip (C, V.Panes (Source_Pane)), S, V, Now); end if;
      if Hit (V.Panes (Locals_Pane)) then Draw_Locals (With_Clip (C, V.Panes (Locals_Pane)), S, V, Now); end if;
      if Hit (V.Panes (Graph_Pane)) then Draw_Graph (With_Clip (C, V.Panes (Graph_Pane)), S, V, Now); end if;
      if Hit (V.Panes (Memory_Pane)) then Draw_Memory (With_Clip (C, V.Panes (Memory_Pane)), S, V, Now); end if;
      if Hit (V.Regions (Timeline_Region)) then Draw_Timeline (C, S, V); end if;
      if Hit (V.Regions (Status_Region)) then Draw_Status (C, Colors, S, V); end if;
   end Render;

   procedure Record_Frame (V : in out View_State; Took_Us : Unsigned_64; Full : Boolean) is
   begin
      V.Stats.Last_Us := Took_Us;
      V.Stats.Worst_Us := Unsigned_64'Max (V.Stats.Worst_Us, Took_Us);
      V.Stats.Total_Us := V.Stats.Total_Us + Took_Us;
      V.Stats.Count := V.Stats.Count + 1;
      V.Stats.Last_Full := Full;
   end Record_Frame;

   ---------------------------------------------------------------------------
   --  Input.
   procedure Add (Dirty : in out Rect; R : Rect) is
   begin
      Dirty := (if Is_Empty (Dirty) then R else Union_Rect (Dirty, R));
   end Add;

   procedure Set_Focus (V : in out View_State; To : Pane; Dirty : in out Rect) is
   begin
      if V.Focus /= To then
         Add (Dirty, V.Panes (V.Focus));
         V.Focus := To;
         Add (Dirty, V.Panes (To));
      end if;
   end Set_Focus;

   procedure Ensure_Local_Visible (V : in out View_State) is
   begin
      CuBit.UI.Tree_Tables.Ensure_Visible (V.Locals, CuBit.UI.Tree_Tables.Rows_In (Body_Of (V.Panes (Locals_Pane))));
   end Ensure_Local_Visible;

   procedure Toggle_Local (V : in out View_State; S : in out Snapshot; I : Local_Index; Open : Boolean;
                           Req : in out Request) is
   begin
      if Open then
         if not S.Locals (I).Loaded and then Kind_Has_Children (S.Locals (I).Kind) then
            Req := (Cmd => Load_Children, Local => I, others => <>);
         elsif S.Locals (I).Loaded then
            S.Locals (I).Expanded := True;
         end if;
      else
         S.Locals (I).Expanded := False;
      end if;
      Rebuild_Visible (V, S);
   end Toggle_Local;



   procedure Key
     (V : in out View_State; S : in out Snapshot; Code, Modifiers : Unsigned_64; Dirty : in out Rect;
      Req : out Request)
   is
      Shift : constant Boolean := (Modifiers and MOD_SHIFT) /= 0;
      Page_Source : constant Positive := Natural'Max (1, CuBit.UI.Code_Views.Rows_In (Body_Of (V.Panes (Source_Pane))) - 2);
      Mem_Rows : constant Natural := (S.Mem_Length + Bytes_Per_Row (V) - 1) / Bytes_Per_Row (V);
   begin
      Req := (others => <>);
      if V.Picking then
         if Code = KEY_UP and then V.Pick_Row > 1 then
            V.Pick_Row := V.Pick_Row - 1;
         elsif Code = KEY_DOWN and then V.Pick_Row < S.Process_Total then
            V.Pick_Row := V.Pick_Row + 1;
         elsif Code = KEY_ENTER and then V.Pick_Row <= S.Process_Total then
            Req := (Cmd => Attach_Process, Process => V.Pick_Row, others => <>);
            V.Picking := False;
            Add (Dirty, Everything (V));
         elsif Code = KEY_ESC then
            V.Picking := False;
            Add (Dirty, Everything (V));
         end if;
         Add (Dirty, V.Panes (Threads_Pane));
         return;
      end if;
      if Code = KEY_A then
         Req.Cmd := Pick_Process;
      elsif Code = KEY_TAB then
         Set_Focus (V, (if Shift then (if V.Focus = Pane'First then Pane'Last else Pane'Pred (V.Focus))
                        else (if V.Focus = Pane'Last then Pane'First else Pane'Succ (V.Focus))), Dirty);
         if V.Maximized then
            Arrange (V, V.Width, V.Height);
            Add (Dirty, Everything (V));
         end if;
      elsif Code = KEY_F5 then
         Req.Cmd := (if S.Can_Resume then Continue else Refresh);
      elsif Code = KEY_ESC then
         Req.Cmd := Quit;
      elsif Code = KEY_F then
         V.Maximized := not V.Maximized;
         Arrange (V, V.Width, V.Height);
         if V.Focus = Graph_Pane then
            GV.Relayout (V.Graph, S, Body_Of (V.Panes (Graph_Pane)));
            GV.Fit (V.Graph, S, Body_Of (V.Panes (Graph_Pane)));
         end if;
         Add (Dirty, Everything (V));
      elsif Code = KEY_L and then S.State /= Not_Attached then
         Req.Cmd := Toggle_Live;
      elsif Code in KEY_F10 | KEY_F11 then
         null;   --  Stepping is slice 2: the buttons are greyed, the keys do nothing.
      else
         case V.Focus is
            when Threads_Pane =>
               if Code = KEY_UP and then V.Thread_Row > 1 then
                  V.Thread_Row := V.Thread_Row - 1;
                  Req := (Cmd => Select_Thread, Thread => V.Thread_Row, others => <>);
               elsif Code = KEY_DOWN and then V.Thread_Row < S.Thread_Total then
                  V.Thread_Row := V.Thread_Row + 1;
                  Req := (Cmd => Select_Thread, Thread => V.Thread_Row, others => <>);
               end if;
               Add (Dirty, V.Panes (Threads_Pane));
            when Stack_Pane =>
               if Code = KEY_UP and then V.Frame_Row > 1 then
                  V.Frame_Row := V.Frame_Row - 1;
                  Req := (Cmd => Select_Frame, Frame => V.Frame_Row, others => <>);
               elsif Code = KEY_DOWN and then V.Frame_Row < S.Frame_Total then
                  V.Frame_Row := V.Frame_Row + 1;
                  Req := (Cmd => Select_Frame, Frame => V.Frame_Row, others => <>);
               end if;
               Add (Dirty, V.Panes (Stack_Pane));
            when Source_Pane =>
               if Code = KEY_UP then
                  CuBit.UI.Code_Views.Scroll (V.Code, -1, S.Line_Total);
               elsif Code = KEY_DOWN then
                  CuBit.UI.Code_Views.Scroll (V.Code, 1, S.Line_Total);
               elsif Code = KEY_PGUP then
                  CuBit.UI.Code_Views.Scroll (V.Code, -Page_Source, S.Line_Total);
               elsif Code = KEY_PGDN then
                  CuBit.UI.Code_Views.Scroll (V.Code, Page_Source, S.Line_Total);
               elsif Code = KEY_HOME or else Code = KEY_ENTER then
                  V.Code.Follow := True;
                  Absorb (V, S);
               end if;
               Add (Dirty, V.Panes (Source_Pane));
            when Locals_Pane =>
               declare
                  K : constant Natural := Visible_Position (V, V.Locals.Selected);
                  Wanted : CuBit.UI.Tree_Tables.Wish;
                  Item : Local_Index;
                  use type CuBit.UI.Tree_Tables.Wish;
               begin
                  if K > 0 and then Code = KEY_G then
                     Req := (Cmd => Show_In_Graph, Local => V.Locals.Selected, others => <>);
                  elsif K > 0 and then Code = KEY_M then
                     Req := (Cmd => Show_Memory, Local => V.Locals.Selected,
                             Address => S.Locals (V.Locals.Selected).Address, others => <>);
                  else
                     Debugger_Locals.Table.Key (V.Locals, S, Body_Of (V.Panes (Locals_Pane)), Code, Wanted, Item);
                     case Wanted is
                        when CuBit.UI.Tree_Tables.Expand => Toggle_Local (V, S, Item, True, Req);
                        when CuBit.UI.Tree_Tables.Collapse => Toggle_Local (V, S, Item, False, Req);
                        when CuBit.UI.Tree_Tables.Toggle =>
                           Toggle_Local (V, S, Item, not (S.Locals (Item).Expanded and then S.Locals (Item).Loaded), Req);
                        when others => null;
                     end case;
                  end if;
                  Ensure_Local_Visible (V);
                  Add (Dirty, V.Panes (Locals_Pane));
                  Add (Dirty, V.Panes (Memory_Pane));
                  --  The memory pane follows the selection when it leaves
                  --  the bytes on show.
                  if Req.Cmd = No_Command and then V.Locals.Selected <= S.Local_Total
                    and then S.Locals (V.Locals.Selected).Address /= NULL_ADDRESS
                    and then (S.Locals (V.Locals.Selected).Address < S.Mem_Base or else
                              S.Locals (V.Locals.Selected).Address >= S.Mem_Base + Unsigned_64 (S.Mem_Length))
                  then
                     Req := (Cmd => Show_Memory, Local => V.Locals.Selected,
                             Address => S.Locals (V.Locals.Selected).Address, others => <>);
                     V.Mem_Top := 0;
                  end if;
               end;
            when Graph_Pane =>
               declare
                  Handled : Boolean;
               begin
                  GV.Key (V.Graph, S, Body_Of (V.Panes (Graph_Pane)), Code, Handled);
               end;
               Add (Dirty, V.Panes (Graph_Pane));
            when Memory_Pane =>
               if Code = KEY_UP and then V.Mem_Top > 0 then
                  V.Mem_Top := V.Mem_Top - 1;
               elsif Code = KEY_DOWN and then V.Mem_Top + 1 < Mem_Rows then
                  V.Mem_Top := V.Mem_Top + 1;
               elsif Code = KEY_HOME then
                  V.Mem_Top := 0;
               end if;
               Add (Dirty, V.Panes (Memory_Pane));
         end case;
      end if;
   end Key;

   function Lower (C : Character) return Character is
     (if C in 'A' .. 'Z' then Character'Val (Character'Pos (C) - Character'Pos ('A') + Character'Pos ('a')) else C);

   --  A frame's own name: the last segment of its path ("run_jobs").
   function Last_Segment (Name : String) return String is
   begin
      for I in reverse Name'First .. Name'Last loop
         if Name (I) = '.' or else Name (I) = ':' then
            return Name (I + 1 .. Name'Last);
         end if;
      end loop;
      return Name;
   end Last_Segment;

   function Starts (Name : String; Ch : Character) return Boolean is
     (Name'Length > 0 and then Lower (Name (Name'First)) = Lower (Ch));

   procedure Text_Key
     (V : in out View_State; S : Snapshot; Ch : Character; Dirty : in out Rect; Req : out Request)
   is
      Skip : Natural;
   begin
      Req := (others => <>);
      if V.Picking then
         if V.Pick_Skip_Text then
            V.Pick_Skip_Text := False;
         elsif V.Pick_Length < V.Pick_Prefix'Last then
            V.Pick_Length := V.Pick_Length + 1;
            V.Pick_Prefix (V.Pick_Length) := Lower (Ch);
            for I in 1 .. S.Process_Total loop
               declare
                  Name : constant String := Image (S.Processes (I).Name);
                  Matches : Boolean := Name'Length >= V.Pick_Length;
               begin
                  for K in 1 .. V.Pick_Length loop
                     exit when not Matches;
                     Matches := Lower (Name (Name'First + K - 1)) = V.Pick_Prefix (K);
                  end loop;
                  if Matches then
                     V.Pick_Row := I;
                     exit;
                  end if;
               end;
            end loop;
         end if;
         Add (Dirty, V.Panes (Threads_Pane));
         return;
      end if;
      if Lower (Ch) in 'g' | 'm' | 'l' | 'f' | 'a' then
         return;
      end if;
      --  A new letter starts at the current row; the same letter again
      --  moves on to the next match.
      Skip := (if Lower (Ch) = Lower (V.Last_Typed) then 1 else 0);
      V.Last_Typed := Ch;
      case V.Focus is
         when Threads_Pane =>
            for K in 1 .. S.Thread_Total loop
               declare
                  T : constant Positive := (V.Thread_Row + K + Skip - 2) mod S.Thread_Total + 1;
               begin
                  if Starts (Image (S.Threads (T).Name), Ch) then
                     V.Thread_Row := T;
                     Req := (Cmd => Select_Thread, Thread => T, others => <>);
                     Add (Dirty, V.Panes (Threads_Pane));
                     exit;
                  end if;
               end;
            end loop;
         when Stack_Pane =>
            for K in 1 .. S.Frame_Total loop
               declare
                  F : constant Positive := (V.Frame_Row + K + Skip - 2) mod S.Frame_Total + 1;
               begin
                  if Starts (Last_Segment (Image (S.Frames (F).Func)), Ch) then
                     V.Frame_Row := F;
                     Req := (Cmd => Select_Frame, Frame => F, others => <>);
                     Add (Dirty, V.Panes (Stack_Pane));
                     exit;
                  end if;
               end;
            end loop;
         when Locals_Pane =>
            declare
               K0 : constant Natural := Visible_Position (V, V.Locals.Selected);
               Total : constant Natural := V.Locals.Visible_Total;
            begin
               for K in 1 .. Total loop
                  declare
                     I : constant Local_Index := V.Locals.Visible ((K0 + K + Skip - 2) mod Total + 1);
                  begin
                     if Starts (Last_Segment (Image (S.Locals (I).Name)), Ch) then
                        V.Locals.Selected := I;
                        Ensure_Local_Visible (V);
                        Add (Dirty, V.Panes (Locals_Pane));
                        Add (Dirty, V.Panes (Memory_Pane));
                        exit;
                     end if;
                  end;
               end loop;
            end;
         when others => null;
      end case;
   end Text_Key;

   function Pane_At (V : View_State; X, Y : Natural; Found : out Boolean) return Pane is
   begin
      for Q in Pane loop
         if Point_In_Rect (X, Y, V.Panes (Q)) then
            Found := True;
            return Q;
         end if;
      end loop;
      Found := False;
      return V.Focus;
   end Pane_At;

   procedure Pointer_Down (V : in out View_State; S : Snapshot; X, Y : Natural; Dirty : in out Rect) is
      pragma Unreferenced (S);
      Found : Boolean;
      Q : constant Pane := Pane_At (V, X, Y, Found);
   begin
      if Found then
         Set_Focus (V, Q, Dirty);
         if Q = Graph_Pane then
            GV.Pointer_Down (V.Graph, X, Y);
         end if;
      end if;
   end Pointer_Down;


   procedure Pointer_Move (V : in out View_State; X, Y : Natural; Dirty : in out Rect) is
      Changed : Boolean;
   begin
      GV.Pointer_Move (V.Graph, X, Y, Changed);
      if Changed then
         Add (Dirty, V.Panes (Graph_Pane));
      end if;
   end Pointer_Move;

   procedure Pointer_Up
     (V : in out View_State; S : in out Snapshot; X, Y : Natural; Dirty : in out Rect; Req : out Request)
   is
      Found : Boolean;
      Q : constant Pane := Pane_At (V, X, Y, Found);
      Was_Drag : constant Boolean := V.Graph.Dragging and then V.Graph.Drag_Moved;
   begin
      Req := (others => <>);
      if not Found or else Q /= Graph_Pane then
         V.Graph.Dragging := False;
      end if;
      if Point_In_Rect (X, Y, V.Regions (Toolbar_Region)) then
         --  Continue/Refresh, then Live (laid out as Draw_Toolbar does).
         declare
            First_W : constant Natural :=
              UI_Text_Width (if S.Can_Resume then "Continue   F5" else "Refresh   F5") + 2 * BUTTON_TEXT_INSET;
            Live_X : constant Natural := INSET + First_W + CuBit.UI_Metrics.Space_S;
            Live_W : constant Natural := UI_Text_Width ("Live   L") + 2 * BUTTON_TEXT_INSET;
         begin
            if X >= INSET and then X < INSET + First_W then
               Req.Cmd := (if S.Can_Resume then Continue else Refresh);
            elsif X >= Live_X and then X < Live_X + Live_W and then S.State /= Not_Attached then
               Req.Cmd := Toggle_Live;
            end if;
         end;
         return;
      end if;
      if not Found or else Was_Drag then
         return;
      end if;
      declare
         B : constant Rect := Body_Of (V.Panes (Q));
         Row : constant Integer := (if Y >= B.y then (Integer (Y) - B.y) / ROW_H + 1 else 0);
      begin
         case Q is
            when Threads_Pane =>
               if Row in 1 .. S.Thread_Total then
                  V.Thread_Row := Row;
                  Req := (Cmd => Select_Thread, Thread => Row, others => <>);
                  Add (Dirty, V.Panes (Q));
               end if;
            when Stack_Pane =>
               if (Row + 1) / 2 in 1 .. S.Frame_Total then
                  V.Frame_Row := (Row + 1) / 2;
                  Req := (Cmd => Select_Frame, Frame => V.Frame_Row, others => <>);
                  Add (Dirty, V.Panes (Q));
               end if;
            when Locals_Pane =>
               declare
                  Wanted : CuBit.UI.Tree_Tables.Wish;
                  Item : Local_Index;
                  use type CuBit.UI.Tree_Tables.Wish;
               begin
                  Debugger_Locals.Table.Click (V.Locals, S, B, X, Y, Wanted, Item);
                  if Wanted = CuBit.UI.Tree_Tables.Toggle then
                     Toggle_Local (V, S, Item, not (S.Locals (Item).Expanded and then S.Locals (Item).Loaded), Req);
                  end if;
                  Add (Dirty, V.Panes (Locals_Pane));
                  Add (Dirty, V.Panes (Memory_Pane));
               end;
            when Graph_Pane =>
               declare
                  Clicked : GV.Hit;
                  Changed : Boolean;
               begin
                  GV.Pointer_Up (V.Graph, S, B, X, Y, Clicked, Changed);
                  if Clicked.Kind = GV.Hit_Port then
                     Req := (Cmd => Follow_Pointer, Node => Clicked.Node, Field => Clicked.Row, others => <>);
                  end if;
                  Add (Dirty, V.Panes (Graph_Pane));
               end;
            when others =>
               null;
         end case;
      end;
   end Pointer_Up;

   procedure Wheel
     (V : in out View_State; S : Snapshot; X, Y : Natural; Steps : Integer; Dirty : in out Rect)
   is
      Found : Boolean;
      Q : constant Pane := Pane_At (V, X, Y, Found);
      LINES_PER_STEP : constant := 3;
   begin
      if not Found then
         return;
      end if;
      case Q is
         when Graph_Pane =>
            declare
               Changed : Boolean;
            begin
               GV.Wheel (V.Graph, Body_Of (V.Panes (Graph_Pane)), X, Y, Steps, Changed);
            end;
         when Source_Pane =>
            CuBit.UI.Code_Views.Scroll (V.Code, (if Steps > 0 then -LINES_PER_STEP else LINES_PER_STEP), S.Line_Total);
         when Locals_Pane =>
            Debugger_Locals.Table.Wheel (V.Locals, Steps);
         when Memory_Pane =>
            if Steps > 0 then
               V.Mem_Top := (if V.Mem_Top > 0 then V.Mem_Top - 1 else 0);
            elsif (V.Mem_Top + 1) * Bytes_Per_Row (V) < S.Mem_Length then
               V.Mem_Top := V.Mem_Top + 1;
            end if;
         when others => null;
      end case;
      Add (Dirty, V.Panes (Q));
   end Wheel;

   procedure Marks_Pending (S : Snapshot; Now : Microseconds; Area : out Rect;
                            Until_Us : out Microseconds; V : View_State) is
      procedure Consider (Stamp : Microseconds; R : Rect) is
      begin
         if Recently (Stamp, Now) then
            Add (Area, R);
            if Until_Us = 0 or else Stamp + CHANGE_MARK_US < Until_Us then
               Until_Us := Stamp + CHANGE_MARK_US;
            end if;
         end if;
      end Consider;
   begin
      Area := (others => 0);
      Until_Us := 0;
      for I in 1 .. S.Local_Total loop
         Consider (S.Locals (I).Changed_At, V.Panes (Locals_Pane));
         Consider (S.Locals (I).Changed_At, V.Panes (Source_Pane));
      end loop;
      for N in 1 .. S.Node_Total loop
         Consider (S.Nodes (N).Appeared_At, V.Panes (Graph_Pane));
         for F in 1 .. S.Nodes (N).Field_Total loop
            Consider (S.Nodes (N).Fields (F).Changed_At, V.Panes (Graph_Pane));
         end loop;
      end loop;
      Consider (S.Mem_Changed_At, V.Panes (Memory_Pane));
   end Marks_Pending;
   procedure Start_Picking (V : in out View_State) is
   begin
      V.Picking := True;
      V.Pick_Row := 1;
      V.Pick_Length := 0;
      V.Pick_Skip_Text := True;
   end Start_Picking;

end Debugger_View;
