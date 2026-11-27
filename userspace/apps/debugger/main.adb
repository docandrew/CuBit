------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Debugger (DBG-001): the native app. Attaches to a debug target through
--  the engine and shows its threads, stack, source, locals, data structures
--  and memory. Preview: cooperative attach, Observe only.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with CuBit.Console_Route;
with CuBit.Logging;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Monotonic;
with CuBit.Process_Calls;
with CuBit.UI;
with CuBit.UI.App;
with CuBit.UI.Controls;
with CuBit.UI.Input;
with CuBit.UI.State;
with Debugger_Model; use Debugger_Model;
with Debugger_View;
with Debugger_Source;
with Debugger_State; use Debugger_State;
with Startup;

procedure Main is
   package V renames Debugger_View;
   use type V.Command;
   use type Debugger_Source.Attach_Result;

   --  Ask for the whole work area: Desktop's placement cuts the request to
   --  fit (Desktop_Window_Placement). A smaller size if it is refused.
   INITIAL_WIDTH  : constant := 7_680;
   INITIAL_HEIGHT : constant := 4_320;
   FALLBACK_WIDTH  : constant := 1_480;
   FALLBACK_HEIGHT : constant := 900;
   MINIMUM_WIDTH  : constant := 1_100;
   MINIMUM_HEIGHT : constant := 680;
   --  A frame-time line every this many frames.
   FRAMES_PER_REPORT : constant := 120;
   US_PER_MS : constant := 1_000;
   --  A text event's character (CuBit.UI.Input: the code point's low bits).
   TEXT_CODE_MASK : constant := 16#FF#;

   --  Live reads: one every LIVE_PERIOD_MS while the target runs.
   LIVE_PERIOD_MS : constant := 100;
   Next_Live_Ms : Unsigned_64 := 0;

   win : CuBit.UI.App.Window;
   ui : CuBit.UI.State.UI_State;
   controls : CuBit.UI.Controls.Control_Map;
   Attach_Outcome : Debugger_Source.Attach_Result := Debugger_Source.Engine_Failed;
   Reported_First : Boolean := False;
   --  Frames that repaint part of the window (damage only).
   PARTIALS_PER_REPORT : constant := 10;
   Partial_Count, Partial_Total, Partial_Worst : Unsigned_64 := 0;

   function Now return Microseconds is (CuBit.Monotonic.Microseconds);

   function Dec (N : Unsigned_64) return String is
      S : constant String := Unsigned_64'Image (N);
   begin
      return S (S'First + 1 .. S'Last);
   end Dec;

   --  What the window shows, as text in the log (the headless check reads it).
   procedure Report_Shown is
   begin
      debugPrint ("debugger: shows " & Image (Current.Target_Name) & " pid " & Dec (Current.Target_Pid)
                  & " threads " & Dec (Unsigned_64 (Current.Thread_Total))
                  & " frames " & Dec (Unsigned_64 (Current.Frame_Total)) & ASCII.LF);
      if Current.Frame_Total >= Current.Frame_Sel and then Current.Frame_Total > 0 then
         debugPrint ("debugger: frame " & Image (Current.Frames (Current.Frame_Sel).Func) & " at "
                     & Image (Current.Frames (Current.Frame_Sel).File) & ":"
                     & Dec (Unsigned_64 (Current.Frames (Current.Frame_Sel).Line)) & ASCII.LF);
      end if;
      for F in 1 .. Current.Frame_Total loop
         debugPrint ("debugger: stack #" & Dec (Unsigned_64 (F - 1)) & " " & Image (Current.Frames (F).Func)
                     & (if F = Current.Frame_Sel then " (selected)" else "") & ASCII.LF);
      end loop;
      debugPrint ("debugger: current line: " & V.Current_Line_Text (Current) & ASCII.LF);
      for I in 1 .. Current.Local_Total loop
         if Current.Locals (I).Depth = 0 then
            debugPrint ("debugger: local " & Image (Current.Locals (I).Name) & " : "
                        & Image (Current.Locals (I).Type_Name) & " = " & Image (Current.Locals (I).Value)
                        & ASCII.LF);
         end if;
      end loop;
      for N in 1 .. Current.Node_Total loop
         debugPrint ("debugger: node " & Image (Current.Nodes (N).Title) & " fields "
                     & Dec (Unsigned_64 (Current.Nodes (N).Field_Total)) & ASCII.LF);
      end loop;
   end Report_Shown;

   procedure Reread (Continue : Boolean) is
   begin
      Before := Current;
      if Continue then
         Debugger_Source.Continue (Before, Current);
      else
         Debugger_Source.Read (Before, Current);
      end if;
      Mark_Changes (Before, Current, Now);
      V.Absorb (View, Current);
   end Reread;

   procedure Execute (Req : V.Request; Dirty : in out CuBit.UI.Rect; Running : in out Boolean) is
   begin
      case Req.Cmd is
         when V.No_Command => return;
         when V.Quit => Running := False; return;
         when V.Pick_Process =>
            Debugger_Source.List_Processes (Current);
            V.Start_Picking (View);
         when V.Attach_Process =>
            declare
               Picked : constant Debugger_Model.Process_Index := Req.Process;
               Outcome : Debugger_Source.Attach_Result;
            begin
               debugPrint ("debugger: attach to process " & Image (Current.Processes (Picked).Name) & ASCII.LF);
               Debugger_Source.Attach_Process (Current, Picked, Outcome);
               debugPrint ("debugger: attach " & Debugger_Source.Attach_Result'Image (Outcome) & ASCII.LF);
               V.Absorb (View, Current);
               if Outcome = Debugger_Source.Attached then
                  Report_Shown;
               end if;
            end;
         when V.Continue =>
            if Current.Mode = Live then
               --  F5 while live parks the target first.
               Debugger_Source.Set_Live (Before, Current, On => False);
               Next_Live_Ms := 0;
            end if;
            Reread (Continue => True);
         when V.Refresh => Reread (Continue => False);
         when V.Load_Children =>
            Debugger_Source.Load_Children (Current, Req.Local);
            V.Absorb (View, Current);
         when V.Select_Thread =>
            Before := Current;
            Debugger_Source.Select_Thread (Before, Current, Req.Thread);
            V.Absorb (View, Current);
         when V.Select_Frame =>
            Before := Current;
            Debugger_Source.Select_Frame (Before, Current, Req.Frame);
            V.Absorb (View, Current);
         when V.Show_In_Graph =>
            Debugger_Source.Graph_Local (Current, Req.Local);
            --  A new root: fit the graph to it again.
            View.Graph_Fitted := False;
            V.Absorb (View, Current);
         when V.Follow_Pointer =>
            Debugger_Source.Follow (Current, Req.Node, Req.Field);
            V.Absorb (View, Current);
         when V.Show_Memory =>
            Debugger_Source.Memory_At (Current, Req.Address);
         when V.Toggle_Live =>
            Before := Current;
            Debugger_Source.Set_Live (Before, Current, On => Current.Mode /= Live);
            Mark_Changes (Before, Current, Now);
            V.Absorb (View, Current);
            Next_Live_Ms := (if Current.Mode = Live then CuBit.Monotonic.Milliseconds + LIVE_PERIOD_MS else 0);
            debugPrint ("debugger: live " & (if Current.Mode = Live then "on" else "off") & ASCII.LF);
      end case;
      Dirty := V.Everything (View);
   end Execute;

   procedure Render (win : in out CuBit.UI.App.Window; damage : CuBit.UI.Rect) is
      C : constant CuBit.UI.Canvas := CuBit.UI.App.Canvas (win, damage);
      Full : constant CuBit.UI.Rect := CuBit.UI.App.Full_Rect (win);
      Started : constant Microseconds := Now;
      Took : Microseconds;
      Is_Full : constant Boolean := damage.w >= Full.w and then damage.h >= Full.h;
   begin
      if View.Width /= Full.w or else View.Height /= Full.h then
         V.Arrange (View, Full.w, Full.h);
         V.Absorb (View, Current);
      end if;
      V.Render (C, damage, CuBit.UI.Current_Theme, Current, View, Started);
      Took := Now - Started;
      V.Record_Frame (View, Took, Is_Full);
      if Is_Full then
         debugPrint ("debugger: " & (if Reported_First then "" else "first ") & "full frame "
                     & Dec (Unsigned_64 (Full.w)) & "x" & Dec (Unsigned_64 (Full.h))
                     & " us=" & Dec (Took) & ASCII.LF);
         Reported_First := True;
      else
         Partial_Count := Partial_Count + 1;
         Partial_Total := Partial_Total + Took;
         Partial_Worst := Unsigned_64'Max (Partial_Worst, Took);
         if Partial_Count mod PARTIALS_PER_REPORT = 0 then
            debugPrint ("debugger: partial frames " & Dec (Partial_Count) & " avg_us=" & Dec (Partial_Total / Partial_Count)
                        & " worst_us=" & Dec (Partial_Worst) & " last " & Dec (Unsigned_64 (damage.w)) & "x"
                        & Dec (Unsigned_64 (damage.h)) & " us=" & Dec (Took) & ASCII.LF);
         end if;
      end if;
      if View.Stats.Count mod FRAMES_PER_REPORT = 0 then
         debugPrint ("debugger: frames " & Dec (View.Stats.Count) & " avg_us="
                     & Dec (View.Stats.Total_Us / View.Stats.Count) & " worst_us=" & Dec (View.Stats.Worst_Us)
                     & " last_us=" & Dec (Took) & ASCII.LF);
      end if;
   end Render;

   procedure Handle_Event
     (win : in out CuBit.UI.App.Window; event : CuBit.UI.App.Input_Event;
      dirty : in out CuBit.UI.Rect; running : in out Boolean)
   is
      package I renames CuBit.UI.Input;
      Req : V.Request;
   begin
      if event.kind = I.INPUT_KEY_DOWN then
         V.Key (View, Current, event.payload0, I.Key_Modifiers (event), dirty, Req);
         if Req.Cmd in V.Continue | V.Refresh then
            debugPrint ("debugger: " & (if Req.Cmd = V.Continue then "continue" else "refresh") & ASCII.LF);
         end if;
         Execute (Req, dirty, running);
         if Req.Cmd in V.Continue | V.Refresh | V.Select_Thread | V.Select_Frame | V.Show_In_Graph then
            Report_Shown;
         end if;
      elsif event.kind = I.INPUT_TEXT then
         if (event.payload0 and TEXT_CODE_MASK) in Character'Pos (' ') .. Character'Pos ('~') then
            V.Text_Key (View, Current, Character'Val (event.payload0 and TEXT_CODE_MASK), dirty, Req);
            if View.Picking and then View.Pick_Row <= Current.Process_Total then
               debugPrint ("debugger: pick " & Image (Current.Processes (View.Pick_Row).Name) & ASCII.LF);
            end if;
            Execute (Req, dirty, running);
            if Req.Cmd in V.Select_Thread | V.Select_Frame then
               Report_Shown;
            end if;
         end if;
      elsif event.kind = I.INPUT_POINTER_DOWN then
         V.Pointer_Down (View, Current, I.Pointer_X (event), I.Pointer_Y (event), dirty);
      elsif event.kind = I.INPUT_POINTER_MOVE then
         V.Pointer_Move (View, I.Pointer_X (event), I.Pointer_Y (event), dirty);
      elsif event.kind = I.INPUT_POINTER_UP then
         V.Pointer_Up (View, Current, I.Pointer_X (event), I.Pointer_Y (event), dirty, Req);
         Execute (Req, dirty, running);
      elsif event.kind = I.INPUT_POINTER_WHEEL then
         V.Wheel (View, Current, I.Pointer_X (event), I.Pointer_Y (event), I.Pointer_Wheel_Delta (event), dirty);
      elsif event.kind = I.INPUT_CONFIGURE or else event.kind = I.INPUT_RESYNC then
         V.Arrange (View, CuBit.UI.App.Width (win), CuBit.UI.App.Height (win));
         V.Absorb (View, Current);
         dirty := CuBit.UI.App.Full_Rect (win);
      elsif event.kind = I.INPUT_CLOSE_REQUEST then
         running := False;
      end if;
   end Handle_Event;

   --  Changed marks go off on time: one repaint of their panes then.
   Mark_Area : CuBit.UI.Rect := (others => 0);
   function Next_Deadline return Unsigned_64 is
      Until_Us : Microseconds;
      Marks_Ms : Unsigned_64;
   begin
      V.Marks_Pending (Current, Now, Mark_Area, Until_Us, View);
      Marks_Ms := (if Until_Us = 0 then 0 else Until_Us / US_PER_MS + 1);
      if Next_Live_Ms /= 0 and then (Marks_Ms = 0 or else Next_Live_Ms < Marks_Ms) then
         return Next_Live_Ms;
      end if;
      return Marks_Ms;
   end Next_Deadline;

   procedure On_Deadline (win : in out CuBit.UI.App.Window; dirty : in out CuBit.UI.Rect; running : in out Boolean) is
      pragma Unreferenced (win, running);
   begin
      if Next_Live_Ms /= 0 and then CuBit.Monotonic.Milliseconds >= Next_Live_Ms then
         Reread (Continue => False);
         Next_Live_Ms := CuBit.Monotonic.Milliseconds + LIVE_PERIOD_MS;
         dirty := V.Everything (View);
      else
         dirty := (if CuBit.UI.Is_Empty (Mark_Area) then V.Everything (View) else Mark_Area);
      end if;
   end On_Deadline;

   procedure Run_UI is new CuBit.UI.App.Run
     (ui => ui, controls => controls, Render => Render, Handle_Event => Handle_Event,
      Next_Deadline => Next_Deadline, On_Deadline => On_Deadline);

   Announced : Boolean;
   Opened : CuBit.UI.App.Open_Result;
   use type CuBit.UI.App.Open_Result;
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   CuBit.Logging.Announce ("debugger: started (cooperative attach preview)", Announced, Startup.Logstore);
   Debugger_Source.Attach (Current, Attach_Outcome);
   debugPrint ("debugger: attach " & Debugger_Source.Attach_Result'Image (Attach_Outcome) & ASCII.LF);
   if Attach_Outcome = Debugger_Source.Attached then
      Report_Shown;
   elsif Attach_Outcome = Debugger_Source.No_Target then
      --  No cooperative target: start at Attach to process...
      Debugger_Source.List_Processes (Current);
      V.Start_Picking (View);
      View.Pick_Skip_Text := False;
   end if;
   CuBit.UI.App.Open
     (win, INITIAL_WIDTH, INITIAL_HEIGHT,
      CuBit.UI.App.WINDOW_FLAG_DECORATED or CuBit.UI.App.WINDOW_FLAG_RESIZABLE or
      CuBit.UI.App.WINDOW_FLAG_MINIMIZABLE or CuBit.UI.App.WINDOW_FLAG_MAXIMIZABLE or
      CuBit.UI.App.WINDOW_FLAG_CLOSEABLE,
      Opened, title => "Debugger", protected_frames => True,
      minimum_width => MINIMUM_WIDTH, minimum_height => MINIMUM_HEIGHT);
   if Opened = CuBit.UI.App.Invalid_Request then
      CuBit.UI.App.Open
        (win, FALLBACK_WIDTH, FALLBACK_HEIGHT,
         CuBit.UI.App.WINDOW_FLAG_DECORATED or CuBit.UI.App.WINDOW_FLAG_RESIZABLE or
         CuBit.UI.App.WINDOW_FLAG_MINIMIZABLE or CuBit.UI.App.WINDOW_FLAG_MAXIMIZABLE or
         CuBit.UI.App.WINDOW_FLAG_CLOSEABLE,
         Opened, title => "Debugger", protected_frames => True,
         minimum_width => MINIMUM_WIDTH, minimum_height => MINIMUM_HEIGHT);
   end if;
   if Opened /= CuBit.UI.App.Opened then
      Debugger_Source.Detach;
      CuBit.Process_Calls.Exit_Process (CuBit.Process_Calls.Failure);
   end if;
   V.Arrange (View, CuBit.UI.App.Width (win), CuBit.UI.App.Height (win));
   V.Absorb (View, Current);
   debugPrint ("debugger: window ready" & ASCII.LF);
   Run_UI (win);
   Debugger_Source.Detach;
   CuBit.UI.App.Close (win);
   CuBit.Process_Calls.Exit_Process (CuBit.Process_Calls.Success);
end Main;
