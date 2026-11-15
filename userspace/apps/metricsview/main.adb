------------------------------------------------------------------------------
--  CuBit Metrics (MET-002's app): the platform glue only. The window
--  (Dash_View), the dashboard type (Dash_Model) and the tracks are what the
--  hosted harness runs (tests/metricsview); here they read metrics.svc
--  through Dash_Query_Feeds, are refreshed once per frame by the frame
--  clock (never while the last frame is still being shown), and the app
--  publishes its own latency through its com.cubit-os.stdmetric outlet.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Desktop_Protocol;
with CuBit.Logging;
with CuBit.Metrics;
with CuBit.Monotonic;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.App;
with CuBit.UI.Clipboard;
with CuBit.UI.Controls;
with CuBit.UI.Input;
with CuBit.UI.State;
with Clipboard_Chords;
with Charts_Format;
with Dash_Feeds;
with Dash_Files;
with Dash_Model;
with Dash_Query_Feeds;
with Dash_Runner;
with Dash_Sync;
with Dash_View;
with Startup;

procedure Main is
   use type Clipboard_Chords.Command;
   use type Dash_Feeds.Status;
   use type Dash_View.Key;

   WINDOW_WIDTH : constant := 1180;
   WINDOW_HEIGHT : constant := 720;
   MINIMUM_WIDTH : constant := 640;
   MINIMUM_HEIGHT : constant := 420;
   --  The frame clock: the display's refresh period (60 Hz) until Desktop
   --  publishes its own (a refresh is one store read per visible series,
   --  DESIGN-metrics-push.md); a frame still on screen defers the refresh.
   FRAME_MS : constant := 16;
   RETRY_MS : constant := 2;
   --  Serial markers (tests/headless) this often.
   REPORT_MS : constant := 2_000;
   --  Set-1 scan codes.
   KEY_ESCAPE : constant Unsigned_64 := CuBit.UI.App.KEY_ESC;
   KEY_DIGIT_1 : constant Unsigned_64 := 16#02#;
   KEY_DIGIT_6 : constant Unsigned_64 := 16#07#;
   KEY_MINUS : constant Unsigned_64 := 16#0C#;
   KEY_EQUALS : constant Unsigned_64 := 16#0D#;
   KEY_L : constant Unsigned_64 := 16#26#;
   KEY_HOME : constant Unsigned_64 := 16#47#;
   KEY_LEFT : constant Unsigned_64 := 16#4B#;
   KEY_RIGHT : constant Unsigned_64 := 16#4D#;
   KEY_END : constant Unsigned_64 := 16#4F#;
   --  A key event's modifiers: payload1's low word (KEYMOD_*).
   --  A saved dashboard's largest file, and how long launch waits for it.
   SAVED_BYTES : constant := 262_144;
   LOAD_DEADLINE_MS : constant := 500;
   KEY_SLASH : constant Unsigned_64 := 16#35#;
   KEY_F1 : constant Unsigned_64 := 16#3B#;
   KEY_MODIFIER_MASK : constant Unsigned_64 := 16#FFFF_FFFF#;
   KEY_UP : constant Unsigned_64 := 16#48#;
   KEY_DOWN : constant Unsigned_64 := 16#50#;
   KEY_TAB : constant Unsigned_64 := 16#0F#;
   KEY_E : constant Unsigned_64 := 16#12#;
   KEY_C : constant Unsigned_64 := 16#2E#;
   KEY_Y : constant Unsigned_64 := 16#15#;
   KEY_Z : constant Unsigned_64 := 16#2C#;
   KEY_INSERT : constant Unsigned_64 := 16#52#;
   KEY_DELETE : constant Unsigned_64 := 16#53#;

   Input_To_Render : constant CuBit.Metrics.Series :=
     CuBit.Metrics.Series_Named ("metricsview.input_to_render_us", CuBit.Metrics.Distribution,
                                 CuBit.Metrics.Microseconds);
   Render_Cost : constant CuBit.Metrics.Series :=
     CuBit.Metrics.Series_Named ("metricsview.render_us", CuBit.Metrics.Distribution, CuBit.Metrics.Microseconds);
   Update_Cost : constant CuBit.Metrics.Series :=
     CuBit.Metrics.Series_Named ("metricsview.update_us", CuBit.Metrics.Distribution, CuBit.Metrics.Microseconds);
   --  The app's own work for an input (its handling and the render that
   --  shows it), without the wait for the last frame to be retired.
   Input_Cost : constant CuBit.Metrics.Series :=
     CuBit.Metrics.Series_Named ("metricsview.input_cost_us", CuBit.Metrics.Distribution, CuBit.Metrics.Microseconds);
   --  The UI thread's waits for the data thread's lock during an input.
   Lock_Wait : constant CuBit.Metrics.Series :=
     CuBit.Metrics.Series_Named ("metricsview.lock_wait_us", CuBit.Metrics.Distribution, CuBit.Metrics.Microseconds);
   Handle_Cost : constant CuBit.Metrics.Series :=
     CuBit.Metrics.Series_Named ("metricsview.handle_us", CuBit.Metrics.Distribution, CuBit.Metrics.Microseconds);
   --  Per kind of input (edit, zoom, hover, pan, other): its cost and its
   --  input-to-rendered, so each can be held to the budget on its own.
   type Input_Kind is (Edit_Input, Zoom_Input, Hover_Input, Pan_Input, Other_Input);
   type Kind_Series is array (Input_Kind) of CuBit.Metrics.Series;
   function Named (Name : String) return CuBit.Metrics.Series is
     (CuBit.Metrics.Series_Named (Name, CuBit.Metrics.Distribution, CuBit.Metrics.Microseconds));
   Cost_By_Kind : constant Kind_Series :=
     [Named ("metricsview.input_cost_edit_us"), Named ("metricsview.input_cost_zoom_us"),
      Named ("metricsview.input_cost_hover_us"), Named ("metricsview.input_cost_pan_us"),
      Named ("metricsview.input_cost_other_us")];
   Render_By_Kind : constant Kind_Series :=
     [Named ("metricsview.input_to_render_edit_us"), Named ("metricsview.input_to_render_zoom_us"),
      Named ("metricsview.input_to_render_hover_us"), Named ("metricsview.input_to_render_pan_us"),
      Named ("metricsview.input_to_render_other_us")];
   --  The copy of the toolkit's one damage rectangle out of the retained
   --  frame, and its pixels against the pixels that changed.
   Copy_Cost : constant CuBit.Metrics.Series := Named ("metricsview.copy_us");
   --  Drawing what changed into the retained frame, per frame.
   Paint_Cost : constant CuBit.Metrics.Series := Named ("metricsview.paint_us");
   Copy_Union : constant CuBit.Metrics.Series :=
     CuBit.Metrics.Series_Named ("metricsview.copy_union_px", CuBit.Metrics.Distribution, CuBit.Metrics.Count);
   Copy_Exact : constant CuBit.Metrics.Series :=
     CuBit.Metrics.Series_Named ("metricsview.copy_exact_px", CuBit.Metrics.Distribution, CuBit.Metrics.Count);
   Frames : constant CuBit.Metrics.Series :=
     CuBit.Metrics.Series_Named ("metricsview.frames", CuBit.Metrics.Counter, CuBit.Metrics.Count);

   Win : CuBit.UI.App.Window;
   UI : CuBit.UI.State.UI_State;
   Controls : CuBit.UI.Controls.Control_Map;
   Feed : Dash_Query_Feeds.Query_Feed renames Dash_Runner.Feed;
   View : Dash_View.View renames Dash_Runner.View;
   --  The data thread runs; else the UI thread steps the data itself.
   Threaded : Boolean := False;
   Board : Dash_Model.Dashboard;
   Saved_Source : String (1 .. SAVED_BYTES);
   Loaded : Dash_Model.Load_Result;
   Opened : Boolean;
   Due_Ms, Report_Due_Ms : Unsigned_64 := 0;
   --  The first input not yet shown, and when it was handled.
   Input_Pending : Boolean := False;
   Input_Started_Us : Unsigned_64 := 0;
   --  Handling time of the inputs waiting for their render.
   Input_Work_Us : Unsigned_64 := 0;
   Pending_Kind : Input_Kind := Other_Input;
   Last_Cost_Us, Worst_Cost_Us : Unsigned_64 := 0;
   --  Measured here, reported at the markers.
   Last_Input_Us, Worst_Input_Us, Inputs_Measured : Unsigned_64 := 0;
   Last_Update_Us, Worst_Update_Us, Updates : Unsigned_64 := 0;
   First_Frame : Boolean := True;

   function Now_Us return Unsigned_64 is (CuBit.Monotonic.Microseconds);
   function Now_Ms return Unsigned_64 is (syscall (SYSCALL_GETTIME));
   function Decimal (V : Unsigned_64) return String renames Charts_Format.Decimal;

   procedure Render (Win : in out CuBit.UI.App.Window; Damage : Rect) is
      Started : constant Unsigned_64 := Now_Us;
   begin
      Dash_View.Render (View, CuBit.UI.App.Canvas (Win, Damage), Damage);
      Dash_Runner.Observe (Render_Cost, Now_Us - Started);
      declare
         Paint_Us, Copy_Us, Union_Pixels, Exact_Pixels : Unsigned_64;
      begin
         Dash_View.Take_Copy_Stats (View, Paint_Us, Copy_Us, Union_Pixels, Exact_Pixels);
         Dash_Runner.Observe (Paint_Cost, Paint_Us);
         Dash_Runner.Observe (Copy_Cost, Copy_Us);
         Dash_Runner.Observe (Copy_Union, Union_Pixels);
         Dash_Runner.Observe (Copy_Exact, Exact_Pixels);
      end;
      Dash_Runner.Observe (Frames, 1);
      if Input_Pending then
         Input_Pending := False;
         Last_Cost_Us := Input_Work_Us + (Now_Us - Started);
         Worst_Cost_Us := Unsigned_64'Max (Worst_Cost_Us, Last_Cost_Us);
         Dash_Runner.Observe (Input_Cost, Last_Cost_Us);
         Dash_Runner.Observe (Cost_By_Kind (Pending_Kind), Last_Cost_Us);
         Dash_Runner.Observe (Handle_Cost, Input_Work_Us);
         declare
            Waited : Unsigned_64;
         begin
            Dash_View.Take_Lock_Wait (View, Waited);
            Dash_Runner.Observe (Lock_Wait, Waited);
         end;
         Input_Work_Us := 0;
         Last_Input_Us := Now_Us - Input_Started_Us;
         Worst_Input_Us := Unsigned_64'Max (Worst_Input_Us, Last_Input_Us);
         Inputs_Measured := Inputs_Measured + 1;
         Dash_Runner.Observe (Input_To_Render, Last_Input_Us);
         Dash_Runner.Observe (Render_By_Kind (Pending_Kind), Last_Input_Us);
      end if;
      if First_Frame then
         First_Frame := False;
         debugPrint ("metricsview: first frame presented" & ASCII.LF);
      end if;
   end Render;

   procedure Set_Bounds (Win : CuBit.UI.App.Window);
   procedure Set_Bounds (Win : CuBit.UI.App.Window) is
      C : constant Canvas := CuBit.UI.App.Canvas (Win);
   begin
      Dash_View.Set_Bounds (View, CuBit.UI.App.Full_Rect (Win), C.densityNumerator, C.densityDenominator);
   end Set_Bounds;

   --  Ctrl chords (no other modifier matters to them).
   function Control_Key_Of (Code : Unsigned_64) return Dash_View.Key is
     (if Code = KEY_Z then Dash_View.Undo elsif Code = KEY_Y then Dash_View.Redo else Dash_View.No_Key);

   function Key_Of (Code : Unsigned_64) return Dash_View.Key is
     (if Code = KEY_LEFT then Dash_View.Left elsif Code = KEY_RIGHT then Dash_View.Right
      elsif Code = KEY_UP then Dash_View.Up elsif Code = KEY_DOWN then Dash_View.Down
      elsif Code = KEY_TAB then Dash_View.Next_Panel elsif Code = KEY_E then Dash_View.Edit
      elsif Code = KEY_C then Dash_View.Show_Source elsif Code = KEY_INSERT then Dash_View.Duplicate
      elsif Code = KEY_DELETE then Dash_View.Remove
      elsif Code in KEY_SLASH | KEY_F1 then Dash_View.Show_Keys
      elsif Code = KEY_HOME then Dash_View.Home elsif Code = KEY_END then Dash_View.End_Key
      elsif Code = KEY_ESCAPE then Dash_View.Escape elsif Code = KEY_EQUALS then Dash_View.Zoom_In
      elsif Code = KEY_MINUS then Dash_View.Zoom_Out elsif Code = KEY_L then Dash_View.Live
      elsif Code in KEY_DIGIT_1 .. KEY_DIGIT_6
      then Dash_View.Key'Val (Dash_View.Key'Pos (Dash_View.Range_1) + Natural (Code - KEY_DIGIT_1))
      else Dash_View.No_Key);

   procedure Handle_Event
     (Win : in out CuBit.UI.App.Window; Event : CuBit.UI.App.Input_Event; Dirty : in out Rect;
      Running : in out Boolean)
   is
      package I renames CuBit.UI.Input;
      Started : constant Unsigned_64 := Now_Us;
      Before : constant Rect := Dirty;
      Shift : constant Boolean :=
        (I.Pointer_Modifiers (Event) and CuBit.UI.App.KEYMOD_SHIFT) /= 0;
      Was_Editing : constant Boolean := Dash_View.Editing (View);
      Kind : Input_Kind := Other_Input;
   begin
      if Event.kind = I.INPUT_CLOSE_REQUEST then
         Running := False;
         return;
      elsif Event.kind in I.INPUT_CONFIGURE | I.INPUT_RESYNC then
         --  A new size, density or colour scheme: everything again.
         Set_Bounds (Win);
         Dirty := CuBit.UI.App.Full_Rect (Win);
      elsif Event.kind = I.INPUT_KEY_DOWN then
         if CuBit.UI.Clipboard.Command_Of (Event.payload0, Event.payload1) = Clipboard_Chords.Copy_Command then
            declare
               --  Editing or showing the CCL: the dashboard; else the
               --  drill-down's expression.
               Expression : constant String :=
                 (if Dash_View.Editing (View) or else Dash_View.Source_Showing (View)
                  then Dash_View.Dashboard_Source (View) else Dash_View.Drill_Down_Expression (View));
               Result : CuBit.UI.Clipboard.Outcome;
            begin
               if Expression'Length > 0 then
                  CuBit.UI.Clipboard.Copy_Text (CuBit.UI.App.Surface_ID (Win), Expression,
                                                CuBit.UI.Clipboard.Ordinary, Result);
                  debugPrint ("metricsview: copied " & Expression & ASCII.LF);
               end if;
            end;
         else
            declare
               Modifiers : constant Unsigned_64 := Event.payload1 and KEY_MODIFIER_MASK;
               Control : constant Boolean := (Modifiers and CuBit.UI.App.KEYMOD_CTRL) /= 0;
               Pressed : constant Dash_View.Key :=
                 (if Control then Control_Key_Of (Event.payload0) else Key_Of (Event.payload0));
            begin
               --  The CCL and keys overlays are not edits (they draw a page
               --  of text once): Other.
               Kind :=
                 (if Pressed in Dash_View.Show_Source | Dash_View.Show_Keys then Other_Input
                  elsif Was_Editing or else Pressed in Dash_View.Edit .. Dash_View.Redo then Edit_Input
                  elsif Pressed in Dash_View.Zoom_In | Dash_View.Zoom_Out | Dash_View.Range_1 .. Dash_View.Range_6
                  then Zoom_Input
                  elsif Pressed in Dash_View.Left | Dash_View.Right | Dash_View.Home | Dash_View.End_Key
                    | Dash_View.Live
                  then Pan_Input
                  else Other_Input);
               if Pressed /= Dash_View.No_Key then
                  Dash_View.Handle
                    (View, (Kind => Dash_View.Key_Press, Pressed => Pressed,
                            Shift => (Modifiers and CuBit.UI.App.KEYMOD_SHIFT) /= 0, others => <>), Dirty);
               end if;
            end;
         end if;
      elsif Event.kind in I.INPUT_POINTER_MOVE | I.INPUT_POINTER_DOWN | I.INPUT_POINTER_UP then
         Kind := (if Was_Editing then Edit_Input
                  elsif Event.kind = I.INPUT_POINTER_MOVE and then I.Pointer_Buttons (Event) = 0 then Hover_Input
                  elsif Event.kind = I.INPUT_POINTER_MOVE then Pan_Input
                  else Other_Input);
         Dash_View.Handle
           (View, (Kind => (if Event.kind = I.INPUT_POINTER_DOWN then Dash_View.Pointer_Press
                            elsif Event.kind = I.INPUT_POINTER_UP then Dash_View.Pointer_Release
                            else Dash_View.Pointer_Move),
                   X => I.Pointer_X (Event), Y => I.Pointer_Y (Event), Shift => Shift, others => <>), Dirty);
      elsif Event.kind = I.INPUT_POINTER_WHEEL then
         Kind := Zoom_Input;
         Dash_View.Handle (View, (Kind => Dash_View.Wheel, X => I.Pointer_X (Event), Y => I.Pointer_Y (Event),
                                  Steps => I.Pointer_Wheel_Delta (Event), others => <>), Dirty);
      end if;
      --  A new viewport: the data thread fetches for it now, not at its poll.
      if Threaded and then Event.kind in I.INPUT_KEY_DOWN | I.INPUT_POINTER_WHEEL | I.INPUT_POINTER_UP
        | I.INPUT_POINTER_MOVE and then (Event.kind /= I.INPUT_POINTER_MOVE or else I.Pointer_Buttons (Event) /= 0)
      then
         Dash_Runner.Wake;
      end if;
      --  Input that changed the window: measured to the end of its render.
      if not Input_Pending and then (Dirty.w /= Before.w or else Dirty.h /= Before.h or else
                                     Dirty.x /= Before.x or else Dirty.y /= Before.y)
      then
         Input_Pending := True;
         Input_Started_Us := Started;
         Pending_Kind := Kind;
      end if;
      if Input_Pending then
         Input_Work_Us := Input_Work_Us + (Now_Us - Started);
      end if;
   end Handle_Event;

   function Deadline return Unsigned_64 is (Due_Ms);


   procedure Report;
   --  Full redraws probed so far (a few, then stop: Probe_Full_Redraw).
   Probes_Done : Natural := 0;
   PROBES : constant := 5;

   procedure Report is
      Samples, P99, From_Us, To_Us : Unsigned_64;
      Found : Boolean;
      Resolved, Total_Tracks, Columns : Natural;
      Pushed : Unsigned_64;
      Push_On : Boolean;
   begin
      if Probes_Done < PROBES then
         declare
            Took, Again, Warm : Unsigned_64;
            B : constant Rect := Dash_View.Bounds (View);
         begin
            --  The first redraw after the app was idle (caches and predictors
            --  cold), then the best of two more at once (warm): the hosted
            --  bench prints both (PERF-011).
            Dash_View.Probe_Full_Redraw (View, Took);
            Dash_View.Probe_Full_Redraw (View, Warm);
            Dash_View.Probe_Full_Redraw (View, Again);
            Warm := Unsigned_64'Min (Warm, Again);
            Probes_Done := Probes_Done + 1;
            debugPrint ("metricsview: full redraw " & Decimal (Took) & " us at " & Decimal (Unsigned_64 (B.w)) & "x"
                        & Decimal (Unsigned_64 (B.h)) & " (cold), " & Decimal (Warm) & " us warm" & ASCII.LF);
         end;
      end if;
      Dash_Sync.Lock;
      Dash_View.Probe (View, "Input to ready", Samples, P99, From_Us, To_Us, Found);
      Resolved := Dash_View.Resolved_Tracks (View);
      Total_Tracks := Dash_View.Track_Total (View);
      Columns := Dash_View.Columns_With_Data (View);
      Pushed := Dash_View.Pushed_Samples (View);
      Push_On := Dash_View.Push_Active (View);
      Dash_Sync.Unlock;
      debugPrint ("metricsview: tracks=" & Decimal (Unsigned_64 (Resolved)) & "/" & Decimal (Unsigned_64 (Total_Tracks))
                  & " columns=" & Decimal (Unsigned_64 (Columns))
                  & " input_to_ready samples=" & Decimal (Samples) & " newest_p99_us=" & Decimal (P99)
                  & " queries=" & Decimal (Dash_Query_Feeds.Calls (Feed))
                  & " query_mean_us=" & Decimal (Dash_Query_Feeds.Call_Time (Feed)
                                                 / Unsigned_64'Max (1, Dash_Query_Feeds.Calls (Feed)))
                  & " query_max_us=" & Decimal (Dash_Query_Feeds.Slowest_Call (Feed))
                  & " refresh_last_us=" & Decimal (Last_Update_Us) & " refresh_max_us=" & Decimal (Worst_Update_Us)
                  & " refreshes=" & Decimal (Updates)
                  & " input_to_render_last_us=" & Decimal (Last_Input_Us)
                  & " input_to_render_max_us=" & Decimal (Worst_Input_Us)
                  & " inputs=" & Decimal (Inputs_Measured)
                  & " input_cost_last_us=" & Decimal (Last_Cost_Us) & " input_cost_max_us=" & Decimal (Worst_Cost_Us)
                  & " push=" & (if Push_On then "on" else "off") & " pushed=" & Decimal (Pushed)
                  & " data_thread=" & (if Threaded then "on" else "off")
                  & " data_steps=" & Decimal (Dash_Runner.Steps)
                  & " data_last_us=" & Decimal (Dash_Runner.Last_Step_Us)
                  & " data_max_us=" & Decimal (Dash_Runner.Slowest_Step_Us) & ASCII.LF);
   end Report;

   --  The frame clock: one refresh per frame, none while the last frame is
   --  still pending (hidden or the compositor busy: nothing to show it on).
   procedure Tick (Win : in out CuBit.UI.App.Window; Dirty : in out Rect; Running : in out Boolean) is
      pragma Unreferenced (Running);
      Now : constant Unsigned_64 := Now_Ms;
   begin
      if CuBit.UI.App.Frame_Pending (Win) then
         Due_Ms := Now + RETRY_MS;
         return;
      end if;
      Due_Ms := Now + FRAME_MS;
      declare
         Started : constant Unsigned_64 := Now_Us;
      begin
         if not Threaded then
            Dash_View.Data_Step (View);
            Dash_Runner.Save_Step;
         end if;
         Dash_View.Refresh (View, Dirty);
         Last_Update_Us := Now_Us - Started;
         Worst_Update_Us := Unsigned_64'Max (Worst_Update_Us, Last_Update_Us);
         Updates := Updates + 1;
         Dash_Runner.Observe (Update_Cost, Last_Update_Us);
      end;
      if Now >= Report_Due_Ms then
         Report_Due_Ms := Now + REPORT_MS;
         Report;
      end if;
   end Tick;

   procedure Run is new CuBit.UI.App.Run
     (UI, Controls, Render => Render, Handle_Event => Handle_Event, Next_Deadline => Deadline,
      On_Deadline => Tick, Activity_Wait => True);
begin
   debugPrint ("metricsview: starting" & ASCII.LF);
   CuBit.Logging.Announce ("metricsview: started", Opened, Startup.Logstore);
   Dash_Model.Load (Dash_Model.DEFAULT_SOURCE, Board, Loaded);
   if not Loaded.Success then
      debugPrint ("metricsview: the default dashboard does not check: " & Loaded.Message (1 .. Loaded.Message_Length)
                  & ASCII.LF);
      return;
   end if;
   --  The user's saved copy of the built-in dashboard, when there is one
   --  and it checks; else the built-in (which is never written over).
   declare
      Ready : Boolean;
      Name : constant String := Dash_Model.File_Name (Board);
      Got : Natural;
      Result : Dash_Files.Outcome;
      Saved : Dash_Model.Dashboard;
      Saved_Check : Dash_Model.Load_Result;
      use type Dash_Files.Outcome;
   begin
      if Startup.Dashboards.Present then
         Dash_Files.Open (Startup.Dashboards.Value, Ready);
      else
         Ready := False;
      end if;
      if not Ready then
         debugPrint ("metricsview: dashboards unavailable: built-in, unsaved" & ASCII.LF);
      else
         Dash_Files.Load (Name, Saved_Source, Got, Now_Ms + LOAD_DEADLINE_MS, Result);
         if Result = Dash_Files.Done then
            Dash_Model.Load (Saved_Source (1 .. Got), Saved, Saved_Check);
            if Saved_Check.Success then
               Board := Saved;
               debugPrint ("metricsview: dashboard from " & Name & ASCII.LF);
            else
               debugPrint ("metricsview: " & Name & " does not check, built-in: "
                           & Saved_Check.Message (1 .. Saved_Check.Message_Length) & ASCII.LF);
            end if;
         else
            debugPrint ("metricsview: dashboard built-in (" & Name & ": " & Result'Image & ")" & ASCII.LF);
         end if;
      end if;
   end;
   Dash_Query_Feeds.Bind (Feed);
   debugPrint ("metricsview: dashboard """ & Dash_Model.Image (Board.Title) & """ panels="
               & Decimal (Unsigned_64 (Board.Panels_Used))
               & (if Dash_Query_Feeds.Bound (Feed) then " store=bound" else " store=absent") & ASCII.LF);
   Dash_View.Initialize (View, Feed'Unchecked_Access, Board);
   CuBit.UI.App.Open
     (Win, WINDOW_WIDTH, WINDOW_HEIGHT,
      CuBit.Desktop_Protocol.Feature_Bits
        ([CuBit.Desktop_Protocol.Decorated | CuBit.Desktop_Protocol.Resizable |
          CuBit.Desktop_Protocol.Minimizable | CuBit.Desktop_Protocol.Maximizable |
          CuBit.Desktop_Protocol.Closeable | CuBit.Desktop_Protocol.Graceful_Close => True,
          others => False]),
      Opened, title => "Metrics", protected_frames => True, batched_input => False,
      minimum_width => MINIMUM_WIDTH, minimum_height => MINIMUM_HEIGHT);
   if not Opened then
      debugPrint ("metricsview: no window" & ASCII.LF);
      return;
   end if;
   Set_Bounds (Win);
   Dash_Runner.Start (Threaded);
   debugPrint ("metricsview: data thread " & (if Threaded then "started" else "not started: one thread") & ASCII.LF);
   Due_Ms := Now_Ms + 1;
   Report_Due_Ms := Now_Ms + REPORT_MS;
   debugPrint ("metricsview: window ready" & ASCII.LF);
   Run (Win);
   Dash_Runner.Stop;
   CuBit.UI.App.Close (Win);
   debugPrint ("metricsview: closed" & ASCII.LF);
end Main;
