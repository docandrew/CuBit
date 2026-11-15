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
with Dash_Model;
with Dash_Query_Feeds;
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
   Frames : constant CuBit.Metrics.Series :=
     CuBit.Metrics.Series_Named ("metricsview.frames", CuBit.Metrics.Counter, CuBit.Metrics.Count);

   Win : CuBit.UI.App.Window;
   UI : CuBit.UI.State.UI_State;
   Controls : CuBit.UI.Controls.Control_Map;
   Feed : aliased Dash_Query_Feeds.Query_Feed;
   View : Dash_View.View;
   Board : Dash_Model.Dashboard;
   Loaded : Dash_Model.Load_Result;
   Opened : Boolean;
   Due_Ms, Report_Due_Ms : Unsigned_64 := 0;
   --  The first input not yet shown, and when it was handled.
   Input_Pending : Boolean := False;
   Input_Started_Us : Unsigned_64 := 0;
   --  Handling time of the inputs waiting for their render.
   Input_Work_Us : Unsigned_64 := 0;
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
      CuBit.Metrics.Observe (Render_Cost, Now_Us - Started);
      CuBit.Metrics.Observe (Frames, 1);
      if Input_Pending then
         Input_Pending := False;
         Last_Cost_Us := Input_Work_Us + (Now_Us - Started);
         Worst_Cost_Us := Unsigned_64'Max (Worst_Cost_Us, Last_Cost_Us);
         CuBit.Metrics.Observe (Input_Cost, Last_Cost_Us);
         Input_Work_Us := 0;
         Last_Input_Us := Now_Us - Input_Started_Us;
         Worst_Input_Us := Unsigned_64'Max (Worst_Input_Us, Last_Input_Us);
         Inputs_Measured := Inputs_Measured + 1;
         CuBit.Metrics.Observe (Input_To_Render, Last_Input_Us);
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

   function Key_Of (Code : Unsigned_64) return Dash_View.Key is
     (if Code = KEY_LEFT then Dash_View.Left elsif Code = KEY_RIGHT then Dash_View.Right
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
               Expression : constant String := Dash_View.Drill_Down_Expression (View);
               Result : CuBit.UI.Clipboard.Outcome;
            begin
               if Expression'Length > 0 then
                  CuBit.UI.Clipboard.Copy_Text (CuBit.UI.App.Surface_ID (Win), Expression,
                                                CuBit.UI.Clipboard.Ordinary, Result);
                  debugPrint ("metricsview: copied " & Expression & ASCII.LF);
               end if;
            end;
         elsif Key_Of (Event.payload0) /= Dash_View.No_Key then
            Dash_View.Handle (View, (Kind => Dash_View.Key_Press, Pressed => Key_Of (Event.payload0), others => <>),
                              Dirty);
         end if;
      elsif Event.kind in I.INPUT_POINTER_MOVE | I.INPUT_POINTER_DOWN | I.INPUT_POINTER_UP then
         Dash_View.Handle
           (View, (Kind => (if Event.kind = I.INPUT_POINTER_DOWN then Dash_View.Pointer_Press
                            elsif Event.kind = I.INPUT_POINTER_UP then Dash_View.Pointer_Release
                            else Dash_View.Pointer_Move),
                   X => I.Pointer_X (Event), Y => I.Pointer_Y (Event), Shift => Shift, others => <>), Dirty);
      elsif Event.kind = I.INPUT_POINTER_WHEEL then
         Dash_View.Handle (View, (Kind => Dash_View.Wheel, X => I.Pointer_X (Event), Y => I.Pointer_Y (Event),
                                  Steps => I.Pointer_Wheel_Delta (Event), others => <>), Dirty);
      end if;
      --  Input that changed the window: measured to the end of its render.
      if not Input_Pending and then (Dirty.w /= Before.w or else Dirty.h /= Before.h or else
                                     Dirty.x /= Before.x or else Dirty.y /= Before.y)
      then
         Input_Pending := True;
         Input_Started_Us := Started;
      end if;
      if Input_Pending then
         Input_Work_Us := Input_Work_Us + (Now_Us - Started);
      end if;
   end Handle_Event;

   function Deadline return Unsigned_64 is (Due_Ms);


   type Name_Access is access constant String;
   Input_Name : aliased constant String := "metricsview.input_to_render_us";
   Cost_Name : aliased constant String := "metricsview.input_cost_us";
   Update_Name : aliased constant String := "metricsview.update_us";
   Own_Names : constant array (1 .. 3) of Name_Access :=
     [Input_Name'Access, Cost_Name'Access, Update_Name'Access];

   procedure Report;
   procedure Report is
      Samples, P99, From_Us, To_Us : Unsigned_64;
      Found : Boolean;
      Own : Dash_Feeds.Series_Infos (1 .. 1);
      Listed, Total : Natural;
      Result : Dash_Feeds.Status;
      Stats : Dash_Feeds.Window_Stats;
   begin
      Dash_View.Probe (View, "Input to ready", Samples, P99, From_Us, To_Us, Found);
      debugPrint ("metricsview: tracks=" & Decimal (Unsigned_64 (Dash_View.Resolved_Tracks (View))) & "/"
                  & Decimal (Unsigned_64 (Dash_View.Track_Total (View)))
                  & " columns=" & Decimal (Unsigned_64 (Dash_View.Columns_With_Data (View)))
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
                  & ASCII.LF);
      --  Its own series, read back through the store like any other.
      for Name of Own_Names loop
         Dash_Query_Feeds.Find (Feed, (Name => Dash_Model.To_Text (Name.all), others => <>), Own, Listed, Total,
                                Result);
         if Result = Dash_Feeds.OK and then Listed = 1 then
            Dash_Query_Feeds.Read_Stats (Feed, Own (1).Id, 0, Now_Us, Stats, Result);
            if Result = Dash_Feeds.OK then
               debugPrint ("metricsview: own series " & Name.all & " n=" & Decimal (Stats.Count)
                           & " p50_us=" & Decimal (Stats.P50) & " p99_us=" & Decimal (Stats.P99)
                           & " max_us=" & Decimal (Stats.Maximum) & ASCII.LF);
            end if;
         end if;
      end loop;
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
         Dash_View.Refresh (View, Dirty);
         Last_Update_Us := Now_Us - Started;
         Worst_Update_Us := Unsigned_64'Max (Worst_Update_Us, Last_Update_Us);
         Updates := Updates + 1;
         CuBit.Metrics.Observe (Update_Cost, Last_Update_Us);
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
   Due_Ms := Now_Ms + 1;
   Report_Due_Ms := Now_Ms + REPORT_MS;
   debugPrint ("metricsview: window ready" & ASCII.LF);
   Run (Win);
   CuBit.UI.App.Close (Win);
   debugPrint ("metricsview: closed" & ASCII.LF);
end Main;
