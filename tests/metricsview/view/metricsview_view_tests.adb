------------------------------------------------------------------------------
--  Hosted tests of the metrics viewer's window (MET-002's app): the default
--  dashboard loaded through CCL, a synthetic store (Synthetic_Feeds) with
--  Desktop-like latency series, frames rendered into memory with the real
--  toolkit and fonts, live updates, hover, zoom, pan, drill-down, a refused
--  scope, and the time of each interaction. --screens writes PPM frames.
--  Linux-hosted: no desktop, no metrics.svc (the feed stands in for it).
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Calendar;
with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Appearance;
with CuBit.Metric_Updates;
with CuBit.UI;
with CuBit.UI.Theme_CCL;
with Charts_Format;
with Dash_Edit;
with Dash_Feeds;
with Dash_Model;
with Dash_View;
with Synthetic_Feeds;

procedure Metricsview_View_Tests is
   use type Dash_View.Save_State;
   use type Ada.Calendar.Time;
   use type Dash_Model.Panel_Kind;
   BYTES_PER_PIXEL : constant := 4;
   CHANNEL_BITS : constant := 8;
   CHANNEL_MASK : constant := 16#FF#;
   WIDTH : constant := 1440;
   HEIGHT : constant := 900;
   MINUTE : constant := 60_000_000;
   NOW_START : constant Unsigned_64 := 30 * MINUTE;
   FRAME_US : constant := 16_667;
   Screens : constant Boolean :=
     Ada.Command_Line.Argument_Count >= 1 and then Ada.Command_Line.Argument (1) = "--screens";

   type Pixels is array (Natural range <>) of Unsigned_32;
   type Pixels_Access is access Pixels;
   Image : constant Pixels_Access := new Pixels'(0 .. WIDTH * HEIGHT - 1 => 0);

   function Whole return CuBit.UI.Canvas is
     ((addr => Image.all'Address, width => WIDTH, height => HEIGHT, pitch => WIDTH * BYTES_PER_PIXEL,
       others => <>));

   Feed : aliased Synthetic_Feeds.Synthetic_Feed;
   View : Dash_View.View;
   Board : Dash_Model.Dashboard;
   Passed, Failed : Natural := 0;
   Now : Unsigned_64 := NOW_START;

   procedure Check (Condition : Boolean; What : String) is
   begin
      if Condition then
         Passed := Passed + 1;
      else
         Failed := Failed + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   State : Unsigned_64 := 16#2545_F491_4F6C_DD1D#;
   function Next return Unsigned_64 is
   begin
      State := State xor Shift_Left (State, 13);
      State := State xor Shift_Right (State, 7);
      State := State xor Shift_Left (State, 17);
      return State;
   end Next;
   function Between (Low, High : Unsigned_64) return Unsigned_64 is (Low + Next mod (High - Low + 1));

   procedure Save_PPM (Name : String) is
      use Ada.Streams;
      File : Ada.Streams.Stream_IO.File_Type;
      Header : constant String :=
        "P6" & ASCII.LF & Natural'Image (WIDTH) & Natural'Image (HEIGHT) & ASCII.LF & "255" & ASCII.LF;
      Row : Stream_Element_Array (1 .. Stream_Element_Offset (WIDTH * 3));
   begin
      if not Screens then
         return;
      end if;
      Ada.Streams.Stream_IO.Create (File, Ada.Streams.Stream_IO.Out_File, "build/screen-" & Name & ".ppm");
      for C of Header loop
         Stream_Element_Array'Write (Ada.Streams.Stream_IO.Stream (File), [1 => Stream_Element (Character'Pos (C))]);
      end loop;
      for Y in 0 .. HEIGHT - 1 loop
         for X in 0 .. WIDTH - 1 loop
            declare
               P : constant Unsigned_32 := Image (Y * WIDTH + X);
               Base : constant Stream_Element_Offset := Stream_Element_Offset (X * 3);
            begin
               Row (Base + 1) := Stream_Element (Shift_Right (P, 2 * CHANNEL_BITS) and CHANNEL_MASK);
               Row (Base + 2) := Stream_Element (Shift_Right (P, CHANNEL_BITS) and CHANNEL_MASK);
               Row (Base + 3) := Stream_Element (P and CHANNEL_MASK);
            end;
         end loop;
         Stream_Element_Array'Write (Ada.Streams.Stream_IO.Stream (File), Row);
      end loop;
      Ada.Streams.Stream_IO.Close (File);
   end Save_PPM;

   procedure Install_Themes is
   begin
      for Each in CuBit.Appearance.Color_Scheme loop
         declare
            use type CuBit.Appearance.Color_Scheme;
            Name : constant String :=
              (if Each = CuBit.Appearance.Alloy_Light then "METRICSVIEW_THEME_LIGHT" else "METRICSVIEW_THEME_DARK");
            Fallback : constant CuBit.UI.Theme := CuBit.UI.Default_Palette (Each);
            Loaded : CuBit.UI.Theme_CCL.Result;
         begin
            CuBit.UI.Install_Palette (Each, Fallback);
            if Ada.Environment_Variables.Exists (Name)
              and then Ada.Environment_Variables.Value (Name)'Length in 1 .. CuBit.UI.Theme_CCL.Maximum_Source
            then
               CuBit.UI.Theme_CCL.Load (Ada.Environment_Variables.Value (Name), Fallback, Loaded);
               if Loaded.Success then
                  CuBit.UI.Install_Palette (Each, Loaded.Value);
               end if;
            end if;
         end;
      end loop;
   end Install_Themes;

   type Stage_Name is
     (Driver_To_Intake, Intake_To_Delivery, Delivery_To_Publish, Publish_To_Compose, Compose_To_Submit,
      Submit_To_Ready, Input_To_Ready, Ready_To_Latch_Slack, Input_To_Latch);
   function Series_Of (S : Stage_Name) return String is
     (case S is
        when Driver_To_Intake => "desktop.latency.driver_to_intake",
        when Intake_To_Delivery => "desktop.latency.intake_to_delivery",
        when Delivery_To_Publish => "desktop.latency.delivery_to_publish",
        when Publish_To_Compose => "desktop.latency.publish_to_compose",
        when Compose_To_Submit => "desktop.latency.compose_to_submit",
        when Submit_To_Ready => "desktop.latency.submit_to_ready",
        when Input_To_Ready => "desktop.latency.input_to_ready",
        when Ready_To_Latch_Slack => "desktop.latency.ready_to_latch_slack",
        when Input_To_Latch => "desktop.latency.input_to_latch");

   --  One input's samples at T, as Desktop's ledger would publish them;
   --  Slow: a stall (the compositor late) for a spike to drill into.
   procedure Input_At (T : Unsigned_64; Slow : Boolean) is
      D : constant Unsigned_64 := Between (4, 22);
      I : constant Unsigned_64 := Between (3, 14);
      P : constant Unsigned_64 := Between (28, 70) + (if Next mod 17 = 0 then Between (40, 120) else 0);
      C : constant Unsigned_64 := (if Slow then Between (900, 3_200) else Between (8, 45));
      S : constant Unsigned_64 := Between (10, 30);
      R : constant Unsigned_64 := Between (18, 60) + (if Next mod 29 = 0 then Between (60, 200) else 0);
      Ready : constant Unsigned_64 := D + I + P + C + S + R;
      Slack : constant Unsigned_64 := Between (500, 15_000);
      Late : constant Unsigned_64 := (if Slow then Between (1, 3) elsif Next mod 23 = 0 then 1 else 0);
   begin
      Feed.Observe (Series_Of (Driver_To_Intake), T, D);
      Feed.Observe (Series_Of (Intake_To_Delivery), T, I);
      Feed.Observe (Series_Of (Delivery_To_Publish), T, P);
      Feed.Observe (Series_Of (Publish_To_Compose), T, C);
      Feed.Observe (Series_Of (Compose_To_Submit), T, S);
      Feed.Observe (Series_Of (Submit_To_Ready), T, R);
      Feed.Observe (Series_Of (Input_To_Ready), T, Ready);
      Feed.Observe (Series_Of (Ready_To_Latch_Slack), T, Slack);
      Feed.Observe (Series_Of (Input_To_Latch), T, Ready + Slack);
      Feed.Observe ("desktop.latency.refreshes_late", T, Late);
      if Late > 1 then
         Feed.Observe ("desktop.frames.dropped", T, 1);
      end if;
      Feed.Observe ("metricsview.input_to_render_us", T, Between (60, (if Slow then 420 else 190)));
      Feed.Observe ("metricsview.update_us", T, Between (35, 160));
      Feed.Observe ("metricsview.render_us", T, Between (25, 120));
   end Input_At;

   --  Typing in bursts from From to To: about eight inputs a second while a
   --  burst lasts, a stall around Spike.
   procedure Generate (From, To, Spike : Unsigned_64) is
      T : Unsigned_64 := From;
   begin
      while T < To loop
         if (T / 4_000_000) mod 3 /= 2 then
            Input_At (T, T in Spike .. Spike + 2_500_000);
         end if;
         T := T + Between (60_000, 190_000);
      end loop;
   end Generate;

   Damage_Total : CuBit.UI.Rect := (others => 0);

   procedure Frame is
      Damage : CuBit.UI.Rect := (others => 0);
   begin
      Feed.Set_Clock (Now);
      --  Both threads' steps, in order, on the test's one thread.
      Dash_View.Data_Step (View);
      Dash_View.Refresh (View, Damage);
      Damage_Total := Damage;
      if not CuBit.UI.Is_Empty (Damage) then
         Dash_View.Render (View, CuBit.UI.With_Clip (Whole, Damage), Damage);
      end if;
   end Frame;

   function Elapsed_Us (From : Ada.Calendar.Time) return Natural is
     (Natural (Float (Ada.Calendar.Clock - From) * 1.0E6));

   procedure Interact (E : Dash_View.Event; What : String) is
      Damage : CuBit.UI.Rect := (others => 0);
      Started : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Taken : Natural;
      Handled : Natural;
   begin
      Dash_View.Handle (View, E, Damage);
      Handled := Elapsed_Us (Started);
      if not CuBit.UI.Is_Empty (Damage) then
         Dash_View.Render (View, CuBit.UI.With_Clip (Whole, Damage), Damage);
      end if;
      Taken := Elapsed_Us (Started);
      Put_Line ("interaction " & What & ": " & Natural'Image (Taken) & " us (handle" & Handled'Image & "), damage"
                & Natural'Image (Damage.w) & "x" & Natural'Image (Damage.h));
   end Interact;

   --  Everything drawn again (not copied from the retained frame).
   procedure Repaint_All is
      Damage : CuBit.UI.Rect := (others => 0);
   begin
      Dash_View.Invalidate (View, (0, 0, WIDTH, HEIGHT), Damage);
      Dash_View.Render (View, Whole, Damage);
   end Repaint_All;

   --  The retained frame's damage tracking is right when what the frames
   --  built up equals a redraw of everything from scratch.
   procedure Check_Coherent (What : String) is
      Built : constant Pixels_Access := new Pixels'(Image.all);
      Different : Natural := 0;
      Low_X, Low_Y : Natural := Natural'Last;
      High_X, High_Y : Natural := 0;
   begin
      Repaint_All;
      for I in Image'Range loop
         if Image (I) /= Built (I) then
            Different := Different + 1;
            Low_X := Natural'Min (Low_X, I mod WIDTH);
            High_X := Natural'Max (High_X, I mod WIDTH);
            Low_Y := Natural'Min (Low_Y, I / WIDTH);
            High_Y := Natural'Max (High_Y, I / WIDTH);
         end if;
      end loop;
      if Different > 0 then
         Put_Line ("  differences within x" & Low_X'Image & " .." & High_X'Image & ", y" & Low_Y'Image & " .."
                   & High_Y'Image);
         if Screens then
            declare
               Redrawn : constant Pixels := Image.all;
            begin
               Image.all := Built.all;
               Save_PPM ("diff-" & What & "-built");
               Image.all := Redrawn;
            end;
         end if;
      end if;
      Check (Different = 0, What & ": frames built from damage equal a full redraw (" & Different'Image
             & " pixels differ)");
   end Check_Coherent;

   procedure Add_Series is
   begin
      for S in Stage_Name loop
         Feed.Add_Series (Series_Of (S), Dash_Feeds.Distribution, Charts_Format.Microseconds);
      end loop;
      Feed.Add_Series ("desktop.latency.refreshes_late", Dash_Feeds.Distribution, Charts_Format.Count);
      Feed.Add_Series ("desktop.frames.dropped", Dash_Feeds.Counter, Charts_Format.Count);
      Feed.Add_Series ("metricsview.input_to_render_us", Dash_Feeds.Distribution, Charts_Format.Microseconds);
      Feed.Add_Series ("metricsview.update_us", Dash_Feeds.Distribution, Charts_Format.Microseconds);
      Feed.Add_Series ("metricsview.render_us", Dash_Feeds.Distribution, Charts_Format.Microseconds);
   end Add_Series;

   Loaded : Dash_Model.Load_Result;
   Samples, P99, From_Us, To_Us : Unsigned_64;
   Found : Boolean;
   --  The push ring's elements (CuBit.Metric_Updates): what metrics.svc
   --  encodes reads back, and malformed words are refused.
   procedure Update_Codec is
      package U renames CuBit.Metric_Updates;
      use type U.Element_Kind;
      use type U.Gap_Cause;
      Wire : U.Element_Buffer;
      D : U.Decoded;
   begin
      U.Encode ((Kind => U.Sample, Series => 42, Time_Us => 1_234_567, Value => 89), Wire);
      D := U.Decode (Wire);
      Check (D.Success and then D.Value.Kind = U.Sample and then D.Value.Series = 42
             and then D.Value.Time_Us = 1_234_567 and then D.Value.Value = 89, "a pushed sample reads back");
      U.Encode ((Kind => U.Gap, Series => 0, Cause => U.Subscriber_Shed, Missing => 7, First_Us => 10,
                 Last_Us => 20), Wire);
      D := U.Decode (Wire);
      Check (D.Success and then D.Value.Kind = U.Gap and then D.Value.Cause = U.Subscriber_Shed
             and then D.Value.Missing = 7, "the subscription's own shed reads back");
      Wire (5) := 1;
      Check (not U.Decode (Wire).Success, "a cause the element cannot carry is refused");
      U.Encode ((Kind => U.Gap, Series => 3, Cause => U.Producer_Shed, Missing => 1, First_Us => 30,
                 Last_Us => 20), Wire);
      Check (not U.Decode (Wire).Success, "a gap that ends before it starts is refused");
      U.Encode ((Kind => U.Sample, Series => 0, Time_Us => 1, Value => 1), Wire);
      Check (not U.Decode (Wire).Success, "a sample of no series is refused");
   end Update_Codec;
   --  Dashboards round-trip through their CCL source, and edits undo.
   procedure Model_Edits is
      use type Dash_Model.Panel_Kind;
      use type Dash_Model.Statistic_Set;
      use type Dash_Model.Match_Kind;
      Original, Again, Edited : Dash_Model.Dashboard;
      Result : Dash_Model.Load_Result;
      Done : Boolean;
      Selected : Natural;
      function Same (A, B : Dash_Model.Dashboard) return Boolean is
      begin
         if A.Panels_Used /= B.Panels_Used or else A.Span /= B.Span or else A.Live /= B.Live
           or else Dash_Model.Image (A.Title) /= Dash_Model.Image (B.Title)
         then
            return False;
         end if;
         for P in 1 .. A.Panels_Used loop
            declare
               X : Dash_Model.Panel renames A.Panels (P);
               Y : Dash_Model.Panel renames B.Panels (P);
            begin
               if Dash_Model.Image (X.Title) /= Dash_Model.Image (Y.Title) or else X.Kind /= Y.Kind
                 or else X.Width /= Y.Width or else X.Height /= Y.Height or else X.Series_Used /= Y.Series_Used
                 or else X.Lines /= Y.Lines or else X.Band /= Y.Band or else X.Budget /= Y.Budget
               then
                  return False;
               end if;
               for S in 1 .. X.Series_Used loop
                  if Dash_Model.Image (X.Series (S).Name) /= Dash_Model.Image (Y.Series (S).Name)
                    or else Dash_Model.Image (X.Series (S).Label) /= Dash_Model.Image (Y.Series (S).Label)
                    or else X.Series (S).Match /= Y.Series (S).Match
                  then
                     return False;
                  end if;
               end loop;
            end;
         end loop;
         return True;
      end Same;
   begin
      Dash_Model.Load (Dash_Model.DEFAULT_SOURCE, Original, Result);
      Dash_Model.Load (Dash_Model.To_Source (Original), Again, Result);
      Check (Result.Success and then Same (Original, Again),
             "a dashboard reads back from its own CCL source: " & Result.Message (1 .. Result.Message_Length));
      Edited := Original;
      Dash_Edit.Start (Edited);
      Dash_Edit.Apply (Edited, Dash_Edit.Move_Later, 1, Done, Selected);
      Check (Done and then Selected = 2 and then Dash_Model.Image (Edited.Panels (2).Title) = "Input to ready",
             "a panel moves later");
      Dash_Edit.Apply (Edited, Dash_Edit.Narrower, 2, Done, Selected);
      Dash_Edit.Apply (Edited, Dash_Edit.Taller, 2, Done, Selected);
      Check (Edited.Panels (2).Width = 8 and then Edited.Panels (2).Height = 6, "a panel resizes");
      Dash_Edit.Apply (Edited, Dash_Edit.Remove, 3, Done, Selected);
      Dash_Edit.Apply (Edited, Dash_Edit.Duplicate, 1, Done, Selected);
      Check (Edited.Panels_Used = Original.Panels_Used and then Selected = 2, "remove, then duplicate");
      Dash_Model.Load (Dash_Model.To_Source (Edited), Again, Result);
      Check (Result.Success and then Same (Edited, Again), "an edited dashboard is still a Dashboard");
      for I in 1 .. 5 loop
         Dash_Edit.Undo (Edited, Done);
      end loop;
      Check (Same (Edited, Original) and then not Dash_Edit.Can_Undo, "five undos give the original back");
      Dash_Edit.Redo (Edited, Done);
      Check (Done and then Dash_Model.Image (Edited.Panels (2).Title) = "Input to ready", "redo");
      Dash_Edit.Apply (Edited, Dash_Edit.Narrower, 1, Done, Selected);
      Check (not Dash_Edit.Can_Redo, "an edit after undo drops the redo");
      Edited.Panels (1).Width := 1;
      Dash_Edit.Apply (Edited, Dash_Edit.Narrower, 1, Done, Selected);
      Check (not Done, "a panel never narrower than one column");
   end Model_Edits;
begin
   Update_Codec;
   Model_Edits;
   Install_Themes;
   CuBit.UI.Set_Color_Scheme (CuBit.Appearance.Alloy_Dark);
   Add_Series;
   Generate (NOW_START - 29 * MINUTE, NOW_START, NOW_START - 70_000_000);

   --  The default dashboard, through CCL.
   declare
      Started : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      Dash_Model.Load (Dash_Model.DEFAULT_SOURCE, Board, Loaded);
      Put_Line ("dashboard: loaded in" & Natural'Image (Elapsed_Us (Started)) & " us, source"
                & Natural'Image (Dash_Model.DEFAULT_SOURCE'Length + Dash_Model.TYPE_SOURCE'Length) & " bytes");
   end;
   Check (Loaded.Success, "the default dashboard checks: " & Loaded.Message (1 .. Loaded.Message_Length));
   Check (Board.Panels_Used = 8 and then Dash_Model.Image (Board.Title) = "Display latency", "default panels");
   Check (Board.Panels (1).Kind = Dash_Model.Time_Series and then Board.Panels (1).Lines (Dash_Model.P99)
          and then Board.Panels (1).Budget = 250 and then Board.Panels (1).Width = 9, "the input-to-ready panel");
   Check (Board.Panels (7).Kind = Dash_Model.Histogram and then Board.Panels (7).Buckets.Count = 5, "histogram");
   Check (Board.Panels (4).Kind = Dash_Model.Heatmap and then Board.Panels (4).Series_Used = 7, "heatmap rows");
   declare
      Other : Dash_Model.Dashboard := Board;
      Bad : Dash_Model.Load_Result;
   begin
      Dash_Model.Load ("(Dashboard title => ""x"" time => (Dashboard_Time span_ms => 60000) panels => [" &
                       "(Panel title => ""p"" width => 13 kind => (Panel_Kind.Stat (Stat_Panel statistic => " &
                       "Metric_Statistic.P99)) series => [])])", Other, Bad);
      Check (not Bad.Success and then Bad.Message_Length > 0, "a 13-column panel is refused by its type");
      Check (Other.Panels_Used = Board.Panels_Used, "a refused dashboard leaves the old one");
      Dash_Model.Load ("(Dashboard title => ""x"" panels => [])", Other, Bad);
      Check (not Bad.Success, "a dashboard without its time is refused");
   end;

   Feed.Set_Clock (Now);
   if Ada.Environment_Variables.Exists ("METRICSVIEW_NO_PUSH") then
      Feed.Disable_Push;
   end if;
   Dash_View.Initialize (View, Feed'Unchecked_Access, Board);
   Dash_View.Set_Bounds (View, (0, 0, WIDTH, HEIGHT));
   for I in 1 .. 4 loop
      Frame;
   end loop;
   Repaint_All;
   Check (Dash_View.Resolved_Tracks (View) = Dash_View.Track_Total (View),
          "every series resolved:" & Natural'Image (Dash_View.Resolved_Tracks (View)) & " of"
          & Natural'Image (Dash_View.Track_Total (View)));
   Dash_View.Probe (View, "Input to ready", Samples, P99, From_Us, To_Us, Found);
   Check (Found and then Natural (Samples) = Feed.Samples_Between (Series_Of (Input_To_Ready), From_Us, To_Us),
          "raw columns hold every sample in view:" & Samples'Image & " of"
          & Feed.Samples_Between (Series_Of (Input_To_Ready), From_Us, To_Us)'Image);
   Save_PPM ("01-dashboard-dark");

   --  Live: a new sample is a sliver; crossing a column scrolls.
   declare
      Slivers, Scrolls : Natural := 0;
      Narrowest : Natural := Natural'Last;
      Started : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Frames : constant := 120;
   begin
      for F in 1 .. Frames loop
         Input_At (Now + FRAME_US / 2, False);
         Now := Now + FRAME_US;
         Frame;
         if not CuBit.UI.Is_Empty (Damage_Total) then
            if Damage_Total.w < WIDTH / 2 then
               Slivers := Slivers + 1;
               Narrowest := Natural'Min (Narrowest, Damage_Total.w);
            else
               Scrolls := Scrolls + 1;
            end if;
         end if;
      end loop;
      Put_Line ("live: " & Natural'Image (Frames) & " frames," & Natural'Image (Slivers) & " slivers (narrowest"
                & Natural'Image (Narrowest) & " px)," & Natural'Image (Scrolls) & " scrolls, mean frame"
                & Natural'Image (Elapsed_Us (Started) / Frames) & " us (refresh + render)");
      Check (Slivers > 0, "new samples repaint slivers, not the window");
      Check_Coherent ("live");
      Check (Scrolls > 0, "crossing a column scrolls the plots");
   end;
   Repaint_All;
   Dash_View.Probe (View, "Input to ready", Samples, P99, From_Us, To_Us, Found);
   Check (Natural (Samples) = Feed.Samples_Between (Series_Of (Input_To_Ready), From_Us, To_Us),
          "after live frames the columns still hold exactly the store's samples:" & Samples'Image & " of"
          & Feed.Samples_Between (Series_Of (Input_To_Ready), From_Us, To_Us)'Image);
   Save_PPM ("02-live");

   --  Hover the input-to-ready plot (panel 5, the second row, left).
   Interact ((Kind => Dash_View.Pointer_Move, X => 700, Y => 300, others => <>), "hover");
   Interact ((Kind => Dash_View.Pointer_Move, X => 708, Y => 302, others => <>), "hover move");
   Save_PPM ("03-hover");
   --  Zoom in twice about the pointer: still live.
   Interact ((Kind => Dash_View.Wheel, X => 708, Y => 302, Steps => 2, others => <>), "zoom");
   Check (Dash_View.Is_Live (View) and then Dash_View.Span_Us (View) < 120_000_000, "zooming in while live");
   Frame;
   Frame;
   Save_PPM ("04-zoomed");
   --  Sequences as a pointer delivers them: per event, mean and worst.
   declare
      procedure Sequence (Name : String; Kind : Dash_View.Event_Kind; X0, Y0, Step, Count : Integer) is
         Total, Worst : Natural := 0;
      begin
         for I in 1 .. Count loop
            declare
               Damage : CuBit.UI.Rect := (others => 0);
               Started : constant Ada.Calendar.Time := Ada.Calendar.Clock;
               Taken : Natural;
            begin
               Dash_View.Handle (View, (Kind => Kind, X => X0 + I * Step, Y => Y0, others => <>), Damage);
               if not CuBit.UI.Is_Empty (Damage) then
                  Dash_View.Render (View, CuBit.UI.With_Clip (Whole, Damage), Damage);
               end if;
               Taken := Elapsed_Us (Started);
               Total := Total + Taken;
               Worst := Natural'Max (Worst, Taken);
            end;
         end loop;
         Put_Line ("sequence " & Name & ":" & Natural'Image (Count) & " events, mean" & Natural'Image (Total / Count)
                   & " us, worst" & Natural'Image (Worst) & " us (handle + render)");
      end Sequence;
      Plot : constant CuBit.UI.Rect := Dash_View.Plot_Bounds (View, 1);
   begin
      Sequence ("hover", Dash_View.Pointer_Move, Plot.x + 40, Plot.y + 60, 7, 60);
      Interact ((Kind => Dash_View.Pointer_Press, X => Plot.x + 500, Y => Plot.y + 60, others => <>), "drag press");
      Sequence ("drag pan", Dash_View.Pointer_Move, Plot.x + 500, Plot.y + 60, -6, 40);
      Interact ((Kind => Dash_View.Pointer_Release, X => Plot.x + 260, Y => Plot.y + 60, others => <>),
                "drag release");
      Check_Coherent ("drag pan");
      Interact ((Kind => Dash_View.Key_Press, Pressed => Dash_View.Live, others => <>), "back to live");
      Frame;
   end;

   --  Drag to pan back: paused.
   Interact ((Kind => Dash_View.Pointer_Press, X => 700, Y => 320, others => <>), "press");
   Interact ((Kind => Dash_View.Pointer_Move, X => 900, Y => 320, others => <>), "pan");
   Interact ((Kind => Dash_View.Pointer_Release, X => 900, Y => 320, others => <>), "release");
   Check (not Dash_View.Is_Live (View), "panning pauses");
   Frame;
   --  A click: the slice's logs.
   Interact ((Kind => Dash_View.Pointer_Press, X => 640, Y => 320, others => <>), "click press");
   Interact ((Kind => Dash_View.Pointer_Release, X => 640, Y => 320, others => <>), "click");
   declare
      E : constant String := Dash_View.Drill_Down_Expression (View);
      Prefix : constant String := "(logs.between (LogWindow ""desktop"" ";
   begin
      Put_Line ("drill-down: " & E);
      Check (E'Length > Prefix'Length and then E (E'First .. E'First + Prefix'Length - 1) = Prefix,
             "a click gives the slice's logs.between");
   end;
   Frame;
   Save_PPM ("05-paused-slice");
   --  The last hour: tier windows.
   Interact ((Kind => Dash_View.Key_Press, Pressed => Dash_View.Live, others => <>), "live");
   Interact ((Kind => Dash_View.Key_Press, Pressed => Dash_View.Range_4, others => <>), "1 h");
   for I in 1 .. 3 loop
      Frame;
   end loop;
   Dash_View.Probe (View, "Input to ready", Samples, P99, From_Us, To_Us, Found);
   declare
      Truth : constant Natural := Feed.Samples_Between (Series_Of (Input_To_Ready), From_Us, To_Us);
   begin
      --  A window goes to the column of its start: where a column edge cuts
      --  one, its count lands on one side, so the totals agree within 2%.
      Check (Found and then Natural (Samples) in Truth - Truth / 50 .. Truth + Truth / 50 + 50,
             "tier columns count the samples in view:" & Samples'Image & " vs" & Truth'Image);
   end;
   Repaint_All;
   Save_PPM ("06-hour");
   Interact ((Kind => Dash_View.Key_Press, Pressed => Dash_View.Range_1, others => <>), "1 min");
   Frame;
   Frame;
   CuBit.UI.Set_Color_Scheme (CuBit.Appearance.Alloy_Light);
   Repaint_All;
   Save_PPM ("07-light");
   CuBit.UI.Set_Color_Scheme (CuBit.Appearance.Alloy_Dark);

   --  The editor: keys and the pointer edit the layout; the tracks (and
   --  their data) follow their panels; undo gives the original back.
   declare
      Original : constant String := Dash_View.Dashboard_Source (View);
      Parsed : Dash_Model.Dashboard;
      function Key (K : Dash_View.Key; Shift : Boolean := False) return Dash_View.Event is
        (Kind => Dash_View.Key_Press, Pressed => K, Shift => Shift, others => <>);
      function Title_Of (P : Positive) return String is
      begin
         Dash_Model.Load (Dash_View.Dashboard_Source (View), Parsed, Loaded);
         return (if Loaded.Success and then P <= Parsed.Panels_Used then Dash_Model.Image (Parsed.Panels (P).Title)
                 else "");
      end Title_Of;
      First_Title : constant String := Title_Of (1);
      Second_Title : constant String := Title_Of (2);
      Width_First : constant Natural := Parsed.Panels (1).Width;
      Panels_Before : constant Natural := Dash_View.Panel_Count (View);
   begin
      Interact (Key (Dash_View.Edit), "edit");
      Check (Dash_View.Editing (View), "E starts editing");
      Check (Dash_View.Save_Name (View) = "@system/Dashboards/display-latency.ccl", "a dashboard's file is its title");
      Interact (Key (Dash_View.Right), "move later");
      declare
         Due : Boolean;
         Generation : Natural;
      begin
         Dash_View.Take_Save (View, Due, Generation);
         Check (not Due, "while editing, a save waits for the edits to settle");
         Feed.Set_Clock (Now + Dash_View.Autosave_Delay_Ms * 1_000);
         Dash_View.Take_Save (View, Due, Generation);
         Check (Due and then Dash_View.Saving_State (View) = Dash_View.Saving, "settled edits are saved");
         Dash_View.End_Save (View, Generation, Saved => True);
         Check (Dash_View.Saving_State (View) = Dash_View.Saved, "saved");
         Dash_View.Take_Save (View, Due, Generation);
         Check (not Due, "saved once");
         Feed.Set_Clock (Now);
      end;
      Check (Title_Of (1) = Second_Title and then Title_Of (2) = First_Title, "Right moves the panel later");
      Check (Dash_View.Columns_With_Data (View) > 0 and then Dash_View.Resolved_Tracks (View)
             = Dash_View.Track_Total (View), "the tracks (and their data) follow their panels");
      Interact (Key (Dash_View.Right, Shift => True), "wider");
      Check (Title_Of (2) = First_Title
             and then (Parsed.Panels (2).Width = Width_First + 1 or else Width_First = Dash_Model.Grid_Columns),
             "Shift+Right widens it");
      Frame;
      Check_Coherent ("edit-move");
      Interact (Key (Dash_View.Remove), "remove");
      Check (Dash_View.Panel_Count (View) = Panels_Before - 1, "Delete removes it");
      Frame;
      Check (Dash_View.Resolved_Tracks (View) = Dash_View.Track_Total (View), "removed: the rest still read");
      Interact (Key (Dash_View.Duplicate), "duplicate");
      Check (Dash_View.Panel_Count (View) = Panels_Before, "Insert copies it");
      for I in 1 .. 3 loop
         Frame;
      end loop;
      Check (Dash_View.Resolved_Tracks (View) = Dash_View.Track_Total (View), "a copy's series resolve");
      Check_Coherent ("edit-remove-copy");
      --  Size by the grip: a drag two columns left, one edit.
      declare
         R : constant CuBit.UI.Rect := Dash_View.Panel_Bounds (View, 1);
         Unit : constant Natural := WIDTH / Dash_Model.Grid_Columns;
         Width_1 : Natural;
      begin
         Width_1 := (if Title_Of (1)'Length > 0 then Parsed.Panels (1).Width else 0);
         Interact ((Kind => Dash_View.Pointer_Press, X => R.x + R.w - 2, Y => R.y + R.h - 2, others => <>),
                   "grip press");
         Interact ((Kind => Dash_View.Pointer_Move, X => R.x + R.w - 2 - Unit, Y => R.y + R.h - 2, others => <>),
                   "grip drag");
         Interact ((Kind => Dash_View.Pointer_Move, X => R.x + R.w - 2 - 2 * Unit, Y => R.y + R.h - 2,
                    others => <>), "grip drag");
         Interact ((Kind => Dash_View.Pointer_Release, X => R.x + R.w - 2 - 2 * Unit, Y => R.y + R.h - 2,
                    others => <>), "grip release");
         Check (Title_Of (1)'Length > 0 and then Parsed.Panels (1).Width = Natural'Max (1, Width_1 - 2),
                "the grip sizes the panel by grid units");
         Interact (Key (Dash_View.Undo), "undo the drag");
         Check (Title_Of (1)'Length > 0 and then Parsed.Panels (1).Width = Width_1, "a drag undoes as one edit");
      end;
      Frame;
      Repaint_All;
      Save_PPM ("09-edit");
      Interact (Key (Dash_View.Show_Source), "show CCL");
      --  Live data behind the overlay repaints parts of it.
      for I in 1 .. 4 loop
         Generate (Now, Now + 4 * FRAME_US, 0);
         Now := Now + 4 * FRAME_US;
         Frame;
      end loop;
      Check_Coherent ("edit-source-live");
      Repaint_All;
      Save_PPM ("10-edit-source");
      Interact (Key (Dash_View.Show_Source), "hide CCL");
      for I in 1 .. 4 loop
         Interact (Key (Dash_View.Undo), "undo");
      end loop;
      Check (Dash_View.Dashboard_Source (View) = Original, "undo gives the original dashboard back");
      Interact (Key (Dash_View.Escape), "done");
      Check (not Dash_View.Editing (View), "Esc ends editing");
      declare
         Due : Boolean;
         Generation : Natural;
      begin
         Dash_View.Take_Save (View, Due, Generation);
         Check (Due, "ending the editor saves at once");
         Dash_View.End_Save (View, Generation, Saved => False, Reason => "no filesystem");
         Check (Dash_View.Saving_State (View) = Dash_View.Not_Saved, "a failed save is shown");
         Dash_View.Take_Save (View, Due, Generation);
         Check (not Due, "a failed save is not retried until the next edit");
      end;
      Frame;
      Check_Coherent ("edit-undone");
   end;

   --  Repaint cost of the whole window.
   declare
      Started : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Rounds : constant := 20;
   begin
      for I in 1 .. Rounds loop
         Repaint_All;
      end loop;
      Put_Line ("full redraw:" & Natural'Image (Elapsed_Us (Started) / Rounds) & " us at"
                & Natural'Image (WIDTH) & "x" & Natural'Image (HEIGHT));
      declare
         Fill_Started : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      begin
         for I in 1 .. Rounds loop
            CuBit.UI.Fill_Rect (Whole, (0, 0, WIDTH, HEIGHT), Unsigned_32 (I));
         end loop;
         Put_Line ("one window fill (the floor):" & Natural'Image (Elapsed_Us (Fill_Started) / Rounds) & " us");
      end;
      declare
         Copy_Started : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      begin
         for I in 1 .. Rounds loop
            Dash_View.Render (View, Whole, (0, 0, WIDTH, HEIGHT));
         end loop;
         Put_Line ("whole frame from the retained frame (a copy):" & Natural'Image (Elapsed_Us (Copy_Started) / Rounds)
                   & " us");
      end;
      for P in 1 .. Dash_View.Panel_Count (View) loop
         declare
            R : constant CuBit.UI.Rect := Dash_View.Panel_Bounds (View, P);
            Begun : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         begin
            for I in 1 .. Rounds loop
               declare
                  Damage : CuBit.UI.Rect := (others => 0);
               begin
                  Dash_View.Invalidate (View, R, Damage);
                  Dash_View.Render (View, CuBit.UI.With_Clip (Whole, R), R);
               end;
            end loop;
            Put_Line ("  panel" & P'Image & " " & Natural'Image (R.w) & "x" & Natural'Image (R.h) & ":"
                      & Natural'Image (Elapsed_Us (Begun) / Rounds) & " us");
            declare
               Plot : constant CuBit.UI.Rect := Dash_View.Plot_Bounds (View, P);
               Plot_Begun : constant Ada.Calendar.Time := Ada.Calendar.Clock;
            begin
               for I in 1 .. Rounds loop
                  declare
                     Damage : CuBit.UI.Rect := (others => 0);
                  begin
                     Dash_View.Invalidate (View, Plot, Damage);
                     Dash_View.Render (View, CuBit.UI.With_Clip (Whole, Plot), Plot);
                  end;
               end loop;
               Put_Line ("    its plot " & Natural'Image (Plot.w) & "x" & Natural'Image (Plot.h) & ":"
                         & Natural'Image (Elapsed_Us (Plot_Begun) / Rounds) & " us");
            end;
         end;
      end loop;
   end;

   --  A series outside the reader's scope.
   declare
      Small : Dash_Model.Dashboard;
   begin
      Feed.Refuse ("desktop.frames.");
      Dash_Model.Load ("(Dashboard title => ""Scoped"" time => (Dashboard_Time span_ms => 60000) panels => [" &
                       "(Panel title => ""Dropped"" kind => (Panel_Kind.Stat (Stat_Panel statistic => " &
                       "Metric_Statistic.Count)) series => [(series ""desktop.frames.dropped"" """")])])",
                       Small, Loaded);
      Check (Loaded.Success, "a one-panel dashboard");
      Dash_View.Initialize (View, Feed'Unchecked_Access, Small);
      Dash_View.Set_Bounds (View, (0, 0, WIDTH, HEIGHT));
      Frame;
      Check (Dash_View.Resolved_Tracks (View) = 0, "a refused series is not read");
      Repaint_All;
      Save_PPM ("08-refused");
   end;

   Put_Line ("metricsview view:" & Natural'Image (Passed) & " passed," & Natural'Image (Failed) & " failed");
   if Failed > 0 then
      raise Program_Error with "metricsview view tests failed";
   end if;
end Metricsview_View_Tests;
