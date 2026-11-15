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
with Ada.Environment_Variables;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Appearance;
with CuBit.File_Selection;
with CuBit.Metric_Updates;
with CuBit.UI;
with CuBit.UI.Theme_CCL;
with Charts_Format;
with Ada.Real_Time;
with Ada.Strings.Unbounded;
with Dash_Choices;
with Dash_Edit;
with Dash_Feeds;
with Dash_Grid;
with Dash_Jobs;
with Dash_Layers;
with Perf_Probe;
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
   --  METRICSVIEW_WIDTH / METRICSVIEW_HEIGHT: another window size, for
   --  timings only (PERF-011: per-pixel figures compare at one size; the
   --  scripted checks assume the default).
   WIDTH : constant Positive := Positive'Value (Ada.Environment_Variables.Value ("METRICSVIEW_WIDTH", "1440"));
   HEIGHT : constant Positive := Positive'Value (Ada.Environment_Variables.Value ("METRICSVIEW_HEIGHT", "900"));
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

   --  The dashboards folder, in memory: what the data thread's worker
   --  reads and writes on CuBit (Dash_Jobs.Worker's formals).
   package SU renames Ada.Strings.Unbounded;
   MAXIMUM_TEST_FILES : constant := 8;
   type Test_File is record
      Leaf, Content : SU.Unbounded_String;
      Modified : Unsigned_64 := 0;
   end record;
   Folder_Files : array (1 .. MAXIMUM_TEST_FILES) of Test_File;
   Folder_Used : Natural range 0 .. MAXIMUM_TEST_FILES := 0;
   Folder_Clock : Unsigned_64 := 0;
   function File_Index (Leaf : String) return Natural is
   begin
      for K in 1 .. Folder_Used loop
         if SU.To_String (Folder_Files (K).Leaf) = Leaf then
            return K;
         end if;
      end loop;
      return 0;
   end File_Index;
   procedure Test_List (Into : out Dash_View.Folder_Listing) is
      Accepted : Boolean;
   begin
      Into := (Ready => True, others => <>);
      for K in 1 .. Folder_Used loop
         Into.Count := Into.Count + 1;
         CuBit.File_Selection.Set (Into.Entries (Into.Count).Leaf, SU.To_String (Folder_Files (K).Leaf), Accepted);
         Into.Entries (Into.Count).Modified := Folder_Files (K).Modified;
      end loop;
   end Test_List;
   procedure Test_Read (Leaf : String; Into : out String; Got : out Natural; Result : out Dash_Jobs.File_Result) is
      K : constant Natural := File_Index (Leaf);
   begin
      Into := [others => ' '];
      Got := 0;
      if K = 0 then
         Result := Dash_Jobs.File_Missing;
         return;
      end if;
      declare
         Bytes : constant String := SU.To_String (Folder_Files (K).Content);
      begin
         Got := Bytes'Length;
         Into (Into'First .. Into'First + Got - 1) := Bytes;
         Result := Dash_Jobs.File_Done;
      end;
   end Test_Read;
   procedure Test_Write (Leaf, Bytes : String; Result : out Dash_Jobs.File_Result) is
      K : Natural := File_Index (Leaf);
   begin
      if K = 0 then
         Folder_Used := Folder_Used + 1;
         K := Folder_Used;
         Folder_Files (K).Leaf := SU.To_Unbounded_String (Leaf);
      end if;
      Folder_Clock := Folder_Clock + 1;
      Folder_Files (K).Content := SU.To_Unbounded_String (Bytes);
      Folder_Files (K).Modified := Folder_Clock;
      Result := Dash_Jobs.File_Done;
   end Test_Write;
   function Test_Exists (Leaf : String) return Boolean is (File_Index (Leaf) /= 0);
   package Jobs is new Dash_Jobs.Worker (Test_List, Test_Read, Test_Write, Test_Exists);
   --  Saving as the native data thread does (Dash_Runner.Save_Step), into
   --  the folder above: only where a test says so (the editor's checks
   --  take and end saves themselves).
   Autosave : Boolean := False;
   procedure Save_Step is
      Due : Boolean;
      Generation : Natural;
      Result : Dash_Jobs.File_Result;
      Place : constant String := Dash_Model.Dashboards_Place;
   begin
      Dash_View.Take_Save (View, Due, Generation);
      if Due then
         declare
            Name : constant String := Dash_View.Save_Name (View);
         begin
            Test_Write (Name (Name'First + Place'Length .. Name'Last), Dash_View.Save_Source (View) & ASCII.LF, Result);
            Dash_View.End_Save (View, Generation, Saved => True);
         end;
      end if;
   end Save_Step;

   procedure Frame is
      Damage : CuBit.UI.Rect := (others => 0);
   begin
      Feed.Set_Clock (Now);
      --  Both threads' steps, in order, on the test's one thread.
      Dash_View.Data_Step (View);
      if Autosave then
         Save_Step;
      end if;
      Jobs.Step (View);
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
   --  The picker's rules and the edits it makes, on values: every view
   --  and source a panel can be given is a Dashboard value that reads
   --  back as given, a pick undoes as one edit, and a new panel goes in
   --  the first free place without moving another.
   procedure Model_Picks is
      use type Dash_Choices.View_Choice;
      use type Dash_Model.Match_Kind;
      use type Dash_Grid.Placement;
      Original, Edited, Again : Dash_Model.Dashboard;
      Result : Dash_Model.Load_Result;
      Done : Boolean;
      At_Index : Dash_Model.Panel_Index;
      Where : Dash_Grid.Placement;
      Before, After : Dash_Grid.Placements;
      Rows : Dash_Grid.Row_Count;
      Value_Top : constant := 400;
      function Grid (Widths : String) return Dash_Model.Dashboard is
         D : Dash_Model.Dashboard;
         R : Dash_Model.Load_Result;
      begin
         Dash_Model.Load ("(Dashboard title => ""g"" time => (Dashboard_Time span_ms => 60000) panels => [" & Widths
                          & "])", D, R);
         Check (R.Success, "a grid dashboard: " & R.Message (1 .. R.Message_Length));
         return D;
      end Grid;
      function Card (Width, Height : Positive) return String is
        ("(Panel title => ""c"" width =>" & Width'Image & " height =>" & Height'Image
         & " kind => (Panel_Kind.Stat (Stat_Panel statistic => Metric_Statistic.P99)) series => (list-of Panel_Series))");
   begin
      Dash_Model.Load (Dash_Model.DEFAULT_SOURCE, Original, Result);
      Edited := Original;
      Dash_Edit.Start (Edited);
      for Choice in Dash_Choices.View_Choice loop
         declare
            Item : constant Dash_Model.Panel := Dash_Choices.With_View (Edited.Panels (1), Choice, Value_Top);
            Got : Dash_Choices.View_Choice;
            Found : Boolean;
         begin
            Dash_Edit.Replace (Edited, 1, Item, Done);
            Dash_Model.Load (Dash_Model.To_Source (Edited), Again, Result);
            Dash_Choices.View_Of (Again.Panels (1), Got, Found);
            Check (Done and then Result.Success and then Found and then Got = Choice,
                   "the view " & Choice'Image & " is a Dashboard value and reads back: "
                   & Result.Message (1 .. Result.Message_Length));
         end;
      end loop;
      Dash_Edit.Replace
        (Edited, 2, Dash_Choices.With_Source (Edited.Panels (2), (Dash_Model.To_Text ("desktop.latency."),
                                                                    Dash_Model.Prefix)), Done);
      Dash_Model.Load (Dash_Model.To_Source (Edited), Again, Result);
      Check (Done and then Result.Success and then Again.Panels (2).Series_Used = 1
             and then Again.Panels (2).Series (1).Match = Dash_Model.Prefix
             and then Dash_Model.Image (Again.Panels (2).Series (1).Name) = "desktop.latency."
             and then Dash_Model.Image (Again.Panels (2).Title) = "desktop.latency.*, p99",
             "a prefix source reads back, the title follows it: " & Dash_Model.Image (Again.Panels (2).Title));
      while Dash_Edit.Can_Undo loop
         Dash_Edit.Undo (Edited, Done);
      end loop;
      Check (Dash_Model.To_Source (Edited) = Dash_Model.To_Source (Original), "every pick undoes, one edit each");
      --  A new panel: first free place, nothing else moves; an empty
      --  series list is a Dashboard value.
      Dash_Grid.Pack (Dash_Grid.Sizes_Of (Edited), Edited.Panels_Used, Before, Rows);
      Dash_Grid.First_Free (Edited, Dash_Choices.New_Width, Dash_Choices.New_Height, At_Index, Where);
      Dash_Edit.Insert (Edited, Dash_Choices.New_Panel, At_Index, Done);
      Dash_Grid.Pack (Dash_Grid.Sizes_Of (Edited), Edited.Panels_Used, After, Rows);
      Check (Done and then Edited.Panels_Used = Original.Panels_Used + 1 and then After (At_Index) = Where
             and then (for all P in 1 .. Original.Panels_Used =>
                         After ((if P < At_Index then P else P + 1)) = Before (P)),
             "a new panel takes the first free place and moves no other (at" & At_Index'Image & ", column"
             & Where.Column'Image & " row" & Where.Row'Image & ")");
      Dash_Model.Load (Dash_Model.To_Source (Edited), Again, Result);
      Check (Result.Success and then Again.Panels (At_Index).Series_Used = 0,
             "a new panel without series is a Dashboard value: " & Result.Message (1 .. Result.Message_Length));
      Dash_Edit.Undo (Edited, Done);
      Check (Dash_Model.To_Source (Edited) = Dash_Model.To_Source (Original), "adding a panel undoes");
      --  Hand-made grids: the hole beside a panel, the row under a full one.
      declare
         Beside : constant Dash_Model.Dashboard := Grid (Card (8, 2));
         Under : constant Dash_Model.Dashboard := Grid (Card (12, 2) & " " & Card (4, 2));
         Hole : constant Dash_Model.Dashboard := Grid (Card (8, 4) & " " & Card (4, 2) & " " & Card (12, 2));
      begin
         Dash_Grid.First_Free (Beside, 4, 3, At_Index, Where);
         Check (At_Index = 2 and then Where = (8, 0), "a 4-wide panel goes beside an 8-wide one");
         Dash_Grid.First_Free (Under, 4, 3, At_Index, Where);
         Check (At_Index = 3 and then Where = (4, 2), "and beside a 4-wide one under a full row");
         Dash_Grid.First_Free (Hole, 4, 2, At_Index, Where);
         Check (At_Index = 3 and then Where = (8, 2), "and into the hole under a short one, before the row below");
      end;
   end Model_Picks;
begin
   Update_Codec;
   Model_Edits;
   Model_Picks;
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
      --  Zoom keys as a person presses them: in, out, a frame between
      --  (the frame's own refresh is not the key's cost).
      declare
         Total, Worst, Handled : Natural := 0;
         Rounds : constant Natural :=
           (if Ada.Environment_Variables.Exists ("METRICSVIEW_ZOOM_ROUNDS")
            then Natural'Value (Ada.Environment_Variables.Value ("METRICSVIEW_ZOOM_ROUNDS")) else 40);
      begin
         for I in 1 .. Rounds loop
            declare
               Damage : CuBit.UI.Rect := (others => 0);
               Started : constant Ada.Calendar.Time := Ada.Calendar.Clock;
               Taken : Natural;
            begin
               Dash_View.Handle (View, (Kind => Dash_View.Key_Press,
                                        Pressed => (if I mod 2 = 1 then Dash_View.Zoom_In else Dash_View.Zoom_Out),
                                        others => <>), Damage);
               Handled := Handled + Elapsed_Us (Started);
               if not CuBit.UI.Is_Empty (Damage) then
                  Dash_View.Render (View, CuBit.UI.With_Clip (Whole, Damage), Damage);
               end if;
               Taken := Elapsed_Us (Started);
               Total := Total + Taken;
               Worst := Natural'Max (Worst, Taken);
            end;
            Frame;
         end loop;
         Put_Line ("sequence zoom keys:" & Rounds'Image & " events, mean" & Natural'Image (Total / Rounds)
                   & " us (handle" & Natural'Image (Handled / Rounds) & "), worst" & Worst'Image
                   & " us (handle + render)");
      end;
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
   --  A new scheme repaints the window (the app gets a configure event).
   Repaint_All;

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
      Check (Dash_View.Save_Name (View) = "@nvme:0/Documents/Dashboards/display-latency.ccl", "a dashboard's file is its title");
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
      Interact (Key (Dash_View.Show_Keys), "keys");
      Check (not Dash_View.Source_Showing (View), "? shows the keys instead of the CCL");
      for I in 1 .. 4 loop
         Generate (Now, Now + 4 * FRAME_US, 0);
         Now := Now + 4 * FRAME_US;
         Frame;
      end loop;
      Check_Coherent ("edit-keys-live");
      Repaint_All;
      Save_PPM ("11-keys");
      Interact (Key (Dash_View.Show_Keys), "hide keys");
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
         Dash_View.End_Save (View, Generation, Saved => False,
                             Reason => "Can't save: this app has no folder to save dashboards in");
         --  The panel brought back by undo reads its series again (its
         --  tracks went with it): two steps, a port and then a fetch.
         for I in 1 .. 3 loop
            Frame;
         end loop;
         Check (Dash_View.Resolved_Tracks (View) = Dash_View.Track_Total (View), "a panel brought back reads again");
         Repaint_All;
         Save_PPM ("12-cannot-save");
         Check (Dash_View.Saving_State (View) = Dash_View.Not_Saved, "a failed save is shown");
         Dash_View.Take_Save (View, Due, Generation);
         Check (not Due, "a failed save is not retried until the next edit");
      end;
      Frame;
      Check_Coherent ("edit-undone");
   end;

   --  Editor keys as the headless test presses them (select, move and size
   --  back and forth), each with a frame after it: per key, its handle and
   --  render. Then undone, and the editor left.
   Interact ((Kind => Dash_View.Key_Press, Pressed => Dash_View.Edit, others => <>), "edit again");
   declare
      type Edit_Key is record
         Pressed : Dash_View.Key;
         Shift : Boolean;
         Name : access constant String;
      end record;
      N_Tab : aliased constant String := "tab";
      N_Right : aliased constant String := "right";
      N_Left : aliased constant String := "left";
      N_Wider : aliased constant String := "shift-right";
      N_Narrower : aliased constant String := "shift-left";
      Keys : constant array (1 .. 5) of Edit_Key :=
        [(Dash_View.Next_Panel, False, N_Tab'Access), (Dash_View.Right, False, N_Right'Access),
         (Dash_View.Left, False, N_Left'Access), (Dash_View.Right, True, N_Wider'Access),
         (Dash_View.Left, True, N_Narrower'Access)];
      Totals, Worsts : array (Keys'Range) of Natural := [others => 0];
      Rounds : constant := 8;
   begin
      for Round in 1 .. Rounds loop
         for K in Keys'Range loop
            declare
               Damage : CuBit.UI.Rect := (others => 0);
               Started : constant Ada.Calendar.Time := Ada.Calendar.Clock;
               Taken : Natural;
            begin
               Dash_View.Handle (View, (Kind => Dash_View.Key_Press, Pressed => Keys (K).Pressed, Shift => Keys (K).Shift, others => <>), Damage);
               if not CuBit.UI.Is_Empty (Damage) then
                  Dash_View.Render (View, CuBit.UI.With_Clip (Whole, Damage), Damage);
               end if;
               Taken := Elapsed_Us (Started);
               Totals (K) := Totals (K) + Taken;
               Worsts (K) := Natural'Max (Worsts (K), Taken);
            end;
            Frame;
         end loop;
      end loop;
      for K in Keys'Range loop
         Put_Line ("sequence edit " & Keys (K).Name.all & ":" & Natural'Image (Rounds) & " keys, mean"
                   & Natural'Image (Totals (K) / Rounds) & " us, worst" & Worsts (K)'Image
                   & " us (handle + render)");
      end loop;
      Check_Coherent ("edit-keys");
      --  Back to the layout the checks below start from.
      while Dash_Edit.Can_Undo loop
         Interact ((Kind => Dash_View.Key_Press, Pressed => Dash_View.Undo, others => <>), "undo the key sequence");
      end loop;
   end;
   Interact ((Kind => Dash_View.Key_Press, Pressed => Dash_View.Escape, others => <>), "done again");
   Frame;

   --  MET-002's menu bar, a panel's source picker, a new panel, and the
   --  dashboards folder (save as, open, a file that does not check).
   declare
      use type Dash_Choices.View_Choice;
      use type Dash_Model.Match_Kind;
      use type CuBit.UI.Rect;
      function Key (K : Dash_View.Key; Shift, Control, Alt : Boolean := False; Letter : Character := ' ')
        return Dash_View.Event is
        (Kind => Dash_View.Key_Press, Pressed => K, Shift => Shift, Control => Control, Alt => Alt,
         Letter => Letter, others => <>);
      procedure Click (X, Y : Natural; What : String) is
      begin
         Interact ((Kind => Dash_View.Pointer_Press, X => X, Y => Y, others => <>), What & " press");
         Interact ((Kind => Dash_View.Pointer_Release, X => X, Y => Y, others => <>), What & " release");
      end Click;
      procedure Type_Text (Text : String) is
      begin
         for C of Text loop
            Interact ((Kind => Dash_View.Text_Input, Character_Value => C, others => <>), "type");
         end loop;
      end Type_Text;
      Parsed : Dash_Model.Dashboard;
      function Parse return Boolean is
      begin
         Dash_Model.Load (Dash_View.Dashboard_Source (View), Parsed, Loaded);
         return Loaded.Success;
      end Parse;
      function Choice_Index (Field : Dash_View.Picker_Field; Caption : String) return Natural is
      begin
         for I in 1 .. Dash_View.Picker_Choices (View, Field) loop
            if Dash_View.Picker_Choice (View, Field, I) = Caption then
               return I;
            end if;
         end loop;
         return 0;
      end Choice_Index;
      --  Through the open list: highlight Caption, Enter commits (one edit).
      procedure Choose (Field : Dash_View.Picker_Field; Caption : String) is
         Want : constant Natural := Choice_Index (Field, Caption);
         Have : constant Natural := Dash_View.Picker_Selection (View, Field);
      begin
         Check (Want /= 0, "the picker offers " & Caption);
         Interact (Key (Dash_View.Enter), "picker list open");
         for I in 1 .. abs (Want - Have) loop
            Interact (Key ((if Want > Have then Dash_View.Down else Dash_View.Up)), "picker list move");
         end loop;
         Interact (Key (Dash_View.Enter), "picker commit");
      end Choose;
      Before_Source : constant String := Dash_View.Dashboard_Source (View);
      Bar : constant CuBit.UI.Rect := Dash_View.Menu_Bar_Bounds (View);
   begin
      Check (not CuBit.UI.Is_Empty (Bar) and then Bar.y = 0, "the menu bar heads the window");
      --  File, by the pointer: a press opens it, items light up under it.
      Click (Bar.x + 12, Bar.y + Bar.h / 2, "menu File");
      Check (Dash_View.Open_Menu (View) = 1, "a press on File opens it");
      Frame;
      Check_Coherent ("menu-open");
      Repaint_All;
      Save_PPM ("13-menu-file");
      declare
         Popup : constant CuBit.UI.Rect := Dash_View.Menu_Popup_Bounds (View);
         Total, Worst : Natural := 0;
         Steps : constant := 8;
         Row : constant := 29;
      begin
         for I in 0 .. Steps - 1 loop
            declare
               Damage : CuBit.UI.Rect := (others => 0);
               Started : constant Ada.Calendar.Time := Ada.Calendar.Clock;
               Taken : Natural;
            begin
               Dash_View.Handle (View, (Kind => Dash_View.Pointer_Move, X => Popup.x + 40,
                                        Y => Popup.y + Row / 2 + I * Row, others => <>), Damage);
               if not CuBit.UI.Is_Empty (Damage) then
                  Dash_View.Render (View, CuBit.UI.With_Clip (Whole, Damage), Damage);
               end if;
               Taken := Elapsed_Us (Started);
               Total := Total + Taken;
               Worst := Natural'Max (Worst, Taken);
            end;
         end loop;
         Put_Line ("sequence menu hover:" & Natural'Image (Steps) & " moves, mean" & Natural'Image (Total / Steps)
                   & " us, worst" & Worst'Image & " us (handle + render)");
      end;
      Check_Coherent ("menu-hover");
      Interact (Key (Dash_View.Right), "menu right");
      Check (Dash_View.Open_Menu (View) = 2, "Right opens Edit");
      Check_Coherent ("menu-edit");
      Interact (Key (Dash_View.Escape), "menu escape");
      Check (Dash_View.Open_Menu (View) = 0, "Esc closes the menu");
      Frame;
      Check_Coherent ("menu-closed");
      Interact (Key (Dash_View.Menu_Bar), "F10");
      Check (Dash_View.Open_Menu (View) = 1, "F10 opens the menu bar");
      Interact (Key (Dash_View.Escape), "menu escape");
      Interact (Key (Dash_View.Letter_Key, Alt => True, Letter => 'v'), "Alt+V");
      Check (Dash_View.Open_Menu (View) = 3, "Alt+V opens View");
      Interact (Key (Dash_View.Show_Source, Letter => 'c'), "View > Show CCL by its letter");
      Check (Dash_View.Open_Menu (View) = 0 and then Dash_View.Source_Showing (View), "a menu item runs its command");
      Interact (Key (Dash_View.Show_Source, Letter => 'c'), "hide CCL");
      Frame;
      Check_Coherent ("menu-command");

      --  A panel's source picker: its button, the store's series, a pick.
      declare
         Button : constant CuBit.UI.Rect := Dash_View.Source_Button_Bounds (View, 1);
      begin
         Click (Button.x + Button.w / 2, Button.y + Button.h / 2, "picker button");
      end;
      Check (Dash_View.Picker_Panel (View) = 1, "the source button opens the panel's picker");
      Frame;
      Frame;
      Check (Dash_View.Picker_Choices (View, Dash_View.Series_Field) > 10
             and then Choice_Index (Dash_View.Series_Field, "desktop.latency.*  (10 series)") /= 0,
             "the picker lists the store's series and their groups:"
             & Dash_View.Picker_Choices (View, Dash_View.Series_Field)'Image);
      Check_Coherent ("picker-open");
      Repaint_All;
      Save_PPM ("14-picker");
      Interact (Key (Dash_View.Enter), "picker list open");
      Check_Coherent ("picker-list");
      Repaint_All;
      Save_PPM ("15-picker-list");
      Interact (Key (Dash_View.Escape), "picker list cancel");
      Check (Dash_View.Picker_Panel (View) = 1, "Esc closes the list first");
      Choose (Dash_View.Series_Field, "desktop.latency.submit_to_ready");
      Check (Parse and then Parsed.Panels (1).Series_Used = 1
             and then Dash_Model.Image (Parsed.Panels (1).Series (1).Name) = "desktop.latency.submit_to_ready"
             and then Parsed.Panels (1).Series (1).Match = Dash_Model.Exact
             and then Dash_Model.Image (Parsed.Panels (1).Title) = "desktop.latency.submit_to_ready, p50 and p99",
             "a picked series is in the dashboard's CCL value, which checks: " & Loaded.Message (1 .. Loaded.Message_Length));
      for I in 1 .. 3 loop
         Frame;
      end loop;
      Check (Dash_View.Resolved_Tracks (View) = Dash_View.Track_Total (View)
             and then Dash_View.Columns_With_Data (View) > 0, "the re-sourced panel reads its series");
      Check_Coherent ("picker-picked");
      Interact (Key (Dash_View.Next_Panel), "picker tab");
      Interact (Key (Dash_View.Down), "picker view down");
      declare
         Shown : Dash_Choices.View_Choice;
      begin
         Check (Dash_View.Panel_View (View, 1, Shown) and then Shown = Dash_Choices.Line_Tail,
                "Down on the view field shows the next view");
      end;
      Frame;
      Check_Coherent ("picker-view");
      Interact (Key (Dash_View.Undo, Control => True), "picker undo view");
      declare
         Shown : Dash_Choices.View_Choice;
      begin
         Check (Dash_View.Panel_View (View, 1, Shown) and then Shown = Dash_Choices.Lines_Median_And_Tail,
                "Ctrl+Z undoes the view, one edit");
      end;
      Interact (Key (Dash_View.Undo, Control => True), "picker undo series");
      Check (Dash_View.Dashboard_Source (View) = Before_Source, "and again the series: the dashboard as it was");
      Interact (Key (Dash_View.Escape), "picker close");
      Check (Dash_View.Picker_Panel (View) = 0, "Esc closes the picker");
      for I in 1 .. 3 loop
         Frame;
      end loop;
      Check_Coherent ("picker-closed");

      --  A new panel: the first free place, its picker open, then picked.
      declare
         D : Dash_Model.Dashboard;
         At_Index : Dash_Model.Panel_Index;
         Where : Dash_Grid.Placement;
         Count : constant Natural := Dash_View.Panel_Count (View);
         type Rect_List is array (1 .. Dash_Model.Maximum_Panels) of CuBit.UI.Rect;
         Before_Rects : Rect_List := [others => (others => 0)];
         Shown : Dash_Choices.View_Choice;
      begin
         Check (Parse, "the dashboard parses");
         D := Parsed;
         Dash_Grid.First_Free (D, Dash_Choices.New_Width, Dash_Choices.New_Height, At_Index, Where);
         for P in 1 .. Count loop
            Before_Rects (P) := Dash_View.Panel_Bounds (View, P);
         end loop;
         Interact (Key (Dash_View.Add_Panel, Letter => 'a'), "add panel");
         Check (Dash_View.Panel_Count (View) = Count + 1 and then Dash_View.Picker_Panel (View) = At_Index
                and then Dash_View.Selected (View) = At_Index and then Dash_View.Editing (View),
                "A adds a panel at the first free place (" & At_Index'Image & "), selected, its picker open");
         Check ((for all P in 1 .. Count =>
                   Dash_View.Panel_Bounds (View, (if P < At_Index then P else P + 1)) = Before_Rects (P)),
                "no other panel moved");
         Frame;
         Check_Coherent ("added");
         Choose (Dash_View.Series_Field, "desktop.latency.input_to_ready");
         Interact (Key (Dash_View.Next_Panel), "picker tab");
         Choose (Dash_View.View_Field, Dash_Choices.Caption (Dash_Choices.Number_Tail));
         Check (Dash_View.Panel_View (View, At_Index, Shown) and then Shown = Dash_Choices.Number_Tail
                and then Parse and then Dash_Model.Image (Parsed.Panels (At_Index).Title)
                = "desktop.latency.input_to_ready, p99", "the new panel shows the series picked as a number");
         Interact (Key (Dash_View.Escape), "picker close");
         Interact (Key (Dash_View.Escape), "editor done");
         for I in 1 .. 3 loop
            Frame;
         end loop;
         Check (Dash_View.Resolved_Tracks (View) = Dash_View.Track_Total (View), "the new panel's series resolves");
         Check_Coherent ("added-picked");
         Repaint_All;
         Save_PPM ("16-added-panel");
      end;

      --  Save as, then open: the same dashboard. A name taken is refused.
      Autosave := True;
      Frame;
      Interact (Key (Dash_View.Save_As, Shift => True, Control => True, Letter => 's'), "save as");
      Frame;
      Check (Dash_View.Dialog_Open (View), "Ctrl+Shift+S shows the save dialog");
      Check_Coherent ("save-dialog");
      Repaint_All;
      Save_PPM ("17-save-as");
      Type_Text ("latency-copy");
      Interact (Key (Dash_View.Enter), "save as enter");
      Frame;
      declare
         Saved_Source : constant String := Dash_View.Dashboard_Source (View);
         K : constant Natural := File_Index ("latency-copy.ccl");
         Bad : Dash_Model.Load_Result;
         Ignored : Dash_Model.Dashboard;
         Broken : constant String := "(Dashboard title => ""Broken"" panels => [])";
         Written : Dash_Jobs.File_Result;
         function Contains (S, Part : String) return Boolean is
           (for some I in S'First .. S'Last - Part'Length + 1 => S (I .. I + Part'Length - 1) = Part);
      begin
         Check (not Dash_View.Dialog_Open (View) and then Dash_View.Current_File (View) = "latency-copy.ccl"
                and then K /= 0 and then SU.To_String (Folder_Files (K).Content) = Saved_Source & ASCII.LF,
                "Save As writes the dashboard's CCL as the new file, which becomes its own: "
                & Dash_View.Notice (View));
         Interact (Key (Dash_View.Save_As, Shift => True, Control => True, Letter => 's'), "save as again");
         Frame;
         Interact (Key (Dash_View.Select_All, Control => True, Letter => 'a'), "select all");
         Type_Text ("latency-copy");
         Interact (Key (Dash_View.Enter), "save as taken");
         Frame;
         Check (Dash_View.Dialog_Open (View), "a name already in the folder is refused (the dialog stays)");
         Interact (Key (Dash_View.Escape), "dialog cancel");
         Check (not Dash_View.Dialog_Open (View), "Esc cancels the dialog");
         Frame;
         Check_Coherent ("dialog-closed");
         Interact (Key (Dash_View.New_Dashboard, Control => True, Letter => 'n'), "new dashboard");
         Frame;
         Check (Dash_View.Title (View) = Dash_Choices.New_Dashboard_Title and then Dash_View.Picker_Panel (View) = 1
                and then Dash_View.Current_File (View) = "new-dashboard.ccl",
                "Ctrl+N: a new dashboard, its first panel's picker open");
         Interact (Key (Dash_View.Escape), "picker close");
         Interact (Key (Dash_View.Escape), "editor done");
         Frame;
         Check_Coherent ("new-dashboard");
         Interact (Key (Dash_View.Open_Dashboard, Control => True, Letter => 'o'), "open");
         Frame;
         Check (Dash_View.Dialog_Open (View), "Ctrl+O shows the open dialog");
         Interact (Key (Dash_View.Next_Panel), "dialog tab");
         Interact (Key (Dash_View.Select_All, Control => True, Letter => 'a'), "select all");
         Type_Text ("latency-copy.ccl");
         Interact (Key (Dash_View.Enter), "open enter");
         Frame;
         Check (not Dash_View.Dialog_Open (View) and then Dash_View.Dashboard_Source (View) = Saved_Source
                and then Dash_View.Current_File (View) = "latency-copy.ccl",
                "Save As then Open gives the same dashboard: " & Dash_View.Notice (View));
         for I in 1 .. 3 loop
            Frame;
         end loop;
         Check (Dash_View.Resolved_Tracks (View) = Dash_View.Track_Total (View), "the opened dashboard reads");
         Check_Coherent ("opened");
         --  A file that does not check: refused with the checker's words.
         Test_Write ("broken.ccl", Broken, Written);
         Dash_Model.Load (Broken, Ignored, Bad);
         Check (not Bad.Success, "the broken file does not check");
         Interact (Key (Dash_View.Open_Dashboard, Control => True, Letter => 'o'), "open");
         Frame;
         Interact (Key (Dash_View.Next_Panel), "dialog tab");
         Interact (Key (Dash_View.Select_All, Control => True, Letter => 'a'), "select all");
         Type_Text ("broken.ccl");
         Interact (Key (Dash_View.Enter), "open broken");
         Frame;
         Check (Dash_View.Dialog_Open (View) and then Contains (Dash_View.Notice (View), Bad.Message (1 .. Bad.Message_Length))
                and then Dash_View.Dashboard_Source (View) = Saved_Source,
                "a file that does not check is refused with the checker's message, the dashboard stays: "
                & Dash_View.Notice (View));
         Repaint_All;
         Save_PPM ("18-refused-file");
         Interact (Key (Dash_View.Escape), "dialog cancel");
         Frame;
         Check_Coherent ("refused-file");
      end;
      Autosave := False;
   end;

   --  Repaint cost of the whole window.
   declare
      Started : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Rounds : constant := 20;
   begin
      for I in 1 .. Rounds loop
         Repaint_All;
      end loop;
      declare
         Took, Best : Unsigned_64 := Unsigned_64'Last;
         Probe_Started : Ada.Calendar.Time;
      begin
         for I in 1 .. Rounds loop
            Probe_Started := Ada.Calendar.Clock;
            Dash_View.Probe_Full_Redraw (View, Took);
            Best := Unsigned_64'Min (Best, Unsigned_64 (Elapsed_Us (Probe_Started)));
         end loop;
         Put_Line ("full redraw into the retained frame (the native probe's work): best" & Best'Image & " us");
      end;
      Put_Line ("full redraw:" & Natural'Image (Elapsed_Us (Started) / Rounds) & " us at"
                & Natural'Image (WIDTH) & "x" & Natural'Image (HEIGHT));
      --  The native probe redraws once every report, after the app was idle:
      --  caches and predictors cold. The same here, for a like comparison.
      declare
         Took : Unsigned_64;
         Probe_Started : Ada.Calendar.Time;
         IDLE : constant Duration := 0.2;
         COLD_ROUNDS : constant := 5;
         Cold : array (1 .. COLD_ROUNDS) of Unsigned_64;
         Swap : Unsigned_64;
      begin
         for I in Cold'Range loop
            delay IDLE;
            Probe_Started := Ada.Calendar.Clock;
            Dash_View.Probe_Full_Redraw (View, Took);
            Cold (I) := Unsigned_64 (Elapsed_Us (Probe_Started));
         end loop;
         for I in Cold'Range loop
            for J in I + 1 .. Cold'Last loop
               if Cold (J) < Cold (I) then
                  Swap := Cold (I);
                  Cold (I) := Cold (J);
                  Cold (J) := Swap;
               end if;
            end loop;
         end loop;
         Put_Line ("full redraw after 200 ms idle (cold, as the native probe): median" & Cold (3)'Image
                   & " us, best" & Cold (1)'Image & " us");
      end;
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

   --  PERF-011: the platform probe the native app runs (PERF011=1).
   if Ada.Environment_Variables.Value ("PERF011", "0") = "1" then
      declare
         use type Ada.Real_Time.Time;
         Epoch : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
         function Probe_Now return Unsigned_64 is
           (Unsigned_64 (Ada.Real_Time.To_Duration (Ada.Real_Time.Clock - Epoch) * 1_000_000.0));
         procedure Probe_Redraw is
            Took : Unsigned_64;
         begin
            Dash_View.Probe_Full_Redraw (View, Took);
         end Probe_Redraw;
         procedure Probe_Print (Line : String) is
         begin
            Put_Line (Line);
         end Probe_Print;
         procedure Probe_Sleep (Ms : Natural) is
         begin
            delay Duration (Ms) / 1000.0;
         end Probe_Sleep;
         package Probe is new Perf_Probe (Probe_Now, Probe_Redraw, Probe_Print, Probe_Sleep);
      begin
         Probe.Run (Dash_Layers.Canvas);
      end;
   end if;

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
