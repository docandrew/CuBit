with Ada.Calendar;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.UI;
with CuBit.UI.Combo_Boxes;
with CuBit.UI.State;
with CuBit.Log_Records;
with Log_View; use Log_View;

--  The Logs app's view, hosted: records, keys and clicks in, state and
--  frames out. Clicks go through the toolkit's retained dispatch before the
--  view sees them, as CuBit.UI.App.Run does on CuBit.
procedure Log_Viewer_Tests is
   package LR renames CuBit.Log_Records;
   package CB renames CuBit.UI.Combo_Boxes;
   use type Ada.Streams.Stream_Element_Offset;
   use type LR.Severity;
   use type Controls.Pointer_Action;
   WIDTH : constant := 960;
   HEIGHT : constant := 600;
   type Pixels is array (0 .. WIDTH * HEIGHT - 1) of Unsigned_32;
   type Pixels_Access is access Pixels;
   Image : constant Pixels_Access := new Pixels'(others => 0);
   Canvas : constant CuBit.UI.Canvas :=
     (addr => Image.all'Address, width => WIDTH, height => HEIGHT, pitch => WIDTH * 4,
      clipEnabled => False, clip => (others => 0), others => <>);
   Bounds : constant CuBit.UI.Rect := (0, 0, WIDTH, HEIGHT);
   type View_Access is access View_State;
   State : constant View_Access := new View_State;
   type UI_Access is access CuBit.UI.State.UI_State;
   UI : constant UI_Access := new CuBit.UI.State.UI_State;
   type Map_Access is access Controls.Control_Map;
   Map : constant Map_Access := new Controls.Control_Map;
   Checks, Failures : Natural := 0;
   Redraw : Boolean;

   procedure Check (Condition : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Label);
      end if;
   end Check;

   function Made (Text : String; Level : LR.Severity) return LR.Log_Record is
      Result : constant LR.Decoded := LR.Make (Text, Level);
   begin
      return (if Result.Success then Result.Value else LR.Empty_Record);
   end Made;
   procedure Log (Time : Unsigned_64; Source : Unsigned_64; Level : LR.Severity; Text : String) is
   begin
      Add (State.all, Time, Source, Made (Text, Level));
   end Log;
   --  A frame, which also rebuilds the controls the next event is hit against.
   procedure Frame is
   begin
      Render (State.all, Canvas, Bounds, UI.all, Map.all);
   end Frame;
   procedure Key (Name : Key_Name; Shift : Boolean := False) is
   begin
      Handle (State.all, (Kind => Key_Event, Key => Name, Shift => Shift, others => <>), Map.all, Redraw);
      Frame;
   end Key;
   procedure Type_Text (Text : String) is
   begin
      for C of Text loop
         Handle (State.all, (Kind => Text_Event, Character_Value => C, others => <>), Map.all, Redraw);
         Frame;
      end loop;
   end Type_Text;
   procedure Pointer (Target : Controls.Control_ID; Action : Controls.Pointer_Action) is
      Box : constant CuBit.UI.Rect := Controls.Bounds (Map.all, Target);
      X : constant Natural := Box.x + Box.w / 2;
      Y : constant Natural := Box.y + Box.h / 2;
      Visual, Dispatched : Boolean;
   begin
      CuBit.UI.State.Set_Pointer (UI.all, X, Y, down => Action = Controls.Pointer_Press);
      Controls.Dispatch_Pointer (Map.all, Target, Action, X, Y, Visual, Dispatched);
      Handle (State.all, (Kind => Pointer_Event, Action => Action, X => X, Y => Y, others => <>), Map.all, Redraw);
   end Pointer;
   --  Press and release over a control, rendering between as App.Run does.
   procedure Click (Target : Controls.Control_ID) is
   begin
      Frame;
      Check (Controls.Bounds (Map.all, Target).w > 0, "control" & Target'Image & " is on screen");
      Pointer (Target, Controls.Pointer_Press);
      Frame;
      Pointer (Target, Controls.Pointer_Release);
      Frame;
   end Click;

   procedure Save (Name : String) is
      use Ada.Streams.Stream_IO;
      File : File_Type;
      Header : constant String := "P6" & ASCII.LF & "960 600" & ASCII.LF & "255" & ASCII.LF;
      Row : Ada.Streams.Stream_Element_Array (1 .. WIDTH * 3);
   begin
      Frame;
      Create (File, Out_File, "build/" & Name & ".ppm");
      for C of Header loop
         Ada.Streams.Stream_Element_Array'Write
           (Stream (File), [1 => Ada.Streams.Stream_Element (Character'Pos (C))]);
      end loop;
      for Y in 0 .. HEIGHT - 1 loop
         for X in 0 .. WIDTH - 1 loop
            declare
               P : constant Unsigned_32 := Image (Y * WIDTH + X);
               Base : constant Ada.Streams.Stream_Element_Offset := Ada.Streams.Stream_Element_Offset (X * 3);
            begin
               Row (Base + 1) := Ada.Streams.Stream_Element (Shift_Right (P, 16) and 16#FF#);
               Row (Base + 2) := Ada.Streams.Stream_Element (Shift_Right (P, 8) and 16#FF#);
               Row (Base + 3) := Ada.Streams.Stream_Element (P and 16#FF#);
            end;
         end loop;
         Write (File, Row);
      end loop;
      Close (File);
   end Save;

   NETSTACK : constant := 24;
   FILESYSTEM : constant := 18;
   DESKTOP : constant := 33;
   GAP_TEXT : constant String := "--- 12 records lost before delivery ---";
begin
   Initialize (State.all);
   Handle (State.all, (Kind => Resize, X => WIDTH, Y => HEIGHT, others => <>), Map.all, Redraw);
   Frame;
   Check (Total (State.all) = 0 and then Following (State.all), "starts empty and following");
   Save ("logs-empty");
   Set_Connection (State.all, Connected);
   --  Names for processes that never published are not taken.
   Name_Source (State.all, 77, "idle.svc");

   --  A boot's worth of records from three services.
   for I in 1 .. 60 loop
      Log (Unsigned_64 (I) * 137, (if I mod 3 = 0 then NETSTACK elsif I mod 3 = 1 then FILESYSTEM else DESKTOP),
           (if I mod 17 = 0 then LR.Error elsif I mod 7 = 0 then LR.Warning
            elsif I mod 2 = 0 then LR.Debug else LR.Information),
           "record" & Integer'Image (I) & (if I mod 3 = 0 then " tcp: connection accepted" else " ready"));
   end loop;
   --  Names arrive after the records, as procmgr's answer does.
   Name_Source (State.all, NETSTACK, "netstack.svc");
   Name_Source (State.all, FILESYSTEM, "filesystem.svc");
   Name_Source (State.all, DESKTOP, "desktop.svc");
   Name_Source (State.all, 77, "idle.svc");
   Add_Gap (State.all, 12);
   Log (9_000, NETSTACK, LR.Critical, "dhcp: no lease after 5 attempts");
   Check (Total (State.all) = 62 and then Shown (State.all) = 62, "every record and the gap are shown");
   Check (Selected_Text (State.all) = "dhcp: no lease after 5 attempts", "following selects the newest");
   Save ("logs-live");

   --  Moving pauses following; new records count as unseen; End resumes.
   Key (Up);
   Check (not Following (State.all), "moving up pauses following");
   Log (9_100, DESKTOP, LR.Information, "window opened");
   Check (Unseen (State.all) = 1, "a record arriving while paused is unseen");
   Key (End_Key);
   Check (Following (State.all) and then Selected_Text (State.all) = "window opened", "End follows again");
   --  The Pause button does the same.
   Click (FOLLOW_ID);
   Check (not Following (State.all), "Pause stops following");
   Click (FOLLOW_ID);
   Check (Following (State.all), "Follow resumes");

   --  Severity floor: 4 = Warning and above (gaps stay visible); the level
   --  combo box sets the same floor.
   Type_Text ("4");
   Check (Minimum (State.all) = LR.Warning, "4 sets the floor to Warning");
   Check (Shown (State.all) = 13, "8 warnings, 3 errors, the critical record and the gap: " &
          Natural'Image (Shown (State.all)));
   Save ("logs-warnings");
   Click (LEVEL_BASE);
   Save ("logs-level-popup");
   Click (CB.Choice_ID (LEVEL_BASE, 5));
   Check (Minimum (State.all) = LR.Error and then Shown (State.all) = 5,
          "the level combo box chose Errors and above: " & Natural'Image (Shown (State.all)));
   Click (LEVEL_BASE);
   Click (CB.Choice_ID (LEVEL_BASE, 1));
   Check (Minimum (State.all) = LR.Trace, "All levels again");

   --  Search: case-insensitive, in the message or the source's name.
   Type_Text ("/TCP");
   Check (Search_Text (State.all) = "TCP", "search text: " & Search_Text (State.all));
   Check (Shown (State.all) = 21, "records mentioning tcp, and the gap: " & Natural'Image (Shown (State.all)));
   Key (Enter);
   Save ("logs-search");
   Key (Backspace);
   Check (Search_Text (State.all) = "TCP", "Backspace outside the search field edits nothing");
   Key (Escape);
   Check (Shown (State.all) = 63 and then Search_Text (State.all) = "", "Esc clears every filter");
   --  Clicking the field focuses it.
   Click (SEARCH_ID);
   Type_Text ("dhcp");
   Check (Shown (State.all) = 2, "typed into the clicked field: " & Natural'Image (Shown (State.all)));
   Key (Escape);
   Key (Escape);
   Check (Search_Text (State.all) = "" and then Shown (State.all) = 63, "Esc in the field clears it");

   --  One service: from the combo box, or s for the selected record's.
   Click (SERVICE_BASE);
   Save ("logs-service-popup");
   --  The choices are the publishers, in the order they first published.
   Click (CB.Choice_ID (SERVICE_BASE, 4));
   Check (Service_Filter (State.all) = "netstack.svc", "service: " & Service_Filter (State.all));
   Check (Shown (State.all) = 22, "netstack's records and the gap: " & Natural'Image (Shown (State.all)));
   Save ("logs-service");
   --  The combo box keeps focus; Esc hands it back to the table, where s works.
   Key (Escape);
   Type_Text ("s");
   Check (Service_Filter (State.all) = "" and then Shown (State.all) = 63, "s again shows every service");

   --  Sorting: a header click sorts by that column, a second reverses it.
   Click (Header_ID (Level_Column));
   Check (Sorted_By (State.all) = Level_Column and then not Descending (State.all), "sorted by level");
   Check (Row_Text (State.all, 1) = "record 2 ready", "the first debug record leads: " & Row_Text (State.all, 1));
   Click (Header_ID (Level_Column));
   Check (Descending (State.all) and then Row_Text (State.all, 1) = GAP_TEXT and then
          Row_Text (State.all, 2) = "dhcp: no lease after 5 attempts",
          "descending: the gap, then the critical record: " & Row_Text (State.all, 2));
   Save ("logs-sorted");
   Click (Header_ID (Source_Column));
   Check (Sorted_By (State.all) = Source_Column and then Row_Text (State.all, 1) = "record 2 ready",
          "by source: desktop.svc first, oldest first: " & Row_Text (State.all, 1));
   Click (Header_ID (Time_Column));
   Check (Sorted_By (State.all) = Time_Column and then Row_Text (State.all, 1) = "record 1 ready",
          "by time again: " & Row_Text (State.all, 1));

   --  Time: the last minute before now, from the time combo box.
   Set_Time (State.all, 69_050);
   Click (TIME_BASE);
   Click (CB.Choice_ID (TIME_BASE, 2));
   Check (Window (State.all) = Last_Minute and then Shown (State.all) = 2,
          "the last minute: one record and the gap: " & Natural'Image (Shown (State.all)));
   Click (CLEAR_ID);
   Check (Window (State.all) = All_Time and then Shown (State.all) = 63, "Clear shows everything");

   --  A reused process number: records from before the new holder started
   --  keep the bare number instead of borrowing the new holder's name.
   Log (9_200, 41, LR.Warning, "timesync: no network authority; exiting");
   Name_Source (State.all, 41, "com.cubit.desktop", Started => 9_300);
   Log (9_400, 41, LR.Information, "desktop: ready");
   Type_Text ("/pid 41");
   Check (Shown (State.all) = 2, "the old holder's record and the gap match its bare number: " &
          Natural'Image (Shown (State.all)));
   Key (Escape);
   Key (Escape);
   Type_Text ("/com.cubit.desktop");
   Check (Shown (State.all) = 2, "only the new holder's record and the gap carry its name: " &
          Natural'Image (Shown (State.all)));
   Key (Escape);
   Key (Escape);
   --  An unnamed publisher, and the old holder of a reused number, can be
   --  chosen as services too.
   Log (9_500, 52, LR.Error, "crashed before procmgr knew its name");
   Click (SERVICE_BASE);
   Click (CB.Choice_ID (SERVICE_BASE, 7));
   Check (Service_Filter (State.all) = "pid 52" and then Row_Text (State.all, 2) = "crashed before procmgr knew its name",
          "an unnamed publisher is a choice: " & Service_Filter (State.all));
   Click (SERVICE_BASE);
   Click (CB.Choice_ID (SERVICE_BASE, 5));
   Check (Service_Filter (State.all) = "pid 41", "the reused number's old holder is a choice: " &
          Service_Filter (State.all));
   Key (Escape);
   Key (Escape);

   --  A record ingested from another node carries that node, searchable by
   --  its identity; this node's records say "local".
   Add (State.all, 9_600, 60, Made ("replicated from node b", LR.Information),
        (High => 16#00B0_0B00_DEAD_BEEF#, Low => 16#0000_0000_0000_0001#));
   Type_Text ("/00b00b00");
   Check (Shown (State.all) = 2 and then Row_Text (State.all, 2) = "replicated from node b",
          "a remote node's record found by node: " & Natural'Image (Shown (State.all)));
   Key (Escape);
   Key (Escape);
   Click (Header_ID (Node_Column));
   Check (Sorted_By (State.all) = Node_Column, "sortable by node");
   Save ("logs-node");
   Click (Header_ID (Time_Column));

   --  What logstore keeps: shown once known; a choice becomes one request
   --  for the platform; a refusal is explained in the status bar.
   Check (not Kept_Known (State.all), "what logstore keeps is unknown at first");
   Set_Kept (State.all, LR.Information);
   Check (Kept_Known (State.all) and then Kept (State.all) = LR.Information, "logstore keeps Information and up");
   Click (KEEP_BASE);
   Save ("logs-keep-popup");
   Click (CB.Choice_ID (KEEP_BASE, 2));
   declare
      Level : LR.Severity;
      Requested : Boolean;
   begin
      Take_Keep_Request (State.all, Level, Requested);
      Check (Requested and then Level = LR.Debug, "choosing Debug and above asks for it once");
      Take_Keep_Request (State.all, Level, Requested);
      Check (not Requested, "the request is taken only once");
   end;
   Set_Keep_Refused (State.all, "Not changed: this program needs log-control");
   Set_Kept (State.all, LR.Information);
   Check (Kept (State.all) = LR.Information, "a refused change shows what logstore still keeps");
   Save ("logs-keep-refused");
   Key (Escape);

   --  The ring keeps the newest MAXIMUM_RECORDS; the oldest leave first.
   for I in 1 .. MAXIMUM_RECORDS loop
      Log (10_000 + Unsigned_64 (I), FILESYSTEM, LR.Trace, "flood" & Integer'Image (I));
   end loop;
   Check (Total (State.all) = MAXIMUM_RECORDS, "the ring holds its capacity");
   Key (Home);
   Check (Selected_Text (State.all) = "flood 1", "the oldest kept record is the first flood record: " &
          Selected_Text (State.all));

   --  Cost, hosted (not a CuBit measurement): a full ring, then the native
   --  main's work each tick (64 records, a name refresh for 60 processes,
   --  a frame), sorted by time and then by source.
   declare
      use type Ada.Calendar.Time;
      Start : Ada.Calendar.Time;
      procedure Ticks (Label : String) is
      begin
         Start := Ada.Calendar.Clock;
         for Tick in 1 .. 50 loop
            for I in 1 .. 64 loop
               Log (20_000 + Unsigned_64 (Tick * 64 + I), Unsigned_64 (100 + I mod 30), LR.Information,
                    "tick" & Integer'Image (Tick) & " record" & Integer'Image (I));
            end loop;
            for P in 1 .. 60 loop
               Name_Source (State.all, Unsigned_64 (100 + P mod 30), "service" & Integer'Image (P mod 30));
            end loop;
            Frame;
         end loop;
         Ada.Text_IO.Put_Line
           ("TIME: " & Label & ":" &
            Integer'Image (Integer (Float (Ada.Calendar.Clock - Start) * 1000.0 / 50.0)) & " ms per tick");
      end Ticks;
      --  A heavy load: the ring full of long messages, then the native
      --  app's per-tick cap (1024 records) arriving each tick.
      procedure Heavy (Label : String) is
         Add_Time, Draw_Time : Duration := 0.0;
         Mark : Ada.Calendar.Time;
         Long : constant String (1 .. 200) := [others => 'x'];
      begin
         for Tick in 1 .. 10 loop
            Mark := Ada.Calendar.Clock;
            for I in 1 .. 1_024 loop
               Log (40_000 + Unsigned_64 (Tick * 1_024 + I), Unsigned_64 (100 + I mod 30), LR.Information,
                    "heavy" & Integer'Image (I) & " " & Long);
            end loop;
            Add_Time := Add_Time + (Ada.Calendar.Clock - Mark);
            Mark := Ada.Calendar.Clock;
            Frame;
            Draw_Time := Draw_Time + (Ada.Calendar.Clock - Mark);
         end loop;
         Ada.Text_IO.Put_Line
           ("TIME: heavy " & Label & ": add" & Integer'Image (Integer (Float (Add_Time) * 100.0)) &
            " ms, frame" & Integer'Image (Integer (Float (Draw_Time) * 100.0)) & " ms per tick");
      end Heavy;
      --  Frame cost at real display sizes (logical size times density).
      procedure Big (Label : String; W, H : Positive; Density : Positive) is
         type Big_Pixels is array (Natural range <>) of Unsigned_32;
         type Big_Access is access Big_Pixels;
         Store : constant Big_Access := new Big_Pixels (0 .. W * H * Density * Density - 1);
         Big_Canvas : constant CuBit.UI.Canvas :=
           (addr => Store.all'Address, width => W, height => H, pitch => W * Density * 4,
            densityNumerator => Density, densityDenominator => 1, clipEnabled => False,
            clip => (others => 0), others => <>);
         Mark : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         Elapsed : Duration;
         Big_Rounds : constant := 60;
      begin
         Handle (State.all, (Kind => Resize, X => W, Y => H, others => <>), Map.all, Redraw);
         for I in 1 .. Big_Rounds loop
            Render (State.all, Big_Canvas, (0, 0, W, H), UI.all, Map.all);
         end loop;
         Elapsed := Ada.Calendar.Clock - Mark;
         --  The last frame's physical pixels, for pixel comparisons.
         declare
            use Ada.Streams.Stream_IO;
            File : File_Type;
            Header : constant String :=
              "P6" & ASCII.LF & Integer'Image (W * Density) & Integer'Image (H * Density) & ASCII.LF & "255" & ASCII.LF;
         begin
            Create (File, Out_File, "build/frame-" & Integer'Image (Density) (2) & "x.ppm");
            for Ch of Header loop
               Ada.Streams.Stream_Element_Array'Write
                 (Stream (File), [1 => Ada.Streams.Stream_Element (Character'Pos (Ch))]);
            end loop;
            for P of Store.all loop
               Ada.Streams.Stream_Element_Array'Write
                 (Stream (File),
                  [Ada.Streams.Stream_Element (Shift_Right (P, 16) and 16#FF#),
                   Ada.Streams.Stream_Element (Shift_Right (P, 8) and 16#FF#),
                   Ada.Streams.Stream_Element (P and 16#FF#)]);
            end loop;
            Close (File);
         end;
         Ada.Text_IO.Put_Line
           ("TIME: frame " & Label & ":" & Integer'Image (Integer (Float (Elapsed) * 1_000_000.0 / Float (Big_Rounds))) &
            " us");
         Handle (State.all, (Kind => Resize, X => WIDTH, Y => HEIGHT, others => <>), Map.all, Redraw);
      end Big;
      --  A frame clipped to Damage, as CuBit.UI.App renders a repair region.
      procedure Clipped (Label : String; Damage : CuBit.UI.Rect) is
         W : constant := 1920;
         H : constant := 1080;
         type Big_Pixels is array (Natural range <>) of Unsigned_32;
         type Big_Access is access Big_Pixels;
         Store : constant Big_Access := new Big_Pixels (0 .. W * H * 4 - 1);
         Big_Canvas : constant CuBit.UI.Canvas :=
           (addr => Store.all'Address, width => W, height => H, pitch => W * 2 * 4,
            densityNumerator => 2, densityDenominator => 1, clipEnabled => True,
            clip => Damage, others => <>);
         Mark : Ada.Calendar.Time;
         ROUNDS : constant := 100;
      begin
         Handle (State.all, (Kind => Resize, X => W, Y => H, others => <>), Map.all, Redraw);
         Render (State.all, Big_Canvas, (0, 0, W, H), UI.all, Map.all);
         Mark := Ada.Calendar.Clock;
         for I in 1 .. ROUNDS loop
            Render (State.all, Big_Canvas, (0, 0, W, H), UI.all, Map.all);
         end loop;
         Ada.Text_IO.Put_Line
           ("TIME: damage " & Label & ":" &
            Integer'Image (Integer (Float (Ada.Calendar.Clock - Mark) * 1_000_000.0 / Float (ROUNDS))) & " us");
         Handle (State.all, (Kind => Resize, X => WIDTH, Y => HEIGHT, others => <>), Map.all, Redraw);
      end Clipped;
   begin
      Clipped ("4K, one table row", (8, 200, 1904, 20));
      Clipped ("4K, the status bar", (8, 1050, 1904, 26));
      Big ("1920x1080", 1920, 1080, 1);
      Big ("1920x1080 at 2x (3840x2160 pixels)", 1920, 1080, 2);
      Heavy ("sorted by time");
      Ticks ("sorted by time");
      Click (Header_ID (Source_Column));
      Ticks ("sorted by source");
      Click (Header_ID (Time_Column));
   end;

   Ada.Text_IO.Put_Line
     ((if Failures = 0 then "PASS" else "FAIL") & ": Logs view," & Natural'Image (Checks) & " checks");
   if Failures > 0 then Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure); end if;
end Log_Viewer_Tests;
