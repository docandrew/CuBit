with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.UI;
with CuBit.UI.Controls;
with CuBit.UI.State;
with Files_Limits;
with Files_View; use Files_View;
with Files_Host_Support; use Files_Host_Support;

--  Hosted Files benchmarks (docs/files-app.md, "Measurements"): listing
--  throughput through the real queue protocol, keypress-to-frame-complete
--  for cursor moves and scrolling, full frames at 1080p and 4K, sorting
--  and type-ahead filtering in a million-entry folder. Run on a quiet host:
--  tests/files-app/run.sh --bench [largest folder]
procedure Files_Bench is
   LARGEST_DEFAULT : constant := 1_000_000;
   Largest : constant Positive :=
     (if Ada.Command_Line.Argument_Count > 0 then Positive'Value (Ada.Command_Line.Argument (1))
      else LARGEST_DEFAULT);
   --  "frames": only the 4K frame work (for run.sh --profile).
   Frames_Only : constant Boolean :=
     Ada.Command_Line.Argument_Count > 1 and then Ada.Command_Line.Argument (2) = "frames";
   CAPACITY : constant Positive := Largest + Largest / 8;
   NAME_BYTES : constant Positive := CAPACITY * 24;
   --  One frame's slice of sorting and filtering (units: entries moved or
   --  scanned), the budget the native app will use.
   FRAME_BUDGET : constant Files_Limits.Work_Budget := 20_000;
   PRESSES : constant := 2_000;
   SIZES : constant array (1 .. 3) of Positive := [10_000, 100_000, Largest];
   type Resolution is record
      Width, Height : Positive;
      Name : String (1 .. 5);
   end record;
   RESOLUTIONS : constant array (1 .. 2) of Resolution := [(1920, 1080, "1080p"), (3840, 2160, "4K   ")];

   View : constant View_Access := new View_State;
   UI : constant UI_Access := new CuBit.UI.State.UI_State;
   Map : constant Map_Access := new CuBit.UI.Controls.Control_Map;
   Redraw : Boolean;

   function Ms (Us : Unsigned_64) return String is
      Text : constant String := Unsigned_64'Image (1_000 + Us mod 1_000);
   begin
      return Unsigned_64'Image (Us / 1_000) & "." & Text (Text'Last - 2 .. Text'Last) & " ms";
   end Ms;

   function Synthetic (N : Positive) return String is
      Text : constant String := Positive'Image (N);
   begin
      return "@synthetic:" & Text (Text'First + 1 .. Text'Last) & "/";
   end Synthetic;

   --  Pump frame-sized slices until settled; the slowest slice.
   procedure Run_Frames (Slowest : out Unsigned_64; Frames : out Natural) is
      Busy, Changed : Boolean;
      Start : Unsigned_64;
   begin
      Slowest := 0;
      Frames := 0;
      loop
         Start := Now_Us;
         Pump (View.all, FRAME_BUDGET, Now_Us, Busy, Changed);
         Slowest := Unsigned_64'Max (Slowest, Now_Us - Start);
         Frames := Frames + 1;
         exit when not Busy and then not Waiting_For_IO (View.all);
         if not Busy then
            --  Nothing to do until the service's wake: sleep, no polling.
            Wait_For_Wake (50);
         end if;
      end loop;
   end Run_Frames;
begin
   Configure_Service ("/");
   Put_Line ("Files hosted benchmarks (Linux, -O2; not CuBit measurements)");
   Put_Line ("frame budget" & Files_Limits.Work_Budget'Image (FRAME_BUDGET) & " units");

   --  Listing: Initialize asks for both panes; the right one is the big one.
   for N of SIZES loop
      exit when Frames_Only;
      declare
         Start : constant Unsigned_64 := Now_Us;
         Slowest : Unsigned_64;
         Frames : Natural;
      begin
         Initialize (View.all, CAPACITY, NAME_BYTES, Synthetic (1), Synthetic (N));
         Run_Frames (Slowest, Frames);
         Put_Line ("listing" & Positive'Image (N) & " entries: listed+sorted in " & Ms (Now_Us - Start)
                   & " (listing alone " & Ms (Listing_Us (View.all, Right_Pane)) & ", "
                   & Unsigned_64'Image (Unsigned_64 (N) * 1_000_000 / Unsigned_64'Max (1, Now_Us - Start))
                   & " entries/s), slowest pump " & Ms (Slowest) & "," & Natural'Image (Frames) & " pumps");
         if Listed (View.all, Right_Pane) /= N then
            Put_Line ("  WARNING: listed" & Natural'Image (Listed (View.all, Right_Pane)));
         end if;
         Close (View.all);
      end;
   end loop;

   --  The largest folder stays for the interaction benchmarks.
   Initialize (View.all, CAPACITY, NAME_BYTES, Synthetic (1), Synthetic (Largest));
   declare
      Slowest : Unsigned_64;
      Frames : Natural;
   begin
      Run_Frames (Slowest, Frames);
   end;
   Handle (View.all, (Kind => Key_Event, Key => Tab, others => <>), Map.all, Redraw);

   for Index in (if Frames_Only then RESOLUTIONS'Last else RESOLUTIONS'First) .. RESOLUTIONS'Last loop
      declare
         R : constant Resolution := RESOLUTIONS (Index);
         Screen : constant Surface := New_Surface (R.Width, R.Height);
         Damage : CuBit.UI.Rect;
         Total, Worst, Start, Took : Unsigned_64;
         procedure Frame is
         begin
            Take_Damage (View.all, Damage);
            if not CuBit.UI.Is_Empty (Damage) then
               Render (View.all, Canvas (Screen, Damage), Bounds (Screen), UI.all, Map.all);
            end if;
         end Frame;
         procedure Press (Key : Key_Name; Label : String) is
         begin
            Total := 0;
            Worst := 0;
            for K in 1 .. PRESSES loop
               Start := Now_Us;
               Handle (View.all, (Kind => Key_Event, Key => Key, others => <>), Map.all, Redraw);
               Frame;
               Took := Now_Us - Start;
               Total := Total + Took;
               Worst := Unsigned_64'Max (Worst, Took);
            end loop;
            Put_Line (R.Name & " " & Label & ": mean " & Ms (Total / PRESSES) & ", worst " & Ms (Worst));
         end Press;
      begin
         --  Full frames.
         Total := 0;
         Worst := 0;
         for K in 1 .. 50 loop
            Start := Now_Us;
            Render (View.all, Canvas (Screen), Bounds (Screen), UI.all, Map.all);
            Took := Now_Us - Start;
            Total := Total + Took;
            Worst := Unsigned_64'Max (Worst, Took);
         end loop;
         Take_Damage (View.all, Damage);
         Put_Line (R.Name & " full frame: mean " & Ms (Total / 50) & ", worst " & Ms (Worst));
         --  The floor: one fill of every pixel, and text alone.
         declare
            Colors : constant CuBit.UI.Theme := CuBit.UI.Current_Theme;
            Glyphs : Natural := 0;
         begin
            Start := Now_Us;
            for K in 1 .. 50 loop
               CuBit.UI.Fill_Rect (Canvas (Screen), Bounds (Screen), Colors.field);
            end loop;
            Put_Line (R.Name & " floor, one fill of the frame: " & Ms ((Now_Us - Start) / 50));
            Start := Now_Us;
            for K in 1 .. 50 loop
               for Line in 0 .. R.Height / 20 - 1 loop
                  for Column in 0 .. 5 loop
                     CuBit.UI.Draw_UI_Text
                       (Canvas (Screen), Column * 600, Line * 20, "entry-0012345.txt", Colors.text, Colors.field);
                     Glyphs := Glyphs + 17;
                  end loop;
               end loop;
            end loop;
            Put_Line (R.Name & " text, a frame of 6 cells per row: " & Ms ((Now_Us - Start) / 50) & " ("
                      & Natural'Image (Glyphs / 50) & " glyphs)");
         end;
         Handle (View.all, (Kind => Key_Event, Key => Home, others => <>), Map.all, Redraw);
         Frame;
         --  Within the page: Down then Up, the cursor never leaves the view.
         Total := 0;
         Worst := 0;
         for K in 1 .. PRESSES loop
            Start := Now_Us;
            Handle (View.all, (Kind => Key_Event, Key => (if K mod 2 = 1 then Down else Up), others => <>),
                    Map.all, Redraw);
            Frame;
            Took := Now_Us - Start;
            Total := Total + Took;
            Worst := Unsigned_64'Max (Worst, Took);
         end loop;
         Put_Line (R.Name & " key to frame complete, cursor move in view: mean " & Ms (Total / PRESSES)
                   & ", worst " & Ms (Worst));
         Press (Page_Down, "key PgDn to frame complete (new page of rows)");
         Handle (View.all, (Kind => Key_Event, Key => Home, others => <>), Map.all, Redraw);
         Frame;
         for K in 1 .. 60 loop
            Handle (View.all, (Kind => Key_Event, Key => Down, others => <>), Map.all, Redraw);
            Frame;
         end loop;
         Press (Down, "key Down at the bottom edge (scrolls one row)");
         --  The context menu: open (Shift+F10), move, close.
         declare
            procedure Timed (Key : Key_Name; Label : String; Control : Boolean := False) is
               Begin_Us : constant Unsigned_64 := Now_Us;
            begin
               Handle (View.all, (Kind => Key_Event, Key => Key, Control => Control, others => <>), Map.all, Redraw);
               Frame;
               Put_Line (R.Name & " " & Label & ": " & Ms (Now_Us - Begin_Us));
            end Timed;
         begin
            for Round in 1 .. 3 loop
               Timed (Menu_Key, "context menu open");
               Timed (Down, "context menu Down");
               Timed (Escape, "context menu close");
            end loop;
            Timed (Letter_B, "drawer hide (relayout, full repaint)", Control => True);
            Timed (Letter_B, "drawer show (relayout, full repaint)", Control => True);
            --  Tabs: open one, then close it with its x (press, release).
            Timed (Letter_T, "tab open (strip appears)", Control => True);
            declare
               Close_Box : constant CuBit.UI.Rect := Tab_Close_Area (View.all, Active (View.all), 2);
               Begin_Us : constant Unsigned_64 := Now_Us;
            begin
               Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Press,
                        Close_Box.x + Close_Box.w / 2, Close_Box.y + Close_Box.h / 2, 9_000);
               Frame;
               Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Release,
                        Close_Box.x + Close_Box.w / 2, Close_Box.y + Close_Box.h / 2, 9_010);
               Frame;
               Put_Line (R.Name & " tab close by x (press and release, two frames): " & Ms (Now_Us - Begin_Us)
                         & (if Tab_Count (View.all, Active (View.all)) = 1 then "" else "  WARNING: not closed"));
            end;
         end;
         Save_PPM (Screen, "build/bench-" & (if R.Width > 2000 then "4k" else "1080p") & ".ppm");
      end;
   end loop;

   if Frames_Only then
      Close (View.all);
      return;
   end if;

   --  Sort: Ctrl+F6 (size) on the largest folder.
   declare
      Start : constant Unsigned_64 := Now_Us;
      Slowest : Unsigned_64;
      Frames : Natural;
   begin
      Handle (View.all, (Kind => Key_Event, Key => F6, Control => True, others => <>), Map.all, Redraw);
      Run_Frames (Slowest, Frames);
      Put_Line ("sort by size," & Positive'Image (Largest) & " entries: " & Ms (Now_Us - Start) & " over"
                & Natural'Image (Frames) & " pumps, slowest pump " & Ms (Slowest));
   end;

   --  Type-ahead: three keystrokes; the first frame after each and the end.
   for C of String'("123") loop
      declare
         Start : constant Unsigned_64 := Now_Us;
         Busy, Changed : Boolean;
         First_Slice : Unsigned_64;
         Slowest : Unsigned_64;
         Frames : Natural;
      begin
         Handle (View.all, (Kind => Text_Event, Character_Value => C, others => <>), Map.all, Redraw);
         Pump (View.all, FRAME_BUDGET, Now_Us, Busy, Changed);
         First_Slice := Now_Us - Start;
         Run_Frames (Slowest, Frames);
         Put_Line ("filter keystroke '" & C & "': first rows after " & Ms (First_Slice) & ", complete "
                   & Ms (Now_Us - Start) & "," & Natural'Image (Rows (View.all, Right_Pane)) & " matches");
      end;
   end loop;
   Close (View.all);
end Files_Bench;
