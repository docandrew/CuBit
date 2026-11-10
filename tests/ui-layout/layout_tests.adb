--  Hosted tests for the toolkit's layout containers (UI-033): the proved
--  distribution (Box_Distribution) against random inputs, including what
--  is tested rather than proved (expanding children fill the box exactly);
--  rows, columns, grids and wrapping labels measured with the real UI font
--  (CuBit.UI.Boxes) at narrow and wide sizes and with long labels; and the
--  read-only text field (CuBit.UI.Text_Views): scrolling, following the
--  end, selecting by pointer and keys.
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.Boxes;
with CuBit.UI.Text_Views;
with CuBit.UI_Metrics;
with Box_Distribution;
procedure Layout_Tests is
   package BD renames Box_Distribution;
   package B renames CuBit.UI.Boxes;
   package TV renames CuBit.UI.Text_Views;
   package M renames CuBit.UI_Metrics;
   use type BD.Fit, BD.Policy;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      if not Condition then
         raise Program_Error with "FAIL: " & What;
      end if;
      Checks := Checks + 1;
   end Check;
   function Inside (Inner, Outer : Rect) return Boolean is
     (Inner.x >= Outer.x and then Inner.y >= Outer.y and then
      Inner.x + Inner.w <= Outer.x + Outer.w and then Inner.y + Inner.h <= Outer.y + Outer.h);

   ---------------------------------------------------------------------------
   --  Box_Distribution, random inputs
   ---------------------------------------------------------------------------
   procedure Random_Distributions is
      subtype Small is Natural range 0 .. 400;
      package Random_Small is new Ada.Numerics.Discrete_Random (Small);
      G : Random_Small.Generator;
      function Draw (Limit : Natural) return Natural is (Random_Small.Random (G) mod (Limit + 1));
      Cases : constant := 20_000;
      Maximum_Children : constant := 12;
   begin
      Random_Small.Reset (G, 33);
      for Case_Number in 1 .. Cases loop
         declare
            Count : constant Natural := Draw (Maximum_Children);
            Hints : BD.Hint_Array (1 .. Count);
            Spans : BD.Span_Array (1 .. Count);
            Status : BD.Fit;
            Start : constant Natural := Draw (100);
            Spacing : constant Natural := Draw (12);
            Available : constant Natural := Draw (400) * 3;
            Expanding_Children : Boolean := False;
            Preferred_Total : Natural := 0;
         begin
            for I in Hints'Range loop
               declare
                  Least : constant Natural := Draw (60);
                  Kind : constant BD.Policy := BD.Policy'Val (Draw (2));
                  Wanted : constant Natural := (if Kind = BD.Fixed then Least else Least + Draw (80));
               begin
                  Hints (I) := (Least, Wanted, Kind, 1 + Draw (4));
                  Expanding_Children := Expanding_Children or else Kind = BD.Expanding;
                  Preferred_Total := Preferred_Total + Wanted + (if I > 1 then Spacing else 0);
               end;
            end loop;
            BD.Distribute (Hints, Start, Available, Spacing, Spans, Status);
            Check ((Status = BD.Fits) = (BD.Required (Hints, Spacing) <= Available), "fit verdict");
            for I in Spans'Range loop
               Check (Spans (I).Length >= Hints (I).Minimum, "minimum kept");
               if Hints (I).Kind /= BD.Expanding then
                  Check (Spans (I).Length <= Hints (I).Preferred, "no growth past preferred");
               end if;
               if I > Spans'First then
                  Check (Spans (I).Offset = Spans (I - 1).Offset + Spans (I - 1).Length + Spacing, "no overlap");
               end if;
            end loop;
            if Count > 0 and then Status = BD.Fits then
               Check (Spans (Count).Offset + Spans (Count).Length <= Start + Available, "inside the box");
               --  Tested, not proved: expanding children fill the box; and
               --  with room for everyone's preferred length, the others get it.
               if Expanding_Children and then Preferred_Total <= Available then
                  Check (Spans (Count).Offset + Spans (Count).Length = Start + Available, "expanding fills");
               end if;
               if Preferred_Total <= Available then
                  for I in Spans'Range loop
                     if Hints (I).Kind /= BD.Expanding then
                        Check (Spans (I).Length = Hints (I).Preferred, "preferred given when room");
                     end if;
                  end loop;
               end if;
            end if;
         end;
      end loop;
      --  Stretch factors share the rest 1 : 3.
      declare
         Hints : constant BD.Hint_Array :=
           [(0, 0, BD.Expanding, 1), (0, 0, BD.Expanding, 3)];
         Spans : BD.Span_Array (1 .. 2);
         Status : BD.Fit;
      begin
         BD.Distribute (Hints, 0, 400, 0, Spans, Status);
         Check (Spans (1).Length = 100 and Spans (2).Length = 300, "stretch 1:3");
      end;
   end Random_Distributions;

   ---------------------------------------------------------------------------
   --  Boxes
   ---------------------------------------------------------------------------
   Status_Id : constant := 1;
   Apply_Id : constant := 2;
   Revert_Id : constant := 3;
   Page_Id : constant := 4;
   List_Id : constant := 5;
   Title_Id : constant := 6;
   Long_Text : constant String :=
     "Best mode 1920 x 1080; Auto keeps the firmware mode at startup (desktop.display.mode = Auto)";

   --  A page: a title, a list that expands, a wrapping note, and a row of
   --  a status line with two buttons pushed right.
   procedure Page (T : in out B.Tree; Title : String) is
   begin
      B.Clear (T);
      B.Begin_Column (T, Page_Id, Margin => M.Space_XL, Width_Growth => B.Fill, Height_Growth => B.Fill);
      B.Add_Label (T, Title, Id => Title_Id);
      B.Add_Block (T, List_Id, (200, 300, BD.Expanding, 1), (100, 150, BD.Expanding, 1));
      B.Add_Wrapping_Label (T, Long_Text, B.Muted_Text);
      B.Begin_Row (T);
      B.Add_Wrapping_Label (T, "Choose a mode and Apply; refused modes show why", Id => Status_Id);
      B.Add_Button (T, "Apply", Apply_Id);
      B.Add_Button (T, "Revert", Revert_Id);
      B.End_Box (T);
      B.End_Box (T);
   end Page;

   procedure Boxes_Tests is
      T : B.Tree (32, 1024);
      Least, Wanted : B.Size;
   begin
      Page (T, "Monitor");
      Check (not B.Overflowed (T), "balanced tree");
      B.Measure (T, Least, Wanted);
      Check (Least.Width > 0 and Wanted.Width >= Least.Width and Wanted.Height >= Least.Height, "sizes");
      Check (Least.Width >= 200 + 2 * M.Space_XL, "the list's minimum counts");
      --  Narrow (the minimum), natural and wide windows all fit; every
      --  widget inside the window, the buttons side by side at the right.
      for Shape in 1 .. 3 loop
         declare
            Window : constant Rect :=
              (case Shape is
                 when 1 => (10, 20, Least.Width, Least.Height),
                 when 2 => (10, 20, Wanted.Width, Wanted.Height),
                 when others => (10, 20, 2 * Wanted.Width, 2 * Wanted.Height));
            Apply : Rect;
            Revert : Rect;
            Status : Rect;
         begin
            B.Arrange (T, Window);
            Check (B.Fits (T), "fits at shape" & Shape'Image);
            Apply := B.Bounds (T, Apply_Id);
            Revert := B.Bounds (T, Revert_Id);
            Status := B.Bounds (T, Status_Id);
            for Id in Status_Id .. Title_Id loop
               Check (Inside (B.Bounds (T, Id), Window), "inside at shape" & Shape'Image);
            end loop;
            Check (Apply.x + Apply.w + M.Space_M = Revert.x, "buttons a gap apart");
            Check (Revert.x + Revert.w = Window.x + Window.w - M.Space_XL, "pushed to the right margin");
            Check (Status.x + Status.w + M.Space_M = Apply.x, "status takes the rest");
            Check (Apply.w >= UI_Text_Width ("Apply") + 2 * BUTTON_TEXT_INSET and Apply.h = M.Control_Height,
                   "a button sized from its label");
            Check (B.Hit (T, Apply.x + 1, Apply.y + 1) = Apply_Id and B.Hit (T, Window.x, Window.y) = B.No_Id,
                   "hit by identity");
            if Shape = 3 then
               Check (B.Bounds (T, List_Id).w = Window.w - 2 * M.Space_XL, "the list expands to the window");
            end if;
         end;
      end loop;
      --  One pixel narrower than the minimum: a typed "too small", not an overlap.
      B.Arrange (T, (0, 0, Least.Width - 1, Least.Height));
      Check (not B.Fits (T), "too small reported");
      --  Wrapping: narrower means more lines, each fitting its width.
      declare
         Wide_Lines : constant Natural := B.Line_Count (Long_Text, B.Body_Text, 2_000);
         Narrow_Lines : constant Natural := B.Line_Count (Long_Text, B.Body_Text, B.Wrap_Minimum_Width);
         Joined : Natural := 0;
         procedure Line (First, Last : Natural) is
         begin
            Check (B.Text_Width (Long_Text (First .. Last), B.Body_Text) <= B.Wrap_Minimum_Width, "line fits");
            Check (Long_Text (First) /= ' ' and Long_Text (Last) /= ' ', "lines trimmed");
            Joined := Joined + (Last - First + 1);
         end Line;
         procedure Lines is new B.Wrap (Line);
      begin
         Check (Wide_Lines = 1 and Narrow_Lines > 2, "wraps when narrow");
         Lines (Long_Text, B.Body_Text, B.Wrap_Minimum_Width);
         Check (Joined + Narrow_Lines - 1 = Long_Text'Length, "no characters lost");
         --  A word wider than the line breaks between characters.
         Check (B.Line_Count ("Supercalifragilistic", B.Body_Text, M.Base_Em) > 1, "long word broken");
         Check (B.Line_Count ("", B.Body_Text, M.Base_Em) = 0, "empty text, no lines");
      end;
      --  The wrapped label's height at the minimum width: whole lines.
      B.Arrange (T, (0, 0, Least.Width, Least.Height));
      --  A long one-line label raises the minimum width (never cut).
      B.Clear (T);
      B.Begin_Column (T, Page_Id);
      B.Add_Label (T, Long_Text, Id => Title_Id);
      B.End_Box (T);
      B.Measure (T, Least, Wanted);
      Check (Least.Width = UI_Text_Width (Long_Text), "a label's width is its text's");
      --  Grids: columns as wide as their widest cell, rows aligned.
      B.Clear (T);
      B.Begin_Grid (T, Columns => 2, Id => Page_Id, Spacing => M.Space_L);
      B.Add_Label (T, "HDMI", Id => 11);
      B.Add_Label (T, "LG ULTRAGEAR+", Id => 12);
      B.Add_Label (T, "DisplayPort", Id => 13);
      B.Add_Label (T, "U2720Q", Id => 14);
      B.End_Box (T);
      B.Measure (T, Least, Wanted);
      B.Arrange (T, (0, 0, Wanted.Width, Wanted.Height));
      Check (B.Bounds (T, 12).x = B.Bounds (T, 14).x, "a grid column lines up");
      Check (B.Bounds (T, 12).x = UI_Text_Width ("DisplayPort") + M.Space_L, "measured column");
      Check (B.Bounds (T, 13).y = B.Bounds (T, 11).y + M.Line_Height + M.Space_XS, "rows a gap apart");
      Check (Least.Width = UI_Text_Width ("DisplayPort") + M.Space_L + UI_Text_Width ("LG ULTRAGEAR+"),
             "grid width");
      --  Shrink-wrapped columns of filling buttons are as wide as the widest.
      B.Clear (T);
      B.Begin_Row (T, Spacing => 0, Align => B.Align_Start);
      B.Begin_Column (T, Page_Id, Width_Growth => B.Shrink_Wrap);
      B.Add_Tab (T, "Appearance", 21);
      B.Add_Tab (T, "Date & Time", 22);
      B.End_Box (T);
      B.Add_Glue (T);
      B.End_Box (T);
      B.Arrange (T, (0, 0, 800, 200));
      Check (B.Bounds (T, 21).w = B.Bounds (T, 22).w and B.Bounds (T, Page_Id).w < 800 / 2,
             "tabs share the sidebar's width, which does not grow");
      Check (B.Bounds (T, 22).w >= UI_Text_Width ("Date & Time") + TAB_TEXT_LEFT + TAB_TEXT_INSET,
             "a tab shows its whole label");
      --  UI-035: the wheel goes to the scrolling block under the pointer.
      B.Clear (T);
      B.Begin_Row (T, Spacing => 0);
      B.Add_Block (T, 31, BD.Fixed_Hint (100), BD.Fixed_Hint (50), Scrolls => True);
      B.Add_Block (T, 32, BD.Fixed_Hint (100), BD.Fixed_Hint (50));
      B.End_Box (T);
      B.Arrange (T, (0, 0, 200, 50));
      Check (B.Scroll_Target (T, 10, 10) = 31, "the scrolling block takes the wheel");
      Check (B.Scroll_Target (T, 110, 10) = B.No_Id, "a block that does not scroll does not");
      Check (B.Scroll_Target (T, 10, 60) = B.No_Id, "nor does empty space");
      declare
         Named : Natural := 0;
         procedure Count (Id : B.Widget_Id; Box : Rect) is
         begin
            Check (Box = B.Bounds (T, Id), "visited at its place");
            Named := Named + 1;
         end Count;
         procedure Each is new B.Visit_Named (Count);
      begin
         Each (T);
         Check (Named = 2, "every named widget visited");
      end;
      --  Capacity is refused, never silently cut; unbalanced boxes too.
      declare
         Tiny : B.Tree (2, 8);
      begin
         B.Clear (Tiny);
         B.Begin_Column (Tiny);
         B.Add_Label (Tiny, "one");
         B.Add_Label (Tiny, "two");
         B.End_Box (Tiny);
         Check (B.Overflowed (Tiny), "node capacity refused");
         B.Clear (Tiny);
         B.Begin_Column (Tiny);
         B.Add_Label (Tiny, "a label longer than eight");
         B.End_Box (Tiny);
         Check (B.Overflowed (Tiny), "text capacity refused");
         B.Clear (Tiny);
         B.Begin_Column (Tiny);
         Check (B.Overflowed (Tiny), "an open box is incomplete");
      end;
   end Boxes_Tests;

   ---------------------------------------------------------------------------
   --  Text views
   ---------------------------------------------------------------------------
   procedure Text_View_Tests is
      Lines : constant := 40;
      function Log (Count : Natural) return String is
        (if Count = 0 then "" elsif Count = 1 then "0:00:01  evaluate (logs.recent): ok"
         else Log (Count - 1) & ASCII.LF & "0:00:" & (if Count < 10 then "0" else "") &
              Count'Image (2 .. Count'Image'Length) & "  evaluate: ok");
      Text : constant String := Log (Lines);
      Box : constant Rect := (10, 10, 400, TV.Height_Hint (8, 8).Minimum);
      V : TV.View;
      Handled : Boolean;
      Scan_Up : constant := 16#48#;
      Scan_Down : constant := 16#50#;
      Scan_Home : constant := 16#47#;
      Scan_End : constant := 16#4F#;
      Scan_A : constant := 16#1E#;
      Pitch : constant Positive := TV.Line_Pitch;
      Text_Left : constant Natural := Box.x + Multiline_Text_Left;
      Text_Top : constant Natural := Box.y + Multiline_Text_Top;
   begin
      Check (TV.Line_Count (Text) = Lines and TV.Visible_Lines (Box) = 8, "lines and the view's height");
      TV.Sync (V, Text, Box);
      Check (V.First_Line = Lines - 7 and V.Follow_End, "a log opens at its end");
      --  New text at the end scrolls into view while following...
      TV.Sync (V, Text & ASCII.LF & "0:01:00  build: ok", Box);
      Check (V.First_Line = Lines - 6, "follows the end");
      --  ... not once scrolled back (the wheel, keys).
      TV.Wheel (V, Text & ASCII.LF & "0:01:00  build: ok", Box, 1);
      Check (V.First_Line = Lines - 6 - TV.Wheel_Lines and not V.Follow_End, "the wheel scrolls up");
      TV.Sync (V, Text & ASCII.LF & "0:01:00  build: ok" & ASCII.LF & "0:01:01  build: ok", Box);
      Check (V.First_Line = Lines - 6 - TV.Wheel_Lines, "a scrolled view keeps its place");
      TV.Key (V, Text, Box, Scan_Home, False, False, Handled);
      Check (Handled and V.First_Line = 1, "Home");
      TV.Key (V, Text, Box, Scan_Up, False, False, Handled);
      Check (V.First_Line = 1, "stops at the start");
      TV.Key (V, Text, Box, Scan_End, False, False, Handled);
      Check (V.First_Line = Lines - 7 and V.Follow_End, "End follows again");
      --  Drag-select the first two lines' start: from line 1 column 0 to line 2 column 4.
      TV.Key (V, Text, Box, Scan_Home, False, False, Handled);
      TV.Pointer (V, Text, Box, Text_Left, Text_Top + 1, True, True, False, False);
      TV.Pointer (V, Text, Box, Text_Left + 4 * M.Mono_Advance, Text_Top + Pitch + 1, True, False, False, False);
      TV.Pointer (V, Text, Box, Text_Left + 4 * M.Mono_Advance, Text_Top + Pitch + 1, False, False, True, False);
      Check (TV.Selection (V, Text) = "0:00:01  evaluate (logs.recent): ok" & ASCII.LF & "0:00",
             "drag selects across lines: " & TV.Selection (V, Text));
      --  Shift+Down extends by a line, Ctrl+A takes it all.
      TV.Key (V, Text, Box, Scan_Down, True, False, Handled);
      Check (TV.Selection (V, Text)'Length > 40, "Shift+Down extends");
      TV.Key (V, Text, Box, Scan_A, False, True, Handled);
      Check (TV.Selection (V, Text) = Text, "Ctrl+A");
      --  A press in the scroll bar below the thumb pages down.
      TV.Key (V, Text, Box, Scan_Home, False, False, Handled);
      declare
         Bar : constant Rect := TV.Bar_Box (Box);
      begin
         TV.Pointer (V, Text, Box, Bar.x + Bar.w / 2, Bar.y + Bar.h - M.Space_XS - CuBit.UI_Metrics.Glyph_Box,
                     True, True, False, False);
         Check (V.First_Line = 1 + TV.Visible_Lines (Box), "the bar pages");
      end;
      --  Text that shrinks clamps the selection.
      TV.Sync (V, "short", Box);
      Check (TV.Selection_Last (V) <= 6 and V.First_Line = 1, "clamped");
   end Text_View_Tests;
begin
   Random_Distributions;
   Boxes_Tests;
   Text_View_Tests;
   Ada.Text_IO.Put_Line ("PASS layout containers (UI-033):" & Checks'Image & " checks");
end Layout_Tests;
