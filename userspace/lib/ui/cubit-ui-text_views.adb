package body CuBit.UI.Text_Views is
   package M renames CuBit.UI_Metrics;
   package BD renames Box_Distribution;

   --  Set-1 make codes the view acts on.
   Scan_A : constant := 16#1E#;
   Scan_Home : constant := 16#47#;
   Scan_Up : constant := 16#48#;
   Scan_Page_Up : constant := 16#49#;
   Scan_End : constant := 16#4F#;
   Scan_Down : constant := 16#50#;
   Scan_Page_Down : constant := 16#51#;

   function Grid_Width (Columns : Positive) return Natural is
     (Columns * M.Mono_Advance + 2 * Multiline_Text_Left + Scroll_Bar_Width);
   function Grid_Height (Lines : Positive) return Natural is
     (Lines * Line_Pitch + 2 * Multiline_Text_Top);

   function Width_Hint (Columns, Preferred_Columns : Positive) return BD.Size_Hint is
     ((Minimum => Natural'Min (Grid_Width (Columns), BD.MAXIMUM_EXTENT),
       Preferred => Natural'Min (Grid_Width (Positive'Max (Columns, Preferred_Columns)), BD.MAXIMUM_EXTENT),
       Kind => BD.Expanding, Stretch => 1));
   function Height_Hint (Lines, Preferred_Lines : Positive) return BD.Size_Hint is
     ((Minimum => Natural'Min (Grid_Height (Lines), BD.MAXIMUM_EXTENT),
       Preferred => Natural'Min (Grid_Height (Positive'Max (Lines, Preferred_Lines)), BD.MAXIMUM_EXTENT),
       Kind => BD.Expanding, Stretch => 1));

   function Line_Count (Text : String) return Positive is
      Count : Positive := 1;
   begin
      for Ch of Text loop
         if Ch = ASCII.LF then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Line_Count;

   function Text_Box (Box : Rect) return Rect is
     ((Box.x, Box.y, (if Box.w > Scroll_Bar_Width then Box.w - Scroll_Bar_Width else 0), Box.h));
   function Bar_Box (Box : Rect) return Rect is
     ((Box.x + Text_Box (Box).w, Box.y, Box.w - Text_Box (Box).w, Box.h));

   function Visible_Lines (Box : Rect) return Positive is
     (Positive'Max (1, (if Box.h > 2 * Multiline_Text_Top then (Box.h - 2 * Multiline_Text_Top) / Line_Pitch
                        else 1)));

   function Last_Top (Text : String; Box : Rect) return Positive is
     (Positive'Max (1, Line_Count (Text) - Visible_Lines (Box) + 1));

   --  Position of the first character of Line (1-based in Text).
   function Line_Start (Text : String; Line : Positive) return Positive is
      Seen : Positive := 1;
   begin
      if Line = 1 then
         return 1;
      end if;
      for I in Text'Range loop
         if Text (I) = ASCII.LF then
            Seen := Seen + 1;
            if Seen = Line then
               return I - Text'First + 2;
            end if;
         end if;
      end loop;
      return Text'Length + 1;
   end Line_Start;

   function Line_Length (Text : String; Line : Positive) return Natural is
      Start : constant Positive := Line_Start (Text, Line);
      Length : Natural := 0;
   begin
      while Start + Length <= Text'Length and then Text (Text'First + Start + Length - 1) /= ASCII.LF loop
         Length := Length + 1;
      end loop;
      return Length;
   end Line_Length;

   function Line_Of (Text : String; Position : Positive) return Positive is
      Line : Positive := 1;
   begin
      for I in Text'First .. Text'First + Natural'Min (Position - 1, Text'Length) - 1 loop
         if Text (I) = ASCII.LF then
            Line := Line + 1;
         end if;
      end loop;
      return Line;
   end Line_Of;

   function Column_Of (Text : String; Position : Positive) return Natural is
     (Position - Line_Start (Text, Line_Of (Text, Position)));

   function At_Line_Column (Text : String; Line : Positive; Column : Natural) return Positive is
     (Line_Start (Text, Line) + Natural'Min (Column, Line_Length (Text, Line)));

   procedure Sync (V : in out View; Text : String; Box : Rect) is
      Limit : constant Positive := Text'Length + 1;
   begin
      V.Cursor := Positive'Min (V.Cursor, Limit);
      V.Anchor := Positive'Min (V.Anchor, Limit);
      if V.Follow_End then
         V.First_Line := Last_Top (Text, Box);
      else
         V.First_Line := Positive'Min (V.First_Line, Last_Top (Text, Box));
      end if;
   end Sync;

   procedure Scroll (V : in out View; Text : String; Box : Rect; By : Integer) is
      Last : constant Positive := Last_Top (Text, Box);
   begin
      V.First_Line := Positive (Integer'Max (1, Integer'Min (Last, V.First_Line + By)));
      V.Follow_End := V.First_Line = Last;
   end Scroll;

   procedure Wheel (V : in out View; Text : String; Box : Rect; Notches : Integer) is
   begin
      --  Up (positive) shows earlier lines.
      Scroll (V, Text, Box, -(Notches * Wheel_Lines));
   end Wheel;

   --  Scroll so the cursor's line shows.
   procedure Reveal (V : in out View; Text : String; Box : Rect) is
      Line : constant Positive := Line_Of (Text, V.Cursor);
      Shown : constant Positive := Visible_Lines (Box);
   begin
      if Line < V.First_Line then
         Scroll (V, Text, Box, Line - V.First_Line);
      elsif Line >= V.First_Line + Shown then
         Scroll (V, Text, Box, Line - (V.First_Line + Shown - 1));
      end if;
   end Reveal;

   procedure Key
     (V : in out View; Text : String; Box : Rect; Scan_Code : Natural; Shift, Ctrl : Boolean;
      Handled : out Boolean)
   is
      Page : constant Integer := Visible_Lines (Box);
      Lines : constant Positive := Line_Count (Text);
      procedure Move_Cursor (By_Lines : Integer) is
         Line : constant Positive :=
           Positive (Integer'Max (1, Integer'Min (Lines, Line_Of (Text, V.Cursor) + By_Lines)));
      begin
         V.Cursor := At_Line_Column (Text, Line, Column_Of (Text, V.Cursor));
         Reveal (V, Text, Box);
      end Move_Cursor;
   begin
      Handled := True;
      if Ctrl and then Scan_Code = Scan_A then
         V.Anchor := 1;
         V.Cursor := Text'Length + 1;
         return;
      end if;
      if Shift then
         case Scan_Code is
            when Scan_Up => Move_Cursor (-1);
            when Scan_Down => Move_Cursor (1);
            when Scan_Page_Up => Move_Cursor (-Page);
            when Scan_Page_Down => Move_Cursor (Page);
            when Scan_Home => V.Cursor := 1; Reveal (V, Text, Box);
            when Scan_End => V.Cursor := Text'Length + 1; Reveal (V, Text, Box);
            when others => Handled := False;
         end case;
      else
         case Scan_Code is
            when Scan_Up => Scroll (V, Text, Box, -1);
            when Scan_Down => Scroll (V, Text, Box, 1);
            when Scan_Page_Up => Scroll (V, Text, Box, -Page);
            when Scan_Page_Down => Scroll (V, Text, Box, Page);
            when Scan_Home => Scroll (V, Text, Box, -Lines);
            when Scan_End => Scroll (V, Text, Box, Lines);
            when others => Handled := False;
         end case;
      end if;
   end Key;

   function Position_At (V : View; Text : String; Box : Rect; X, Y : Natural) return Positive is
      T : constant Rect := Text_Box (Box);
      Top : constant Natural := T.y + Multiline_Text_Top;
      Left : constant Natural := T.x + Multiline_Text_Left;
      Line : constant Positive :=
        Positive'Min (Line_Count (Text), V.First_Line + (if Y > Top then (Y - Top) / Line_Pitch else 0));
      Column : constant Natural := (if X > Left then (X - Left + M.Mono_Advance / 2) / M.Mono_Advance else 0);
   begin
      return At_Line_Column (Text, Line, Column);
   end Position_At;

   procedure Pointer
     (V : in out View; Text : String; Box : Rect; X, Y : Natural; Down, Pressed, Released, Shift : Boolean)
   is
      T : constant Rect := Text_Box (Box);
   begin
      if Pressed and then Point_In_Rect (X, Y, Bar_Box (Box)) then
         declare
            Bar : constant Vertical_Scrollbar_Layout := Layout_Vertical_Scrollbar
              (Bar_Box (Box), 1, Last_Top (Text, Box), V.First_Line, Visible_Lines (Box));
         begin
            if Y < Bar.thumb.y then
               Scroll (V, Text, Box, -Visible_Lines (Box));
            elsif Y >= Bar.thumb.y + Bar.thumb.h then
               Scroll (V, Text, Box, Visible_Lines (Box));
            end if;
         end;
      elsif Pressed and then Point_In_Rect (X, Y, T) then
         V.Cursor := Position_At (V, Text, Box, X, Y);
         if not Shift then
            V.Anchor := V.Cursor;
         end if;
         V.Selecting := True;
      elsif V.Selecting and then Down then
         --  Dragging past the top or bottom scrolls.
         if Y < T.y then
            Scroll (V, Text, Box, -1);
         elsif Y >= T.y + T.h then
            Scroll (V, Text, Box, 1);
         end if;
         V.Cursor := Position_At (V, Text, Box, X, Y);
      end if;
      if Released or else not Down then
         V.Selecting := False;
      end if;
   end Pointer;

   function Selection (V : View; Text : String) return String is
      First : constant Positive := Positive'Min (Selection_First (V), Text'Length + 1);
      Last : constant Positive := Positive'Min (Selection_Last (V), Text'Length + 1);
   begin
      return Text (Text'First + First - 1 .. Text'First + Last - 2);
   end Selection;

   --  One shown row: its line's characters (Text indices First .. Last,
   --  empty when Last < First) and the columns of it that are selected
   --  (From .. To - 1, none when To <= From). Present: the line exists.
   type Row is record
      Present : Boolean := False;
      First : Positive := 1;
      Last : Natural := 0;
      From, To : Natural := 0;
   end record;
   type Rows is array (Natural range <>) of Row;

   --  The rows V shows of Text, in one pass over it.
   function Shown_Rows (Text : String; V : View; Count : Positive) return Rows is
      Result : Rows (0 .. Count - 1);
      Line : Positive := 1;
      Start : Positive := Text'First;
      Sel_First : constant Positive := Selection_First (V);
      Sel_Last : constant Positive := Selection_Last (V);
      procedure Finish (Last : Natural) is
         R : Natural;
         Start_Position, End_Position : Positive;
      begin
         if Line >= V.First_Line and then Line - V.First_Line < Count then
            R := Line - V.First_Line;
            Result (R) := (Present => True, First => Start, Last => Last, From => 0, To => 0);
            --  Positions are 1-based in Text.
            Start_Position := Start - Text'First + 1;
            End_Position := Start_Position + (if Last >= Start then Last - Start + 1 else 0);
            if Sel_First < Sel_Last and then Sel_First < End_Position and then Sel_Last > Start_Position then
               Result (R).From := Positive'Max (Sel_First, Start_Position) - Start_Position;
               Result (R).To := Positive'Min (Sel_Last, End_Position) - Start_Position;
            end if;
         end if;
      end Finish;
   begin
      for I in Text'Range loop
         if Text (I) = ASCII.LF then
            Finish (I - 1);
            Line := Line + 1;
            exit when Line >= V.First_Line + Count;
            Start := I + 1;
         end if;
      end loop;
      if Line >= V.First_Line and then Line - V.First_Line < Count then
         Finish (Text'Last);
      end if;
      return Result;
   end Shown_Rows;

   function Same_Row (A : Row; A_Text : String; B : Row; B_Text : String) return Boolean is
     (A.Present = B.Present and then
      (not A.Present or else
       (A.Last - A.First = B.Last - B.First and then A.From = B.From and then A.To = B.To and then
        (A.Last < A.First or else A_Text (A.First .. A.Last) = B_Text (B.First .. B.Last)))));

   --  Row R of Box's text area: the band its line is drawn in.
   function Row_Rect (Box : Rect; R : Natural) return Rect is
     ((Text_Box (Box).x, Text_Box (Box).y + Multiline_Text_Top + R * Line_Pitch,
       Text_Box (Box).w, Line_Pitch));

   function Changed (Box : Rect; Old : View; Old_Text : String; V : View; Text : String) return Rect is
      Count : constant Positive := Visible_Lines (Box);
      Before : constant Rows := Shown_Rows (Old_Text, Old, Count);
      After : constant Rows := Shown_Rows (Text, V, Count);
      Result : Rect := (others => 0);
   begin
      for R in 0 .. Count - 1 loop
         if not Same_Row (Before (R), Old_Text, After (R), Text) then
            Result := Union_Rect (Result, Row_Rect (Box, R));
         end if;
      end loop;
      if Old.First_Line /= V.First_Line or else Last_Top (Old_Text, Box) /= Last_Top (Text, Box) then
         Result := Union_Rect (Result, Bar_Box (Box));
      end if;
      return Result;
   end Changed;

   function Moved_Lines (Box : Rect; Old : View; Old_Text : String; V : View; Text : String) return Natural is
      Count : constant Positive := Visible_Lines (Box);
   begin
      if Selection_First (Old) /= Selection_Last (Old) or else Selection_First (V) /= Selection_Last (V) or else
        Count < 2
      then
         return 0;
      end if;
      declare
         Before : constant Rows := Shown_Rows (Old_Text, Old, Count);
         After : constant Rows := Shown_Rows (Text, V, Count);
         Matches : Boolean;
      begin
         for Lines in 1 .. Count - 1 loop
            Matches := True;
            for R in 0 .. Count - 1 - Lines loop
               if not Same_Row (After (R), Text, Before (R + Lines), Old_Text) then
                  Matches := False;
                  exit;
               end if;
            end loop;
            if Matches then
               return Lines;
            end if;
         end loop;
      end;
      return 0;
   end Moved_Lines;

   Copied_Rows : Natural := 0;
   function Last_Copied_Rows return Natural is (Copied_Rows);

   procedure Draw_Moved
     (C, Previous : Canvas; Box : Rect; Colors : Theme;
      Old : View; Old_Text : String; V : View; Text : String; Focused : Boolean)
   is
      Lines : constant Natural := Moved_Lines (Box, Old, Old_Text, V, Text);
      Count : constant Positive := Visible_Lines (Box);
      Band : Rect;
      Copied : Boolean := False;
   begin
      Copied_Rows := 0;
      if Lines > 0 then
         --  New rows 0 .. Count - 1 - Lines were old rows Lines .. Count - 1.
         Band := Row_Rect (Box, 0);
         Band.h := (Count - Lines) * Line_Pitch;
         Copy_Moved (C, Previous, Band, Band.y + Lines * Line_Pitch, Copied);
      end if;
      if not Copied then
         Draw (C, Box, Colors, V, Text, Focused);
         return;
      end if;
      Copied_Rows := Count - Lines;
      --  Everything else C's clip asks for: at most four pieces around the
      --  copied band (the new rows, the field's edges, the scroll bar).
      declare
         Region : constant Rect := (if C.clipEnabled then C.clip else (0, 0, C.width, C.height));
         Left : constant Natural := Natural'Max (Region.x, Band.x);
         Top : constant Natural := Natural'Max (Region.y, Band.y);
         Right : constant Natural := Natural'Min (Region.x + Region.w, Band.x + Band.w);
         Bottom : constant Natural := Natural'Min (Region.y + Region.h, Band.y + Band.h);
         procedure Piece (R : Rect) is
         begin
            if not Is_Empty (R) then
               Draw (With_Clip (C, R), Box, Colors, V, Text, Focused);
            end if;
         end Piece;
      begin
         if Left >= Right or else Top >= Bottom then
            Draw (C, Box, Colors, V, Text, Focused);
            return;
         end if;
         Piece ((Region.x, Region.y, Region.w, Top - Region.y));
         Piece ((Region.x, Bottom, Region.w, Region.y + Region.h - Bottom));
         Piece ((Region.x, Top, Left - Region.x, Bottom - Top));
         Piece ((Right, Top, Region.x + Region.w - Right, Bottom - Top));
      end;
   end Draw_Moved;

   procedure Draw (C : Canvas; Box : Rect; Colors : Theme; V : View; Text : String; Focused : Boolean) is
      T : constant Rect := Text_Box (Box);
   begin
      --  Read-only: no caret; the focus ring shows where keys go. Only the
      --  shown lines go to the grid (it walks every character it is given),
      --  with the positions moved to that slice and clamped to it.
      declare
         Count : constant Positive := Visible_Lines (Box);
         Shown : constant Rows := Shown_Rows (Text, V, Count);
         First : constant Positive := (if Shown (0).Present then Shown (0).First else Text'First);
         Last : Natural := (if Shown (0).Present then Shown (0).Last else Text'First - 1);
         Offset : Natural;
         function Moved (Position : Positive) return Positive is
           (if Position <= Offset then 1
            else Positive'Min (Position - Offset, Last - First + 2));
      begin
         for R of Shown loop
            if R.Present then
               Last := R.Last;
            end if;
         end loop;
         if Shown (0).Present then
            Offset := First - Text'First;
            Draw_Multiline_Text_Edit
              (C, T, Colors, Text (First .. Last), 1, Count, Moved (V.Cursor),
               Moved (Selection_First (V)), Moved (Selection_Last (V)), focused => False, hot => False);
         else
            Draw_Multiline_Text_Edit
              (C, T, Colors, "", 1, Count, 1, 1, 1, focused => False, hot => False);
         end if;
      end;
      if Focused then
         Draw_Focus_Ring (C, T, Colors);
      end if;
      Draw_Vertical_Scrollbar
        (C, Bar_Box (Box), Colors, 1, Last_Top (Text, Box), V.First_Line,
         hot => False, active => False, pageSize => Visible_Lines (Box));
   end Draw;
end CuBit.UI.Text_Views;
