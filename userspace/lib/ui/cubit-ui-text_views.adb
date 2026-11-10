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

   procedure Draw (C : Canvas; Box : Rect; Colors : Theme; V : View; Text : String; Focused : Boolean) is
      T : constant Rect := Text_Box (Box);
   begin
      --  Read-only: no caret; the focus ring shows where keys go.
      Draw_Multiline_Text_Edit
        (C, T, Colors, Text, V.First_Line, Visible_Lines (Box), V.Cursor,
         Selection_First (V), Selection_Last (V), focused => False, hot => False);
      if Focused then
         Draw_Focus_Ring (C, T, Colors);
      end if;
      Draw_Vertical_Scrollbar
        (C, Bar_Box (Box), Colors, 1, Last_Top (Text, Box), V.First_Line,
         hot => False, active => False, pageSize => Visible_Lines (Box));
   end Draw;
end CuBit.UI.Text_Views;
