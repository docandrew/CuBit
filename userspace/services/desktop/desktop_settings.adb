with Ada.Unchecked_Deallocation;
with Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Box_Distribution;
with Desktop_Wallpaper;
with CuBit.UI.Window_Chrome;
with CuBit.UI_Metrics;
with CuBit.Display_Arrangement;
with Desktop_Auto_Scale;

package body Desktop_Settings is
   use CuBit.UI;
   use type CuBit.Appearance.Preferences;
   package A renames CuBit.Appearance;
   use type A.Color_Scheme;
   use type A.Backdrop_Kind;
   use type A.Solid_Color;
   use type A.Placement;
   package WC_Catalog renames Desktop_Wallpaper_Catalog;
   package L renames CuBit.Display_Layouts;
   package G renames L.G;
   use type L.Layout;
   use type L.Named_Display_ID;
   use type G.Logical_Coordinate;
   package M renames CuBit.UI_Metrics;
   package B renames CuBit.UI.Boxes;
   package BD renames Box_Distribution;
   use type B.Text_Style;

   --  Keyboard set-1 make codes the pages act on.
   Scan_Escape : constant := 16#01#;
   Scan_Tab : constant := 16#0F#;
   Scan_Enter : constant := 16#1C#;
   Scan_Space : constant := 16#39#;
   Scan_Home : constant := 16#47#;
   Scan_Up : constant := 16#48#;
   Scan_Page_Up : constant := 16#49#;
   Scan_End : constant := 16#4F#;
   Scan_Down : constant := 16#50#;
   Scan_Page_Down : constant := 16#51#;
   --  Letter keys by make code (set 1), for type-to-jump in the zone list.
   subtype Letter_Scan is Natural range 16#10# .. 16#32#;
   Letter_Of : constant array (Letter_Scan) of Character :=
     [16#10# => 'q', 16#11# => 'w', 16#12# => 'e', 16#13# => 'r', 16#14# => 't',
      16#15# => 'y', 16#16# => 'u', 16#17# => 'i', 16#18# => 'o', 16#19# => 'p',
      16#1E# => 'a', 16#1F# => 's', 16#20# => 'd', 16#21# => 'f', 16#22# => 'g',
      16#23# => 'h', 16#24# => 'j', 16#25# => 'k', 16#26# => 'l',
      16#2C# => 'z', 16#2D# => 'x', 16#2E# => 'c', 16#2F# => 'v', 16#30# => 'b',
      16#31# => 'n', 16#32# => 'm', others => ' '];

   function Img (Value : Natural) return String is
      T : constant String := Value'Image;
   begin
      return T (T'First + 1 .. T'Last);
   end Img;
   function Signed_Img (Value : Integer) return String is
     (if Value < 0 then "-" & Img (-Value) else Img (Value));

   ---------------------------------------------------------------------------
   --  Lists: a field of rows, each Row_Height tall, text List_Inset in.
   ---------------------------------------------------------------------------
   Row_Height : constant := M.Compact_Row_Height;
   List_Inset : constant := M.Text_Inset;
   --  The keyboard focus inside a button or tab, and inside a list.
   Button_Focus_Inset : constant := Focus_Ring_Inset + 1;
   List_Focus_Inset : constant := Focus_Ring_Inset;
   --  A list's width: its widest row text and the insets; it expands.
   function List_Width (Widest : Natural) return BD.Size_Hint is
     ((Minimum => Natural'Min (Widest + 2 * List_Inset, BD.MAXIMUM_EXTENT),
       Preferred => Natural'Min (Widest + 2 * List_Inset, BD.MAXIMUM_EXTENT),
       Kind => BD.Expanding, Stretch => 1));
   --  The text's top in a row at Y.
   function Text_Top (Y : Natural) return Natural is
     (Y + (Row_Height - Natural'Min (Row_Height, UI_Text_Height)) / 2);

   --  The time-zone list: Zone_Rows rows.
   package TZ renames Time_Zones;
   use type TZ.Time_Zone;
   Zone_Rows : constant := 9;
   --  The widest zone name, measured once (the bundled database is fixed).
   Zone_Widest : Natural := 0;
   function Widest_Zone return Natural is
   begin
      if Zone_Widest = 0 then
         for Zone in TZ.Time_Zone loop
            Zone_Widest := Natural'Max (Zone_Widest, UI_Text_Width (TZ.Name (Zone)));
         end loop;
      end if;
      return Zone_Widest;
   end Widest_Zone;
   --  Zone_Top such that Zone is visible, moving the list as little as possible.
   function Scrolled_To (Top, Zone : TZ.Time_Zone; Page : Positive) return TZ.Time_Zone is
     (if Zone < Top then Zone
      elsif TZ.Time_Zone'Pos (Zone) >= TZ.Time_Zone'Pos (Top) + Page
      then TZ.Time_Zone'Val (TZ.Time_Zone'Pos (Zone) - (Page - 1))
      else Top);
   --  The zone rows the list shows (Zone_Rows until placed).
   function Zone_Page (View : State) return Positive is
     (if View.Places.Zone_Height = 0 then Zone_Rows
      else Positive'Max (1, View.Places.Zone_Height / Row_Height));
   --  Zone moved by Rows (negative: up), stopping at the ends.
   function Moved (Zone : TZ.Time_Zone; Rows : Integer) return TZ.Time_Zone is
     (TZ.Time_Zone'Val
        (Integer'Max (TZ.Time_Zone'Pos (TZ.Time_Zone'First),
                      Integer'Min (TZ.Time_Zone'Pos (TZ.Time_Zone'Last), TZ.Time_Zone'Pos (Zone) + Rows))));
   procedure Choose_Zone (View : in out State; Zone : TZ.Time_Zone) is
   begin
      View.Pending_Zone := Zone;
      View.Zone_Top := Scrolled_To (View.Zone_Top, Zone, Zone_Page (View));
      View.Zone_Status := Unchanged;
   end Choose_Zone;
   --  The next zone after From (wrapping) whose name starts with Letter.
   function Next_Starting (From : TZ.Time_Zone; Letter : Character) return TZ.Time_Zone is
      Candidate : TZ.Time_Zone := From;
      Case_Offset : constant := Character'Pos ('a') - Character'Pos ('A');
      Upper : constant Character := Character'Val (Character'Pos (Letter) - Case_Offset);
   begin
      for Step in TZ.Time_Zone loop
         Candidate := (if Candidate = TZ.Time_Zone'Last then TZ.Time_Zone'First else TZ.Time_Zone'Succ (Candidate));
         declare Name : constant String := TZ.Name (Candidate); begin
            if Name'Length > 0 and then Name (Name'First) in Letter | Upper then return Candidate; end if;
         end;
      end loop;
      return From;
   end Next_Starting;

   --  The Appearance page's wallpaper list: Wallpaper_List_Rows rows,
   --  scrolled by its Up/Down buttons and keys.
   Wallpaper_List_Rows : constant := 3;
   No_Wallpapers : constant String := "No wallpapers";
   function Widest_Wallpaper return Natural is
      Widest : Natural := UI_Text_Width (No_Wallpapers);
   begin
      for Row in 1 .. Wallpaper_Rows loop
         Widest := Natural'Max (Widest, UI_Text_Width (Wallpaper_Label (Row)));
      end loop;
      return Widest;
   end Widest_Wallpaper;
   function Wallpaper_Scrolled_To
     (Top, Row : WC_Catalog.Wallpaper_Index; Page : Positive) return WC_Catalog.Wallpaper_Index is
     (if Row < Top then Row
      elsif Row >= Top + Page then Row - (Page - 1)
      else Top);
   function Wallpaper_Page (View : State) return Positive is
     (if View.Places.Wallpaper_Height = 0 then Wallpaper_List_Rows
      else Positive'Max (1, View.Places.Wallpaper_Height / Row_Height));
   function Chosen_Wallpaper (View : State) return Wallpaper_Row is
     (if View.Pending.Kind /= A.Image or else Wallpaper_Rows = 0 then 0
      elsif View.Pending.Image.Length = 0 then WC_Catalog.Default_Wallpaper
      else WC_Catalog.Find (View.Pending.Image));
   procedure Choose_Wallpaper (View : in out State; Row : WC_Catalog.Wallpaper_Index) is
   begin
      View.Pending.Kind := A.Image;
      View.Pending.Image := WC_Catalog.Name (Row);
      View.Wallpaper_Top := Wallpaper_Scrolled_To (View.Wallpaper_Top, Row, Wallpaper_Page (View));
   end Choose_Wallpaper;
   --  The wallpaper Rows away from the chosen one (or from the first),
   --  stopping at the ends.
   function Moved_Wallpaper (View : State; Rows : Integer) return WC_Catalog.Wallpaper_Index is
     (WC_Catalog.Wallpaper_Index
        (Integer'Max (1, Integer'Min (Wallpaper_Rows,
           Integer (Natural'Max (1, Chosen_Wallpaper (View))) + Rows))));

   --  The Monitor page's mode list: a header row, an Auto row, then each
   --  listed mode, in measured columns (size, refresh rate, pixel clock,
   --  status), Mode_Rows rows shown.
   package OM renames CuBit.Output_Modes;
   use type OM.Admission, Interfaces.Unsigned_32;
   Mode_Rows : constant := 11;
   --  The status column's least width: reasons wrap within it.
   Status_Minimum_Width : constant := 10 * M.Base_Em;
   type Mode_Column is (Size_Column, Rate_Column, Clock_Column, Status_Column);
   function Column_Title (Column : Mode_Column) return String is
     (case Column is
        when Size_Column => "Size", when Rate_Column => "Refresh",
        when Clock_Column => "Pixel clock", when Status_Column => "Status");
   Mode_Columns : constant := Mode_Column'Pos (Mode_Column'Last) + 1;
   function Column_Index (Column : Mode_Column) return BD.Item_Index is (Mode_Column'Pos (Column) + 1);
   Column_Gap : constant := M.Space_L;
   Auto_Text : constant String := "Auto: the best mode the display driver admits";
   function Mode_Count (View : State) return Natural is
     (if View.Monitor_View.Modes = null then 0 else View.Monitor_View.Modes'Length);
   function Last_Row (View : State) return Mode_Row is (Mode_Count (View));
   --  Auto and admitted modes can be chosen; refused modes only shown.
   function Selectable (View : State; Row : Mode_Row) return Boolean is
     (Row = Auto_Row or else
      (Row <= Mode_Count (View) and then View.Monitor_View.Modes (Row).Verdict = OM.Admitted));
   --  The selectable row Rows away from the chosen one (negative: up),
   --  stopping at the ends; refused rows are skipped.
   function Moved_Mode (View : State; Rows : Integer) return Mode_Row is
      Target : constant Integer :=
        Integer'Max (0, Integer'Min (Integer (Last_Row (View)), Integer (View.Mode_Choice) + Rows));
      Step : constant Integer := (if Rows < 0 then -1 else 1);
      Row : Integer := Target;
   begin
      --  The nearest selectable row at or past the target, else before it.
      while Row in 0 .. Integer (Last_Row (View)) loop
         if Selectable (View, Mode_Row (Row)) then return Mode_Row (Row); end if;
         Row := Row + Step;
      end loop;
      Row := Target;
      while Row in 0 .. Integer (Last_Row (View)) loop
         if Selectable (View, Mode_Row (Row)) then return Mode_Row (Row); end if;
         Row := Row - Step;
      end loop;
      return View.Mode_Choice;
   end Moved_Mode;
   --  "59.940" from 59940 (millihertz, or kHz as MHz).
   function Thousandths (Value : Natural) return String is
      F : constant String := Natural'Image (1000 + Value mod 1000);
   begin
      return Img (Value / 1000) & "." & F (F'First + 2 .. F'Last);
   end Thousandths;
   --  What the status column says about a mode: its verdict when refused,
   --  and whether it is current, preferred or saved.
   function Notes_Text (View : State; Row : Mode_Row) return String is
      Item : OM.Listed_Mode renames View.Monitor_View.Modes (Row);
      Saved : constant Boolean := not View.Monitor_View.Saved_Auto and then View.Monitor_View.Saved_Row = Row;
      function Join (Left, Right : String) return String is
        (if Left'Length = 0 then Right elsif Right'Length = 0 then Left else Left & ", " & Right);
   begin
      return Join (Join (Join ((if Item.Verdict = OM.Admitted then "" else OM.Name (Item.Verdict)),
                               (if Item.Current then "current" else "")),
                         (if Item.Preferred then "preferred" else "")),
                   (if Saved then "saved" else ""));
   end Notes_Text;
   function Cell_Text (View : State; Row : Mode_Row; Column : Mode_Column) return String is
      Item : OM.Listed_Mode renames View.Monitor_View.Modes (Row);
   begin
      case Column is
         when Size_Column =>
            return Img (Natural (Item.Size.Width)) & " x " & Img (Natural (Item.Size.Height));
         when Rate_Column =>
            return (if Item.Refresh_Millihertz = 0 then ""
                    else Thousandths (Natural (Item.Refresh_Millihertz)) & " Hz");
         when Clock_Column =>
            return (if Item.Pixel_kHz = 0 then "" else Thousandths (Natural (Item.Pixel_kHz)) & " MHz");
         when Status_Column =>
            return Notes_Text (View, Row);
      end case;
   end Cell_Text;
   --  Each column as wide as its widest cell or title; the status column
   --  takes what the list has left.
   function Column_Hints (View : State) return BD.Hint_Array is
      Hints : BD.Hint_Array (1 .. Mode_Columns);
   begin
      for Column in Mode_Column loop
         declare
            Widest : Natural := UI_Text_Width (Column_Title (Column));
         begin
            for Row in 1 .. Mode_Count (View) loop
               Widest := Natural'Max (Widest, UI_Text_Width (Cell_Text (View, Row, Column)));
            end loop;
            Widest := Natural'Min (Widest, BD.MAXIMUM_EXTENT);
            if Column = Status_Column then
               --  Reasons wrap (the row grows), so the longest reason does
               --  not set the list's width: its longest word, the title, or
               --  Status_Minimum_Width does.
               declare
                  Least : Natural := Natural'Min (Widest, Status_Minimum_Width);
               begin
                  Least := Natural'Max (Least, UI_Text_Width (Column_Title (Column)));
                  for Row in 1 .. Mode_Count (View) loop
                     Least := Natural'Max (Least, B.Longest_Word (Notes_Text (View, Row), B.Body_Text));
                  end loop;
                  Least := Natural'Min (Least, Widest);
                  Hints (Column_Index (Column)) :=
                    (Minimum => Least, Preferred => Widest, Kind => BD.Expanding, Stretch => 1);
               end;
            else
               Hints (Column_Index (Column)) := BD.Fixed_Hint (Widest);
            end if;
         end;
      end loop;
      return Hints;
   end Column_Hints;
   function Mode_List_Width (View : State) return BD.Size_Hint is
      Hints : constant BD.Hint_Array := Column_Hints (View);
      Least : constant Natural := Natural'Max (UI_Text_Width (Auto_Text), BD.Required (Hints, Column_Gap));
      Wanted : Natural := (if Hints'Length = 0 then 0 else (Hints'Length - 1) * Column_Gap);
   begin
      for H of Hints loop
         Wanted := Wanted + H.Preferred;
      end loop;
      Wanted := Natural'Max (Least, Wanted);
      return (Minimum => Natural'Min (Least + 2 * List_Inset, BD.MAXIMUM_EXTENT),
              Preferred => Natural'Min (Wanted + 2 * List_Inset, BD.MAXIMUM_EXTENT),
              Kind => BD.Expanding, Stretch => 1);
   end Mode_List_Width;

   --  The columns shared out over a mode list at List.
   function Mode_Spans (View : State; List : Rect) return BD.Span_Array is
      Spans : BD.Span_Array (1 .. Mode_Columns);
      Status : BD.Fit;
   begin
      BD.Distribute (Column_Hints (View), Natural'Min (List.x + List_Inset, BD.MAXIMUM_EXTENT),
                     Natural'Min (List.w - Natural'Min (List.w, 2 * List_Inset), BD.MAXIMUM_EXTENT),
                     Column_Gap, Spans, Status);
      return Spans;
   end Mode_Spans;

   --  A mode row: one line, or as many as its status wraps to at Width.
   function Status_Lines (View : State; Row : Mode_Row; Width : Natural) return Positive is
     (if Row = Auto_Row or else Row > Last_Row (View) or else Width = 0 then 1
      else Positive'Max (1, B.Line_Count (Notes_Text (View, Row), B.Body_Text, Width)));
   function Mode_Line_Height (View : State; Row : Mode_Row; Width : Natural) return Positive is
     (Row_Height + (Status_Lines (View, Row, Width) - 1) * M.Line_Height);
   --  The rows from Top that fit whole in Space (at least one).
   function Mode_Rows_Fitting (View : State; Top : Mode_Row; Space, Width : Natural) return Positive is
      Used : Natural := 0;
      Count : Natural := 0;
   begin
      for Row in Top .. Last_Row (View) loop
         Used := Used + Mode_Line_Height (View, Row, Width);
         exit when Used > Space;
         Count := Count + 1;
      end loop;
      return Positive'Max (1, Count);
   end Mode_Rows_Fitting;
   --  The placed list's height under its header.
   function Mode_Space (View : State) return Natural is
     (View.Places.Mode_Height - Natural'Min (View.Places.Mode_Height, Row_Height));
   function Mode_Page (View : State) return Positive is
     (if View.Places.Mode_Height = 0 then Mode_Rows
      else Mode_Rows_Fitting (View, View.Mode_Top, Mode_Space (View), View.Places.Status_Width));
   --  The top row nearest Top from which Row shows.
   function Mode_Scrolled_To (View : State; Top, Row : Mode_Row) return Mode_Row is
      First : Mode_Row := Top;
   begin
      if Row < Top then
         return Row;
      elsif View.Places.Mode_Height = 0 then
         return (if Row >= Top + Mode_Rows then Row - (Mode_Rows - 1) else Top);
      end if;
      while First < Row and then
        Row - First >= Mode_Rows_Fitting (View, First, Mode_Space (View), View.Places.Status_Width)
      loop
         First := First + 1;
      end loop;
      return First;
   end Mode_Scrolled_To;
   --  The last top row: the last row shows at the bottom.
   function Last_Mode_Top (View : State) return Mode_Row is (Mode_Scrolled_To (View, Auto_Row, Last_Row (View)));
   procedure Choose_Mode (View : in out State; Row : Mode_Row) is
   begin
      if Row <= Last_Row (View) and then Selectable (View, Row) then
         View.Mode_Choice := Row;
         View.Mode_Top := Mode_Scrolled_To (View, View.Mode_Top, Row);
         if View.Monitor_Status /= Mode_Switching then
            View.Monitor_Status := Mode_Ready;
         end if;
      end if;
   end Choose_Mode;

   function Chosen_Mode (View : State) return OM.Listed_Mode is
     (View.Monitor_View.Modes (View.Mode_Choice));

   ---------------------------------------------------------------------------
   --  The arrangement diagram (Displays): every display's tile scaled by
   --  one divisor so all fit inside Area less Diagram_Margin each side.
   ---------------------------------------------------------------------------
   Diagram_Margin : constant := M.Space_XL;
   Primary_Bar_Inset : constant := M.Space_XS + 1;
   Primary_Bar_Height : constant := 2 * M.Space_XS;

   function Projection (Layout : L.Layout; Area : Rect) return Diagram_Transform is
      Left, Top : Integer := Integer'Last;
      Right, Bottom : Integer := Integer'First;
      Divisor, Width, Height : Positive;
      Inner_W : constant Positive := Positive'Max (1, Area.w - Natural'Min (Area.w, 2 * Diagram_Margin));
      Inner_H : constant Positive := Positive'Max (1, Area.h - Natural'Min (Area.h, 2 * Diagram_Margin));
   begin
      if Layout.Count = 0 then return (others => <>); end if;
      for I in 1 .. Layout.Count loop
         declare R : constant G.Logical_Rectangle := G.Bounds (Layout.Items (I).Geometry); begin
            Left := Integer'Min (Left, Integer (R.Left));
            Top := Integer'Min (Top, Integer (R.Top));
            Right := Integer'Max (Right, Integer (R.Right));
            Bottom := Integer'Max (Bottom, Integer (R.Bottom));
         end;
      end loop;
      Width := Positive (Right - Left);
      Height := Positive (Bottom - Top);
      Divisor := Positive'Max (1, Positive'Max ((Width + Inner_W - 1) / Inner_W, (Height + Inner_H - 1) / Inner_H));
      return (Integer (Area.x) + (Integer (Area.w) - Width / Divisor) / 2,
              Integer (Area.y) + (Integer (Area.h) - Height / Divisor) / 2,
              Left, Top, Divisor);
   end Projection;

   function Tile (View : State; Area : Rect; I : L.Viewport_Index) return Rect is
      T : Diagram_Transform := (if View.Dragging then View.Transform
                               else Projection (View.Pending_Layout, Area));
      R : constant G.Logical_Rectangle := G.Bounds (View.Pending_Layout.Items (I).Geometry);
   begin
      if View.Dragging and View.Pending_Layout.Count > 1 then
         -- Normalization moves the desktop origin, not the stationary tile.
         declare Anchor : constant L.Viewport_Index := (if View.Selected = 1 then 2 else 1); begin
            T.Logical_X := T.Logical_X + Integer (View.Pending_Layout.Items (Anchor).Geometry.X)
              - Integer (View.Drag_Original.Items (Anchor).Geometry.X);
            T.Logical_Y := T.Logical_Y + Integer (View.Pending_Layout.Items (Anchor).Geometry.Y)
              - Integer (View.Drag_Original.Items (Anchor).Geometry.Y);
         end;
      end if;
      return (Natural'Max (0, T.X + (Integer (R.Left) - T.Logical_X) / T.Divisor),
              Natural'Max (0, T.Y + (Integer (R.Top) - T.Logical_Y) / T.Divisor),
              Natural'Max (1, Integer (R.Right - R.Left) / T.Divisor),
              Natural'Max (1, Integer (R.Bottom - R.Top) / T.Divisor));
   end Tile;

   ---------------------------------------------------------------------------
   --  The window's layout (UI-033)
   ---------------------------------------------------------------------------
   --  Widget ids: a control's 'Pos (None is B.No_Id), then the parts that
   --  are not controls.
   type Part is (Sidebar_Part, Sidebar_Title_Part, Page_Part, Preview_Part, Diagram_Part);
   function Id (Item : Control) return B.Widget_Id is (Control'Pos (Item));
   function Id (Item : Part) return B.Widget_Id is (Control'Pos (Control'Last) + 1 + Part'Pos (Item));
   function Control_Of (Item : B.Widget_Id) return Control is
     (if Item in 1 .. Control'Pos (Control'Last) then Control'Val (Item) else None);

   --  The wallpaper preview: a 16:10 field with a miniature window.
   Preview_Width : constant := 15 * M.Base_Em;
   Preview_Height : constant := Preview_Width * 10 / 16;
   --  The arrangement diagram's least and natural size.
   Diagram_Minimum_Width : constant := 20 * M.Base_Em;
   Diagram_Width : constant := 34 * M.Base_Em;
   Diagram_Minimum_Height : constant := 8 * M.Base_Em;
   Diagram_Height : constant := 11 * M.Base_Em;
   --  A tree for one page: every widget of the largest page (Monitor with
   --  two attached monitors is about 50 nodes) with room, and its label
   --  text (each Monitor_Line at most Monitor_Text_Length). Tests check no
   --  page overflows.
   Layout_Nodes : constant := 128;
   Layout_Text : constant := 32 * Monitor_Text_Length;
   subtype Layout_Tree is B.Tree (Layout_Nodes, Layout_Text);

   --  The active window chrome in miniature inside the wallpaper preview:
   --  two thirds of it, centred.
   function Miniature_Window (Preview : Rect) return Rect is
     ((Preview.x + Preview.w / 6, Preview.y + Preview.h * 3 / 16, Preview.w * 2 / 3, Preview.h * 5 / 8));

   function Label (Item : Control) return String is
     (case Item is when Appearance_Tab => "Appearance", when Displays_Tab => "Displays",
      when Date_Time_Tab => "Date & Time", when Monitor_Tab => "Monitor", when Zone_Previous => "Up",
      when Zone_Next => "Down", when Light => "Alloy Light", when Dark => "Alloy Dark",
      when Slate => "Slate", when Ocean => "Ocean",
      when Wallpaper_Previous => "Up", when Wallpaper_Next => "Down",
      when Fill => "Fill", when Fit => "Fit",
      when Center => "Center", when Apply => "Apply", when Revert => "Revert",
      when Make_Primary => "Make primary",
      when Scale_Down => "-", when Scale_Up => "+", when Scale_Auto => "Auto",
      when Mode_Previous => "Up", when Mode_Next => "Down",
      when Keep_Mode => "Keep", when Revert_Mode => "Revert",
      when None | Zone_List | Mode_List | Wallpaper_List => "");

   function Page_Title (View : State) return String is
     (case View.Current_Page is
        when Appearance => "Appearance", when Displays => "Displays",
        when Date_Time => "Date & Time", when Monitor => "Monitor");

   --  The line under each page's controls, beside its two action buttons.
   function Status_Text (View : State) return String is
   begin
      case View.Current_Page is
         when Appearance =>
            return (if View.Pending /= View.Applied then "Unapplied changes"
                    elsif View.Status = Saved_In_Config then "Applied / Config is volatile until reboot"
                    elsif View.Status = Session_Only then "Applied / Config unavailable"
                    else "Tab to navigate; Enter to choose");
         when Displays =>
            return (if View.Layout_Status = Scale_Rejected then "Scale needs 800 x 480 logical pixels"
                    elsif View.Layout_Status = Rejected then "Could not apply arrangement"
                    elsif View.Pending_Layout /= View.Applied_Layout or
                      View.Pending_Primary /= View.Applied_Primary or
                      View.Pending_Auto /= View.Applied_Auto then "Unapplied arrangement"
                    elsif View.Layout_Status = Saved_In_Config then "Applied / scale kept in Config until reboot"
                    elsif View.Layout_Status = Session_Only then "Applied / this session"
                    else "Resolutions stay unchanged");
         when Date_Time =>
            return (if View.Pending_Zone /= View.Applied_Zone then "Unapplied time zone"
                    elsif View.Zone_Status = Saved_In_Config then "Applied / kept in Config until reboot"
                    elsif View.Zone_Status = Rejected then "Config refused the time zone"
                    else "Tab to navigate; Enter to choose");
         when Monitor =>
            if View.Trial_Open then
               return "Keep this display mode? It reverts in " & Img (View.Trial_Seconds) &
                 (if View.Trial_Seconds = 1 then " second." else " seconds.");
            end if;
            return (case View.Monitor_Status is
                      when Mode_Ready =>
                        (if View.Monitor_View.Modes = null then "This output lists no modes"
                         else "Choose a mode and Apply; refused modes show why"),
                      when Mode_Switching => "Switching the display mode...",
                      when Mode_Refused => "The display driver refused that mode",
                      when Mode_Kept =>
                        (if View.Monitor_View.Identified then "Kept for this monitor until reboot"
                         else "Kept for this session"),
                      when Mode_Saved_Session_Only => "Kept; Config refused to save it",
                      when Mode_Reverted => "Reverted to the previous mode",
                      when Mode_Timed_Out => "No answer: reverted to the previous mode");
      end case;
   end Status_Text;

   function Text_Of (Line : Monitor_Line) return String is (Line.Text (1 .. Line.Length));

   --  A list beside its Up and Down buttons, which span the list's height.
   --  At least Rows rows show; Grow: more when the window is taller.
   procedure Add_List
     (T : in out Layout_Tree; List, Up, Down : Control; Width : BD.Size_Hint; Rows : Positive;
      Grow : Boolean; Header : Natural := 0)
   is
      Least : constant Natural := Header + Rows * Row_Height;
   begin
      --  The row is as tall as the list; the buttons' column takes that
      --  height and its widest button's width, the list the rest.
      B.Begin_Row (T, Align => B.Align_Start, Height_Growth => (if Grow then B.From_Content else B.Shrink_Wrap));
      B.Add_Block (T, Id (List), Width,
                   (Least, Least, (if Grow then BD.Expanding else BD.Fixed), 1), Scrolls => True);
      B.Begin_Column (T, Spacing => M.Space_XS, Width_Growth => B.Shrink_Wrap, Height_Growth => B.Fill);
      B.Add_Button (T, Label (Up), Id (Up), Fill => True);
      B.Add_Glue (T);
      B.Add_Button (T, Label (Down), Id (Down), Fill => True);
      B.End_Box (T);
      B.End_Box (T);
   end Add_List;

   --  Monitors attached to outputs nothing drives, one row each.
   Attached_Columns : constant := 4;
   procedure Add_Attached (T : in out Layout_Tree; View : State) is
      Info : Monitor_Info renames View.Monitor_View;
   begin
      if Info.Attached_Used = 0 then
         return;
      end if;
      B.Add_Label (T, "Also connected, not in use:");
      B.Begin_Grid (T, Columns => Attached_Columns, Margin => M.Space_S);
      for Line in 1 .. Info.Attached_Used loop
         B.Add_Label (T, Text_Of (Info.Attached (Line).Connector));
         B.Add_Label (T, Text_Of (Info.Attached (Line).Name));
         B.Add_Label (T, Text_Of (Info.Attached (Line).Size), B.Muted_Text);
         B.Add_Label (T, Text_Of (Info.Attached (Line).Best), B.Muted_Text);
      end loop;
      B.End_Box (T);
   end Add_Attached;

   --  The status line with the page's two action buttons right of it.
   procedure Add_Actions (T : in out Layout_Tree; View : State; First, Second : Control) is
   begin
      B.Begin_Row (T);
      B.Add_Wrapping_Label (T, Status_Text (View));
      B.Add_Button (T, Label (First), Id (First));
      B.Add_Button (T, Label (Second), Id (Second));
      B.End_Box (T);
   end Add_Actions;

   procedure Describe (View : State; T : in out Layout_Tree) is
   begin
      B.Clear (T);
      B.Begin_Row (T, Spacing => 0, Align => B.Align_Start);
      --  The sidebar: a title and one tab per page, as wide as the widest.
      B.Begin_Column (T, Id (Sidebar_Part), Spacing => M.Space_XS, Margin => M.Space_M,
                      Width_Growth => B.Shrink_Wrap, Height_Growth => B.Fill);
      B.Add_Label (T, "Settings", Id => Id (Sidebar_Title_Part));
      for Tab in Tab_Control loop
         B.Add_Tab (T, Label (Tab), Id (Tab));
      end loop;
      B.End_Box (T);
      B.Begin_Column (T, Id (Page_Part), Margin => M.Space_XL,
                      Width_Growth => B.Fill, Height_Growth => B.Fill);
      B.Add_Label (T, Page_Title (View));
      case View.Current_Page is
         when Appearance =>
            B.Begin_Row (T, Spacing => M.Space_XL, Align => B.Align_Start);
            B.Begin_Column (T);
            B.Add_Block (T, Id (Preview_Part), BD.Fixed_Hint (Preview_Width), BD.Fixed_Hint (Preview_Height));
            B.Add_Label (T, "Preview", B.Muted_Text);
            B.End_Box (T);
            B.Begin_Column (T, Width_Growth => B.Fill);
            B.Add_Label (T, "Alloy theme");
            B.Begin_Row (T);
            B.Add_Button (T, Label (Light), Id (Light));
            B.Add_Button (T, Label (Dark), Id (Dark));
            B.End_Box (T);
            B.Add_Label (T, "Background");
            B.Begin_Row (T);
            B.Add_Button (T, Label (Slate), Id (Slate));
            B.Add_Button (T, Label (Ocean), Id (Ocean));
            B.End_Box (T);
            Add_List (T, Wallpaper_List, Wallpaper_Previous, Wallpaper_Next,
                      List_Width (Widest_Wallpaper), Wallpaper_List_Rows, Grow => False);
            B.Add_Label (T, "Wallpaper placement");
            B.Begin_Row (T);
            B.Add_Button (T, Label (Fill), Id (Fill));
            B.Add_Button (T, Label (Fit), Id (Fit));
            B.Add_Button (T, Label (Center), Id (Center));
            B.End_Box (T);
            B.End_Box (T);
            B.End_Box (T);
            B.Add_Wrapping_Label
              (T, "Changes apply to this desktop and shared-toolkit applications. " &
                  "Apply reloads theme CCL from Config. No restart needed.", B.Muted_Text);
            B.Add_Glue (T);
            Add_Actions (T, View, Apply, Revert);
         when Displays =>
            B.Add_Wrapping_Label (T, "Arrange displays: drag a screen to match your desk.");
            B.Add_Block (T, Id (Diagram_Part),
              (Diagram_Minimum_Width, Diagram_Width, BD.Expanding, 1),
              (Diagram_Minimum_Height, Diagram_Height, BD.Expanding, 1));
            if View.Selected in 1 .. View.Pending_Layout.Count then
               declare Screen : G.Output renames View.Pending_Layout.Items (View.Selected).Geometry; begin
                  B.Add_Wrapping_Label
                    (T, "Display " & Img (View.Selected) & ": " & Img (Natural (Screen.Width)) & " x " &
                        Img (Natural (Screen.Height)) & " pixels, position " & Signed_Img (Integer (Screen.X)) &
                        ", " & Signed_Img (Integer (Screen.Y)));
               end;
            end if;
            B.Add_Wrapping_Label
              (T, "Edges snap together. The white bar marks the primary display.", B.Muted_Text);
            B.Begin_Row (T);
            B.Add_Button (T, Label (Make_Primary), Id (Make_Primary));
            B.Add_Glue (T);
            if View.Selected in 1 .. View.Pending_Layout.Count then
               declare Screen : G.Output renames View.Pending_Layout.Items (View.Selected).Geometry; begin
                  B.Add_Label
                    (T, "Scale " &
                        Img (100 * Natural (Screen.Scale.Numerator) / Natural (Screen.Scale.Denominator)) & "%" &
                        (if View.Pending_Auto then " (auto)" else ""));
               end;
            end if;
            B.Add_Button (T, Label (Scale_Down), Id (Scale_Down));
            B.Add_Button (T, Label (Scale_Up), Id (Scale_Up));
            B.Add_Button (T, Label (Scale_Auto), Id (Scale_Auto));
            B.End_Box (T);
            Add_Attached (T, View);
            Add_Actions (T, View, Apply, Revert);
         when Date_Time =>
            B.Add_Label (T, "Time zone (IANA time zone database)");
            Add_List (T, Zone_List, Zone_Previous, Zone_Next, List_Width (Widest_Zone), Zone_Rows, Grow => True);
            B.Add_Wrapping_Label (T, "In Config: " & TZ.Name (View.Applied_Zone));
            B.Add_Wrapping_Label (T, "Type a letter to jump; arrows, Page Up/Down, Home, End move.", B.Muted_Text);
            Add_Actions (T, View, Apply, Revert);
         when Monitor =>
            B.Add_Wrapping_Label (T, Text_Of (View.Monitor_View.Summary));
            B.Add_Wrapping_Label (T, Text_Of (View.Monitor_View.Detail), B.Muted_Text);
            Add_List (T, Mode_List, Mode_Previous, Mode_Next, Mode_List_Width (View), Mode_Rows,
                      Grow => True, Header => Row_Height);
            B.Add_Wrapping_Label
              (T, (if View.Monitor_View.Identified then "Kept per monitor until reboot (EDID identity)"
                   else "No EDID identity: choices last this session"), B.Muted_Text);
            Add_Attached (T, View);
            if View.Trial_Open then
               Add_Actions (T, View, Keep_Mode, Revert_Mode);
            else
               Add_Actions (T, View, Apply, Revert);
            end if;
      end case;
      B.End_Box (T);
      B.End_Box (T);
   end Describe;

   procedure Lay_Out (View : State; Window_Bounds : Rect; T : in out Layout_Tree) is
   begin
      Describe (View, T);
      B.Arrange (T, Window_Bounds);
   end Lay_Out;

   procedure Measure (View : State; Minimum, Natural_Size : out B.Size) is
      T : Layout_Tree;
      Shown : State := View;
   begin
      Minimum := (0, 0);
      Natural_Size := (0, 0);
      --  Shown shares View's mode list (a copy of the access, not freed).
      for P in Page loop
         Shown.Current_Page := P;
         Describe (Shown, T);
         declare
            Least, Wanted : B.Size;
         begin
            B.Measure (T, Least, Wanted);
            Minimum := (Natural'Max (Minimum.Width, Least.Width), Natural'Max (Minimum.Height, Least.Height));
            Natural_Size := (Natural'Max (Natural_Size.Width, Wanted.Width),
                             Natural'Max (Natural_Size.Height, Wanted.Height));
         end;
      end loop;
   end Measure;

   function Control_Bounds (View : State; Window_Bounds : Rect; Item : Control) return Rect is
      T : Layout_Tree;
   begin
      Lay_Out (View, Window_Bounds, T);
      return B.Bounds (T, Id (Item));
   end Control_Bounds;

   function Fits (View : State; Window_Bounds : Rect) return Boolean is
      T : Layout_Tree;
   begin
      Lay_Out (View, Window_Bounds, T);
      return B.Fits (T) and then not B.Overflowed (T);
   end Fits;

   --  A list's first row: under the mode list's header.
   function First_Row_Y (List : Control; Box : Rect) return Natural is
     (if List = Mode_List then Box.y + Row_Height else Box.y);

   --  The height of a list's Line (from its top row shown); Status_Width
   --  is the mode list's status column.
   function Line_Height_Of (View : State; List : Control; Line : Natural; Status_Width : Natural)
     return Positive is
     (if List = Mode_List then Mode_Line_Height (View, View.Mode_Top + Line, Status_Width) else Row_Height);
   function Status_Width_Of (View : State; List : Control; Box : Rect) return Natural is
     (if List = Mode_List then Mode_Spans (View, Box) (Column_Index (Status_Column)).Length else 0);

   function Row_Bounds (View : State; Window_Bounds : Rect; List : Control; Line : Natural) return Rect is
      Box : constant Rect := Control_Bounds (View, Window_Bounds, List);
      Width : constant Natural := Status_Width_Of (View, List, Box);
      Y : Natural := First_Row_Y (List, Box);
   begin
      for Before in 0 .. Line - 1 loop
         Y := Y + Line_Height_Of (View, List, Before, Width);
      end loop;
      return (Box.x, Y, Box.w, Line_Height_Of (View, List, Line, Width));
   end Row_Bounds;

   procedure Place (View : in out State; Window_Bounds : Rect) is
      T : Layout_Tree;
   begin
      Lay_Out (View, Window_Bounds, T);
      case View.Current_Page is
         when Date_Time => View.Places.Zone_Height := B.Bounds (T, Id (Zone_List)).h;
         when Appearance => View.Places.Wallpaper_Height := B.Bounds (T, Id (Wallpaper_List)).h;
         when Monitor =>
            View.Places.Mode_Height := B.Bounds (T, Id (Mode_List)).h;
            View.Places.Status_Width := Status_Width_Of (View, Mode_List, B.Bounds (T, Id (Mode_List)));
         when Displays => null;
      end case;
   end Place;

   function Page_Rows (View : State; List : Control) return Positive is
     (case List is
        when Zone_List => Zone_Page (View),
        when Mode_List => Mode_Page (View),
        when others => Wallpaper_Page (View));

   procedure Wheel (View : in out State; Window_Bounds : Rect; X, Y : Natural; Notches : Integer) is
      T : Layout_Tree;
      By : constant Integer := -(Notches * Wheel_Rows);
      function Clamped (Value, Low, High : Integer) return Integer is
        (Integer'Max (Low, Integer'Min (High, Value)));
   begin
      Place (View, Window_Bounds);
      Lay_Out (View, Window_Bounds, T);
      case Control_Of (B.Scroll_Target (T, X, Y)) is
         when Zone_List =>
            View.Zone_Top := TZ.Time_Zone'Val
              (Clamped (TZ.Time_Zone'Pos (View.Zone_Top) + By, TZ.Time_Zone'Pos (TZ.Time_Zone'First),
                        TZ.Time_Zone'Pos (Scrolled_To (TZ.Time_Zone'First, TZ.Time_Zone'Last, Zone_Page (View)))));
         when Mode_List =>
            View.Mode_Top := Mode_Row (Clamped (Integer (View.Mode_Top) + By, Auto_Row, Last_Mode_Top (View)));
         when Wallpaper_List =>
            if Wallpaper_Rows > 0 then
               View.Wallpaper_Top := Clamped
                 (View.Wallpaper_Top + By, 1, Wallpaper_Scrolled_To (1, Wallpaper_Rows, Wallpaper_Page (View)));
            end if;
         when others =>
            null;
      end case;
   end Wheel;

   --  Names for Publish_Layout (the runtime keeps no enumeration images).
   function Control_Name (Item : Control) return String is
     (case Item is
        when None => "NONE", when Appearance_Tab => "APPEARANCE_TAB", when Displays_Tab => "DISPLAYS_TAB",
        when Date_Time_Tab => "DATE_TIME_TAB", when Monitor_Tab => "MONITOR_TAB", when Light => "LIGHT",
        when Dark => "DARK", when Slate => "SLATE", when Ocean => "OCEAN", when Wallpaper_List => "WALLPAPER_LIST",
        when Wallpaper_Previous => "WALLPAPER_PREVIOUS", when Wallpaper_Next => "WALLPAPER_NEXT",
        when Fill => "FILL", when Fit => "FIT", when Center => "CENTER", when Apply => "APPLY",
        when Revert => "REVERT", when Make_Primary => "MAKE_PRIMARY", when Scale_Down => "SCALE_DOWN",
        when Scale_Up => "SCALE_UP", when Scale_Auto => "SCALE_AUTO", when Zone_List => "ZONE_LIST",
        when Zone_Previous => "ZONE_PREVIOUS", when Zone_Next => "ZONE_NEXT", when Mode_List => "MODE_LIST",
        when Mode_Previous => "MODE_PREVIOUS", when Mode_Next => "MODE_NEXT", when Keep_Mode => "KEEP_MODE",
        when Revert_Mode => "REVERT_MODE");
   function Part_Name (Item : Part) return String is
     (case Item is
        when Sidebar_Part => "SIDEBAR", when Sidebar_Title_Part => "SIDEBAR_TITLE", when Page_Part => "PAGE",
        when Preview_Part => "PREVIEW", when Diagram_Part => "DIAGRAM");

   procedure Publish_Layout (View : State; Window_Bounds : Rect) is
      T : Layout_Tree;
      procedure Line (Name : String; Box : Rect) is
      begin
         Put ("Settings layout " & Name & " " & Img (Box.x) & " " & Img (Box.y) & " " & Img (Box.w) & " " &
              Img (Box.h));
      end Line;
      procedure Named (Item : B.Widget_Id; Box : Rect) is
      begin
         if Control_Of (Item) /= None then
            Line (Control_Name (Control_Of (Item)), Box);
         else
            for P in Part loop
               if Id (P) = Item then
                  Line (Part_Name (P), Box);
               end if;
            end loop;
         end if;
      end Named;
      procedure Each is new B.Visit_Named (Named);
   begin
      Lay_Out (View, Window_Bounds, T);
      Each (T);
      if View.Current_Page = Appearance then
         Line ("PREVIEW_WINDOW", Miniature_Window (B.Bounds (T, Id (Preview_Part))));
      end if;
   end Publish_Layout;

   procedure Open (View : out State; Current : A.Preferences; Layout : L.Layout;
                   Primary : L.Named_Display_ID; Auto_Scale : Boolean;
                   Zone : Time_Zones.Time_Zone) is
      --  The mode list lives on (Set_Monitor replaces and frees it).
      Kept : constant Monitor_Info := View.Monitor_View;
   begin
      View := (Monitor_View => Kept, Pending => Current, Applied => Current,
               Pending_Layout => Layout, Applied_Layout => Layout,
               Pending_Primary => Primary, Applied_Primary => Primary,
               Pending_Auto => Auto_Scale, Applied_Auto => Auto_Scale,
               Pending_Zone => Zone, Applied_Zone => Zone,
               Zone_Top => Scrolled_To (TZ.Time_Zone'First, Zone, Zone_Rows),
               Selected => (if Layout.Count > 0 then 1 else 0), others => <>);
      if Chosen_Wallpaper (View) > 0 then
         View.Wallpaper_Top := Wallpaper_Scrolled_To (1, Chosen_Wallpaper (View), Wallpaper_List_Rows);
      end if;
   end Open;

   function Line_Of (Text : String; Available : Boolean := False) return Monitor_Line is
      Result : Monitor_Line;
      Count : constant Natural := Natural'Min (Text'Length, Monitor_Text_Length);
   begin
      Result.Text (1 .. Count) := Text (Text'First .. Text'First + Count - 1);
      Result.Length := Count;
      Result.Available := Available;
      return Result;
   end Line_Of;

   function Describe_Attached (Item : CuBit.Output_Discovery.Description; Name : String) return Attached_Monitor is
      package OD renames CuBit.Output_Discovery;
      Kind : constant String :=
        (case Item.Connector is
           when OD.Unknown_Connector => "Display", when OD.HDMI => "HDMI", when OD.DVI => "DVI",
           when OD.DisplayPort => "DisplayPort", when OD.Embedded_DisplayPort => "eDP");
   begin
      return (Connector => Line_Of (Kind),
              Name => Line_Of (Name),
              Size => (if Item.Width_MM /= 0 and then Item.Height_MM /= 0
                       then Line_Of (Img (Item.Width_MM) & " x " & Img (Item.Height_MM) & " mm")
                       else Line_Of ("")),
              --  An output without an EDID advertises the smallest extent.
              Best => (if Item.Advertised_Width > OD.Extent'First and then Item.Advertised_Height > OD.Extent'First
                       then Line_Of ("best " & Img (Item.Advertised_Width) & " x " & Img (Item.Advertised_Height))
                       else Line_Of ("")));
   end Describe_Attached;

   procedure Release is new Ada.Unchecked_Deallocation (Monitor_Modes, Monitor_Mode_List);
   procedure Free (Modes : in out Monitor_Mode_List) is
   begin
      Release (Modes);
   end Free;

   procedure Set_Monitor (View : in out State; Info : Monitor_Info) is
      Old : Monitor_Mode_List := View.Monitor_View.Modes;
   begin
      if Old /= Info.Modes then
         Free (Old);
      end if;
      View.Monitor_View := Info;
      View.Mode_Choice := (if Info.Saved_Auto then Auto_Row else Info.Saved_Row);
      if not Selectable (View, View.Mode_Choice) then
         View.Mode_Choice := Auto_Row;
      end if;
      View.Mode_Top := Mode_Scrolled_To (View, Auto_Row, View.Mode_Choice);
   end Set_Monitor;

   function Visible (View : State; Item : Control) return Boolean is
     (Item in Tab_Control or else
       (Item in Apply .. Revert and then not (View.Current_Page = Monitor and then View.Trial_Open)) or else
       (View.Current_Page = Monitor and then
          (if View.Trial_Open then Item in Keep_Mode .. Revert_Mode else Item in Mode_List .. Mode_Next)) or else
       (View.Current_Page = Date_Time and then Item in Zone_List .. Zone_Next) or else
       (View.Current_Page = Displays and then Item in Make_Primary .. Scale_Auto) or else
       (View.Current_Page = Appearance and then Item in Light .. Revert));

   procedure Render (View : State; C : Canvas; Window_Bounds : Rect) is
      Colors : constant Theme := Current_Theme;
      Preview_Colors : constant Theme := Palette (View.Pending.Scheme);
      T : Layout_Tree;
      Sidebar : Rect;
      function Selected (Item : Control) return Boolean is
        (case Item is
         when Appearance_Tab => View.Current_Page = Appearance,
         when Displays_Tab => View.Current_Page = Displays,
         when Date_Time_Tab => View.Current_Page = Date_Time,
         when Monitor_Tab => View.Current_Page = Monitor,
         when Light => View.Pending.Scheme = A.Alloy_Light,
         when Dark => View.Pending.Scheme = A.Alloy_Dark,
         when Slate => View.Pending.Kind = A.Solid and then View.Pending.Color = A.Slate,
         when Ocean => View.Pending.Kind = A.Solid and then View.Pending.Color = A.Ocean,
         when Fill => View.Pending.Position = A.Fill,
         when Fit => View.Pending.Position = A.Fit,
         when Center => View.Pending.Position = A.Center,
         when Scale_Auto => View.Pending_Auto,
         when others => False);
      procedure Focus_Ring (R : Rect; Inset : Natural) is
      begin
         if R.w > 2 * Inset and then R.h > 2 * Inset then
            Stroke_Rect (C, (R.x + Inset, R.y + Inset, R.w - 2 * Inset, R.h - 2 * Inset),
                         Colors.shadow, Colors.shadow);
         end if;
      end Focus_Ring;
      procedure Text (X, Y : Natural; Value : String; Style : B.Text_Style; Item : B.Widget_Id) is
         Back : constant Color := (if Item = Id (Sidebar_Title_Part) then Colors.panel else Colors.face);
      begin
         Draw_UI_Text (C, X, Y, Value, (if Style = B.Muted_Text then Colors.muted else Colors.text), Back);
      end Text;
      procedure Button (R : Rect; Caption : String; Item_Id : B.Widget_Id) is
         Item : constant Control := Control_Of (Item_Id);
      begin
         if Item in Tab_Control then
            Draw_Tab (C, R, Colors, Selected (Item), Item = View.Hovered,
                      Item = View.Pressed and then Item = View.Hovered, Caption, Orientation => Vertical);
         else
            Draw_Button (C, R, Colors,
              (if Item = View.Pressed and Item = View.Hovered then Button_Pressed
               elsif Selected (Item) then Button_Active
               elsif Item = View.Hovered then Button_Hot else Button_Normal), Caption);
         end if;
         if Item = View.Focused then
            Focus_Ring (R, Button_Focus_Inset);
         end if;
      end Button;
      procedure Draw_Labels_And_Buttons is new B.Draw (Text, Button);

      function One_Line (Line : Natural) return Positive is
         pragma Unreferenced (Line);
      begin
         return Row_Height;
      end One_Line;
      --  Lists whose rows can all be chosen.
      function Never_Muted (Line : Natural) return Boolean is
         pragma Unreferenced (Line);
      begin
         return False;
      end Never_Muted;
      --  A list field showing up to Rows rows, as many as fit whole;
      --  Paint_Row draws one row's text.
      generic
         Item : Control;
         Rows : Natural;
         with function Chosen (Line : Natural) return Boolean;
         with function Muted (Line : Natural) return Boolean;
         with function Height_Of (Line : Natural) return Positive;
         with procedure Paint_Row (Line : Natural; Row : Rect; Ink, Back : Color);
      procedure List_Field;
      procedure List_Field is
         List : constant Rect := B.Bounds (T, Id (Item));
         Clip : constant Canvas := With_Clip (C, List);
         Y : Natural := First_Row_Y (Item, List);
      begin
         Fill_Rect (C, List, Colors.field);
         for Line in 0 .. Rows - 1 loop
            exit when Line > 0 and then Y + Height_Of (Line) > List.y + List.h;
            declare
               Row : constant Rect := (List.x, Y, List.w, Height_Of (Line));
               Is_Chosen : constant Boolean := Chosen (Line);
               Back : constant Color := (if Is_Chosen then Colors.selection else Colors.field);
               Ink : constant Color :=
                 (if Is_Chosen then Colors.selectionText elsif Muted (Line) then Colors.muted else Colors.text);
            begin
               if Is_Chosen then Fill_Rect (Clip, Row, Back); end if;
               Paint_Row (Line, Row, Ink, Back);
               Y := Y + Row.h;
            end;
         end loop;
         Stroke_Rect (C, List, Colors.shadow, Colors.highlight);
         if View.Focused = Item then
            Focus_Ring (List, List_Focus_Inset);
         end if;
      end List_Field;
   begin
      Lay_Out (View, Window_Bounds, T);
      Sidebar := B.Bounds (T, Id (Sidebar_Part));
      Fill_Rect (C, Window_Bounds, Colors.face);
      Fill_Rect (C, (Window_Bounds.x, Window_Bounds.y, Sidebar.w, Window_Bounds.h), Colors.panel);
      if Window_Bounds.h > 2 * M.Space_M then
         Fill_Rect (C, (Window_Bounds.x + Sidebar.w, Window_Bounds.y + M.Space_M, 1,
                        Window_Bounds.h - 2 * M.Space_M), Colors.shadow);
      end if;
      Draw_Labels_And_Buttons (T);
      case View.Current_Page is
         when Monitor =>
            declare
               List : constant Rect := B.Bounds (T, Id (Mode_List));
               Spans : constant BD.Span_Array := Mode_Spans (View, List);
               Status_Width : constant Natural := Spans (Column_Index (Status_Column)).Length;
               Clip : constant Canvas := With_Clip (C, List);
               function Height_Of (Line : Natural) return Positive is
                 (Mode_Line_Height (View, View.Mode_Top + Line, Status_Width));
               function Row_Of (Line : Natural) return Mode_Row is (View.Mode_Top + Line);
               function Chosen (Line : Natural) return Boolean is (Row_Of (Line) = View.Mode_Choice);
               function Muted (Line : Natural) return Boolean is (not Selectable (View, Row_Of (Line)));
               procedure Paint (Line : Natural; Row : Rect; Ink, Back : Color) is
                  Mode : constant Mode_Row := Row_Of (Line);
               begin
                  if Mode = Auto_Row then
                     Draw_UI_Text (Clip, Row.x + List_Inset, Text_Top (Row.y), Auto_Text, Ink, Back);
                  else
                     for Column in Mode_Column range Size_Column .. Clock_Column loop
                        Draw_UI_Text (Clip, Spans (Column_Index (Column)).Offset, Text_Top (Row.y),
                                      Cell_Text (View, Mode, Column), Ink, Back);
                     end loop;
                     --  The status wraps within its column; the row is as
                     --  tall as its lines.
                     declare
                        Notes : constant String := Notes_Text (View, Mode);
                        Shown : Natural := 0;
                        procedure Status_Line (First, Last : Natural) is
                        begin
                           Draw_UI_Text (Clip, Spans (Column_Index (Status_Column)).Offset,
                                         Text_Top (Row.y) + Shown * M.Line_Height, Notes (First .. Last), Ink, Back);
                           Shown := Shown + 1;
                        end Status_Line;
                        procedure Status_Lines is new B.Wrap (Status_Line);
                     begin
                        Status_Lines (Notes, B.Body_Text, Status_Width);
                     end;
                  end if;
               end Paint;
               procedure Mode_Field is new List_Field
                 (Mode_List, Last_Row (View) - View.Mode_Top + 1, Chosen, Muted, Height_Of, Paint);
            begin
               Mode_Field;
               --  The header: each column's title over its column.
               for Column in Mode_Column loop
                  Draw_UI_Text (Clip, Spans (Column_Index (Column)).Offset, Text_Top (List.y),
                                Column_Title (Column), Colors.muted, Colors.field);
               end loop;
               if List.h > Row_Height then
                  Fill_Rect (Clip, (List.x, List.y + Row_Height - 1, List.w, 1), Colors.shadow);
               end if;
            end;
         when Date_Time =>
            declare
               List : constant Rect := B.Bounds (T, Id (Zone_List));
               Clip : constant Canvas := With_Clip (C, List);
               function Zone_Of (Line : Natural) return TZ.Time_Zone is
                 (TZ.Time_Zone'Val (TZ.Time_Zone'Pos (View.Zone_Top) + Line));
               function Chosen (Line : Natural) return Boolean is (Zone_Of (Line) = View.Pending_Zone);
               procedure Paint (Line : Natural; Row : Rect; Ink, Back : Color) is
               begin
                  Draw_UI_Text (Clip, Row.x + List_Inset, Text_Top (Row.y), TZ.Name (Zone_Of (Line)), Ink, Back);
               end Paint;
               procedure Zone_Field is new List_Field
                 (Zone_List,
                  TZ.Time_Zone'Pos (TZ.Time_Zone'Last) - TZ.Time_Zone'Pos (View.Zone_Top) + 1,
                  Chosen, Never_Muted, One_Line, Paint);
            begin
               Zone_Field;
            end;
         when Displays =>
            declare
               Area : constant Rect := B.Bounds (T, Id (Diagram_Part));
               Clipped : constant Canvas := With_Clip (C, Area);
            begin
               Fill_Rect (C, Area, Colors.shadow);
               Stroke_Rect (C, Area, Colors.shadow, Colors.highlight);
               for I in 1 .. View.Pending_Layout.Count loop
                  declare
                     R : constant Rect := Tile (View, Area, I);
                     Chosen : constant Boolean := I = View.Selected;
                  begin
                     Fill_Vertical_Gradient (Clipped, R, Colors.activeTitleTop, Colors.activeTitleBottom);
                     Stroke_Rect (Clipped, R, (if Chosen then Colors.highlight else Colors.face), Colors.shadow);
                     if View.Pending_Layout.Items (I).Display = View.Pending_Primary then
                        Fill_Rect (Clipped, (R.x + Primary_Bar_Inset, R.y + Primary_Bar_Inset,
                                             Natural'Max (R.w, 2 * Primary_Bar_Inset) - 2 * Primary_Bar_Inset,
                                             Primary_Bar_Height), Colors.highlight);
                     end if;
                     Draw_UI_Text (Clipped, R.x + M.Text_Inset, R.y + M.Space_M + Primary_Bar_Height,
                                   "Display " & Img (I), Colors.selectionText, Colors.activeTitleTop);
                  end;
               end loop;
            end;
         when Appearance =>
            declare
               Preview : constant Rect := B.Bounds (T, Id (Preview_Part));
               Chosen_Row : constant Wallpaper_Row := Chosen_Wallpaper (View);
               List : constant Rect := B.Bounds (T, Id (Wallpaper_List));
               Clip : constant Canvas := With_Clip (C, List);
               function Row_Of (Line : Natural) return Natural is (View.Wallpaper_Top + Line);
               function Chosen (Line : Natural) return Boolean is (Row_Of (Line) = Chosen_Row);
               procedure Paint (Line : Natural; Row : Rect; Ink, Back : Color) is
               begin
                  Draw_UI_Text (Clip, Row.x + List_Inset, Text_Top (Row.y), Wallpaper_Label (Row_Of (Line)),
                                Ink, Back);
               end Paint;
               procedure Wallpaper_Field is new List_Field
                 (Wallpaper_List,
                  (if Wallpaper_Rows = 0 then 0 else Wallpaper_Rows - View.Wallpaper_Top + 1),
                  Chosen, Never_Muted, One_Line, Paint);
               package WC renames CuBit.UI.Window_Chrome;
               --  The active window chrome in miniature, the accent as app colour.
               Window : constant Rect := Miniature_Window (Preview);
               Chrome : constant WC.Palette := WC.Colors (View.Pending.Scheme);
               Inner : constant Natural := Window.w - Natural'Min (Window.w, 2 * WC.Border_Width);
            begin
               Paint_Wallpaper (C, Preview, View.Pending);
               Wallpaper_Field;
               if Wallpaper_Rows = 0 then
                  Draw_UI_Text (Clip, List.x + List_Inset, Text_Top (List.y), No_Wallpapers,
                                Colors.muted, Colors.field);
               end if;
               Stroke_Rect (C, Preview, Colors.shadow, Colors.highlight);
               Fill_Rect (C, Window, Preview_Colors.face);
               Stroke_Rect (C, Window, Chrome.Active_Border, Chrome.Active_Border);
               Fill_Rect (C, (Window.x + WC.Border_Width, Window.y + WC.Border_Width, Inner, WC.Stripe_Height),
                 Preview_Colors.accent);
               Fill_Rect (C, (Window.x + WC.Border_Width, Window.y + WC.Bar_Top, Inner, WC.Bar_Height),
                 Preview_Colors.titleBar);
               Fill_Rect (C, (Window.x + WC.Border_Width, Window.y + WC.Bar_Top + WC.Bar_Height, Inner,
                 WC.Rule_Height), Chrome.Rule);
               Fill_Rect (C, (Window.x + WC.Tile_Left, Window.y + WC.Bar_Top + (WC.Bar_Height - WC.Tile_Size) / 2,
                 WC.Tile_Size, WC.Tile_Size), Preview_Colors.accent);
               Draw_UI_Text (C, Window.x + WC.Title_Left,
                 Window.y + WC.Bar_Top + (WC.Bar_Height - Natural'Min (WC.Bar_Height, UI_Text_Height)) / 2,
                 "Alloy", Chrome.Active_Title, Preview_Colors.titleBar);
               Draw_Button (C, (Window.x + M.Space_XL, Window.y + WC.Client_Top + M.Space_M,
                                Window.w - Natural'Min (Window.w, 2 * M.Space_XL), M.Control_Height),
                 Preview_Colors, Button_Normal, "Preview");
            end;
      end case;
   end Render;

   procedure Draw (View : State; C : Canvas; Window_Bounds : Rect;
                   Preview : Desktop_Backdrop_Style.Backdrop) is
      --  The preview is Pending, already resolved (Preview).
      procedure Paint_Legacy_Wallpaper
        (C : Canvas; Bounds : Rect; Style : CuBit.Appearance.Preferences)
      is
         pragma Unreferenced (Style);
         P : constant Rect := Clamp_Rect (C, Bounds);
      begin
         if not Is_Empty (P) then
            Desktop_Wallpaper.Paint
              (C.addr + Storage_Offset (Bounds.y * C.pitch + Bounds.x * 4),
               Bounds.w, Bounds.h, C.pitch,
               P.x - Bounds.x, P.y - Bounds.y, P.w, P.h, Preview);
         end if;
      end Paint_Legacy_Wallpaper;

      procedure Draw_Legacy is new Render
        (Fill_Rect => CuBit.UI.Fill_Rect,
         Stroke_Rect => CuBit.UI.Stroke_Rect,
         Fill_Vertical_Gradient => CuBit.UI.Fill_Vertical_Gradient,
         Draw_UI_Text => CuBit.UI.Draw_UI_Text,
         Draw_Button => CuBit.UI.Draw_Button,
         Draw_Tab => CuBit.UI.Draw_Tab,
         Paint_Wallpaper => Paint_Legacy_Wallpaper);
   begin
      Draw_Legacy (View, C, Window_Bounds);
   end Draw;


   procedure Activate (View : in out State; Item : Control; Apply_Requested : out Boolean) is
   begin
      Apply_Requested := False;
      case Item is
         when Appearance_Tab => View.Current_Page := Appearance;
         when Displays_Tab => View.Current_Page := Displays;
         when Date_Time_Tab => View.Current_Page := Date_Time;
         when Monitor_Tab => View.Current_Page := Monitor;
         when Zone_Previous => Choose_Zone (View, Moved (View.Pending_Zone, -1));
         when Zone_Next => Choose_Zone (View, Moved (View.Pending_Zone, 1));
         when Zone_List | Mode_List => null;
         when Mode_Previous =>
            View.Mode_Top := (if View.Mode_Top > Mode_Page (View) then View.Mode_Top - Mode_Page (View) else Auto_Row);
         when Mode_Next =>
            View.Mode_Top := Mode_Row'Min (View.Mode_Top + Mode_Page (View), Last_Mode_Top (View));
         when Keep_Mode => View.Answer := Keep_Response;
         when Revert_Mode => View.Answer := Revert_Response;
         when Light => View.Pending.Scheme := A.Alloy_Light;
         when Dark => View.Pending.Scheme := A.Alloy_Dark;
         when Slate => View.Pending.Kind := A.Solid; View.Pending.Color := A.Slate;
         when Ocean => View.Pending.Kind := A.Solid; View.Pending.Color := A.Ocean;
         when Wallpaper_List =>
            if Wallpaper_Rows > 0 then Choose_Wallpaper (View, Moved_Wallpaper (View, 0)); end if;
         when Wallpaper_Previous =>
            View.Wallpaper_Top := (if View.Wallpaper_Top > Wallpaper_Page (View)
                                   then View.Wallpaper_Top - Wallpaper_Page (View) else 1);
         when Wallpaper_Next =>
            if Wallpaper_Rows > 0 then
               View.Wallpaper_Top := WC_Catalog.Wallpaper_Index'Min
                 (View.Wallpaper_Top + Wallpaper_Page (View),
                  Wallpaper_Scrolled_To (1, Wallpaper_Rows, Wallpaper_Page (View)));
            end if;
         when Fill => View.Pending.Position := A.Fill;
         when Fit => View.Pending.Position := A.Fit;
         when Center => View.Pending.Position := A.Center;
         when Apply => Apply_Requested := True;
         when Make_Primary =>
            if View.Selected in 1 .. View.Pending_Layout.Count then
               View.Pending_Primary := View.Pending_Layout.Items (View.Selected).Display;
               View.Layout_Status := Unchanged;
            end if;
         when Scale_Down | Scale_Up =>
            if View.Selected in 1 .. View.Pending_Layout.Count then
               declare
                  package D renames CuBit.Display_Arrangement;
                  Preset : D.Scale_Preset := D.Scale_100;
                  Candidate : L.Layout;
                  Accepted : Boolean;
                  use type G.UI_Scale, D.Scale_Preset;
               begin
                  for P in D.Scale_Preset loop
                     if D.Factor (P) = View.Pending_Layout.Items (View.Selected).Geometry.Scale then
                        Preset := P;
                        exit;
                     end if;
                  end loop;
                  if Item = Scale_Up and Preset < D.Scale_Preset'Last then
                     Preset := D.Scale_Preset'Succ (Preset);
                  elsif Item = Scale_Down and Preset > D.Scale_Preset'First then
                     Preset := D.Scale_Preset'Pred (Preset);
                  end if;
                  D.Rescale (View.Pending_Layout, View.Selected, Preset, Candidate, Accepted);
                  if Accepted then
                     View.Pending_Layout := Candidate;
                     View.Pending_Auto := False;
                     View.Layout_Status := Unchanged;
                  else View.Layout_Status := Scale_Rejected; end if;
               end;
            end if;
         when Scale_Auto =>
            --  Every output back to the scale its size asks for.
            declare
               package D renames CuBit.Display_Arrangement;
               package R renames Desktop_Auto_Scale;
               Candidate : L.Layout;
               Accepted : Boolean;
            begin
               for I in 1 .. View.Pending_Layout.Count loop
                  declare
                     Screen : constant G.Output := View.Pending_Layout.Items (I).Geometry;
                     Rotated : constant Boolean := Screen.Rotation in G.Clockwise_90 | G.Clockwise_270;
                  begin
                     D.Rescale (View.Pending_Layout, I,
                       R.Choose ((if Rotated then Screen.Height else Screen.Width),
                                 (if Rotated then Screen.Width else Screen.Height)),
                       Candidate, Accepted);
                     if Accepted then View.Pending_Layout := Candidate; end if;
                  end;
               end loop;
               View.Pending_Auto := True;
               View.Layout_Status := Unchanged;
            end;
         when Revert =>
            if View.Current_Page = Date_Time then
               Choose_Zone (View, View.Applied_Zone);
            elsif View.Current_Page = Monitor then
               Choose_Mode (View, (if View.Monitor_View.Saved_Auto then Auto_Row else View.Monitor_View.Saved_Row));
            elsif View.Current_Page = Displays then
               View.Pending_Layout := View.Applied_Layout;
               View.Pending_Primary := View.Applied_Primary;
               View.Pending_Auto := View.Applied_Auto;
               View.Layout_Status := Unchanged;
            else View.Pending := View.Applied; end if;
         when None => null;
      end case;
   end Activate;

   procedure Pointer
     (View : in out State; Bounds : Rect;
      Pointer : Pointer_State; Apply_Requested : out Boolean)
   is
      T : Layout_Tree;
      Hit : Control := None;
      --  Over a list's rows (not its header), and which row from the top
      --  row shown.
      function Over (Item : Control) return Boolean is
        (Point_In_Rect (Pointer.x, Pointer.y, B.Bounds (T, Id (Item)))
         and then Pointer.y >= First_Row_Y (Item, B.Bounds (T, Id (Item))));
      function Row_At (Item : Control) return Natural is
         Box : constant Rect := B.Bounds (T, Id (Item));
         Width : constant Natural := Status_Width_Of (View, Item, Box);
         Y : Natural := First_Row_Y (Item, Box);
         Line : Natural := 0;
      begin
         while Pointer.y >= Y + Line_Height_Of (View, Item, Line, Width) loop
            Y := Y + Line_Height_Of (View, Item, Line, Width);
            Line := Line + 1;
         end loop;
         return Line;
      end Row_At;
   begin
      Apply_Requested := False;
      Place (View, Bounds);
      Lay_Out (View, Bounds, T);
      if View.Current_Page = Date_Time and then Pointer.pressed and then Over (Zone_List) then
         declare
            Index : constant Natural := TZ.Time_Zone'Pos (View.Zone_Top) + Row_At (Zone_List);
         begin
            --  The row shows already (a taller list shows more than
            --  Zone_Rows): choose it without scrolling.
            if Index <= TZ.Time_Zone'Pos (TZ.Time_Zone'Last) then
               View.Pending_Zone := TZ.Time_Zone'Val (Index);
               View.Zone_Status := Unchanged;
            end if;
            View.Focused := Zone_List;
            View.Pressed := None;
            return;
         end;
      end if;
      if View.Current_Page = Appearance and then Pointer.pressed and then Over (Wallpaper_List) then
         declare
            Row : constant Natural := View.Wallpaper_Top + Row_At (Wallpaper_List);
         begin
            if Row in 1 .. Wallpaper_Rows then
               Choose_Wallpaper (View, Row);
            end if;
            View.Focused := Wallpaper_List;
            View.Pressed := None;
            return;
         end;
      end if;
      if View.Current_Page = Monitor and then not View.Trial_Open and then Pointer.pressed and then
        Point_In_Rect (Pointer.x, Pointer.y, B.Bounds (T, Id (Mode_List)))
      then
         if Over (Mode_List) then
            declare
               Row : constant Mode_Row := View.Mode_Top + Row_At (Mode_List);
               Top : constant Mode_Row := View.Mode_Top;
            begin
               Choose_Mode (View, Row);
               --  The row shows already: no scrolling.
               View.Mode_Top := Top;
            end;
         end if;
         View.Focused := Mode_List;
         View.Pressed := None;
         return;
      end if;
      if View.Current_Page = Displays then
         declare
            Area : constant Rect := B.Bounds (T, Id (Diagram_Part));
         begin
            if Pointer.pressed and Point_In_Rect (Pointer.x, Pointer.y, Area) then
               for I in 1 .. View.Pending_Layout.Count loop
                  if Point_In_Rect (Pointer.x, Pointer.y, Tile (View, Area, I)) then
                     View.Selected := I;
                     View.Drag_Original := View.Pending_Layout;
                     View.Transform := Projection (View.Pending_Layout, Area);
                     View.Drag_X := Integer (Pointer.x);
                     View.Drag_Y := Integer (Pointer.y);
                     View.Dragging := View.Pending_Layout.Count > 1;
                     View.Pressed := None;
                     return;
                  end if;
               end loop;
            elsif View.Dragging then
               declare
                  X : constant Integer := Integer (View.Drag_Original.Items (View.Selected).Geometry.X) +
                    (Integer (Pointer.x) - View.Drag_X) * View.Transform.Divisor;
                  Y : constant Integer := Integer (View.Drag_Original.Items (View.Selected).Geometry.Y) +
                    (Integer (Pointer.y) - View.Drag_Y) * View.Transform.Divisor;
                  Candidate : L.Layout;
                  Accepted : Boolean;
               begin
                  CuBit.Display_Arrangement.Move (View.Drag_Original, View.Selected,
                    G.Output_Origin (Integer'Max (Integer (G.Output_Origin'First), Integer'Min (Integer (G.Output_Origin'Last), X))),
                    G.Output_Origin (Integer'Max (Integer (G.Output_Origin'First), Integer'Min (Integer (G.Output_Origin'Last), Y))),
                    Candidate, Accepted);
                  if Accepted then View.Pending_Layout := Candidate; View.Layout_Status := Unchanged; end if;
                  if Pointer.released or not Pointer.down then View.Dragging := False; end if;
               end;
               return;
            end if;
         end;
      end if;
      if Point_In_Rect (Pointer.x, Pointer.y, Bounds) then
         Hit := Control_Of (B.Hit (T, Pointer.x, Pointer.y));
         if Hit in Zone_List | Mode_List | Wallpaper_List or else not Visible (View, Hit) then
            Hit := None;
         end if;
      end if;
      View.Hovered := Hit;
      if Pointer.pressed then
         View.Pressed := Hit;
         if Hit /= None then View.Focused := Hit; end if;
      elsif Pointer.released then
         if View.Pressed = Hit then Activate (View, Hit, Apply_Requested); end if;
         View.Pressed := None;
      end if;
   end Pointer;

   procedure Key
     (View : in out State; Scan_Code : Natural; Backwards : Boolean;
      Apply_Requested : out Boolean) is
   begin
      Apply_Requested := False;
      if Scan_Code = Scan_Escape and then View.Dragging then
         View.Pending_Layout := View.Drag_Original;
         View.Dragging := False;
      elsif View.Focused in Tab_Control and then Scan_Code in Scan_Up | Scan_Down then
         View.Focused :=
           (if Scan_Code = Scan_Up
            then (if View.Focused = Tab_Control'First then View.Focused else Control'Pred (View.Focused))
            else (if View.Focused = Tab_Control'Last then View.Focused else Control'Succ (View.Focused)));
         Activate (View, View.Focused, Apply_Requested);
      elsif View.Focused = Zone_List and then
        Scan_Code in Scan_Up | Scan_Down | Scan_Page_Up | Scan_Page_Down | Scan_Home | Scan_End
      then
         Choose_Zone (View,
           (case Scan_Code is
              when Scan_Up => Moved (View.Pending_Zone, -1),
              when Scan_Down => Moved (View.Pending_Zone, 1),
              when Scan_Page_Up => Moved (View.Pending_Zone, -Zone_Page (View)),
              when Scan_Page_Down => Moved (View.Pending_Zone, Zone_Page (View)),
              when Scan_Home => TZ.Time_Zone'First,
              when others => TZ.Time_Zone'Last));
      elsif View.Focused = Wallpaper_List and then Wallpaper_Rows > 0 and then
        Scan_Code in Scan_Up | Scan_Down | Scan_Page_Up | Scan_Page_Down | Scan_Home | Scan_End
      then
         Choose_Wallpaper (View,
           (case Scan_Code is
              when Scan_Up => Moved_Wallpaper (View, -1),
              when Scan_Down => Moved_Wallpaper (View, 1),
              when Scan_Page_Up => Moved_Wallpaper (View, -Wallpaper_Page (View)),
              when Scan_Page_Down => Moved_Wallpaper (View, Wallpaper_Page (View)),
              when Scan_Home => 1,
              when others => Wallpaper_Rows));
      elsif View.Focused = Mode_List and then
        Scan_Code in Scan_Up | Scan_Down | Scan_Page_Up | Scan_Page_Down | Scan_Home | Scan_End
      then
         Choose_Mode (View,
           (case Scan_Code is
              when Scan_Up => Moved_Mode (View, -1),
              when Scan_Down => Moved_Mode (View, 1),
              when Scan_Page_Up => Moved_Mode (View, -Mode_Page (View)),
              when Scan_Page_Down => Moved_Mode (View, Mode_Page (View)),
              when Scan_Home => Auto_Row,
              when others => Moved_Mode (View, Integer (Last_Row (View)))));
      elsif View.Current_Page = Date_Time and then Scan_Code in Letter_Scan and then
        Letter_Of (Scan_Code) /= ' '
      then
         Choose_Zone (View, Next_Starting (View.Pending_Zone, Letter_Of (Scan_Code)));
         View.Focused := Zone_List;
      elsif Scan_Code = Scan_Tab then
         for Attempt in Control range Appearance_Tab .. Control'Last loop
            View.Focused :=
              (if Backwards then (if View.Focused = Appearance_Tab then Control'Last else Control'Pred (View.Focused))
               else (if View.Focused = Control'Last then Appearance_Tab else Control'Succ (View.Focused)));
            exit when Visible (View, View.Focused);
         end loop;
      elsif Scan_Code in Scan_Enter | Scan_Space then
         Activate (View, View.Focused, Apply_Requested);
      end if;
   end Key;
end Desktop_Settings;
