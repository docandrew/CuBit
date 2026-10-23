with Ada.Unchecked_Deallocation;
with Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Desktop_Wallpaper;
with CuBit.UI.Window_Chrome;
with CuBit.Display_Arrangement;
with Desktop_Auto_Scale;

package body Desktop_Settings is
   use CuBit.UI;
   use type CuBit.Appearance.Preferences;
   package A renames CuBit.Appearance;
   use type A.Color_Scheme;
   use type A.Background;
   use type A.Placement;
   package L renames CuBit.Display_Layouts;
   package G renames L.G;
   use type L.Layout;
   use type L.Named_Display_ID;
   use type G.Logical_Coordinate;

   Sidebar_Width : constant := 132;
   function Page_Bounds (B : Rect) return Rect is
     (B.x + Sidebar_Width, B.y, B.w - Sidebar_Width, B.h);

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

   --  The time-zone list: Zone_Rows rows of Zone_Row_Height pixels.
   package TZ renames Time_Zones;
   use type TZ.Time_Zone;
   Zone_Rows : constant := 9;
   Zone_Row_Height : constant := 22;
   Zone_List_Width : constant := 380;
   Zone_Text_Inset : constant := 8;
   function Zone_List_Box (C : Rect) return Rect is
     (C.x + 20, C.y + 56, Zone_List_Width, Zone_Rows * Zone_Row_Height);
   --  Zone_Top such that Zone is visible, moving the list as little as possible.
   function Scrolled_To (Top, Zone : TZ.Time_Zone) return TZ.Time_Zone is
     (if Zone < Top then Zone
      elsif TZ.Time_Zone'Pos (Zone) >= TZ.Time_Zone'Pos (Top) + Zone_Rows
      then TZ.Time_Zone'Val (TZ.Time_Zone'Pos (Zone) - (Zone_Rows - 1))
      else Top);
   --  Zone moved by Rows (negative: up), stopping at the ends.
   function Moved (Zone : TZ.Time_Zone; Rows : Integer) return TZ.Time_Zone is
     (TZ.Time_Zone'Val
        (Integer'Max (TZ.Time_Zone'Pos (TZ.Time_Zone'First),
                      Integer'Min (TZ.Time_Zone'Pos (TZ.Time_Zone'Last), TZ.Time_Zone'Pos (Zone) + Rows))));
   procedure Choose_Zone (View : in out State; Zone : TZ.Time_Zone) is
   begin
      View.Pending_Zone := Zone;
      View.Zone_Top := Scrolled_To (View.Zone_Top, Zone);
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

   --  The Monitor page's mode list: an Auto row, then each listed mode.
   package OM renames CuBit.Output_Modes;
   use type OM.Admission, Interfaces.Unsigned_32;
   Mode_Rows : constant := 11;
   Mode_Row_Height : constant := 20;
   Mode_List_Width : constant := 510;
   Mode_Button_Width : constant := 60;
   Mode_Button_Gap : constant := 10;
   Mode_Text_Inset : constant := 8;
   --  Column starts within a row: size, refresh rate, pixel clock, notes.
   Rate_Column : constant := 100;
   Clock_Column : constant := 190;
   Notes_Column : constant := 280;
   function Mode_List_Box (C : Rect) return Rect is
     (C.x + 20, C.y + 76, Mode_List_Width, Mode_Rows * Mode_Row_Height);
   function Mode_Count (View : State) return Natural is
     (if View.Monitor_View.Modes = null then 0 else View.Monitor_View.Modes'Length);
   function Last_Row (View : State) return Mode_Row is (Mode_Count (View));
   --  Auto and admitted modes can be chosen; refused modes only shown.
   function Selectable (View : State; Row : Mode_Row) return Boolean is
     (Row = Auto_Row or else
      (Row <= Mode_Count (View) and then View.Monitor_View.Modes (Row).Verdict = OM.Admitted));
   function Mode_Scrolled_To (Top, Row : Mode_Row) return Mode_Row is
     (if Row < Top then Row elsif Row >= Top + Mode_Rows then Row - (Mode_Rows - 1) else Top);
   procedure Choose_Mode (View : in out State; Row : Mode_Row) is
   begin
      if Row <= Last_Row (View) and then Selectable (View, Row) then
         View.Mode_Choice := Row;
         View.Mode_Top := Mode_Scrolled_To (View.Mode_Top, Row);
         if View.Monitor_Status /= Mode_Switching then
            View.Monitor_Status := Mode_Ready;
         end if;
      end if;
   end Choose_Mode;
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
   function Img (Value : Natural) return String is
      T : constant String := Value'Image;
   begin
      return T (T'First + 1 .. T'Last);
   end Img;
   --  "59.940" from 59940 (millihertz, or kHz as MHz).
   function Thousandths (Value : Natural) return String is
      F : constant String := Natural'Image (1000 + Value mod 1000);
   begin
      return Img (Value / 1000) & "." & F (F'First + 2 .. F'Last);
   end Thousandths;
   function Size_Text (Item : OM.Listed_Mode) return String is
     (Img (Natural (Item.Size.Width)) & " x " & Img (Natural (Item.Size.Height)));
   function Rate_Text (Item : OM.Listed_Mode) return String is
     (if Item.Refresh_Millihertz = 0 then "" else Thousandths (Natural (Item.Refresh_Millihertz)) & " Hz");
   --  What the notes column says about a mode.
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

   function Chosen_Mode (View : State) return OM.Listed_Mode is
     (View.Monitor_View.Modes (View.Mode_Choice));

   function Diagram (B : Rect) return Rect is
     (B.x + 20, B.y + 76, 550, 172);

   function Projection (Layout : L.Layout; B : Rect) return Diagram_Transform is
      Area : constant Rect := Diagram (B);
      Left, Top : Integer := Integer'Last;
      Right, Bottom : Integer := Integer'First;
      Divisor, Width, Height : Positive;
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
      Divisor := Positive'Max (1, Positive'Max ((Width + 529) / 530, (Height + 151) / 152));
      return (Integer (Area.x) + (Integer (Area.w) - Width / Divisor) / 2,
              Integer (Area.y) + (Integer (Area.h) - Height / Divisor) / 2,
              Left, Top, Divisor);
   end Projection;

   function Tile (View : State; B : Rect; I : L.Viewport_Index) return Rect is
      T : Diagram_Transform := (if View.Dragging then View.Transform
                               else Projection (View.Pending_Layout, B));
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

   function Box (Item : Control; B : Rect) return Rect is
      C : constant Rect := Page_Bounds (B);
   begin
      case Item is
         when Appearance_Tab => return (B.x + 8, B.y + 34, Sidebar_Width - 12, 30);
         when Displays_Tab => return (B.x + 8, B.y + 64, Sidebar_Width - 12, 30);
         when Date_Time_Tab => return (B.x + 8, B.y + 94, Sidebar_Width - 12, 30);
         when Monitor_Tab => return (B.x + 8, B.y + 124, Sidebar_Width - 12, 30);
         when Zone_List => return Zone_List_Box (C);
         when Zone_Previous => return (C.x + 412, C.y + 56, 84, 28);
         when Zone_Next => return (C.x + 412, C.y + 92, 84, 28);
         when Light => return (C.x + 280, C.y + 56, 140, 30);
         when Dark => return (C.x + 430, C.y + 56, 140, 30);
         when Image => return (C.x + 280, C.y + 128, 68, 30);
         when Cubie => return (C.x + 354, C.y + 128, 68, 30);
         when Slate => return (C.x + 428, C.y + 128, 68, 30);
         when Ocean => return (C.x + 502, C.y + 128, 70, 30);
         when Fill => return (C.x + 280, C.y + 200, 92, 30);
         when Fit => return (C.x + 380, C.y + 200, 92, 30);
         when Center => return (C.x + 480, C.y + 200, 92, 30);
         when Apply => return (C.x + 366, C.y + 334, 100, 30);
         when Revert => return (C.x + 476, C.y + 334, 100, 30);
         when Mode_List => return Mode_List_Box (C);
         when Mode_Previous => return (C.x + 20 + Mode_List_Width + Mode_Button_Gap, C.y + 76, Mode_Button_Width, 28);
         when Mode_Next =>
            return (C.x + 20 + Mode_List_Width + Mode_Button_Gap, C.y + 76 + Mode_Rows * Mode_Row_Height - 28,
                    Mode_Button_Width, 28);
         when Keep_Mode => return (C.x + 366, C.y + 334, 100, 30);
         when Revert_Mode => return (C.x + 476, C.y + 334, 100, 30);
         when Make_Primary => return (C.x + 20, C.y + 304, 156, 28);
         when Scale_Down => return (C.x + 430, C.y + 304, 34, 28);
         when Scale_Up => return (C.x + 472, C.y + 304, 34, 28);
         when Scale_Auto => return (C.x + 514, C.y + 304, 60, 28);
         when None => return (others => 0);
      end case;
   end Box;

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
               Zone_Top => Scrolled_To (TZ.Time_Zone'First, Zone),
               Selected => (if Layout.Count > 0 then 1 else 0), others => <>);
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
      View.Mode_Top := Mode_Scrolled_To (Auto_Row, View.Mode_Choice);
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
      Bounds : constant Rect := Page_Bounds (Window_Bounds);
      Colors : constant Theme := Current_Theme;
      Preview_Colors : constant Theme := Palette (View.Pending.Scheme);
      Preview : constant Rect := (Bounds.x + 20, Bounds.y + 56, 236, 150);
      function Label (Item : Control) return String is
        (case Item is when Appearance_Tab => "Appearance", when Displays_Tab => "Displays",
         when Date_Time_Tab => "Date & Time", when Monitor_Tab => "Monitor", when Zone_Previous => "Up", when Zone_Next => "Down",
         when Light => "Alloy Light", when Dark => "Alloy Dark",
         when Image => "Cubes", when Cubie => "Cubie", when Slate => "Slate",
         when Ocean => "Ocean", when Fill => "Fill", when Fit => "Fit",
         when Center => "Center", when Apply => "Apply", when Revert => "Revert",
         when Make_Primary => "Make primary",
         when Scale_Down => "-", when Scale_Up => "+", when Scale_Auto => "Auto",
         when Mode_Previous => "Up", when Mode_Next => "Down",
         when Keep_Mode => "Keep", when Revert_Mode => "Revert",
         when None | Zone_List | Mode_List => "");
      function Selected (Item : Control) return Boolean is
        (case Item is
         when Appearance_Tab => View.Current_Page = Appearance,
         when Displays_Tab => View.Current_Page = Displays,
         when Date_Time_Tab => View.Current_Page = Date_Time,
         when Monitor_Tab => View.Current_Page = Monitor,
         when Light => View.Pending.Scheme = A.Alloy_Light,
         when Dark => View.Pending.Scheme = A.Alloy_Dark,
         when Image => View.Pending.Backdrop = A.Wallpaper,
         when Cubie => View.Pending.Backdrop = A.Cubie,
         when Slate => View.Pending.Backdrop = A.Slate,
         when Ocean => View.Pending.Backdrop = A.Ocean,
         when Fill => View.Pending.Position = A.Fill,
         when Fit => View.Pending.Position = A.Fit,
         when Center => View.Pending.Position = A.Center,
         when Scale_Auto => View.Pending_Auto,
         when others => False);
      procedure Text (X, Y : Natural; Value : String) is
      begin
         Draw_UI_Text (C, Bounds.x + X, Bounds.y + Y, Value, Colors.text, Colors.face);
      end Text;
      use type A.Color_Scheme;
      use type A.Background;
      use type A.Placement;
   begin
      Fill_Rect (C, Window_Bounds, Colors.face);
      Fill_Rect (C, (Window_Bounds.x, Window_Bounds.y, Sidebar_Width - 5,
                    Window_Bounds.h), Colors.panel);
      Fill_Rect (C, (Window_Bounds.x + Sidebar_Width - 5, Window_Bounds.y + 8,
                    1, Window_Bounds.h - 16), Colors.shadow);
      Draw_UI_Text (C, Window_Bounds.x + 12, Window_Bounds.y + 12,
                    "Settings", Colors.text, Colors.panel);
      Text (20, 12, (case View.Current_Page is
                     when Appearance => "Appearance", when Displays => "Displays",
                     when Date_Time => "Date & Time", when Monitor => "Monitor"));
      for Item in Tab_Control loop
         Draw_Tab (C, Box (Item, Window_Bounds), Colors,
                   Selected (Item), Item = View.Hovered,
                   Item = View.Pressed and then Item = View.Hovered,
                   Label (Item), orientation => Vertical);
         if Item = View.Focused then
            declare B : constant Rect := Box (Item, Window_Bounds); begin
               Stroke_Rect (C, (B.x + 3, B.y + 3, B.w - 6, B.h - 6), Colors.shadow, Colors.shadow);
            end;
         end if;
      end loop;
      if View.Current_Page = Monitor then
         declare
            M : Monitor_Info renames View.Monitor_View;
            List : constant Rect := Mode_List_Box (Bounds);
            Rows : constant Canvas := With_Clip (C, List);
            procedure Button (Item : Control) is
            begin
               Draw_Button (C, Box (Item, Window_Bounds), Colors,
                 (if Item = View.Pressed and Item = View.Hovered then Button_Pressed
                  elsif Item = View.Hovered then Button_Hot else Button_Normal), Label (Item));
               if Item = View.Focused then
                  declare B : constant Rect := Box (Item, Window_Bounds); begin
                     Stroke_Rect (C, (B.x + 3, B.y + 3, B.w - 6, B.h - 6), Colors.shadow, Colors.shadow);
                  end;
               end if;
            end Button;
         begin
            Text (20, 34, M.Summary.Text (1 .. M.Summary.Length));
            Text (20, 54, M.Detail.Text (1 .. M.Detail.Length));
            Fill_Rect (C, List, Colors.field);
            for Line in 0 .. Mode_Rows - 1 loop
               exit when View.Mode_Top + Line > Last_Row (View);
               declare
                  Row : constant Mode_Row := View.Mode_Top + Line;
                  Box_Of_Row : constant Rect := (List.x, List.y + Line * Mode_Row_Height, List.w, Mode_Row_Height);
                  Chosen : constant Boolean := Row = View.Mode_Choice;
                  Back : constant Color := (if Chosen then Colors.selection else Colors.field);
                  Ink : constant Color :=
                    (if Chosen then Colors.selectionText elsif Selectable (View, Row) then Colors.text
                     else Colors.muted);
                  Y : constant Natural :=
                    Box_Of_Row.y + (Mode_Row_Height - Natural'Min (Mode_Row_Height, UI_Text_Height)) / 2;
                  X : constant Natural := Box_Of_Row.x + Mode_Text_Inset;
               begin
                  if Chosen then Fill_Rect (Rows, Box_Of_Row, Back); end if;
                  if Row = Auto_Row then
                     Draw_UI_Text (Rows, X, Y, "Auto: the best mode the display driver admits", Ink, Back);
                  else
                     Draw_UI_Text (Rows, X, Y, Size_Text (M.Modes (Row)), Ink, Back);
                     Draw_UI_Text (Rows, X + Rate_Column, Y, Rate_Text (M.Modes (Row)), Ink, Back);
                     if M.Modes (Row).Pixel_kHz /= 0 then
                        Draw_UI_Text (Rows, X + Clock_Column, Y,
                          Thousandths (Natural (M.Modes (Row).Pixel_kHz)) & " MHz", Ink, Back);
                     end if;
                     Draw_UI_Text (Rows, X + Notes_Column, Y, Notes_Text (View, Row), Ink, Back);
                  end if;
               end;
            end loop;
            Stroke_Rect (C, List, Colors.shadow, Colors.highlight);
            if View.Focused = Mode_List then
               Stroke_Rect (C, (List.x + 2, List.y + 2, List.w - 4, List.h - 4), Colors.shadow, Colors.shadow);
            end if;
            if View.Trial_Open then
               Text (20, 306, "Keep this display mode? It reverts in" & View.Trial_Seconds'Image &
                     (if View.Trial_Seconds = 1 then " second." else " seconds."));
               Button (Keep_Mode);
               Button (Revert_Mode);
            else
               Button (Mode_Previous);
               Button (Mode_Next);
               Button (Apply);
               Button (Revert);
               Text (20, 306,
                 (case View.Monitor_Status is
                    when Mode_Ready =>
                      (if M.Modes = null then "This output lists no modes"
                       else "Choose a mode and Apply; refused modes show why"),
                    when Mode_Switching => "Switching the display mode...",
                    when Mode_Refused => "The display driver refused that mode",
                    when Mode_Kept =>
                      (if M.Identified then "Kept for this monitor until reboot" else "Kept for this session"),
                    when Mode_Saved_Session_Only => "Kept; Config refused to save it",
                    when Mode_Reverted => "Reverted to the previous mode",
                    when Mode_Timed_Out => "No answer: reverted to the previous mode"));
               Text (20, 340, (if M.Identified then "Kept per monitor until reboot (EDID identity)"
                               else "No EDID identity: choices last this session"));
            end if;
         end;
         return;
      end if;
      if View.Current_Page = Date_Time then
         declare
            List : constant Rect := Zone_List_Box (Bounds);
            Rows : constant Canvas := With_Clip (C, List);
         begin
            Text (20, 34, "Time zone (IANA time zone database)");
            Fill_Rect (C, List, Colors.field);
            for Row in 0 .. Zone_Rows - 1 loop
               exit when TZ.Time_Zone'Pos (View.Zone_Top) + Row > TZ.Time_Zone'Pos (TZ.Time_Zone'Last);
               declare
                  Zone : constant TZ.Time_Zone := TZ.Time_Zone'Val (TZ.Time_Zone'Pos (View.Zone_Top) + Row);
                  Line : constant Rect := (List.x, List.y + Row * Zone_Row_Height, List.w, Zone_Row_Height);
                  Chosen : constant Boolean := Zone = View.Pending_Zone;
                  Back : constant Color := (if Chosen then Colors.selection else Colors.field);
               begin
                  if Chosen then Fill_Rect (Rows, Line, Back); end if;
                  Draw_UI_Text (Rows, Line.x + Zone_Text_Inset,
                                Line.y + (Zone_Row_Height - Natural'Min (Zone_Row_Height, UI_Text_Height)) / 2,
                                TZ.Name (Zone),
                                (if Chosen then Colors.selectionText else Colors.text), Back);
               end;
            end loop;
            Stroke_Rect (C, List, Colors.shadow, Colors.highlight);
            if View.Focused = Zone_List then
               Stroke_Rect (C, (List.x + 2, List.y + 2, List.w - 4, List.h - 4), Colors.shadow, Colors.shadow);
            end if;
         end;
         Text (20, 264, "In Config: " & TZ.Name (View.Applied_Zone));
         Text (20, 286, "Type a letter to jump; arrows, Page Up/Down, Home, End move.");
         for Item in Control range Zone_Previous .. Zone_Next loop
            Draw_Button (C, Box (Item, Window_Bounds), Colors,
              (if Item = View.Pressed and Item = View.Hovered then Button_Pressed
               elsif Item = View.Hovered then Button_Hot else Button_Normal), Label (Item));
         end loop;
         for Item in Control range Apply .. Revert loop
            Draw_Button (C, Box (Item, Window_Bounds), Colors,
              (if Item = View.Pressed and Item = View.Hovered then Button_Pressed
               elsif Item = View.Hovered then Button_Hot else Button_Normal), Label (Item));
         end loop;
         if View.Focused in Zone_Previous | Zone_Next | Apply | Revert then
            declare B : constant Rect := Box (View.Focused, Window_Bounds); begin
               Stroke_Rect (C, (B.x + 3, B.y + 3, B.w - 6, B.h - 6), Colors.shadow, Colors.shadow);
            end;
         end if;
         Text (20, 340,
           (if View.Pending_Zone /= View.Applied_Zone then "Unapplied time zone"
            elsif View.Zone_Status = Saved_In_Config then "Applied / kept in Config until reboot"
            elsif View.Zone_Status = Rejected then "Config refused the time zone"
            else "Tab to navigate; Enter to choose"));
         return;
      end if;
      if View.Current_Page = Displays then
         Text (20, 52, "Arrange displays / drag a screen to match your desk");
         Fill_Rect (C, Diagram (Bounds), Colors.shadow);
         Stroke_Rect (C, Diagram (Bounds), Colors.shadow, Colors.highlight);
         for I in 1 .. View.Pending_Layout.Count loop
            declare
               B : constant Rect := Tile (View, Bounds, I);
               Clipped : constant Canvas := With_Clip (C, Diagram (Bounds));
               Selected : constant Boolean := I = View.Selected;
            begin
               Fill_Vertical_Gradient (Clipped, B, Colors.activeTitleTop, Colors.activeTitleBottom);
               Stroke_Rect (Clipped, B, (if Selected then Colors.highlight else Colors.face), Colors.shadow);
               if View.Pending_Layout.Items (I).Display = View.Pending_Primary then
                  Fill_Rect (Clipped, (B.x + 3, B.y + 3, Natural'Max (B.w, 6) - 6, 4), Colors.highlight);
               end if;
               Draw_UI_Text (Clipped, B.x + 8, B.y + 12, "Display" & I'Image,
                             16#FFFFFF#, Colors.activeTitleTop);
            end;
         end loop;
         if View.Selected in 1 .. View.Pending_Layout.Count then
            declare Screen : G.Output renames View.Pending_Layout.Items (View.Selected).Geometry; begin
               Text (20, 264, "Display" & View.Selected'Image & ":" & Screen.Width'Image & " x" &
                     Screen.Height'Image & " pixels / position" & Screen.X'Image & "," & Screen.Y'Image);
               Text (300, 310, "Scale:" &
                 Natural'Image (100 * Natural (Screen.Scale.Numerator) / Natural (Screen.Scale.Denominator)) & "%" &
                 (if View.Pending_Auto then " auto" else ""));
            end;
         end if;
         Text (20, 284, "Edges snap together. The white bar marks the primary display.");
         for Item in Control range Apply .. Scale_Auto loop
            Draw_Button (C, Box (Item, Window_Bounds), Colors,
              (if Item = View.Pressed and Item = View.Hovered then Button_Pressed
               elsif Selected (Item) then Button_Active
               elsif Item = View.Hovered then Button_Hot else Button_Normal), Label (Item));
            if Item = View.Focused then
               declare B : constant Rect := Box (Item, Window_Bounds); begin
                  Stroke_Rect (C, (B.x + 3, B.y + 3, B.w - 6, B.h - 6), Colors.shadow, Colors.shadow);
               end;
            end if;
         end loop;
         Text (20, 340,
           (if View.Layout_Status = Scale_Rejected then "Scale needs 800 x 480 logical pixels"
            elsif View.Layout_Status = Rejected then "Could not apply arrangement"
            elsif View.Pending_Layout /= View.Applied_Layout or
              View.Pending_Primary /= View.Applied_Primary or
              View.Pending_Auto /= View.Applied_Auto then "Unapplied arrangement"
            elsif View.Layout_Status = Saved_In_Config then "Applied / scale kept in Config until reboot"
            elsif View.Layout_Status = Session_Only then "Applied / this session"
            else "Resolutions stay unchanged"));
         return;
      end if;
      Text (280, 32, "Alloy theme");
      Text (280, 104, "Background");
      Text (280, 176, "Wallpaper placement");
      Paint_Wallpaper (C, Preview, View.Pending);
      Stroke_Rect (C, Preview, Colors.shadow, Colors.highlight);
      declare
         package WC renames CuBit.UI.Window_Chrome;
         Window : constant Rect := (Preview.x + 38, Preview.y + 32, 160, 94);
         Chrome : constant WC.Palette := WC.Colors (View.Pending.Scheme);
         Inner : constant Natural := Window.w - 2 * WC.Border_Width;
      begin
         --  The active window chrome in miniature, the accent as app colour.
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
         Draw_UI_Text (C, Window.x + WC.Title_Left, Window.y + WC.Bar_Top + 5, "Alloy",
           Chrome.Active_Title, Preview_Colors.titleBar);
         Draw_Button (C, (Window.x + 24, Window.y + WC.Client_Top + 12, 110, 28),
           Preview_Colors, Button_Normal, "Preview");
      end;
      Text (20, 218, "Preview");
      Text (20, 262, "Changes apply to this desktop and shared-toolkit applications.");
      Text (20, 286, "Apply reloads theme CCL from Config. No restart needed.");
      for Item in Control range Light .. Revert loop
         Draw_Button (C, Box (Item, Window_Bounds), Colors,
           (if Item = View.Pressed and Item = View.Hovered then Button_Pressed
            elsif Selected (Item) then Button_Active
            elsif Item = View.Hovered then Button_Hot else Button_Normal), Label (Item));
         if Item = View.Focused then
            declare B : constant Rect := Box (Item, Window_Bounds); begin
               Stroke_Rect (C, (B.x + 3, B.y + 3, B.w - 6, B.h - 6),
                 Colors.shadow, Colors.shadow);
            end;
         end if;
      end loop;
      if View.Pending /= View.Applied then
         Text (20, 340, "Unapplied changes");
      elsif View.Status = Saved_In_Config then
         Text (20, 340, "Applied / Config is volatile until reboot");
      elsif View.Status = Session_Only then
         Text (20, 340, "Applied / Config unavailable");
      else
         Text (20, 340, "Tab to navigate; Enter to choose");
      end if;
   end Render;

   procedure Paint_Legacy_Wallpaper
     (C : Canvas; Bounds : Rect; Style : CuBit.Appearance.Preferences)
   is
      P : constant Rect := Clamp_Rect (C, Bounds);
   begin
      if not Is_Empty (P) then
         Desktop_Wallpaper.Paint
           (C.addr + Storage_Offset (Bounds.y * C.pitch + Bounds.x * 4),
            Bounds.w, Bounds.h, C.pitch,
            P.x - Bounds.x, P.y - Bounds.y, P.w, P.h, Style);
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

   procedure Draw (View : State; C : Canvas; Window_Bounds : Rect) is
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
         when Mode_Previous => View.Mode_Top := (if View.Mode_Top > Mode_Rows then View.Mode_Top - Mode_Rows else Auto_Row);
         when Mode_Next =>
            if Last_Row (View) >= Mode_Rows then
               View.Mode_Top := Mode_Row'Min (View.Mode_Top + Mode_Rows, Last_Row (View) - (Mode_Rows - 1));
            end if;
         when Keep_Mode => View.Answer := Keep_Response;
         when Revert_Mode => View.Answer := Revert_Response;
         when Light => View.Pending.Scheme := A.Alloy_Light;
         when Dark => View.Pending.Scheme := A.Alloy_Dark;
         when Image => View.Pending.Backdrop := A.Wallpaper;
         when Cubie => View.Pending.Backdrop := A.Cubie;
         when Slate => View.Pending.Backdrop := A.Slate;
         when Ocean => View.Pending.Backdrop := A.Ocean;
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
      Hit : Control := None;
   begin
      Apply_Requested := False;
      if View.Current_Page = Date_Time and then Pointer.pressed and then
        Point_In_Rect (Pointer.x, Pointer.y, Zone_List_Box (Page_Bounds (Bounds)))
      then
         declare
            Row : constant Natural := (Pointer.y - Zone_List_Box (Page_Bounds (Bounds)).y) / Zone_Row_Height;
            Index : constant Natural := TZ.Time_Zone'Pos (View.Zone_Top) + Row;
         begin
            if Index <= TZ.Time_Zone'Pos (TZ.Time_Zone'Last) then
               Choose_Zone (View, TZ.Time_Zone'Val (Index));
            end if;
            View.Focused := Zone_List;
            View.Pressed := None;
            return;
         end;
      end if;
      if View.Current_Page = Monitor and then not View.Trial_Open and then Pointer.pressed and then
        Point_In_Rect (Pointer.x, Pointer.y, Mode_List_Box (Page_Bounds (Bounds)))
      then
         Choose_Mode (View, View.Mode_Top + (Pointer.y - Mode_List_Box (Page_Bounds (Bounds)).y) / Mode_Row_Height);
         View.Focused := Mode_List;
         View.Pressed := None;
         return;
      end if;
      if View.Current_Page = Displays then
         if Pointer.pressed and Point_In_Rect (Pointer.x, Pointer.y, Diagram (Page_Bounds (Bounds))) then
            for I in 1 .. View.Pending_Layout.Count loop
               if Point_In_Rect (Pointer.x, Pointer.y, Tile (View, Page_Bounds (Bounds), I)) then
                  View.Selected := I;
                  View.Drag_Original := View.Pending_Layout;
                  View.Transform := Projection (View.Pending_Layout, Page_Bounds (Bounds));
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
      end if;
      for Item in Control range Appearance_Tab .. Control'Last loop
         if Visible (View, Item) and then Item not in Zone_List | Mode_List and then
           Point_In_Rect (Pointer.x, Pointer.y, Box (Item, Bounds)) and then
           Point_In_Rect (Pointer.x, Pointer.y, Bounds)
         then Hit := Item; end if;
      end loop;
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
              when Scan_Page_Up => Moved (View.Pending_Zone, -Zone_Rows),
              when Scan_Page_Down => Moved (View.Pending_Zone, Zone_Rows),
              when Scan_Home => TZ.Time_Zone'First,
              when others => TZ.Time_Zone'Last));
      elsif View.Focused = Mode_List and then
        Scan_Code in Scan_Up | Scan_Down | Scan_Page_Up | Scan_Page_Down | Scan_Home | Scan_End
      then
         Choose_Mode (View,
           (case Scan_Code is
              when Scan_Up => Moved_Mode (View, -1),
              when Scan_Down => Moved_Mode (View, 1),
              when Scan_Page_Up => Moved_Mode (View, -Mode_Rows),
              when Scan_Page_Down => Moved_Mode (View, Mode_Rows),
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
