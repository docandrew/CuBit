with System.Storage_Elements; use System.Storage_Elements;
with Desktop_Wallpaper;
with CuBit.Display_Arrangement;

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
         when Make_Primary => return (C.x + 20, C.y + 304, 156, 28);
         when Scale_Down => return (C.x + 430, C.y + 304, 34, 28);
         when Scale_Up => return (C.x + 472, C.y + 304, 34, 28);
         when None => return (others => 0);
      end case;
   end Box;

   procedure Open (View : out State; Current : A.Preferences; Layout : L.Layout;
                   Primary : L.Named_Display_ID) is
   begin
      View := (Pending => Current, Applied => Current,
               Pending_Layout => Layout, Applied_Layout => Layout,
               Pending_Primary => Primary, Applied_Primary => Primary,
               Selected => (if Layout.Count > 0 then 1 else 0), others => <>);
   end Open;

   function Visible (View : State; Item : Control) return Boolean is
     (Item in Appearance_Tab .. Displays_Tab or else
       Item in Apply .. Revert or else
       (View.Current_Page = Displays and then Item in Make_Primary .. Scale_Up) or else
       (View.Current_Page = Appearance and then Item in Light .. Revert));

   procedure Draw (View : State; C : Canvas; Window_Bounds : Rect) is
      Bounds : constant Rect := Page_Bounds (Window_Bounds);
      Colors : constant Theme := Current_Theme;
      Preview_Colors : constant Theme := Palette (View.Pending.Scheme);
      Preview : constant Rect := (Bounds.x + 20, Bounds.y + 56, 236, 150);
      P : constant Rect := Clamp_Rect (C, Preview);
      function Label (Item : Control) return String is
        (case Item is when Appearance_Tab => "Appearance", when Displays_Tab => "Displays",
         when Light => "Alloy Light", when Dark => "Alloy Dark",
         when Image => "Cubes", when Cubie => "Cubie", when Slate => "Slate",
         when Ocean => "Ocean", when Fill => "Fill", when Fit => "Fit",
         when Center => "Center", when Apply => "Apply", when Revert => "Revert",
         when Make_Primary => "Make primary",
         when Scale_Down => "-", when Scale_Up => "+",
         when None => "");
      function Selected (Item : Control) return Boolean is
        (case Item is
         when Appearance_Tab => View.Current_Page = Appearance,
         when Displays_Tab => View.Current_Page = Displays,
         when Light => View.Pending.Scheme = A.Alloy_Light,
         when Dark => View.Pending.Scheme = A.Alloy_Dark,
         when Image => View.Pending.Backdrop = A.Wallpaper,
         when Cubie => View.Pending.Backdrop = A.Cubie,
         when Slate => View.Pending.Backdrop = A.Slate,
         when Ocean => View.Pending.Backdrop = A.Ocean,
         when Fill => View.Pending.Position = A.Fill,
         when Fit => View.Pending.Position = A.Fit,
         when Center => View.Pending.Position = A.Center,
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
      Text (20, 12, (if View.Current_Page = Appearance then "Appearance" else "Displays"));
      for Item in Control range Appearance_Tab .. Displays_Tab loop
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
                 Natural'Image (100 * Natural (Screen.Scale.Numerator) / Natural (Screen.Scale.Denominator)) & "%");
            end;
         end if;
         Text (20, 284, "Edges snap together. The white bar marks the primary display.");
         for Item in Control range Apply .. Scale_Up loop
            Draw_Button (C, Box (Item, Window_Bounds), Colors,
              (if Item = View.Pressed and Item = View.Hovered then Button_Pressed
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
              View.Pending_Primary /= View.Applied_Primary then "Unapplied arrangement"
            elsif View.Layout_Status = Session_Only then "Applied / this session"
            else "Resolutions stay unchanged"));
         return;
      end if;
      Text (280, 32, "Alloy theme");
      Text (280, 104, "Background");
      Text (280, 176, "Wallpaper placement");
      if not Is_Empty (P) then
         Desktop_Wallpaper.Paint
           (C.addr + Storage_Offset (Preview.y * C.pitch + Preview.x * 4),
            Preview.w, Preview.h, C.pitch,
            P.x - Preview.x, P.y - Preview.y, P.w, P.h, View.Pending);
      end if;
      Stroke_Rect (C, Preview, Colors.shadow, Colors.highlight);
      declare
         Window : constant Rect := (Preview.x + 38, Preview.y + 32, 160, 94);
      begin
         Fill_Rect (C, Window, Preview_Colors.face);
         Stroke_Rect (C, Window, Preview_Colors.highlight, Preview_Colors.shadow);
         Fill_Vertical_Gradient (C, (Window.x + 3, Window.y + 3, 154, 20),
           Preview_Colors.activeTitleTop, Preview_Colors.activeTitleBottom);
         Draw_UI_Text (C, Window.x + 8, Window.y + 5, "Alloy", 16#FFFFFF#,
           Preview_Colors.activeTitleTop);
         Draw_Button (C, (Window.x + 24, Window.y + 42, 110, 28),
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
   end Draw;

   procedure Activate (View : in out State; Item : Control; Apply_Requested : out Boolean) is
   begin
      Apply_Requested := False;
      case Item is
         when Appearance_Tab => View.Current_Page := Appearance;
         when Displays_Tab => View.Current_Page := Displays;
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
                     View.Layout_Status := Unchanged;
                  else View.Layout_Status := Scale_Rejected; end if;
               end;
            end if;
         when Revert =>
            if View.Current_Page = Displays then
               View.Pending_Layout := View.Applied_Layout;
               View.Pending_Primary := View.Applied_Primary;
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
      for Item in Control range Appearance_Tab .. Scale_Up loop
         if Visible (View, Item) and then
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
      if Scan_Code = 16#01# and then View.Dragging then
         View.Pending_Layout := View.Drag_Original;
         View.Dragging := False;
      elsif View.Focused in Appearance_Tab .. Displays_Tab and then
        Scan_Code in 16#48# | 16#50# then
         View.Focused := (if Scan_Code = 16#48# then Appearance_Tab else Displays_Tab);
         Activate (View, View.Focused, Apply_Requested);
      elsif Scan_Code = 16#0F# then
         for Attempt in Control range Appearance_Tab .. Scale_Up loop
            View.Focused :=
              (if Backwards then (if View.Focused = Appearance_Tab then Scale_Up else Control'Pred (View.Focused))
               else (if View.Focused = Scale_Up then Appearance_Tab else Control'Succ (View.Focused)));
            exit when Visible (View, View.Focused);
         end loop;
      elsif Scan_Code in 16#1C# | 16#39# then
         Activate (View, View.Focused, Apply_Requested);
      end if;
   end Key;
end Desktop_Settings;
