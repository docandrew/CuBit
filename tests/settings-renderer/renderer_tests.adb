with Ada.Text_IO;
with System;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.Boxes;
with CuBit.Appearance;
with CuBit.Display_Layouts;
with Desktop_Settings;
with Desktop_Wallpaper_Catalog;
with Time_Zones;
with CuBit.Output_Modes;
with CuBit.Output_Discovery;
with CuBit.UI_Metrics;
procedure Renderer_Tests is
   use type System.Address;
   package A renames CuBit.Appearance;
   package S renames Desktop_Settings;
   use type S.Control, S.Page;
   type Operation is (Fill, Stroke, Gradient, Text, Button, Tab, Wallpaper);
   Seen : array (Operation) of Natural := [others => 0];
   Canvas_Width : constant := 2048;
   Calls, Cases : Natural := 0;
   procedure Record_Op (C : Canvas; Op : Operation) is
   begin
      pragma Assert (C.addr = System.Null_Address);
      pragma Assert (C.width = 2048 and C.height = 1536 and C.clipEnabled);
      Seen (Op) := Seen (Op) + 1;
      Calls := Calls + 1;
   end Record_Op;
   procedure Fill_Rect (C : Canvas; R : Rect; Fill : Color) is
   begin Record_Op (C, Renderer_Tests.Fill); end;
   procedure Stroke_Rect (C : Canvas; R : Rect; Light, Dark : Color) is
   begin Record_Op (C, Stroke); end;
   procedure Gradient_Rect (C : Canvas; R : Rect; TopColor, BottomColor : Color) is
   begin Record_Op (C, Gradient); end;

   --  Every text run of one rendering, to check that none overlaps another
   --  on its line and that each ends inside the window (UI-033: no
   --  clipped subtitle, no "138.780 MHlink type ..." run-ins).
   Maximum_Runs : constant := 512;
   type Run is record
      X, Y, Width : Natural := 0;
      Clipped : Boolean := False;
   end record;
   Runs : array (1 .. Maximum_Runs) of Run;
   Run_Count : Natural := 0;
   --  Texts naming an attached, unlit monitor.
   Attached_Drawn : Natural := 0;
   procedure Draw_Text (C : Canvas; X, Y : Natural; Value : String; FG, BG : Color) is
   begin
      Record_Op (C, Text);
      if Value = "DELL U2720Q" then
         Attached_Drawn := Attached_Drawn + 1;
      end if;
      if Run_Count < Maximum_Runs and then Value'Length > 0 then
         Run_Count := Run_Count + 1;
         --  Runs drawn through a narrower clip (a list's rows, the diagram,
         --  the preview) are that widget's business, checked by its own
         --  columns; the rest must stay inside the window.
         Runs (Run_Count) := (X, Y, UI_Text_Width (Value), C.clip.w < Canvas_Width);
      end if;
   end Draw_Text;
   procedure Draw_Button (C : Canvas; R : Rect; Colors : Theme; Style : Button_Style; Label : String) is
   begin Record_Op (C, Button); end;
   procedure Draw_Tab (C : Canvas; R : Rect; Colors : Theme; Selected, Hot, Active : Boolean;
                       Label : String; Orientation : Tab_Orientation := Horizontal) is
   begin Record_Op (C, Tab); end;
   procedure Paint_Wallpaper (C : Canvas; Bounds : Rect; Style : A.Preferences) is
   begin Record_Op (C, Wallpaper); end;
   procedure Render is new Desktop_Settings.Render
     (Fill_Rect, Stroke_Rect, Gradient_Rect, Draw_Text, Draw_Button, Draw_Tab, Paint_Wallpaper);
   package Controls is new CuBit.UI.Control_Renderer (Fill_Rect, Stroke_Rect, Draw_Text);
   procedure Render_Controls is new Desktop_Settings.Render
     (Fill_Rect, Stroke_Rect, Gradient_Rect, Draw_Text, Controls.Draw_Button,
      Controls.Draw_Tab, Paint_Wallpaper);
   C : Canvas := (addr => System.Null_Address, width => 2048, height => 1536,
      pitch => 0, clipEnabled => True, clip => (0, 0, 2048, 1536), others => <>);
   Layout : CuBit.Display_Layouts.Layout;
   View : S.State;
   Before : Natural;
   --  A catalog of five wallpapers: the list is sized to it.
   Wallpapers : constant := 5;
   function Wallpaper_Name (N : Positive) return String is ("picture-" & Character'Val (Character'Pos ('0') + N));
   function Catalog_Text return String is
      Text : String (1 .. 0);
      function Line (N : Positive) return String is
        ("(Wallpaper name => """ & Wallpaper_Name (N) & """ label => ""Label" & N'Image & """ file => """ &
         Wallpaper_Name (N) & ".qoi"" attribution => ""test"")" & ASCII.LF);
      function Lines (From : Positive) return String is
        (if From > Wallpapers then "" else Line (From) & Lines (From + 1));
   begin
      return Text & Lines (1);
   end Catalog_Text;
   Loaded : Desktop_Wallpaper_Catalog.Load_Result;
   --  The appearances the pages are drawn with: the default wallpaper, each
   --  wallpaper by name, and the two solid colours.
   function Appearance (Scheme : A.Color_Scheme; Choice : Natural; Placement : A.Placement)
     return A.Preferences is
     (if Choice = 0 then (A.Default with delta Scheme => Scheme, Position => Placement)
      elsif Choice <= Wallpapers then
        (A.Default with delta Scheme => Scheme, Image => A.To_Image_Name (Wallpaper_Name (Choice)),
                              Position => Placement)
      else (A.Default with delta Scheme => Scheme, Kind => A.Solid, Position => Placement,
                               Color => (if Choice = Wallpapers + 1 then A.Slate else A.Ocean)));

   --  Widgets by identity: the middle of a control or of a list's row.
   function Middle (R : Rect) return Pointer_State is
     ((R.x + R.w / 2, R.y + R.h / 2, True, True, False, True));
   function Inside (Inner, Outer : Rect) return Boolean is
     (Inner.x >= Outer.x and then Inner.y >= Outer.y and then
      Inner.x + Inner.w <= Outer.x + Outer.w and then Inner.y + Inner.h <= Outer.y + Outer.h);
   function Overlap (Left, Right : Rect) return Boolean is
     (Left.x < Right.x + Right.w and then Right.x < Left.x + Left.w and then
      Left.y < Right.y + Right.h and then Right.y < Left.y + Left.h);

   --  Render View in Window and check the geometry: every control the
   --  page shows lies inside the window and no two overlap; no text run
   --  leaves the window or runs into another on its line.
   procedure Check_Page (Window : Rect; What : String) is
      Shown : array (S.Control) of Rect := [others => (0, 0, 0, 0)];
   begin
      pragma Assert (S.Fits (View, Window), What & ": the page fits");
      for Item in S.Control range S.Appearance_Tab .. S.Control'Last loop
         Shown (Item) := S.Control_Bounds (View, Window, Item);
         if Shown (Item).w > 0 then
            pragma Assert (Inside (Shown (Item), Window), What & ": " & Item'Image & " inside the window");
         end if;
      end loop;
      for Left in S.Control range S.Appearance_Tab .. S.Control'Last loop
         for Right in S.Control range Left .. S.Control'Last loop
            if Left /= Right and then Shown (Left).w > 0 and then Shown (Right).w > 0 then
               pragma Assert (not Overlap (Shown (Left), Shown (Right)),
                              What & ": " & Left'Image & " overlaps " & Right'Image);
            end if;
         end loop;
      end loop;
      C.clip := (0, 0, C.width, C.height);
      Run_Count := 0;
      Render (View, C, Window);
      for I in 1 .. Run_Count loop
         if not Runs (I).Clipped then
            pragma Assert (Runs (I).X + Runs (I).Width <= Window.x + Window.w,
                           What & ": a text run leaves the window");
         end if;
         for J in I + 1 .. Run_Count loop
            if Runs (I).Y = Runs (J).Y then
               pragma Assert (Runs (I).X + Runs (I).Width <= Runs (J).X or else
                              Runs (J).X + Runs (J).Width <= Runs (I).X,
                              What & ": two text runs overlap on one line");
            end if;
         end loop;
      end loop;
   end Check_Page;

   Least, Natural_Size : CuBit.UI.Boxes.Size;
   Too_Narrow, Too_Short : Natural := 0;
   Window : Rect;
begin
   Desktop_Wallpaper_Catalog.Load (Catalog_Text, Loaded);
   pragma Assert (Desktop_Wallpaper_Catalog.Count = Wallpapers);
   Layout.Count := 2;
   Layout.Items (1) := (1, (1024, 768, others => <>));
   Layout.Items (2) := (2, (Width => 1280, Height => 720, X => 1024, others => <>));
   S.Open (View, A.Default, Layout, 1, Auto_Scale => True, Zone => Time_Zones.Zone_America_Denver);
   S.Measure (View, Least, Natural_Size);
   pragma Assert (Least.Width > 0 and Least.Height > 0 and
                  Natural_Size.Width >= Least.Width and Natural_Size.Height >= Least.Height);
   Window := (100, 100, Natural_Size.Width, Natural_Size.Height);
   for Page in S.Page loop
      for Scheme in A.Color_Scheme loop
         for Backdrop in 0 .. Wallpapers + 2 loop
            for Placement in A.Placement loop
               for Clip in 1 .. 4 loop
                  C.clip := (case Clip is when 1 => Window,
                    when 2 => (300, 180, 80, 40), when 3 => (0, 0, 1, 1), when others => Window);
                  --  The fourth pass: a user-chosen (not automatic) scale.
                  S.Open (View, Appearance (Scheme, Backdrop, Placement), Layout, 1, Auto_Scale => Clip /= 4,
                    Zone => Time_Zones.Zone_America_Denver);
                  View.Current_Page := Page;
                  Before := Calls;
                  Render (View, C, Window);
                  pragma Assert (Calls > Before and Calls - Before < 150);
                  Before := Calls;
                  Render_Controls (View, C, Window);
                  pragma Assert (Calls > Before and Calls - Before < 300);
                  Cases := Cases + 1;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   -- Exercise branches not selected by the initial Settings state, including
   -- empty/tiny controls and every interaction style. No pixel target exists.
   C.clip := (0, 0, 2048, 1536);
   for Size in 0 .. 4 loop
      for Style in Button_Style loop
         Controls.Draw_Button (C, (120, 120, Size, Size), CuBit_Alloy, Style, "Apply");
      end loop;
      for Selected in Boolean loop
         for Hot in Boolean loop
            for Active in Boolean loop
               for Orientation in Tab_Orientation loop
                  Controls.Draw_Tab (C, (120, 120, Size, Size), CuBit_Alloy,
                    Selected, Hot, Active, "Displays", Orientation);
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   for Op in Operation loop pragma Assert (Seen (Op) > 0); end loop;

   --  Date & Time: the zone list by keyboard and pointer, Apply and Revert
   --  (TEST-007: rows addressed by identity, not by pixel constants).
   declare
      use type Time_Zones.Time_Zone;
      Apply : Boolean;
      Scan_Enter : constant := 16#1C#;
      Scan_Down : constant := 16#50#;
      Scan_Page_Down : constant := 16#51#;
      Scan_Home : constant := 16#47#;
      Scan_End : constant := 16#4F#;
      Scan_E : constant := 16#12#;
   begin
      S.Open (View, A.Default, Layout, 1, Auto_Scale => True, Zone => Time_Zones.Zone_UTC);
      View.Focused := S.Date_Time_Tab;
      S.Key (View, Scan_Enter, False, Apply);
      pragma Assert (View.Current_Page = S.Date_Time and not Apply);
      S.Key (View, Scan_E, False, Apply);
      pragma Assert (Time_Zones.Name (View.Pending_Zone) (1) = 'E' and View.Focused = S.Zone_List);
      S.Key (View, Scan_Home, False, Apply);
      pragma Assert (View.Pending_Zone = Time_Zones.Time_Zone'First and View.Zone_Top = View.Pending_Zone);
      S.Key (View, Scan_Down, False, Apply);
      pragma Assert (View.Pending_Zone = Time_Zones.Time_Zone'Val (1));
      S.Key (View, Scan_Page_Down, False, Apply);
      pragma Assert (View.Pending_Zone = Time_Zones.Time_Zone'Val (10));
      pragma Assert (Time_Zones.Time_Zone'Pos (View.Zone_Top) = 2, "the list follows the choice");
      S.Key (View, Scan_End, False, Apply);
      pragma Assert (View.Pending_Zone = Time_Zones.Time_Zone'Last);
      S.Key (View, Scan_Home, False, Apply);
      S.Pointer (View, Window, Middle (S.Row_Bounds (View, Window, S.Zone_List, 3)), Apply);
      pragma Assert (View.Pending_Zone = Time_Zones.Time_Zone'Val (3) and not Apply);
      Check_Page (Window, "Date & Time");
      View.Focused := S.Apply;
      S.Key (View, Scan_Enter, False, Apply);
      pragma Assert (Apply, "Apply requests the page's apply");
      View.Focused := S.Revert;
      S.Key (View, Scan_Enter, False, Apply);
      pragma Assert (View.Pending_Zone = Time_Zones.Zone_UTC, "Revert returns to the zone in Config");
      Cases := Cases + 1;
   end;
   --  Monitor (DISP-016): the sorted mode list under its Auto row, chosen
   --  by keyboard and pointer (refused rows skipped), scrolled, applied;
   --  then the "Keep this display mode?" question and its answers.
   declare
      use type S.Trial_Response;
      package OM renames CuBit.Output_Modes;
      Apply : Boolean;
      Scan_Enter : constant := 16#1C#;
      Scan_Up : constant := 16#48#;
      Scan_Down : constant := 16#50#;
      Scan_Home : constant := 16#47#;
      Scan_End : constant := 16#4F#;
      Info : S.Monitor_Info;
      Count : constant := 14;
      function Mode (Width, Height : Positive; Rate : Natural; Verdict : OM.Admission := OM.Admitted)
        return OM.Listed_Mode is
        ((Size => (OM.OD.Extent (Width), OM.OD.Extent (Height)), Refresh_Millihertz => OM.Millihertz'Base (Rate),
          Pixel_kHz => 148_500, Verdict => Verdict, Preferred => False, Current => False));
      function Row (Line : Natural) return Pointer_State is
        (Middle (S.Row_Bounds (View, Window, S.Mode_List, Line)));
   begin
      Info.Modes := new S.Monitor_Modes (1 .. Count);
      Info.Modes.all :=
        [Mode (4096, 2160, 60_000, OM.Above_Sink_Clock), Mode (3840, 2160, 60_000), Mode (3840, 2160, 50_000),
         Mode (3840, 2160, 30_000), Mode (2560, 1440, 59_951), Mode (1920, 1080, 120_000, OM.Needs_Higher_CD_Clock),
         Mode (1920, 1080, 60_000), Mode (1920, 1080, 59_940), Mode (1920, 1080, 50_000), Mode (1280, 720, 60_000),
         Mode (1024, 768, 60_000), Mode (800, 600, 60_317), Mode (720, 576, 50_000),
         Mode (640, 480, 59_940, OM.Link_Unsupported)];
      Info.Modes (2).Preferred := True;
      Info.Modes (7).Current := True;
      Info.Identified := True;
      Info.Saved_Auto := False;
      Info.Saved_Row := 7;
      --  The photographed laptop's lines (UI-033): long enough to wrap.
      Info.Summary := S.Line_Of ("344 x 193 mm (15.5 in), 142 dpi at 1920 x 1080");
      Info.Detail := S.Line_Of
        ("Best mode 1920 x 1080; Auto keeps the firmware mode at startup (desktop.display.mode = Auto)");
      --  DISP-009/010: a monitor attached to an unlit output is a row of
      --  its own, and nothing on the page selects it.
      declare
         package OD renames CuBit.Output_Discovery;
         Item : constant S.Attached_Monitor := S.Describe_Attached
           ((OD.Detected_Only, OD.Intel_Display, 2, 3840, 2160, 600, 340, OD.Connected_Not_Lit, OD.HDMI),
            "DELL U2720Q");
         Bare : constant S.Attached_Monitor := S.Describe_Attached
           ((OD.Detected_Only, OD.Intel_Display, 3, 1, 1, 0, 0, OD.Connected_Not_Lit, OD.Unknown_Connector), "");
         function Text (Line : S.Monitor_Line) return String is (Line.Text (1 .. Line.Length));
      begin
         pragma Assert (Text (Item.Connector) = "HDMI" and Text (Item.Name) = "DELL U2720Q" and
                        Text (Item.Size) = "600 x 340 mm" and Text (Item.Best) = "best 3840 x 2160");
         pragma Assert (Text (Bare.Connector) = "Display" and Bare.Name.Length = 0 and Bare.Size.Length = 0 and
                        Bare.Best.Length = 0);
         Info.Attached_Used := 2;
         Info.Attached (1) := Item;
         Info.Attached (2) := Bare;
      end;
      S.Open (View, A.Default, Layout, 1, Auto_Scale => True, Zone => Time_Zones.Zone_UTC);
      View.Current_Page := S.Monitor;
      S.Set_Monitor (View, Info);
      S.Measure (View, Least, Natural_Size);
      Window := (100, 100, Natural_Size.Width, Natural_Size.Height);
      pragma Assert (View.Mode_Choice = 7, "the saved mode is chosen");
      pragma Assert (View.Mode_Top <= 7 and then 7 < View.Mode_Top + 11, "and visible");
      Attached_Drawn := 0;
      Check_Page (Window, "Monitor");
      pragma Assert (Attached_Drawn = 1, "the attached monitor's row is drawn");
      --  Also at the window's minimum size, and on Displays.
      Check_Page ((100, 100, Least.Width, Least.Height), "Monitor, smallest");
      View.Current_Page := S.Displays;
      Attached_Drawn := 0;
      Check_Page (Window, "Displays");
      pragma Assert (Attached_Drawn = 1, "Displays names the attached monitor too");
      View.Current_Page := S.Monitor;
      Render_Controls (View, C, Window);
      View.Focused := S.Mode_List;
      S.Key (View, Scan_Up, False, Apply);
      pragma Assert (View.Mode_Choice = 5, "up skips the refused 1080p120 row");
      S.Key (View, Scan_Home, False, Apply);
      pragma Assert (View.Mode_Choice = S.Auto_Row and View.Mode_Top = S.Auto_Row);
      S.Key (View, Scan_Down, False, Apply);
      pragma Assert (View.Mode_Choice = 2, "down skips the refused 4096 x 2160 row");
      S.Key (View, Scan_End, False, Apply);
      pragma Assert (View.Mode_Choice = Count - 1 and View.Mode_Top = Count - 11, "End: the last admitted row");
      --  A pointer on a refused row changes nothing; on an admitted row it chooses it.
      View.Focused := S.Monitor_Tab;
      S.Key (View, Scan_Home, False, Apply);
      View.Mode_Top := S.Auto_Row;
      View.Mode_Choice := 3;
      S.Pointer (View, Window, Row (1), Apply);
      pragma Assert (View.Mode_Choice = 3 and View.Focused = S.Mode_List, "a refused row is not chosen");
      S.Pointer (View, Window, Row (4), Apply);
      pragma Assert (View.Mode_Choice = 4 and not Apply);
      --  The header row chooses nothing.
      declare
         List : constant Rect := S.Control_Bounds (View, Window, S.Mode_List);
      begin
         S.Pointer (View, Window, (List.x + List.w / 2, List.y + 1, True, True, False, True), Apply);
         pragma Assert (View.Mode_Choice = 4);
      end;
      --  The Down button pages the list; Up comes back.
      View.Focused := S.Mode_Next;
      S.Key (View, Scan_Enter, False, Apply);
      pragma Assert (View.Mode_Top = Count - 10, "Down stops at the last page");
      View.Focused := S.Mode_Previous;
      S.Key (View, Scan_Enter, False, Apply);
      pragma Assert (View.Mode_Top = S.Auto_Row);
      --  The same by pointer, through the buttons' own places.
      S.Pointer (View, Window, Middle (S.Control_Bounds (View, Window, S.Mode_Next)), Apply);
      S.Pointer (View, Window, (Middle (S.Control_Bounds (View, Window, S.Mode_Next)) with delta
                                pressed => False, down => False, released => True), Apply);
      pragma Assert (View.Mode_Top = Count - 10, "the Down button by pointer");
      View.Focused := S.Apply;
      S.Key (View, Scan_Enter, False, Apply);
      pragma Assert (Apply, "Apply asks Desktop to switch");
      View.Focused := S.Revert;
      S.Key (View, Scan_Enter, False, Apply);
      pragma Assert (View.Mode_Choice = 7, "Revert returns to the saved mode");
      --  The question: only Keep and Revert (and the tabs) take input.
      View.Trial_Open := True;
      View.Trial_Seconds := 15;
      View.Focused := S.Keep_Mode;
      Check_Page (Window, "Monitor, keep this mode?");
      Render_Controls (View, C, Window);
      S.Pointer (View, Window, Row (2), Apply);
      pragma Assert (View.Mode_Choice = 7, "the list is frozen while asking");
      View.Focused := S.Keep_Mode;
      S.Key (View, Scan_Enter, False, Apply);
      pragma Assert (View.Answer = S.Keep_Response and not Apply);
      View.Answer := S.No_Response;
      View.Focused := S.Revert_Mode;
      S.Key (View, Scan_Enter, False, Apply);
      pragma Assert (View.Answer = S.Revert_Response);
      for Attempt in 1 .. 12 loop
         S.Key (View, 16#0F#, False, Apply);
         pragma Assert (View.Focused not in S.Apply | S.Revert | S.Mode_List | S.Mode_Previous | S.Mode_Next,
                        "Tab never reaches a hidden control");
      end loop;
      --  An empty list (no EDID) still renders.
      S.Set_Monitor (View, (others => <>));
      View.Trial_Open := False;
      Render (View, C, Window);
      Cases := Cases + 1;
   end;
   --  Appearance: the wallpaper list, one row per catalog wallpaper, by
   --  keyboard and pointer; the solid colours; Revert and Apply.
   declare
      use type A.Backdrop_Kind, A.Image_Name, A.Solid_Color, A.Preferences;
      Apply : Boolean;
      Scan_Enter : constant := 16#1C#;
      Scan_Down : constant := 16#50#;
      Scan_Page_Down : constant := 16#51#;
      Scan_Home : constant := 16#47#;
      Scan_End : constant := 16#4F#;
      Scan_Tab : constant := 16#0F#;
      procedure Click (Item : S.Control) is
         P : constant Pointer_State := Middle (S.Control_Bounds (View, Window, Item));
      begin
         S.Pointer (View, Window, P, Apply);
         S.Pointer (View, Window, (P with delta pressed => False, down => False, released => True), Apply);
      end Click;
      function Row (Line : Natural) return Pointer_State is
        (Middle (S.Row_Bounds (View, Window, S.Wallpaper_List, Line)));
      function Chosen_Name return String is (A.Name (View.Pending.Image));
   begin
      pragma Assert (S.Wallpaper_Rows = Wallpapers);
      for Row in 1 .. Wallpapers loop
         pragma Assert (S.Wallpaper_Label (Row) = "Label" & Row'Image);
      end loop;
      S.Open (View, A.Default, Layout, 1, Auto_Scale => True, Zone => Time_Zones.Zone_UTC);
      pragma Assert (S.Chosen_Wallpaper (View) = 1 and View.Wallpaper_Top = 1, "no name: the default row");
      Check_Page (Window, "Appearance");
      --  A row click chooses that wallpaper by its catalog name.
      S.Pointer (View, Window, Row (1), Apply);
      pragma Assert (Chosen_Name = Wallpaper_Name (2) and View.Focused = S.Wallpaper_List);
      pragma Assert (S.Chosen_Wallpaper (View) = 2 and View.Pending /= View.Applied);
      S.Key (View, Scan_Down, False, Apply);
      pragma Assert (Chosen_Name = Wallpaper_Name (3));
      S.Key (View, Scan_End, False, Apply);
      pragma Assert (Chosen_Name = Wallpaper_Name (Wallpapers) and View.Wallpaper_Top = Wallpapers - 2,
                     "the last row scrolls into view");
      Render (View, C, Window);
      S.Key (View, Scan_Home, False, Apply);
      pragma Assert (Chosen_Name = Wallpaper_Name (1) and View.Wallpaper_Top = 1);
      S.Key (View, Scan_Page_Down, False, Apply);
      pragma Assert (Chosen_Name = Wallpaper_Name (4) and View.Wallpaper_Top = 2);
      S.Key (View, Scan_Down, False, Apply);
      S.Key (View, Scan_Down, False, Apply);
      pragma Assert (Chosen_Name = Wallpaper_Name (Wallpapers), "stops at the last row");
      --  The Up and Down buttons page the list without choosing.
      Click (S.Wallpaper_Previous);
      pragma Assert (View.Wallpaper_Top = 1 and Chosen_Name = Wallpaper_Name (Wallpapers));
      Click (S.Wallpaper_Next);
      pragma Assert (View.Wallpaper_Top = Wallpapers - 2);
      --  A row of the scrolled list.
      S.Pointer (View, Window, Row (0), Apply);
      pragma Assert (Chosen_Name = Wallpaper_Name (Wallpapers - 2));
      --  A solid colour, then back to a wallpaper keeps both choices.
      Click (S.Ocean);
      pragma Assert (View.Pending.Kind = A.Solid and View.Pending.Color = A.Ocean and S.Chosen_Wallpaper (View) = 0);
      Render (View, C, Window);
      View.Focused := S.Wallpaper_List;
      S.Key (View, Scan_Home, False, Apply);
      pragma Assert (View.Pending.Kind = A.Image and Chosen_Name = Wallpaper_Name (1) and
                     View.Pending.Color = A.Ocean);
      --  Tab reaches the list and its buttons between the colours and placement.
      View.Focused := S.Dark;
      for Expected in S.Control range S.Slate .. S.Apply loop
         S.Key (View, Scan_Tab, False, Apply);
         pragma Assert (View.Focused = Expected, "tab order " & Expected'Image);
      end loop;
      S.Key (View, Scan_Enter, False, Apply);
      pragma Assert (Apply);
      View.Focused := S.Revert;
      S.Key (View, Scan_Enter, False, Apply);
      pragma Assert (View.Pending = View.Applied and S.Chosen_Wallpaper (View) = 1);
      --  Tabs by pointer.
      Click (S.Displays_Tab);
      pragma Assert (View.Current_Page = S.Displays);
      Cases := Cases + 1;
   end;
   --  UI-033: long refusal reasons wrap in the status column (its row
   --  grows) instead of setting the window's minimum width; reasons are
   --  never cut.
   declare
      package OM renames CuBit.Output_Modes;
      Info : S.Monitor_Info;
      Short_Least, Short_Natural, Long_Least, Long_Natural : CuBit.UI.Boxes.Size;
      function Mode (Verdict : OM.Admission) return OM.Listed_Mode is
        ((Size => (1920, 1080), Refresh_Millihertz => 60_001, Pixel_kHz => 138_780,
          Verdict => Verdict, Preferred => True, Current => True));
   begin
      S.Open (View, A.Default, Layout, 1, Auto_Scale => True, Zone => Time_Zones.Zone_UTC);
      View.Current_Page := S.Monitor;
      Info.Modes := new S.Monitor_Modes'[1 => Mode (OM.Admitted)];
      S.Set_Monitor (View, Info);
      S.Measure (View, Short_Least, Short_Natural);
      Info := (others => <>);
      Info.Modes := new S.Monitor_Modes'[Mode (OM.Needs_Scrambling), Mode (OM.Link_Unsupported)];
      Info.Saved_Auto := False;
      Info.Saved_Row := 1;
      S.Set_Monitor (View, Info);
      S.Measure (View, Long_Least, Long_Natural);
      pragma Assert (Long_Least.Width < Long_Natural.Width, "the reasons do not set the minimum width");
      pragma Assert (Long_Least.Width <= Short_Least.Width + 4 * CuBit.UI_Metrics.Base_Em,
                     "the minimum width is the fixed columns and a status minimum");
      --  At the minimum the reason wraps: its row is taller than one line,
      --  every run stays in the window and none overlaps another.
      Window := (0, 0, Long_Least.Width, Long_Least.Height);
      Check_Page (Window, "Monitor, long reasons, smallest");
      pragma Assert (S.Row_Bounds (View, Window, S.Mode_List, 1).h > CuBit.UI_Metrics.Compact_Row_Height,
                     "a long reason's row grows");
      pragma Assert (S.Row_Bounds (View, Window, S.Mode_List, 2).y =
                     S.Row_Bounds (View, Window, S.Mode_List, 1).y + S.Row_Bounds (View, Window, S.Mode_List, 1).h,
                     "rows stack by their heights");
      Cases := Cases + 1;
   end;
   --  Page Up/Down and the wheel: the rows actually visible (Place), and
   --  the list under the pointer (UI-035), without a click into it first.
   declare
      use type Time_Zones.Time_Zone;
      Apply : Boolean;
      Scan_Page_Down : constant := 16#51#;
      Scan_Home : constant := 16#47#;
      Tall : Rect;
   begin
      S.Open (View, A.Default, Layout, 1, Auto_Scale => True, Zone => Time_Zones.Zone_UTC);
      S.Measure (View, Least, Natural_Size);
      View.Current_Page := S.Date_Time;
      Tall := (0, 0, Natural_Size.Width, 2 * Natural_Size.Height);
      pragma Assert (S.Page_Rows (View, S.Zone_List) = 9, "nine rows until placed");
      S.Place (View, Tall);
      pragma Assert (S.Page_Rows (View, S.Zone_List) > 9, "a taller window shows more rows");
      View.Focused := S.Zone_List;
      S.Key (View, Scan_Home, False, Apply);
      S.Key (View, Scan_Page_Down, False, Apply);
      pragma Assert (Time_Zones.Time_Zone'Pos (View.Pending_Zone) = S.Page_Rows (View, S.Zone_List),
                     "Page Down moves by the rows shown");
      declare
         List : constant Rect := S.Control_Bounds (View, Tall, S.Zone_List);
         Chosen : constant Time_Zones.Time_Zone := View.Pending_Zone;
         Top : constant Natural := Time_Zones.Time_Zone'Pos (View.Zone_Top);
      begin
         View.Focused := S.Apply;
         S.Wheel (View, Tall, List.x + List.w / 2, List.y + List.h / 2, -1);
         pragma Assert (Time_Zones.Time_Zone'Pos (View.Zone_Top) = Top + S.Wheel_Rows and
                        View.Pending_Zone = Chosen, "the wheel scrolls the list under the pointer");
         S.Wheel (View, Tall, List.x + List.w / 2, List.y + List.h / 2, 1);
         pragma Assert (Time_Zones.Time_Zone'Pos (View.Zone_Top) = Top, "and back");
         S.Wheel (View, Tall, Tall.x + 1, Tall.y + 1, -1);
         pragma Assert (Time_Zones.Time_Zone'Pos (View.Zone_Top) = Top, "not outside it");
         S.Wheel (View, Tall, List.x + 1, List.y + 1, 1_000);
         pragma Assert (View.Zone_Top = Time_Zones.Time_Zone'First, "stops at the top");
      end;
      Cases := Cases + 1;
   end;
   --  The published layout: one line per widget, by name.
   declare
      Seen_Apply, Seen_Preview_Window : Boolean := False;
      procedure Put (Line : String) is
         Apply_Prefix : constant String := "Settings layout APPLY ";
         Window_Prefix : constant String := "Settings layout PREVIEW_WINDOW ";
      begin
         if Line'Length > Apply_Prefix'Length
           and then Line (Line'First .. Line'First + Apply_Prefix'Length - 1) = Apply_Prefix
         then
            Seen_Apply := True;
         end if;
         if Line'Length > Window_Prefix'Length
           and then Line (Line'First .. Line'First + Window_Prefix'Length - 1) = Window_Prefix
         then
            Seen_Preview_Window := True;
         end if;
      end Put;
      procedure Publish is new S.Publish_Layout (Put);
   begin
      View.Current_Page := S.Appearance;
      Publish (View, (0, 0, Natural_Size.Width, Natural_Size.Height));
      pragma Assert (Seen_Apply and Seen_Preview_Window, "the layout is published by name");
   end;
   --  UI-033 re-flow: every page at its natural size and at the smallest
   --  window the layout allows fits; one pixel narrower or shorter does not.
   S.Open (View, A.Default, Layout, 1, Auto_Scale => True, Zone => Time_Zones.Zone_UTC);
   S.Measure (View, Least, Natural_Size);
   for Page in S.Page loop
      View.Current_Page := Page;
      Check_Page ((0, 0, Natural_Size.Width, Natural_Size.Height), Page'Image & " natural");
      Check_Page ((0, 0, Least.Width, Least.Height), Page'Image & " smallest");
      --  Wide: twice the natural size.
      Check_Page ((0, 0, 2 * Natural_Size.Width, 2 * Natural_Size.Height), Page'Image & " wide");
      if not S.Fits (View, (0, 0, Least.Width - 1, Least.Height)) then
         Too_Narrow := Too_Narrow + 1;
      end if;
      if not S.Fits (View, (0, 0, Least.Width, Least.Height - 1)) then
         Too_Short := Too_Short + 1;
      end if;
      Cases := Cases + 1;
   end loop;
   --  The minimum is the largest page's: some page does not fit in less.
   pragma Assert (Too_Narrow > 0, "the minimum width is tight" & Too_Narrow'Image & Too_Short'Image);
   --  At 2x density the layout is the same logical geometry (the canvas
   --  scales it): identical control places, and the page still fits.
   declare
      Double : Canvas := C;
      Place : constant Rect := S.Control_Bounds (View, (0, 0, Natural_Size.Width, Natural_Size.Height), S.Apply);
   begin
      Double.densityNumerator := 2;
      View.Current_Page := S.Appearance;
      Render (View, Double, (0, 0, Natural_Size.Width, Natural_Size.Height));
      pragma Assert (S.Control_Bounds (View, (0, 0, Natural_Size.Width, Natural_Size.Height), S.Apply) = Place);
   end;
   Ada.Text_IO.Put_Line ("PASS Settings layout (UI-033) and callback-only rendering:" & Cases'Image &
     " pages/styles/clips/sizes; minimum" & Least.Width'Image & " x" & Least.Height'Image &
     ", natural" & Natural_Size.Width'Image & " x" & Natural_Size.Height'Image);
end Renderer_Tests;
