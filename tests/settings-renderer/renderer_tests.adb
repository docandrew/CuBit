with Ada.Text_IO;
with System;
with CuBit.UI; use CuBit.UI;
with CuBit.Appearance;
with CuBit.Display_Layouts;
with Desktop_Settings;
with Time_Zones;
with CuBit.Output_Modes;
procedure Renderer_Tests is
   use type System.Address;
   package A renames CuBit.Appearance;
   type Operation is (Fill, Stroke, Gradient, Text, Button, Tab, Wallpaper);
   Seen : array (Operation) of Natural := [others => 0];
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
   procedure Draw_Text (C : Canvas; X, Y : Natural; Value : String; FG, BG : Color) is
   begin Record_Op (C, Text); end;
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
      pitch => 0, clipEnabled => True, clip => (100, 100, 744, 400), others => <>);
   Layout : CuBit.Display_Layouts.Layout;
   View : Desktop_Settings.State;
   Before : Natural;
begin
   Layout.Count := 2;
   Layout.Items (1) := (1, (1024, 768, others => <>));
   Layout.Items (2) := (2, (Width => 1280, Height => 720, X => 1024, others => <>));
   for Page in Desktop_Settings.Page loop
      for Scheme in A.Color_Scheme loop
         for Backdrop in A.Background loop
            for Placement in A.Placement loop
               for Clip in 1 .. 4 loop
                  C.clip := (case Clip is when 1 => (100, 100, 744, 400),
                    when 2 => (300, 180, 80, 40), when 3 => (0, 0, 1, 1), when others => (100, 100, 744, 400));
                  --  The fourth pass: a user-chosen (not automatic) scale.
                  Desktop_Settings.Open (View, (Scheme, Backdrop, Placement), Layout, 1, Auto_Scale => Clip /= 4,
                    Zone => Time_Zones.Zone_America_Denver);
                  View.Current_Page := Page;
                  Before := Calls;
                  Render (View, C, (100, 100, 744, 400));
                  pragma Assert (Calls > Before and Calls - Before < 100);
                  Before := Calls;
                  Render_Controls (View, C, (100, 100, 744, 400));
                  pragma Assert (Calls > Before and Calls - Before < 250);
                  Cases := Cases + 1;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   -- Exercise branches not selected by the initial Settings state, including
   -- empty/tiny controls and every interaction style. No pixel target exists.
   C.clip := (100, 100, 744, 400);
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

   --  Date & Time: the zone list by keyboard and pointer, Apply and Revert.
   declare
      use type Time_Zones.Time_Zone;
      use type Desktop_Settings.Page;
      use type Desktop_Settings.Control;
      package S renames Desktop_Settings;
      Window : constant Rect := (100, 100, 744, 400);
      Apply : Boolean;
      Scan_Enter : constant := 16#1C#;
      Scan_Down : constant := 16#50#;
      Scan_Page_Down : constant := 16#51#;
      Scan_Home : constant := 16#47#;
      Scan_End : constant := 16#4F#;
      Scan_E : constant := 16#12#;
      --  The list's first row in this window: page left 132, list at +20, +56.
      List_X : constant := 100 + 132 + 20 + 10;
      List_Row_Y : constant := 100 + 56 + 5;
      Row_Height : constant := 22;
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
      S.Pointer (View, Window, (List_X, List_Row_Y + 3 * Row_Height, True, True, False, True), Apply);
      pragma Assert (View.Pending_Zone = Time_Zones.Time_Zone'Val (3) and not Apply);
      Render (View, C, Window);
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
      use type Desktop_Settings.Control;
      use type Desktop_Settings.Trial_Response;
      package S renames Desktop_Settings;
      package OM renames CuBit.Output_Modes;
      Window : constant Rect := (100, 100, 744, 400);
      Apply : Boolean;
      Scan_Enter : constant := 16#1C#;
      Scan_Up : constant := 16#48#;
      Scan_Down : constant := 16#50#;
      Scan_Home : constant := 16#47#;
      Scan_End : constant := 16#4F#;
      --  The list's rows in this window: page left 132, list at +20, +76.
      List_X : constant := 100 + 132 + 20 + 10;
      List_Row_Y : constant := 100 + 76 + 5;
      Row_Height : constant := 20;
      Info : S.Monitor_Info;
      Count : constant := 14;
      function Mode (Width, Height : Positive; Rate : Natural; Verdict : OM.Admission := OM.Admitted)
        return OM.Listed_Mode is
        ((Size => (OM.OD.Extent (Width), OM.OD.Extent (Height)), Refresh_Millihertz => OM.Millihertz'Base (Rate),
          Pixel_kHz => 148_500, Verdict => Verdict, Preferred => False, Current => False));
   begin
      Info.Modes := new S.Monitor_Modes (1 .. Count);
      Info.Modes.all :=
        [Mode (4096, 2160, 60_000, OM.Above_Sink_Clock), Mode (3840, 2160, 60_000), Mode (3840, 2160, 50_000),
         Mode (3840, 2160, 30_000), Mode (2560, 1440, 59_951), Mode (1920, 1080, 120_000, OM.Needs_Higher_CD_Clock),
         Mode (1920, 1080, 60_000), Mode (1920, 1080, 59_940), Mode (1920, 1080, 50_000), Mode (1280, 720, 60_000),
         Mode (1024, 768, 60_000), Mode (800, 600, 60_317), Mode (720, 576, 50_000), Mode (640, 480, 59_940)];
      Info.Modes (2).Preferred := True;
      Info.Modes (7).Current := True;
      Info.Identified := True;
      Info.Saved_Auto := False;
      Info.Saved_Row := 7;
      S.Open (View, A.Default, Layout, 1, Auto_Scale => True, Zone => Time_Zones.Zone_UTC);
      View.Current_Page := S.Monitor;
      S.Set_Monitor (View, Info);
      pragma Assert (View.Mode_Choice = 7, "the saved mode is chosen");
      pragma Assert (View.Mode_Top <= 7 and then 7 < View.Mode_Top + 11, "and visible");
      Render (View, C, Window);
      Render_Controls (View, C, Window);
      View.Focused := S.Mode_List;
      S.Key (View, Scan_Up, False, Apply);
      pragma Assert (View.Mode_Choice = 5, "up skips the refused 1080p120 row");
      S.Key (View, Scan_Home, False, Apply);
      pragma Assert (View.Mode_Choice = S.Auto_Row and View.Mode_Top = S.Auto_Row);
      S.Key (View, Scan_Down, False, Apply);
      pragma Assert (View.Mode_Choice = 2, "down skips the refused 4096 x 2160 row");
      S.Key (View, Scan_End, False, Apply);
      pragma Assert (View.Mode_Choice = Count and View.Mode_Top = Count - 10, "the list follows the choice");
      --  A pointer on a refused row changes nothing; on an admitted row it chooses it.
      View.Focused := S.Monitor_Tab;
      S.Key (View, Scan_Home, False, Apply);
      View.Mode_Top := S.Auto_Row;
      View.Mode_Choice := 3;
      S.Pointer (View, Window, (List_X, List_Row_Y + 1 * Row_Height, True, True, False, True), Apply);
      pragma Assert (View.Mode_Choice = 3 and View.Focused = S.Mode_List, "a refused row is not chosen");
      S.Pointer (View, Window, (List_X, List_Row_Y + 4 * Row_Height, True, True, False, True), Apply);
      pragma Assert (View.Mode_Choice = 4 and not Apply);
      --  The Down button pages the list; Up comes back.
      View.Focused := S.Mode_Next;
      S.Key (View, Scan_Enter, False, Apply);
      pragma Assert (View.Mode_Top = Count - 10, "Down stops at the last page");
      View.Focused := S.Mode_Previous;
      S.Key (View, Scan_Enter, False, Apply);
      pragma Assert (View.Mode_Top = S.Auto_Row);
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
      Render (View, C, Window);
      Render_Controls (View, C, Window);
      S.Pointer (View, Window, (List_X, List_Row_Y + 2 * Row_Height, True, True, False, True), Apply);
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
   Ada.Text_IO.Put_Line ("PASS Settings callback-only rendering:" & Cases'Image &
     " pages/styles/clips, all seven operations plus toolkit control decomposition, null pixel target");
end Renderer_Tests;
