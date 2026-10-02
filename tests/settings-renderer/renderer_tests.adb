with Ada.Text_IO;
with System;
with CuBit.UI; use CuBit.UI;
with CuBit.Appearance;
with CuBit.Display_Layouts;
with Desktop_Settings;
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
               for Clip in 1 .. 3 loop
                  C.clip := (case Clip is when 1 => (100, 100, 744, 400),
                    when 2 => (300, 180, 80, 40), when others => (0, 0, 1, 1));
                  Desktop_Settings.Open (View, (Scheme, Backdrop, Placement), Layout, 1);
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
   Ada.Text_IO.Put_Line ("PASS Settings callback-only rendering:" & Cases'Image &
     " pages/styles/clips, all seven operations plus toolkit control decomposition, null pixel target");
end Renderer_Tests;
