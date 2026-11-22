with Ada.Text_IO; use Ada.Text_IO;
with CuBit.UI; use CuBit.UI;
with Client_Canvas_Geometry;
with CuBit.UI.Widgets;
procedure Polish_Tests is
   use type Color;
   Fills, Strokes, Area : Natural := 0;
   type Surface is array (0 .. 139, 0 .. 199) of Color;
   Sentinel : constant Color := 16#A153CF#;
   Pixels : aliased Surface := [others => [others => Sentinel]];
   Reference : Surface;
   C : Canvas := (addr => Pixels'Address, width => 96, height => 64,
     pitch => 800, others => <>);
   subtype Kind is Natural range 0 .. 19;
   procedure Paint (K : Kind; Target : Canvas; R : Rect; T : Theme) is
   begin
      case K is
         when 19 => Draw_Table_Viewport (Target, R, T);
         when 0 => Draw_Button (Target, R, T, Button_Normal, "Long button caption");
         when 1 => Draw_Tab (Target, R, T, True, False, False, "Long tab caption");
         when 2 => Draw_Status_Bar (Target, R, T, "Long status caption", "Right caption");
         when 3 => Draw_Menu_Bar (Target, R, T);
         when 4 => Draw_Menu_Title (Target, R, T, True, True, "Long menu title");
         when 5 => Draw_Pane (Target, R, T, "Long group title");
         when 6 => Draw_Checkbox (Target, R, T, True, True, False);
         when 7 => Draw_Radio_Button (Target, R, T, True, True, False, "Long radio caption");
         when 8 => Draw_Text_Field (Target, R, T, "Very long field", True, False);
         when 9 => Draw_Text_Edit_Field (Target, R, T, "Long selected field", 19, 0, 8, True, False);
         when 10 => Draw_Swatch (Target, R, T, T.accent, "Long swatch caption");
         when 11 => Draw_Menu_Item (Target, R, T, True, False, True, "Long item caption");
         when 12 => Draw_List_Item (Target, R, T, True, False, "Long list caption");
         when 13 => Draw_Progress_Bar (Target, R, T, 0, 100, 70);
         when 14 => Draw_Horizontal_Slider (Target, R, T, 0, 100, 70, False, False);
         when 15 => Draw_Vertical_Scrollbar (Target, R, T, 1, 100, 20, False, False, 10);
         when 17 => Draw_Table_Header (Target, R, T, "Long column", "State", "Size");
         when 18 => Draw_Table_Row (Target, R, T, True, False, "Long cell", "Ready", "100");
         when 16 => Draw_Horizontal_Scrollbar (Target, R, T, 1, 100, 20, False, False, 10);
      end case;
   end Paint;
   procedure Count_Fill (C : Canvas; R : Rect; Fill : Color) is
      pragma Unreferenced (C, Fill);
   begin Fills := Fills + 1; Area := Area + R.w * R.h; end Count_Fill;
   procedure Count_Stroke (C : Canvas; R : Rect; Light, Dark : Color) is
      pragma Unreferenced (C, Light, Dark);
   begin Strokes := Strokes + 1; Area := Area + 2 * R.w + 2 * R.h; end Count_Stroke;
   procedure Text (C : Canvas; X, Y : Natural; Text : String; FG, BG : Color) is
      pragma Unreferenced (C, X, Y, Text, FG, BG);
   begin null; end Text;
   package Renderer is new Control_Renderer (Count_Fill, Count_Stroke, Text);
begin
   Renderer.Draw_Button_Frame ((others => <>), (0, 0, 116, 32), CuBit_Alloy, Button_Normal);
   Put_Line ("button fills=" & Fills'Image & " strokes=" & Strokes'Image & " logical pixel writes=" & Area'Image);
   pragma Assert (Fills = 3 and Strokes = 2 and Area <= 4750);
   for N in 4 .. 8 loop
      C.densityNumerator := N; C.densityDenominator := 4;
      for Dark in Boolean loop
         for K in Kind loop
            declare
               T : constant Theme := (if Dark then CuBit_Alloy_Dark else CuBit_Alloy);
               R : constant Rect := (10, 10, 60, 30);
               Clip : constant Rect := (19, 13, 13, 11);
               function Scale (X : Natural) return Natural is
                 (Client_Canvas_Geometry.Relative (0, X, N, 4));
            begin
               Pixels := [others => [others => Sentinel]];
               Paint (K, C, R, T); Reference := Pixels;
               for Y in Pixels'Range (1) loop
                  for X in Pixels'Range (2) loop
                     if X < Scale (R.x) or X >= Scale (R.x + R.w) or
                       Y < Scale (R.y) or Y >= Scale (R.y + R.h)
                     then pragma Assert (Pixels (Y, X) = Sentinel); end if;
                  end loop;
               end loop;
               Pixels := [others => [others => Sentinel]];
               Paint (K, With_Clip (C, Clip), R, T);
               for Y in Pixels'Range (1) loop
                  for X in Pixels'Range (2) loop
                     pragma Assert (Pixels (Y, X) =
                       (if X >= Scale (Clip.x) and X < Scale (Clip.x + Clip.w) and
                         Y >= Scale (Clip.y) and Y < Scale (Clip.y + Clip.h)
                        then Reference (Y, X) else Sentinel));
                  end loop;
               end loop;
            end;
         end loop;
      end loop;
   end loop;
   C.densityNumerator := 1; C.densityDenominator := 1;
   for K in Kind loop
      for W in 0 .. 12 loop
         for H in 0 .. 12 loop
            Pixels := [others => [others => Sentinel]];
            Paint (K, C, (10, 10, W, H), CuBit_Alloy);
            for Y in 0 .. 63 loop
               for X in 0 .. 95 loop
                  if not Point_In_Rect (X, Y, (10, 10, W, H)) then
                     pragma Assert (Pixels (Y, X) = Sentinel);
                  end if;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   -- Long status and key labels must not repaint their neighbouring value lane.
   C.width := 192;
   Pixels := [others => [others => Sentinel]];
   Draw_Status_Bar (C, (10, 10, 170, 30), CuBit_Alloy, "", "100%");
   Reference := Pixels;
   Draw_Status_Bar (C, (10, 10, 170, 30), CuBit_Alloy,
     "A very long status that must remain in the left lane", "100%");
   for Y in 10 .. 39 loop
      for X in 180 - 16 - UI_Text_Width ("100%") .. 179 loop
         pragma Assert (Pixels (Y, X) = Reference (Y, X));
      end loop;
   end loop;
   Pixels := [others => [others => Sentinel]];
   CuBit.UI.Widgets.Key_Value (C, (10, 10, 170, 30), CuBit_Alloy, "", "Value");
   Reference := Pixels;
   CuBit.UI.Widgets.Key_Value (C, (10, 10, 170, 30), CuBit_Alloy,
     "A long key must not overlap its value", "Value");
   for Y in 10 .. 39 loop
      for X in 103 .. 179 loop
         pragma Assert (Pixels (Y, X) = Reference (Y, X));
      end loop;
   end loop;
   Put_Line ("UI polish: 200 widget/theme/density clips + 3380 tiny/empty bounds PASS");
end Polish_Tests;
