with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.Widgets;
with CuBit.UI.State;
with CuBit.UI.Controls;
procedure Tab_Style_Preview is
   package IO renames Ada.Streams.Stream_IO;
   package W renames CuBit.UI.Widgets;
   Pixels : aliased array (0 .. 720 * 420 - 1) of Color := [others => 0];
   C : constant Canvas := (addr => Pixels'Address, width => 720, height => 420,
     pitch => 720 * 4, others => <>);
   St : CuBit.UI.State.UI_State;
   Map : CuBit.UI.Controls.Control_Map;
   Child : Canvas;
   Child_Colors : Theme;
   Result : Widget_Result;
   F : IO.File_Type;
   RGB : Stream_Element_Array (1 .. 720 * 420 * 3);
   At_Byte : Stream_Element_Offset := 1;
begin
   for Dark in Boolean loop
      declare
         Colors : constant Theme := (if Dark then CuBit_Alloy_Dark else CuBit_Alloy);
         Y : constant Natural := (if Dark then 210 else 0);
      begin
         Fill_Rect (C, (0, Y, 720, 210), Colors.face);
         W.Label (C, (20, Y + 12, 600, 24), Colors,
           (if Dark then "Native controls - dark" else "Native controls - light"));
         for Style in Button_Style loop
            Draw_Button (C, (20 + Button_Style'Pos (Style) * 110, Y + 44, 98, 30),
              Colors, Style, (case Style is when Button_Normal => "Normal",
                when Button_Hot => "Hover", when Button_Pressed => "Pressed",
                when Button_Disabled => "Disabled", when Button_Active => "Active"));
         end loop;
         CuBit.UI.Controls.Clear (Map);
         W.Tab (C, St, Map, 1, (20, Y + 94, 220, 32), (0, Y, 720, 210),
           Colors, True, Child, Child_Colors, Result);
         W.Label (Child, (32, Y + 96, 176, 28), Child_Colors, "Documentation");
         W.Button (Child, St, Map, 2, (216, Y + 98, 20, 24), Child.clip,
           Child_Colors, "x", Result, retainedInput => True, quiet => True);
         W.Tab (C, St, Map, 3, (244, Y + 94, 220, 32), (0, Y, 720, 210),
           Colors, False, Child, Child_Colors, Result);
         W.Label (Child, (256, Y + 96, 176, 28), Child_Colors, "Release notes");
         W.Button (Child, St, Map, 4, (440, Y + 98, 20, 24), Child.clip,
           Child_Colors, "x", Result, retainedInput => True, quiet => True);
         W.Navigation_Button (C, St, Map, 7, (244, Y + 156, 72, 26),
           (0, Y, 720, 210), Colors, W.Navigate_Back, "Back", True, Result);
         W.Navigation_Button (C, St, Map, 8, (324, Y + 156, 84, 26),
           (0, Y, 720, 210), Colors, W.Navigate_Forward, "Forward", False, Result);
         W.Tab (C, St, Map, 5, (20, Y + 154, 176, 32), (0, Y, 720, 210),
           Colors, True, Child, Child_Colors, Result, Vertical);
         W.Label (Child, (32, Y + 156, 130, 28), Child_Colors, "Vertical tab");
         W.Button (Child, St, Map, 6, (172, Y + 158, 20, 24), Child.clip,
           Child_Colors, "x", Result, retainedInput => True, quiet => True);
      end;
   end loop;
   for Pixel of Pixels loop
      RGB (At_Byte) := Stream_Element (Shift_Right (Pixel, 16) and 255);
      RGB (At_Byte + 1) := Stream_Element (Shift_Right (Pixel, 8) and 255);
      RGB (At_Byte + 2) := Stream_Element (Pixel and 255);
      At_Byte := At_Byte + 3;
   end loop;
   IO.Create (F, IO.Out_File, "/tmp/cubit-tab-style-preview.ppm");
   String'Write (IO.Stream (F), "P6" & ASCII.LF & "720 420" & ASCII.LF & "255" & ASCII.LF);
   IO.Write (F, RGB); IO.Close (F);
end Tab_Style_Preview;
