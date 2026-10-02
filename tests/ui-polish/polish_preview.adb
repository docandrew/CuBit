with Ada.Command_Line;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.Menus;
with CuBit.UI.Controls;
with CuBit.UI.Combo_Boxes;
procedure Polish_Preview is
   package IO renames Ada.Streams.Stream_IO;
   Width : constant := 1080;
   Height : constant := 820;
   Pixels : aliased array (0 .. Width * Height - 1) of Color := [others => 0];
   C : constant Canvas := (addr => Pixels'Address, width => Width, height => Height,
     pitch => Width * 4, others => <>);
   F : IO.File_Type;
   RGB : Stream_Element_Array (1 .. Width * Height * 3);
   At_Byte : Stream_Element_Offset := 1;
   File_T : aliased constant String := "File";
   Edit_T : aliased constant String := "Edit";
   View_T : aliased constant String := "View";
   M : CuBit.UI.Menus.Model;
   S : CuBit.UI.Menus.Menu_State;
   Map : CuBit.UI.Controls.Control_Map;
   Combo_Model : CuBit.UI.Combo_Boxes.Model;
   Closed_Combo, Open_Combo : CuBit.UI.Combo_Boxes.Combo_State;
   Default_Text : aliased constant String := "System default";
   Compact_Text : aliased constant String := "Compact";
   Comfortable_Text : aliased constant String := "Comfortable";
   Changed, Handled : Boolean;
   procedure Label (X, Y : Natural; Text : String; Colors : Theme) is
   begin Draw_UI_Text (C, X, Y, Text, Colors.muted, Colors.panel); end Label;
begin
   Combo_Model.Count := 3;
   Combo_Model.Choices (1) := (Default_Text'Unchecked_Access, True);
   Combo_Model.Choices (2) := (Compact_Text'Unchecked_Access, True);
   Combo_Model.Choices (3) := (Comfortable_Text'Unchecked_Access, True);
   CuBit.UI.Combo_Boxes.Set_Selection (Closed_Combo, Combo_Model, 1);
   CuBit.UI.Combo_Boxes.Set_Selection (Open_Combo, Combo_Model, 2);
   CuBit.UI.Combo_Boxes.Handle_Key (Open_Combo, Combo_Model,
     CuBit.UI.Combo_Boxes.Toggle, Changed, Handled);
   M.Menu_Count := 3;
   M.Menus (1) := (File_T'Unchecked_Access, 'f');
   M.Menus (2) := (Edit_T'Unchecked_Access, 'e');
   M.Menus (3) := (View_T'Unchecked_Access, 'v');
   for Dark in Boolean loop
      declare
         X : constant Natural := (if Dark then 540 else 0);
         T : constant Theme := (if Dark then CuBit_Alloy_Dark else CuBit_Alloy);
      begin
         Fill_Rect (C, (X, 0, 540, Height), T.panel);
         Draw_UI_Text (C, X + 24, 20,
           (if Dark then "CuBit native widgets / dark" else "CuBit native widgets / light"), T.text, T.panel);
         CuBit.UI.Controls.Clear (Map);
         CuBit.UI.Menus.Draw (C, Map, S, M, 500, (X + 24, 52, 492, 28), T);
         Label (X + 24, 94, "Buttons", T);
         for I in 0 .. 3 loop
            Draw_Button (C, (X + 24 + I * 124, 116, 116, 32), T,
              Button_Style'Val (I),
              (case I is when 0 => "Normal", when 1 => "Hover",
                when 2 => "Pressed", when others => "Disabled"));
         end loop;
         Label (X + 24, 164, "Text fields", T);
         Draw_Text_Edit_Field (C, (X + 24, 186, 238, 32), T, "Search documents", 16, 0, 0, False, False);
         Draw_Text_Edit_Field (C, (X + 278, 186, 238, 32), T, "Selected text", 13, 0, 8, True, False);
         Draw_Checkbox (C, (X + 24, 237, 20, 20), T, True, False, False);
         Draw_UI_Text (C, X + 52, 239, "Remember layout", T.text, T.panel);
         Draw_Radio_Button (C, (X + 278, 233, 238, 28), T, True, False, False, "Use system font");
         Draw_Tab_Strip (C, (X + 24, 280, 492, 34), T);
         Draw_Tab (C, (X + 24, 280, 156, 34), T, True, False, False, "General");
         Draw_Tab (C, (X + 184, 280, 156, 34), T, False, True, False, "Appearance");
         Draw_Tab (C, (X + 344, 280, 156, 34), T, False, False, False, "Advanced");
         Draw_Table_Viewport (C, (X + 22, 326, 496, 94), T);
         Draw_List_Item (C, (X + 24, 328, 472, 30), T, False, False, "Documents");
         Draw_List_Item (C, (X + 24, 358, 472, 30), T, True, False, "Downloads");
         Draw_List_Item (C, (X + 24, 388, 472, 30), T, False, True, "Bookmarks");
         Draw_Vertical_Scrollbar (C, (X + 500, 328, 16, 90), T, 1, 30, 3, False, False, 3);
         Label (X + 24, 434, "Progress", T);
         Label (X + 278, 434, "Scale", T);
         Draw_Progress_Bar (C, (X + 24, 460, 238, 12), T, 0, 100, 62);
         Draw_Horizontal_Slider (C, (X + 278, 450, 238, 28), T, 0, 100, 60, False, False);
         Draw_Pane (C, (X + 24, 496, 492, 102), T, "Details");
         Draw_Multiline_Text_Edit (C, (X + 36, 524, 468, 60), T,
           "Clean spacing. Consistent borders." & ASCII.LF & "Simple, opaque drawing operations.",
           1, 2, 1, 1, 1, False, False);
         Label (X + 24, 620, "Combo boxes", T);
         CuBit.UI.Combo_Boxes.Draw (C, Map, Closed_Combo, Combo_Model, 800,
           (X + 24, 646, 238, CuBit.UI.Combo_Boxes.Default_Height), T, Focused => True);
         CuBit.UI.Combo_Boxes.Draw (C, Map, Open_Combo, Combo_Model, 900,
           (X + 278, 646, 238, CuBit.UI.Combo_Boxes.Default_Height), T, Focused => True);
         Draw_Status_Bar (C, (X + 24, 778, 492, 26), T, "Ready", "100%");
      end;
   end loop;
   for P of Pixels loop
      RGB (At_Byte) := Stream_Element (Shift_Right (P, 16) and 255);
      RGB (At_Byte + 1) := Stream_Element (Shift_Right (P, 8) and 255);
      RGB (At_Byte + 2) := Stream_Element (P and 255);
      At_Byte := At_Byte + 3;
   end loop;
   IO.Create (F, IO.Out_File, Ada.Command_Line.Argument (1));
   String'Write (IO.Stream (F), "P6" & ASCII.LF & "1080 820" & ASCII.LF & "255" & ASCII.LF);
   IO.Write (F, RGB); IO.Close (F);
end Polish_Preview;
