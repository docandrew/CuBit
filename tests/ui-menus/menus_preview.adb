with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.Controls;
with CuBit.UI.Menus; use CuBit.UI.Menus;
procedure Menus_Preview is
   package IO renames Ada.Streams.Stream_IO;
   Pixels : aliased array (0 .. 840 * 320 - 1) of Color := [others => 0];
   C : constant Canvas := (addr => Pixels'Address, width => 840, height => 320,
     pitch => 840 * 4, others => <>);
   D : Model;
   S : Menu_State;
   Map : CuBit.UI.Controls.Control_Map;
   Command : Natural;
   Handled : Boolean;
   File_T : aliased constant String := "File";
   Edit_T : aliased constant String := "Edit";
   View_T : aliased constant String := "View";
   Help_T : aliased constant String := "Help";
   New_Tab_T : aliased constant String := "New tab";
   New_Window_T : aliased constant String := "New window";
   Save_T : aliased constant String := "Save page";
   Settings_T : aliased constant String := "Settings...";
   Close_T : aliased constant String := "Close window";
   Tabs_T : aliased constant String := "Restore tabs";
   Ctrl_T : aliased constant String := "Ctrl+T";
   Ctrl_N : aliased constant String := "Ctrl+N";
   Ctrl_S : aliased constant String := "Ctrl+S";
   Ctrl_W : aliased constant String := "Ctrl+Shift+W";
   F : IO.File_Type;
   RGB : Stream_Element_Array (1 .. 840 * 320 * 3);
   At_Byte : Stream_Element_Offset := 1;
begin
   D.Menu_Count := 4; D.Item_Count := 8;
   D.Menus (1) := (File_T'Unchecked_Access, 'F');
   D.Menus (2) := (Edit_T'Unchecked_Access, 'E');
   D.Menus (3) := (View_T'Unchecked_Access, 'V');
   D.Menus (4) := (Help_T'Unchecked_Access, 'H');
   D.Items (1) := (Caption => New_Tab_T'Unchecked_Access,
      Shortcut => Ctrl_T'Unchecked_Access, Command => 1, others => <>);
   D.Items (2) := (Caption => New_Window_T'Unchecked_Access,
      Shortcut => Ctrl_N'Unchecked_Access, Command => 2, others => <>);
   D.Items (3) := (Separator => True, others => <>);
   D.Items (4) := (Caption => Save_T'Unchecked_Access,
      Shortcut => Ctrl_S'Unchecked_Access, Command => 3, Enabled => False,
      others => <>);
   D.Items (5) := (Caption => Tabs_T'Unchecked_Access, Checked => True,
      Command => 4, others => <>);
   D.Items (6) := (Caption => Settings_T'Unchecked_Access, Command => 5, others => <>);
   D.Items (7) := (Separator => True, others => <>);
   D.Items (8) := (Caption => Close_T'Unchecked_Access,
      Shortcut => Ctrl_W'Unchecked_Access, Command => 6, others => <>);
   D.Items (1).Mnemonic := 't';
   D.Items (2).Mnemonic := 'w';
   D.Items (4).Mnemonic := 's';
   D.Items (5).Mnemonic := 'r';
   D.Items (6).Mnemonic := 'e';
   D.Items (8).Mnemonic := 'c';
   Handle_Key (S, D, Activate, Command, Handled);
   Handle_Key (S, D, Down, Command, Handled);
   Handle_Key (S, D, Down, Command, Handled);
   for Dark in Boolean loop
      declare
         X : constant Natural := (if Dark then 420 else 0);
         Colors : constant Theme := (if Dark then CuBit_Alloy_Dark else CuBit_Alloy);
         PC : constant Canvas := With_Clip (C, (X, 0, 420, 320));
      begin
         Fill_Rect (PC, (X, 0, 420, 320), Colors.face);
         Draw_UI_Text (PC, X + 20, 16,
           (if Dark then "Native menubar - dark" else "Native menubar - light"),
           Colors.text, Colors.face);
         CuBit.UI.Controls.Clear (Map);
         Draw (PC, Map, S, D, 1, (X + 12, 48, 396, 28), Colors, 320);
      end;
   end loop;
   for Pixel of Pixels loop
      RGB (At_Byte) := Stream_Element (Shift_Right (Pixel, 16) and 255);
      RGB (At_Byte + 1) := Stream_Element (Shift_Right (Pixel, 8) and 255);
      RGB (At_Byte + 2) := Stream_Element (Pixel and 255);
      At_Byte := At_Byte + 3;
   end loop;
   IO.Create (F, IO.Out_File, "/tmp/cubit-native-menus-preview.ppm");
   String'Write (IO.Stream (F), "P6" & ASCII.LF & "840 320" & ASCII.LF & "255" & ASCII.LF);
   IO.Write (F, RGB); IO.Close (F);
end Menus_Preview;
