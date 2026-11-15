pragma Ada_2022;
with CuBit.UI_Metrics;

package body Dash_Commands is
   package Menus renames CuBit.UI.Menus;
   use type Menus.Text;

   --  A popup is never narrower than this (short menus look cramped).
   Minimum_Popup_Width : constant := 14 * CuBit.UI_Metrics.Base_Em;

   type Menu_Name is (File_Menu, Edit_Menu, View_Menu);

   File_Title : aliased constant String := "File";
   Edit_Title : aliased constant String := "Edit";
   View_Title : aliased constant String := "View";
   Titles : constant array (Menu_Name) of Menus.Text :=
     [File_Title'Access, Edit_Title'Access, View_Title'Access];
   Title_Letters : constant array (Menu_Name) of Character := ['f', 'e', 'v'];

   --  The table: each command's menu, caption, shortcut and mnemonic, and
   --  whether a separator comes before it.
   type Entry_Text is record
      Menu : Menu_Name := File_Menu;
      Caption, Shortcut : Menus.Text := null;
      Letter : Character := ' ';
      Starts_Group : Boolean := False;
   end record;

   C_New : aliased constant String := "New Dashboard";
   C_Open : aliased constant String := "Open...";
   C_Save_As : aliased constant String := "Save As...";
   C_Duplicate_Board : aliased constant String := "Duplicate Dashboard";
   C_Undo : aliased constant String := "Undo";
   C_Redo : aliased constant String := "Redo";
   C_Editor : aliased constant String := "Edit Layout";
   C_Add : aliased constant String := "Add Panel";
   C_Source : aliased constant String := "Panel Source...";
   C_Duplicate : aliased constant String := "Duplicate Panel";
   C_Remove : aliased constant String := "Remove Panel";
   C_Copy : aliased constant String := "Copy as CCL";
   C_Zoom_In : aliased constant String := "Zoom In";
   C_Zoom_Out : aliased constant String := "Zoom Out";
   C_Range_1 : aliased constant String := "Last Minute";
   C_Range_2 : aliased constant String := "Last 5 Minutes";
   C_Range_3 : aliased constant String := "Last 15 Minutes";
   C_Range_4 : aliased constant String := "Last Hour";
   C_Range_5 : aliased constant String := "Last 6 Hours";
   C_Range_6 : aliased constant String := "Last 24 Hours";
   C_Live : aliased constant String := "Live";
   C_Show_Source : aliased constant String := "Show CCL";
   C_Keys : aliased constant String := "Keys";
   C_No_Recent : aliased constant String := "No Recent Dashboards";

   S_New : aliased constant String := "Ctrl+N";
   S_Open : aliased constant String := "Ctrl+O";
   S_Save_As : aliased constant String := "Ctrl+Shift+S";
   S_Undo : aliased constant String := "Ctrl+Z";
   S_Redo : aliased constant String := "Ctrl+Y";
   S_Editor : aliased constant String := "E";
   S_Add : aliased constant String := "A";
   S_Source : aliased constant String := "Enter";
   S_Duplicate : aliased constant String := "Insert";
   S_Remove : aliased constant String := "Delete";
   S_Copy : aliased constant String := "Ctrl+C";
   S_Zoom_In : aliased constant String := "=";
   S_Zoom_Out : aliased constant String := "-";
   S_Range_1 : aliased constant String := "1";
   S_Range_2 : aliased constant String := "2";
   S_Range_3 : aliased constant String := "3";
   S_Range_4 : aliased constant String := "4";
   S_Range_5 : aliased constant String := "5";
   S_Range_6 : aliased constant String := "6";
   S_Live : aliased constant String := "L";
   S_Show_Source : aliased constant String := "C";
   S_Keys : aliased constant String := "F1";

   Table : constant array (Command range New_Dashboard .. Show_Keys) of Entry_Text :=
     [New_Dashboard => (File_Menu, C_New'Access, S_New'Access, 'n', False),
      Open_Dashboard => (File_Menu, C_Open'Access, S_Open'Access, 'o', False),
      Save_As => (File_Menu, C_Save_As'Access, S_Save_As'Access, 'a', False),
      Duplicate_Dashboard => (File_Menu, C_Duplicate_Board'Access, null, 'd', False),
      Undo => (Edit_Menu, C_Undo'Access, S_Undo'Access, 'u', False),
      Redo => (Edit_Menu, C_Redo'Access, S_Redo'Access, 'r', False),
      Toggle_Editor => (Edit_Menu, C_Editor'Access, S_Editor'Access, 'e', True),
      Add_Panel => (Edit_Menu, C_Add'Access, S_Add'Access, 'a', False),
      Panel_Source => (Edit_Menu, C_Source'Access, S_Source'Access, 's', False),
      Duplicate_Panel => (Edit_Menu, C_Duplicate'Access, S_Duplicate'Access, 'd', False),
      Remove_Panel => (Edit_Menu, C_Remove'Access, S_Remove'Access, 'm', False),
      Copy_Source => (Edit_Menu, C_Copy'Access, S_Copy'Access, 'c', True),
      Zoom_In => (View_Menu, C_Zoom_In'Access, S_Zoom_In'Access, 'i', False),
      Zoom_Out => (View_Menu, C_Zoom_Out'Access, S_Zoom_Out'Access, 'o', False),
      Range_1 => (View_Menu, C_Range_1'Access, S_Range_1'Access, 'm', True),
      Range_2 => (View_Menu, C_Range_2'Access, S_Range_2'Access, '5', False),
      Range_3 => (View_Menu, C_Range_3'Access, S_Range_3'Access, 'u', False),
      Range_4 => (View_Menu, C_Range_4'Access, S_Range_4'Access, 'h', False),
      Range_5 => (View_Menu, C_Range_5'Access, S_Range_5'Access, '6', False),
      Range_6 => (View_Menu, C_Range_6'Access, S_Range_6'Access, '2', False),
      Go_Live => (View_Menu, C_Live'Access, S_Live'Access, 'l', False),
      Show_Source => (View_Menu, C_Show_Source'Access, S_Show_Source'Access, 'c', True),
      Show_Keys => (View_Menu, C_Keys'Access, S_Keys'Access, 'k', False)];

   --  Menu item commands: the table's by position (from 1), the recent
   --  dashboards' after them.
   Recent_Base : constant := Command'Pos (Command'Last) + 1;
   function Recent_Command (Index : Recent_Index) return Positive is (Recent_Base + Index - 1);

   procedure Decode (Item : Natural; Which : out Command; Recent : out Recent_Count) is
   begin
      Which := No_Command;
      Recent := 0;
      if Item in Command'Pos (New_Dashboard) .. Command'Pos (Command'Last) then
         Which := Command'Val (Item);
      elsif Item in Recent_Base .. Recent_Base + Maximum_Recent - 1 then
         Recent := Item - Recent_Base + 1;
      end if;
   end Decode;

   function Enabled (S : State; C : Command) return Boolean is
     (case C is
        when Open_Dashboard | Save_As => S.Folder_Ready,
        when Undo => S.Can_Undo,
        when Redo => S.Can_Redo,
        when Add_Panel => S.Can_Add,
        when Duplicate_Panel => S.Editing and then S.Has_Selection and then S.Can_Add,
        when Remove_Panel => S.Editing and then S.Has_Selection,
        when others => True);

   function Checked (S : State; C : Command) return Boolean is
     (case C is
        when Toggle_Editor => S.Editing,
        when Range_Command => S.Shown_Range = Command'Pos (C) - Command'Pos (Range_1) + 1,
        when Go_Live => S.Live,
        when Show_Source => S.Source_Shown,
        when Show_Keys => S.Keys_Shown,
        when others => False);

   function Menu_Bar (S : State) return Menus.Model is
      Result : Menus.Model;
      procedure Add (Item : Menus.Item) is
      begin
         if Result.Item_Count < Menus.Max_Items then
            Result.Item_Count := Result.Item_Count + 1;
            Result.Items (Result.Item_Count) := Item;
         end if;
      end Add;
      procedure Separator (Parent : Positive) is
      begin
         Add ((Parent => Parent, Separator => True, others => <>));
      end Separator;
   begin
      for M in Menu_Name loop
         Result.Menu_Count := Result.Menu_Count + 1;
         Result.Menus (Result.Menu_Count) := (Caption => Titles (M), Mnemonic => Title_Letters (M));
         for C in Table'Range loop
            if Table (C).Menu = M then
               if Table (C).Starts_Group then
                  Separator (Result.Menu_Count);
               end if;
               Add ((Parent => Result.Menu_Count, Caption => Table (C).Caption, Shortcut => Table (C).Shortcut,
                     Mnemonic => Table (C).Letter, Command => Command'Pos (C), Enabled => Enabled (S, C),
                     Checked => Checked (S, C), Separator => False));
            end if;
         end loop;
         if M = File_Menu then
            Separator (Result.Menu_Count);
            if S.Recent_Used = 0 or else not S.Folder_Ready then
               Add ((Parent => Result.Menu_Count, Caption => C_No_Recent'Access, Command => Recent_Command (1),
                     Enabled => False, others => <>));
            else
               for R in 1 .. S.Recent_Used loop
                  Add ((Parent => Result.Menu_Count, Caption => S.Recent (R), Command => Recent_Command (R),
                        Mnemonic => Character'Val (Character'Pos ('0') + R), Enabled => S.Recent (R) /= null,
                        others => <>));
               end loop;
            end if;
         end if;
      end loop;
      return Result;
   end Menu_Bar;

   function Popup_Width (Model : Menus.Model) return Positive is
     (Positive'Max (Minimum_Popup_Width, Menus.Required_Width (Model)));
end Dash_Commands;
