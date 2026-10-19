--  The command table (docs/text-editor.md, "Commands and CCL"). Every
--  editor command is one row: its CCL name (text.goto-line), what it does,
--  the argument it takes, its keys in the default keymap, where it shows
--  (menu bar, palette, or keys only) and its menu caption. The menu bar,
--  the palette, the key help (F1) and the `text` CCL interface descriptor
--  all read this table, and keys, menu and palette choices and CCL calls
--  all end in one Execute. A host adds its own commands beside these
--  (Text_Hosts).
package Text_Commands with SPARK_Mode is
   type Command is
     (No_Command,
      --  Movement (Shift extends the selection: the Select_ forms).
      Move_Left, Move_Right, Move_Up, Move_Down, Move_Word_Left, Move_Word_Right, Move_Line_Home,
      Move_Line_End, Move_Page_Up, Move_Page_Down, Move_Document_Start, Move_Document_End,
      Select_Left, Select_Right, Select_Up, Select_Down, Select_Word_Left, Select_Word_Right, Select_Line_Home,
      Select_Line_End, Select_Page_Up, Select_Page_Down, Select_Document_Start, Select_Document_End,
      Scroll_Up, Scroll_Down,
      --  Selections.
      Select_All, Select_Line, Select_Word_Or_Next, Split_Into_Lines, Add_Cursor_Above, Add_Cursor_Below,
      Single_Selection, Collapse_Selection,
      --  Editing.
      New_Line, Delete_Backward, Delete_Forward, Delete_Word_Backward, Delete_Word_Forward, Delete_Line,
      Duplicate_Line, Indent, Outdent, Toggle_Comment, Undo, Redo, Copy, Cut, Paste, Insert_Text,
      --  Search.
      Find, Find_Next, Find_Previous, Replace, Replace_All, Select_All_Matches, Go_To_Line, Go_To_Anything,
      --  Files and tabs.
      New_Document, Open, Save, Save_As, Close_Document, Next_Tab, Previous_Tab,
      --  View.
      Split_View, Other_View, Toggle_Wrap, Toggle_Minimap, Toggle_Modal, Toggle_Overlay, Toggle_Theme, Set_Tab_Width,
      Command_Palette, Describe_Key, Quit);

   subtype Real_Command is Command range Move_Left .. Command'Last;

   type Argument_Kind is (No_Argument, Integer_Argument, String_Argument);
   type Category is (Movement, Selection, Editing, Search, Files, View);

   --  The CCL name without the "text." prefix ("goto-line").
   function Name (Item : Command) return String;
   --  "text." & Name.
   function Qualified_Name (Item : Command) return String;
   function Description (Item : Command) return String;
   function Argument_Of (Item : Command) return Argument_Kind;
   function Category_Of (Item : Command) return Category;
   --  The command named (with or without "text."), No_Command for none.
   function Named (Text : String) return Command;

   --  Keys, as the keymaps see them.
   type Key is
     (No_Key, Key_A, Key_B, Key_C, Key_D, Key_E, Key_F, Key_G, Key_H, Key_I, Key_J, Key_K, Key_L, Key_M, Key_N,
      Key_O, Key_P, Key_Q, Key_R, Key_S, Key_T, Key_U, Key_V, Key_W, Key_X, Key_Y, Key_Z,
      Key_0, Key_1, Key_2, Key_3, Key_4, Key_5, Key_6, Key_7, Key_8, Key_9,
      F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12,
      Up, Down, Left, Right, Home, End_Key, Page_Up, Page_Down, Enter, Escape, Backspace, Delete, Tab, Insert_Key,
      Space, Slash, Backslash, Minus, Equals, Period, Comma, Semicolon, Quote, Left_Bracket, Right_Bracket,
      Grave);
   type Chord is record
      Item : Key := No_Key;
      Control, Shift, Alt : Boolean := False;
   end record;
   NO_CHORD : constant Chord := (Item => No_Key, others => False);

   --  The default keymap (desktop conventions): a chord's command.
   function Lookup (Pressed : Chord) return Command;
   --  A command's first and second chord in the default keymap.
   function Binding (Item : Command) return Chord;
   function Second_Binding (Item : Command) return Chord;
   --  "Ctrl+Shift+P"; "" for no chord.
   function Chord_Text (Pressed : Chord) return String;
   function Key_Text (Item : Key) return String;

   --  The movement command and whether it extends, for the shared motion
   --  table.
   function Extends (Item : Command) return Boolean is (Item in Select_Left .. Select_Document_End);

   --  Where a command shows. Every command stays reachable by its keys
   --  and by CCL (text.left); Binding_Only ones (cursor motion, selection
   --  extension, scrolling, the editing keys) are too fine-grained to list.
   type Visibility is (Palette_And_Menu, Menu_Only, Binding_Only);
   function Visibility_Of (Item : Real_Command) return Visibility
     with Post =>
       --  Motions and the plain editing keys are keys, not choices.
       (if Category_Of (Item) = Movement then Visibility_Of'Result = Binding_Only)
       --  Nothing hidden is unreachable: a hidden command has a key, or is
       --  typing itself.
       and then (if Visibility_Of'Result = Binding_Only then Binding (Item) /= NO_CHORD or else Item = Insert_Text);
   function In_Palette (Item : Command) return Boolean is
     (Item in Real_Command and then Visibility_Of (Item) = Palette_And_Menu);
   function In_Menu (Item : Command) return Boolean is
     (Item in Real_Command and then Visibility_Of (Item) /= Binding_Only);

   --  The menu bar (docs/text-editor.md, "Layout"), left to right.
   type Menu_Name is (File_Menu, Edit_Menu, Selection_Menu, View_Menu, Go_Menu, Help_Menu);
   function Menu_Title (Item : Menu_Name) return String;
   --  The Alt+letter that opens it (its title's first letter, lower case).
   function Menu_Mnemonic (Item : Menu_Name) return Character;
   --  A menu's items come in groups, separated by a rule; within a group
   --  they keep the table's order.
   MAXIMUM_MENU_GROUPS : constant := 5;
   subtype Menu_Group is Positive range 1 .. MAXIMUM_MENU_GROUPS;
   function Menu_Of (Item : Command) return Menu_Name with Pre => In_Menu (Item);
   function Group_Of (Item : Command) return Menu_Group with Pre => In_Menu (Item);
   --  The menu caption ("Go to Line..."; "..." asks for more).
   function Caption (Item : Command) return String with Pre => In_Menu (Item);

   --  On/off settings: their menu items carry a checkmark.
   function Is_Toggle (Item : Command) return Boolean is
     (Item in Split_View | Toggle_Wrap | Toggle_Minimap | Toggle_Modal | Toggle_Overlay | Toggle_Theme);
   --  Commands that act on the shown document (refused, and their menu
   --  items disabled, without one).
   function Needs_Document (Item : Command) return Boolean is
     (Item not in New_Document | Open | Command_Palette | Toggle_Modal | Toggle_Overlay | Toggle_Theme
                | Describe_Key | Quit | No_Command | Go_To_Anything);
end Text_Commands;
