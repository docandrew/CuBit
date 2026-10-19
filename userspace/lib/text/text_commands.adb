package body Text_Commands with SPARK_Mode is
   function Name (Item : Command) return String is
     (case Item is
         when No_Command => "",
         when Move_Left => "left", when Move_Right => "right", when Move_Up => "up", when Move_Down => "down",
         when Move_Word_Left => "word-left", when Move_Word_Right => "word-right",
         when Move_Line_Home => "line-home", when Move_Line_End => "line-end",
         when Move_Page_Up => "page-up", when Move_Page_Down => "page-down",
         when Move_Document_Start => "document-start", when Move_Document_End => "document-end",
         when Select_Left => "select-left", when Select_Right => "select-right", when Select_Up => "select-up",
         when Select_Down => "select-down", when Select_Word_Left => "select-word-left",
         when Select_Word_Right => "select-word-right", when Select_Line_Home => "select-line-home",
         when Select_Line_End => "select-line-end", when Select_Page_Up => "select-page-up",
         when Select_Page_Down => "select-page-down", when Select_Document_Start => "select-document-start",
         when Select_Document_End => "select-document-end",
         when Scroll_Up => "scroll-up", when Scroll_Down => "scroll-down",
         when Select_All => "select-all", when Select_Line => "select-line",
         when Select_Word_Or_Next => "select-next", when Split_Into_Lines => "split-into-lines",
         when Add_Cursor_Above => "add-cursor-above", when Add_Cursor_Below => "add-cursor-below",
         when Single_Selection => "single-selection", when Collapse_Selection => "collapse-selection",
         when New_Line => "new-line", when Delete_Backward => "delete-backward",
         when Delete_Forward => "delete-forward", when Delete_Word_Backward => "delete-word-backward",
         when Delete_Word_Forward => "delete-word-forward", when Delete_Line => "delete-line",
         when Duplicate_Line => "duplicate-line", when Indent => "indent", when Outdent => "outdent",
         when Toggle_Comment => "toggle-comment", when Undo => "undo", when Redo => "redo",
         when Copy => "copy", when Cut => "cut", when Paste => "paste", when Insert_Text => "insert",
         when Find => "find", when Find_Next => "find-next", when Find_Previous => "find-previous",
         when Replace => "replace", when Replace_All => "replace-all",
         when Select_All_Matches => "select-all-matches", when Go_To_Line => "goto-line",
         when Go_To_Anything => "goto-anything",
         when New_Document => "new", when Open => "open", when Save => "save", when Save_As => "save-as",
         when Close_Document => "close", when Next_Tab => "next-tab", when Previous_Tab => "previous-tab",
         when Split_View => "split", when Other_View => "other-view", when Toggle_Wrap => "toggle-wrap", when Toggle_Minimap => "toggle-minimap",
         when Toggle_Modal => "toggle-modal", when Toggle_Overlay => "toggle-overlay",
         when Toggle_Theme => "toggle-theme", when Set_Tab_Width => "tab-width",
         when Command_Palette => "palette", when Describe_Key => "describe-key", when Quit => "quit");

   function Qualified_Name (Item : Command) return String is ("text." & Name (Item));

   function Description (Item : Command) return String is
     (case Item is
         when No_Command => "",
         when Move_Left => "Move left one character", when Move_Right => "Move right one character",
         when Move_Up => "Move up one row", when Move_Down => "Move down one row",
         when Move_Word_Left => "Move to the previous word", when Move_Word_Right => "Move to the next word",
         when Move_Line_Home => "Move to the first non-blank, then the line start",
         when Move_Line_End => "Move to the line end", when Move_Page_Up => "Move up one page",
         when Move_Page_Down => "Move down one page", when Move_Document_Start => "Move to the document start",
         when Move_Document_End => "Move to the document end",
         when Select_Left => "Extend the selection left", when Select_Right => "Extend the selection right",
         when Select_Up => "Extend the selection up", when Select_Down => "Extend the selection down",
         when Select_Word_Left => "Extend the selection to the previous word",
         when Select_Word_Right => "Extend the selection to the next word",
         when Select_Line_Home => "Extend the selection to the line start",
         when Select_Line_End => "Extend the selection to the line end",
         when Select_Page_Up => "Extend the selection up one page",
         when Select_Page_Down => "Extend the selection down one page",
         when Select_Document_Start => "Extend the selection to the document start",
         when Select_Document_End => "Extend the selection to the document end",
         when Scroll_Up => "Scroll up a row, cursors stay", when Scroll_Down => "Scroll down a row, cursors stay",
         when Select_All => "Select the whole document", when Select_Line => "Select the cursors' lines",
         when Select_Word_Or_Next => "Select the word, or add the next occurrence as a cursor",
         when Split_Into_Lines => "One cursor per selected line",
         when Add_Cursor_Above => "Add a cursor on the row above",
         when Add_Cursor_Below => "Add a cursor on the row below",
         when Single_Selection => "Back to one cursor",
         when Collapse_Selection => "Each selection becomes a cursor at its start",
         when New_Line => "Break the line, keeping its indent",
         when Delete_Backward => "Delete the character before each cursor",
         when Delete_Forward => "Delete the character after each cursor",
         when Delete_Word_Backward => "Delete to the previous word",
         when Delete_Word_Forward => "Delete to the next word",
         when Delete_Line => "Delete the cursors' lines", when Duplicate_Line => "Duplicate the selected lines",
         when Indent => "Indent the selected lines", when Outdent => "Outdent the selected lines",
         when Toggle_Comment => "Comment or uncomment the selected lines",
         when Undo => "Undo the last step", when Redo => "Redo the undone step",
         when Copy => "Copy the selections (or the lines) to the clipboard",
         when Cut => "Cut the selections (or the lines) to the clipboard",
         when Paste => "Paste the clipboard at every cursor",
         when Insert_Text => "Type text at every cursor",
         when Find => "Find in the document", when Find_Next => "Select the next match",
         when Find_Previous => "Select the previous match",
         when Replace => "Replace the selected match, find the next",
         when Replace_All => "Replace every match (one undo step)",
         when Select_All_Matches => "A cursor on every match",
         when Go_To_Line => "Go to a line number", when Go_To_Anything => "Go to a line (:N) or a symbol (@name)",
         when New_Document => "A new, empty document", when Open => "Open a granted file",
         when Save => "Save (atomic replace)", when Save_As => "Save under another granted name",
         when Close_Document => "Close the document", when Next_Tab => "The next tab",
         when Previous_Tab => "The previous tab",
         when Split_View => "Show the document in two views", when Other_View => "Focus the other view", when Toggle_Wrap => "Soft wrap on or off",
         when Toggle_Minimap => "Minimap on or off", when Toggle_Modal => "Vim-like modal keys on or off",
         when Toggle_Overlay => "Timing overlay on or off", when Toggle_Theme => "Light or dark",
         when Set_Tab_Width => "Set the tab width",
         when Command_Palette => "Every command, by name", when Describe_Key => "Say what the next key does",
         when Quit => "Quit");

   function Argument_Of (Item : Command) return Argument_Kind is
     (case Item is
         when Go_To_Line | Set_Tab_Width => Integer_Argument,
         when Insert_Text | Find | Replace | Replace_All | Open | Save_As | Go_To_Anything => String_Argument,
         when others => No_Argument);

   function Category_Of (Item : Command) return Category is
     (case Item is
         when No_Command .. Scroll_Down => Movement,
         when Select_All .. Collapse_Selection => Selection,
         when New_Line .. Insert_Text => Editing,
         when Find .. Go_To_Anything => Search,
         when New_Document .. Previous_Tab => Files,
         when Split_View .. Quit => View);

   function Named (Text : String) return Command is
      PREFIX : constant String := "text.";
   begin
      for C in Real_Command loop
         if Text = Name (C)
           or else (Text'Length = PREFIX'Length + Name (C)'Length and then Text'Length > PREFIX'Length
                    and then Text (Text'First .. Text'First + PREFIX'Length - 1) = PREFIX
                    and then Text (Text'First + PREFIX'Length .. Text'Last) = Name (C))
         then
            return C;
         end if;
      end loop;
      return No_Command;
   end Named;

   function C (Item : Key) return Chord is (Item => Item, Control => True, others => False);
   function S (Item : Key) return Chord is (Item => Item, Shift => True, others => False);
   function CS (Item : Key) return Chord is (Item => Item, Control => True, Shift => True, Alt => False);
   function A (Item : Key) return Chord is (Item => Item, Alt => True, others => False);
   function AS (Item : Key) return Chord is (Item => Item, Alt => True, Shift => True, Control => False);
   function P (Item : Key) return Chord is (Item => Item, others => False);

   function Binding (Item : Command) return Chord is
     (case Item is
         when Move_Left => P (Left), when Move_Right => P (Right), when Move_Up => P (Up), when Move_Down => P (Down),
         when Move_Word_Left => C (Left), when Move_Word_Right => C (Right),
         when Move_Line_Home => P (Home), when Move_Line_End => P (End_Key),
         when Move_Page_Up => P (Page_Up), when Move_Page_Down => P (Page_Down),
         when Move_Document_Start => C (Home), when Move_Document_End => C (End_Key),
         when Select_Left => S (Left), when Select_Right => S (Right), when Select_Up => S (Up),
         when Select_Down => S (Down), when Select_Word_Left => CS (Left), when Select_Word_Right => CS (Right),
         when Select_Line_Home => S (Home), when Select_Line_End => S (End_Key),
         when Select_Page_Up => S (Page_Up), when Select_Page_Down => S (Page_Down),
         when Select_Document_Start => CS (Home), when Select_Document_End => CS (End_Key),
         when Scroll_Up => C (Up), when Scroll_Down => C (Down),
         when Select_All => C (Key_A), when Select_Line => C (Key_L), when Select_Word_Or_Next => C (Key_D),
         when Split_Into_Lines => CS (Key_L), when Add_Cursor_Above => AS (Up), when Add_Cursor_Below => AS (Down),
         when Single_Selection => P (Escape),
         when New_Line => P (Enter), when Delete_Backward => P (Backspace), when Delete_Forward => P (Delete),
         when Delete_Word_Backward => C (Backspace), when Delete_Word_Forward => C (Delete),
         when Delete_Line => CS (Key_K), when Duplicate_Line => CS (Key_D), when Indent => P (Tab),
         when Outdent => S (Tab), when Toggle_Comment => C (Slash), when Undo => C (Key_Z), when Redo => CS (Key_Z),
         when Copy => C (Key_C), when Cut => C (Key_X), when Paste => C (Key_V),
         when Find => C (Key_F), when Find_Next => P (F3), when Find_Previous => S (F3), when Replace => C (Key_H),
         when Replace_All => (Item => Enter, Control => True, Alt => True, Shift => False),
         when Select_All_Matches => A (Enter), when Go_To_Line => C (Key_G), when Go_To_Anything => C (Key_P),
         when New_Document => C (Key_N), when Open => C (Key_O), when Save => C (Key_S), when Save_As => CS (Key_S),
         when Close_Document => C (Key_W), when Next_Tab => C (Page_Down), when Previous_Tab => C (Page_Up),
         when Split_View => C (Backslash), when Other_View => P (F6), when Toggle_Wrap => A (Key_Z), when Toggle_Minimap => C (Key_M),
         when Toggle_Modal => P (F9), when Toggle_Overlay => P (F12), when Toggle_Theme => P (F11),
         when Command_Palette => CS (Key_P), when Describe_Key => P (F1), when Quit => C (Key_Q),
         when others => NO_CHORD);

   function Second_Binding (Item : Command) return Chord is
     (case Item is
         when Redo => C (Key_Y), when Copy => C (Insert_Key), when Paste => S (Insert_Key), when Cut => S (Delete),
         when Next_Tab => C (Tab), when Previous_Tab => CS (Tab), when Find_Next => C (Key_G),
         when others => NO_CHORD);

   function Lookup (Pressed : Chord) return Command is
   begin
      if Pressed.Item = No_Key then
         return No_Command;
      end if;
      for Item in Real_Command loop
         if Binding (Item) = Pressed then
            return Item;
         end if;
      end loop;
      for Item in Real_Command loop
         if Second_Binding (Item) = Pressed then
            return Item;
         end if;
      end loop;
      return No_Command;
   end Lookup;

   function Key_Text (Item : Key) return String is
     (case Item is
         when No_Key => "",
         when Key_A .. Key_Z => [1 => Character'Val (Character'Pos ('A') + Key'Pos (Item) - Key'Pos (Key_A))],
         when Key_0 .. Key_9 => [1 => Character'Val (Character'Pos ('0') + Key'Pos (Item) - Key'Pos (Key_0))],
         when F1 => "F1", when F2 => "F2", when F3 => "F3", when F4 => "F4", when F5 => "F5", when F6 => "F6",
         when F7 => "F7", when F8 => "F8", when F9 => "F9", when F10 => "F10", when F11 => "F11", when F12 => "F12",
         when Up => "Up", when Down => "Down", when Left => "Left", when Right => "Right", when Home => "Home",
         when End_Key => "End", when Page_Up => "PgUp", when Page_Down => "PgDn", when Enter => "Enter",
         when Escape => "Esc", when Backspace => "Backspace", when Delete => "Del", when Tab => "Tab",
         when Insert_Key => "Ins", when Space => "Space", when Slash => "/", when Backslash => "\",
         when Minus => "-", when Equals => "=", when Period => ".", when Comma => ",", when Semicolon => ";",
         when Quote => "'", when Left_Bracket => "[", when Right_Bracket => "]", when Grave => "`");

   function Chord_Text (Pressed : Chord) return String is
     (if Pressed.Item = No_Key then ""
      else (if Pressed.Control then "Ctrl+" else "") & (if Pressed.Alt then "Alt+" else "")
           & (if Pressed.Shift then "Shift+" else "") & Key_Text (Pressed.Item));

   function Visibility_Of (Item : Real_Command) return Visibility is
     (case Item is
         when Move_Left .. Scroll_Down | New_Line | Delete_Backward | Delete_Forward | Delete_Word_Backward
            | Delete_Word_Forward | Insert_Text => Binding_Only,
         when Single_Selection | Next_Tab | Previous_Tab | Other_View | Command_Palette => Menu_Only,
         when others => Palette_And_Menu);

   function Menu_Title (Item : Menu_Name) return String is
     (case Item is
         when File_Menu => "File", when Edit_Menu => "Edit", when Selection_Menu => "Selection",
         when View_Menu => "View", when Go_Menu => "Go", when Help_Menu => "Help");

   function Menu_Mnemonic (Item : Menu_Name) return Character is
     (case Item is
         when File_Menu => 'f', when Edit_Menu => 'e', when Selection_Menu => 's', when View_Menu => 'v',
         when Go_Menu => 'g', when Help_Menu => 'h');

   function Menu_Of (Item : Command) return Menu_Name is
     (case Item is
         when New_Document | Open | Save | Save_As | Close_Document | Quit => File_Menu,
         when Undo | Redo | Copy | Cut | Paste | Find | Find_Next | Find_Previous | Replace | Replace_All
            | Delete_Line | Duplicate_Line | Toggle_Comment | Indent | Outdent => Edit_Menu,
         when Select_All .. Collapse_Selection | Select_All_Matches => Selection_Menu,
         when Command_Palette | Split_View | Toggle_Wrap | Toggle_Minimap | Toggle_Theme | Toggle_Modal
            | Set_Tab_Width | Toggle_Overlay => View_Menu,
         when Go_To_Line | Go_To_Anything | Next_Tab | Previous_Tab | Other_View => Go_Menu,
         when others => Help_Menu);

   function Group_Of (Item : Command) return Menu_Group is
     (case Item is
         --  File: New, Open | Save, Save As | Close | Quit.
         when Save | Save_As => 2, when Close_Document => 3, when Quit => 4,
         --  Edit: Undo, Redo | clipboard | find | lines | indent.
         when Copy | Cut | Paste => 2, when Find .. Replace_All => 3,
         when Delete_Line | Duplicate_Line | Toggle_Comment => 4, when Indent | Outdent => 5,
         --  Selection: selecting | more cursors | fewer.
         when Split_Into_Lines | Add_Cursor_Above | Add_Cursor_Below => 2,
         when Single_Selection | Collapse_Selection => 3,
         --  View: the palette | what shows | keys and tabs | diagnostics.
         when Split_View | Toggle_Wrap | Toggle_Minimap | Toggle_Theme => 2,
         when Toggle_Modal | Set_Tab_Width => 3, when Toggle_Overlay => 4,
         --  Go: places | tabs | views.
         when Next_Tab | Previous_Tab => 2, when Other_View => 3,
         when others => 1);

   function Caption (Item : Command) return String is
     (case Item is
         when New_Document => "New", when Open => "Open...", when Save => "Save", when Save_As => "Save As...",
         when Close_Document => "Close", when Quit => "Quit",
         when Undo => "Undo", when Redo => "Redo", when Copy => "Copy", when Cut => "Cut", when Paste => "Paste",
         when Find => "Find...", when Find_Next => "Find Next", when Find_Previous => "Find Previous",
         when Replace => "Replace...", when Replace_All => "Replace All", when Delete_Line => "Delete Line",
         when Duplicate_Line => "Duplicate Line", when Toggle_Comment => "Toggle Comment", when Indent => "Indent",
         when Outdent => "Outdent",
         when Select_All => "Select All", when Select_Line => "Select Line",
         when Select_Word_Or_Next => "Add Next Occurrence", when Select_All_Matches => "Select All Matches",
         when Split_Into_Lines => "Split into Lines", when Add_Cursor_Above => "Add Cursor Above",
         when Add_Cursor_Below => "Add Cursor Below", when Single_Selection => "Single Cursor",
         when Collapse_Selection => "Collapse Selections",
         when Command_Palette => "Command Palette...", when Split_View => "Split View", when Toggle_Wrap => "Soft Wrap",
         when Toggle_Minimap => "Minimap", when Toggle_Theme => "Dark Theme", when Toggle_Modal => "Modal Keys",
         when Set_Tab_Width => "Tab Width...", when Toggle_Overlay => "Timing Overlay",
         when Go_To_Line => "Go to Line...", when Go_To_Anything => "Go to Anything...", when Next_Tab => "Next Tab",
         when Previous_Tab => "Previous Tab", when Other_View => "Other View",
         when Describe_Key => "Describe Key...",
         when others => Description (Item));
end Text_Commands;
