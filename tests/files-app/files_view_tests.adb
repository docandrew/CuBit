with CuBit.Filesystems;
with Ada.Directories;
with Ada.Text_IO;
with CuBit.UI;
with CuBit.UI.Controls;
with CuBit.UI.Icons;
with CuBit.UI.Tables;
with CuBit.UI.State;
with Ada.Strings.Fixed;
with Interfaces; use Interfaces;
with Files_Mock_Service;
with Files_Order;
with Files_View; use Files_View;
with Files_Host_Support; use Files_Host_Support;

package body Files_View_Tests is
   use type Files_Order.Sort_Key;
   Total_Checks, Total_Failures : Natural := 0;
   function Checks return Natural is (Total_Checks);
   function Failures return Natural is (Total_Failures);

   procedure Check (Condition : Boolean; Label : String) is
   begin
      Total_Checks := Total_Checks + 1;
      if not Condition then
         Total_Failures := Total_Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Label);
      end if;
   end Check;

   WIDTH : constant := 1280;
   HEIGHT : constant := 800;
   SYNTHETIC_ENTRIES : constant := 5_000;
   CAPACITY : constant := 65_536;
   NAME_BYTES : constant := CAPACITY * 32;

   --  A fixed folder tree in the scratch place.
   FIXTURE : constant String := "fixture";
   procedure Make_Fixture is
      use Ada.Directories;
      Root : constant String := Scratch_Root & "/" & FIXTURE;
      procedure Touch (Name : String; Bytes : Natural) is
         File : Ada.Text_IO.File_Type;
      begin
         Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Root & "/" & Name);
         Ada.Text_IO.Put (File, [1 .. Bytes => 'x']);
         Ada.Text_IO.Close (File);
      end Touch;
   begin
      if Exists (Root) then
         Delete_Tree (Root);
      end if;
      Create_Path (Root & "/alpha");
      Create_Path (Root & "/beta");
      Create_Path (Root & "/nested/deep");
      if Exists (Scratch_Root & "/out") then
         Delete_Tree (Scratch_Root & "/out");
      end if;
      Create_Path (Scratch_Root & "/out");
      Touch ("file2.txt", 300);
      Touch ("file10.txt", 20);
      Touch ("Zeta.md", 4_000);
      Touch ("alpha/inside.txt", 1);
   end Make_Fixture;

   procedure Run is
      View : constant View_Access := new View_State;
      UI : constant UI_Access := new CuBit.UI.State.UI_State;
      Map : constant Map_Access := new CuBit.UI.Controls.Control_Map;
      Screen : constant Surface := New_Surface (WIDTH, HEIGHT);
      Redraw : Boolean;
      Damage : CuBit.UI.Rect;

      procedure Frame is
      begin
         Take_Damage (View.all, Damage);
         Render (View.all, Canvas (Screen), Bounds (Screen), UI.all, Map.all);
      end Frame;
      procedure Key (Name : Key_Name; Shift, Control, Alt : Boolean := False) is
      begin
         Handle (View.all, (Kind => Key_Event, Key => Name, Shift => Shift, Control => Control, Alt => Alt,
                            others => <>), Map.all, Redraw);
         Frame;
      end Key;
      procedure Type_Text (Text : String) is
      begin
         for C of Text loop
            Handle (View.all, (Kind => Text_Event, Character_Value => C, others => <>), Map.all, Redraw);
         end loop;
         Frame;
      end Type_Text;
      procedure Wait (Label : String) is
      begin
         Check (Settle (View.all), Label & ": panes settle");
         Frame;
      end Wait;
      function Names (Pane : Side) return String is
         Result : String (1 .. 4096);
         Length : Natural := 0;
      begin
         for Row in 1 .. Rows (View.all, Pane) loop
            declare
               Name : constant String := Row_Name (View.all, Pane, Row) & "|";
            begin
               exit when Length + Name'Length > Result'Length;
               Result (Length + 1 .. Length + Name'Length) := Name;
               Length := Length + Name'Length;
            end;
         end loop;
         return Result (1 .. Length);
      end Names;
   begin
      Make_Fixture;
      Configure_Service (Ada.Directories.Current_Directory);
      Initialize (View.all, CAPACITY, NAME_BYTES, "@scratch:0/" & FIXTURE,
                  "@synthetic:" & Natural'Image (SYNTHETIC_ENTRIES) (2 .. 5) & "/");
      Frame;
      Wait ("startup");
      Check (Load (View.all, Left_Pane) = Loaded and then Listed (View.all, Left_Pane) = 6, "fixture listed");
      Check (Names (Left_Pane) = "..|alpha|beta|nested|file2.txt|file10.txt|Zeta.md|",
             "folders first, natural order: " & Names (Left_Pane));
      Check (Listed (View.all, Right_Pane) = SYNTHETIC_ENTRIES, "synthetic folder listed");
      Check (Cursor_Row (View.all, Left_Pane) = 1 and then Cursor_Name (View.all, Left_Pane) = "..",
             "the cursor starts on the first row");
      Save_PPM (Screen, "build/files-start.ppm");

      --  Navigation: into alpha and back, landing on alpha.
      Key (Down);
      Check (Cursor_Name (View.all, Left_Pane) = "alpha", "Down moves the cursor");
      Key (Enter);
      Wait ("enter alpha");
      Check (Path (View.all, Left_Pane) = "@scratch:0/fixture/alpha" and then Names (Left_Pane) = "..|inside.txt|",
             "Enter opens a folder: " & Path (View.all, Left_Pane) & " " & Names (Left_Pane));
      Key (Backspace);
      Wait ("back up");
      Check (Path (View.all, Left_Pane) = "@scratch:0/fixture" and then Cursor_Name (View.all, Left_Pane) = "alpha",
             "Backspace goes up and lands on the folder left: " & Cursor_Name (View.all, Left_Pane));
      Key (Left, Alt => True);
      Wait ("history back");
      Check (Path (View.all, Left_Pane) = "@scratch:0/fixture/alpha", "Alt+Left goes back");
      Key (Right, Alt => True);
      Wait ("history forward");
      Check (Path (View.all, Left_Pane) = "@scratch:0/fixture", "Alt+Right goes forward");

      --  Type-ahead filter.
      Type_Text ("FI");
      Wait ("filter");
      Check (Names (Left_Pane) = "file2.txt|file10.txt|" and then Cursor_Name (View.all, Left_Pane) = "file2.txt",
             "typing filters, the first match takes the cursor: " & Names (Left_Pane));
      Type_Text ("le1");
      Wait ("refine");
      Check (Names (Left_Pane) = "file10.txt|", "typing more refines");
      Key (Backspace);
      Key (Backspace);
      Key (Backspace);
      Wait ("widen");
      Check (Filter_Text (View.all, Left_Pane) = "fi" and then Names (Left_Pane) = "file2.txt|file10.txt|",
             "Backspace widens the filter");
      Key (Escape);
      Wait ("clear");
      Check (Filter_Text (View.all, Left_Pane) = "" and then Rows (View.all, Left_Pane) = 7
             and then Cursor_Name (View.all, Left_Pane) = "file2.txt",
             "Escape clears the filter; the cursor stays on its entry");

      --  Marks.
      Key (Insert);
      Check (Marked_Count (View.all, Left_Pane) = 1 and then Cursor_Name (View.all, Left_Pane) = "file10.txt",
             "Insert marks and moves down");
      Key (Down, Shift => True);
      Check (Marked_Count (View.all, Left_Pane) = 2, "Shift+Down marks the row passed");
      Key (Letter_A, Control => True);
      Check (Marked_Count (View.all, Left_Pane) = 6, "Ctrl+A marks every entry");
      Key (Letter_I, Control => True);
      Check (Marked_Count (View.all, Left_Pane) = 0, "Ctrl+I inverts");
      Key (Space);
      Check (Marked_Count (View.all, Left_Pane) = 1, "Space marks");
      Save_PPM (Screen, "build/files-marked.ppm");

      --  Sorting keeps the cursor on its entry.
      declare
         Name : constant String := Cursor_Name (View.all, Left_Pane);
      begin
         Key (F6, Control => True);
         Wait ("sort by size");
         Check (Rule (View.all, Left_Pane).Key = Files_Order.By_Size, "Ctrl+F6 sorts by size");
         Check (Names (Left_Pane) = "..|alpha|beta|nested|file10.txt|file2.txt|Zeta.md|",
                "by size: " & Names (Left_Pane));
         Check (Cursor_Name (View.all, Left_Pane) = Name, "the cursor follows its entry through a sort");
         Key (F6, Control => True);
         Wait ("size descending");
         Check (Names (Left_Pane) = "..|nested|beta|alpha|Zeta.md|file2.txt|file10.txt|",
                "again reverses (folders stay first): " & Names (Left_Pane));
         Key (F3, Control => True);
         Wait ("by name");
      end;

      --  Panes.
      Key (Tab);
      Check (Active (View.all) = Right_Pane, "Tab switches panes");
      Key (End_Key);
      Check (Cursor_Row (View.all, Right_Pane) = Rows (View.all, Right_Pane), "End goes to the last row");
      Key (Page_Up);
      Check (Cursor_Row (View.all, Right_Pane) < Rows (View.all, Right_Pane), "Page Up");
      Save_PPM (Screen, "build/files-right.ppm");

      --  The pointer: a click on the left pane's fourth row.
      declare
         Rows_Box : constant CuBit.UI.Rect := CuBit.UI.Controls.Bounds (Map.all, ROWS_ID (Left_Pane));
         X : constant Natural := Rows_Box.x + 40;
         Row_Y : constant Natural := Rows_Box.y;
      begin
         Check (Rows_Box.h > 0, "the rows area is a control");
         Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Press, X, Row_Y + 3 * 20 + 5, 1_000);
         Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Release, X, Row_Y + 3 * 20 + 5, 1_050);
         Frame;
         Check (Active (View.all) = Left_Pane and then Cursor_Name (View.all, Left_Pane) = "nested",
                "a click moves the cursor and the active pane: " & Cursor_Name (View.all, Left_Pane));
         Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Press, X, Row_Y + 3 * 20 + 5, 1_200);
         Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Release, X, Row_Y + 3 * 20 + 5, 1_250);
         Wait ("double click");
         Check (Path (View.all, Left_Pane) = "@scratch:0/fixture/nested", "a double click opens");
      end;

      --  Scrollbar thumb drag (regression: the hosted dispatch lost capture
      --  and never repainted, so the thumb did not move).
      Key (Tab);
      Check (Active (View.all) = Right_Pane, "right pane active for the drag");
      Key (Home);
      declare
         Bar : constant CuBit.UI.Rect := CuBit.UI.Controls.Bounds (Map.all, SCROLL_ID (Right_Pane));
         X : constant Natural := Bar.x + Bar.w / 2;
         Y : constant Natural := Bar.y + 20;
         Before : constant Natural := Top_Row (View.all, Right_Pane);
      begin
         Check (Bar.h > 0, "the right pane has a scrollbar");
         Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Press, X, Y, 2_000);
         Frame;
         for Step in 1 .. 10 loop
            --  Moves leave the bar sideways too: capture keeps the drag.
            Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Move, X + Step * 15, Y + Step * 20);
            Frame;
         end loop;
         Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Release, X + 150, Y + 200, 2_100);
         Frame;
         Check (Top_Row (View.all, Right_Pane) > Before + 100,
                "dragging the thumb scrolls:" & Natural'Image (Top_Row (View.all, Right_Pane)));
         Check (Cursor_Row (View.all, Right_Pane) = 1, "a drag leaves the cursor");
      end;
      Key (Tab);

      --  The quick viewer: Enter on a file reads it through the queue.
      Key (Backspace);
      Wait ("back to the fixture");
      Key (Home);
      for N in 1 .. 4 loop
         Key (Down);
      end loop;
      Check (Cursor_Name (View.all, Left_Pane) = "file2.txt", "cursor on a file: " & Cursor_Name (View.all, Left_Pane));
      Key (Enter);
      Wait ("viewer");
      Check (Viewer_Open (View.all) and then Viewer_Line (View.all, 1) = [1 .. 300 => 'x'],
             "Enter on a file shows its text");
      Save_PPM (Screen, "build/files-viewer.ppm");
      Key (Escape);
      Check (not Viewer_Open (View.all), "Escape closes the viewer");
      --  A read that misses its deadline ends visibly, and nothing stays stuck.
      Files_Mock_Service.Set_Latency (2_500_000);
      Key (Enter);
      declare
         Busy, Changed : Boolean;
         Until_Us : constant Interfaces.Unsigned_64 := Now_Us + 4_000_000;
      begin
         while Viewer_Open (View.all) and then Now_Us < Until_Us loop
            Pump (View.all, 20_000, Now_Us, Busy, Changed);
            delay 0.001;
         end loop;
         Check (not Viewer_Open (View.all), "a slow read still ends");
      end;
      Frame;
      Check (not Viewer_Open (View.all) and then Status_Message (View.all)'Length > 0
             and then Ada.Strings.Fixed.Index (Status_Message (View.all), "took longer") > 0,
             "a slow read times out with a message: " & Status_Message (View.all));
      Files_Mock_Service.Set_Latency (0);
      Check (Settle (View.all, 15_000), "late answers are consumed");
      Key (Enter);
      Wait ("viewer again");
      Check (Viewer_Open (View.all) and then Viewer_Line (View.all, 1)'Length = 300, "the viewer works after a timeout");
      Key (Escape);

      --  Toolbar, context menu and drawer.
      Add_Place (View.all, "Fixture", "@scratch:0/fixture", CuBit.UI.Icons.Folder);
      Add_Place (View.all, "Synthetic", "@synthetic:500/", CuBit.UI.Icons.Network);
      Frame;
      Check (Drawer_Open (View.all) and then Drawer_Row (View.all, 1) = "Places"
             and then Drawer_Row (View.all, 2) = "Fixture", "the drawer lists places: " & Drawer_Row (View.all, 2));
      declare
         function Center (Id : CuBit.UI.Controls.Control_ID; DX, DY : Natural := 0) return CuBit.UI.Rect is
           (CuBit.UI.Controls.Bounds (Map.all, Id));
         procedure Click (X, Y : Natural; Secondary : Boolean := False) is
         begin
            Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Press, X, Y, 5_000, Secondary => Secondary);
            Frame;
            Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Release, X, Y, 5_010, Secondary => Secondary);
            Frame;
         end Click;
         Up_Box : constant CuBit.UI.Rect := Center (Tool_ID (Tool_Up));
         Drawer_Box : constant CuBit.UI.Rect := Center (DRAWER_ID);
      begin
         --  Left pane is in fixture/nested: Up via the toolbar.
         Check (Up_Box.w > 0, "the Up tool is a control");
         Click (Up_Box.x + Up_Box.w / 2, Up_Box.y + Up_Box.h / 2);
         Wait ("toolbar up");
         Check (Path (View.all, Left_Pane) = "@scratch:0/", "the Up tool goes up: " & Path (View.all, Left_Pane));
         Save_PPM (Screen, "build/files-toolbar.ppm");
         --  The drawer: a click on the Synthetic place navigates the active pane.
         Click (Drawer_Box.x + 40, Drawer_Box.y + 26 + 24 + 12);
         Wait ("drawer place");
         Check (Path (View.all, Left_Pane) = "@synthetic:500/", "a drawer place opens: " & Path (View.all, Left_Pane));
         Click (Drawer_Box.x + 40, Drawer_Box.y + 26 + 12);
         Wait ("back to the fixture");
         Check (Path (View.all, Left_Pane) = "@scratch:0/fixture", "back to the fixture: " & Path (View.all, Left_Pane));
         --  Bookmarks.
         Key (Letter_D, Control => True);
         Check (Bookmarked (View.all, Left_Pane), "Ctrl+D bookmarks the folder");
         Key (Letter_B, Control => True);
         Check (not Drawer_Open (View.all), "Ctrl+B hides the drawer");
         Key (Letter_B, Control => True);
         Check (Drawer_Open (View.all), "Ctrl+B shows it again");
         --  Tooltips: resting on a tool shows what it does and its key, after
         --  the delay (the view's deadline says when); a click hides it.
         declare
            Box : constant CuBit.UI.Rect := Center (Tool_ID (Tool_Refresh));
            Busy, Changed : Boolean;
            T0 : constant Unsigned_64 := 50_000_000;
         begin
            Set_Clock (View.all, T0);
            Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Move, Box.x + 4, Box.y + 4, 0);
            Frame;
            Check (Tooltip_Text (View.all) = "", "no tooltip at once");
            Check (Next_Deadline_Us (View.all) <= T0 + 600_000, "the view asks to be woken for the tooltip");
            Pump (View.all, 1_000, T0 + 600_000, Busy, Changed);
            Frame;
            Check (Tooltip_Text (View.all) = "Refresh" and then Changed, "resting shows the tooltip: "
                   & Tooltip_Text (View.all));
            Save_PPM (Screen, "build/files-tooltip.ppm");
            Key (Down);
            Check (Tooltip_Text (View.all) = "", "a key hides it");
         end;
         --  The function-key bar: F1 .. F10 all drawn; a key with an action
         --  is a control and a click on it acts as the key; greyed ones are not.
         declare
            F1_Box : constant CuBit.UI.Rect := Center (FUNCTION_KEY_FIRST);
            F9_Box : constant CuBit.UI.Rect := Center (FUNCTION_KEY_FIRST + 8);
            F10_Box : constant CuBit.UI.Rect := Center (FUNCTION_KEY_FIRST + 9);
         begin
            Check (F1_Box.w = 0, "F1 (no help yet) is greyed, not a control");
            Check (F9_Box.w > 0 and then F10_Box.w > 0 and then F10_Box.x > F9_Box.x
                   and then F10_Box.y + F10_Box.h <= HEIGHT and then F10_Box.h < 24,
                   "F9 and F10 are slim keys along the bottom");
            Click (F9_Box.x + F9_Box.w / 2, F9_Box.y + F9_Box.h / 2);
            Check (not Pane_Shown (View.all, Right_Pane), "a click on F9 shows one pane");
            Click (F9_Box.x + F9_Box.w / 2, F9_Box.y + F9_Box.h / 2);
            Check (Pane_Shown (View.all, Right_Pane), "and again shows both");
            Save_PPM (Screen, "build/files-keys.ppm");
            Key (F12, Control => True);
            Check (not Keys_Shown (View.all) and then Center (FUNCTION_KEY_FIRST + 8).w = 0,
                   "Ctrl+F12 hides the bar");
            Key (F12, Control => True);
            Check (Keys_Shown (View.all) and then Center (FUNCTION_KEY_FIRST + 8).w > 0, "and shows it");
         end;
         --  Context menu on a file: View is first; Enter chooses it.
         Key (Home);
         for N in 1 .. 4 loop
            Key (Down);
         end loop;
         declare
            Rows_Box : constant CuBit.UI.Rect := CuBit.UI.Controls.Bounds (Map.all, ROWS_ID (Left_Pane));
            Y : constant Natural := Rows_Box.y + (Cursor_Row (View.all, Left_Pane) - 1) * 20 + 10;
         begin
            Click (Rows_Box.x + 60, Y, Secondary => True);
            Check (Menu_Open (View.all), "a right-click opens the context menu");
            Save_PPM (Screen, "build/files-menu.ppm");
            Key (Down);
            Check (Menu_Selected (View.all) = 1, "Down selects the first item");
            Key (Enter);
            Wait ("menu view");
            Check (not Menu_Open (View.all) and then Viewer_Open (View.all), "Enter on View opens the viewer");
            Key (Escape);
            --  On empty space: Sort by > Size through the keyboard.
            Click (Rows_Box.x + 60, Rows_Box.y + Rows_Box.h - 10, Secondary => True);
            Check (Menu_Open (View.all), "a right-click on empty space opens a menu");
            for N in 1 .. 4 loop
               Key (Down);
            end loop;
            Key (Right);
            Save_PPM (Screen, "build/files-submenu.ppm");
            Key (End_Key);
            Key (Enter);
            Wait ("menu sort");
            Check (Rule (View.all, Left_Pane).Key = Files_Order.By_Size, "Sort by > Size from the submenu");
            Key (F3, Control => True);
            Wait ("by name again");
            --  An outside press closes the menu and is consumed.
            Click (Rows_Box.x + 60, Rows_Box.y + 10, Secondary => True);
            declare
               Before_Row : constant Natural := Cursor_Row (View.all, Left_Pane);
            begin
               Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Press, Rows_Box.x + 300,
                        Rows_Box.y + 5 * 20 + 5, 6_000);
               Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Release, Rows_Box.x + 300,
                        Rows_Box.y + 5 * 20 + 5, 6_010);
               Frame;
               Check (not Menu_Open (View.all) and then Cursor_Row (View.all, Left_Pane) = Before_Row,
                      "an outside press closes the menu without clicking through");
            end;
            --  Near the bottom-right corner the menu stays on screen.
            Click (WIDTH - 5, HEIGHT - 40, Secondary => True);
            Frame;
            if Menu_Open (View.all) then
               Key (Escape);
            end if;
         end;
      end;

      --  Columns: the header's menu adds one; a header drag moves one.
      declare
         Base : constant CuBit.UI.Controls.Control_ID := COLUMNS_BASE (Left_Pane) + CuBit.UI.Tables.MAX_COLUMNS;
         Name_Cell : constant CuBit.UI.Rect := CuBit.UI.Controls.Bounds (Map.all, Base);
      begin
         Check (Column_Titles (View.all, Left_Pane) = "Name|Size|Modified|", "default columns");
         Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Press, Name_Cell.x + 20, Name_Cell.y + 5,
                  7_000, Secondary => True);
         Frame;
         Check (Menu_Open (View.all), "a right-click on the header opens the columns menu");
         Save_PPM (Screen, "build/files-columns-menu.ppm");
         for N in 1 .. 6 loop
            Key (Down);
         end loop;
         Key (Enter);
         Check (Column_Titles (View.all, Left_Pane) = "Name|Size|Modified|Permissions|",
                "Permissions added: " & Column_Titles (View.all, Left_Pane));
         declare
            Size_Cell : constant CuBit.UI.Rect := CuBit.UI.Controls.Bounds (Map.all, Base + 1);
            Modified_Cell : constant CuBit.UI.Rect := CuBit.UI.Controls.Bounds (Map.all, Base + 2);
         begin
            Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Press, Modified_Cell.x + 20,
                     Modified_Cell.y + 5, 7_100);
            Frame;
            Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Move, Size_Cell.x + 20, Size_Cell.y + 5);
            Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Release, Size_Cell.x + 20, Size_Cell.y + 5,
                     7_200);
            Frame;
         end;
         Check (Column_Titles (View.all, Left_Pane) = "Name|Modified|Size|Permissions|",
                "dragging a header moves its column: " & Column_Titles (View.all, Left_Pane));
         Check (Rule (View.all, Left_Pane).Key = Files_Order.By_Name, "a drag does not sort");
         Save_PPM (Screen, "build/files-columns.ppm");
      end;

      --  The divider: dragging it gives the left pane more room.
      declare
         Divider : constant CuBit.UI.Rect := CuBit.UI.Controls.Bounds (Map.all, DIVIDER_FIRST);
         Before : constant Natural := Pane_Width (View.all, Left_Pane);
      begin
         Check (Divider.w > 0, "the divider between panes is a control");
         Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Press, Divider.x + 2, Divider.y + 100, 8_000);
         Frame;
         Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Move, Divider.x + 102, Divider.y + 100);
         Frame;
         Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Release, Divider.x + 102, Divider.y + 100, 8_100);
         Frame;
         Check (Pane_Width (View.all, Left_Pane) in Before + 90 .. Before + 110,
                "dragging the divider resizes the panes:" & Natural'Image (Before) & " ->"
                & Natural'Image (Pane_Width (View.all, Left_Pane)));
      end;

      --  Panes: add one, cycle focus, single-pane mode, hide, close.
      Key (Letter_N, Control => True);
      Wait ("third pane");
      Check (Pane_Count (View.all) = 3 and then Active (View.all) = 2, "Ctrl+N adds a pane after the active one");
      Save_PPM (Screen, "build/files-three-panes.ppm");
      Key (Tab);
      Check (Active (View.all) = 3, "Tab cycles to the next pane");
      Key (Tab);
      Check (Active (View.all) = 1, "and wraps");
      Key (Tab, Shift => True);
      Check (Active (View.all) = 3, "Shift+Tab goes back");
      Key (F9);
      Check (Pane_Shown (View.all, 3) and then not Pane_Shown (View.all, 1) and then not Pane_Shown (View.all, 2),
             "F9: single-pane mode");
      Save_PPM (Screen, "build/files-single-pane.ppm");
      Key (F9);
      Check (Pane_Shown (View.all, 1) and then Pane_Shown (View.all, 2), "F9 again shows them all");
      Key (F1, Control => True);
      Check (not Pane_Shown (View.all, 1), "Ctrl+F1 hides the first pane");
      Key (F1, Control => True);
      Check (Pane_Shown (View.all, 1), "and shows it again");
      Key (Letter_W, Control => True, Shift => True);
      Check (Pane_Count (View.all) = 2, "Ctrl+Shift+W closes the active pane");
      if Active (View.all) /= Left_Pane then
         Key (Tab);
      end if;

      --  Tabs.
      Key (Letter_T, Control => True);
      Check (Tab_Count (View.all, Left_Pane) = 2 and then Current_Tab (View.all, Left_Pane) = 2, "Ctrl+T opens a tab");
      Key (Down);
      Key (Down);
      Key (Enter);
      Wait ("tab 2 navigates");
      declare
         Tab_Two : constant String := Path (View.all, Left_Pane);
      begin
         Key (Tab, Control => True);
         Wait ("tab 1");
         Check (Current_Tab (View.all, Left_Pane) = 1 and then Path (View.all, Left_Pane) = "@scratch:0/fixture",
                "Ctrl+Tab switches tabs: " & Path (View.all, Left_Pane));
         Save_PPM (Screen, "build/files-tabs.ppm");
         Key (Page_Down, Control => True, Shift => True);
         Check (Current_Tab (View.all, Left_Pane) = 2, "Ctrl+Shift+PgDn moves the tab right");
         Key (Letter_W, Control => True);
         Wait ("tab closed");
         Check (Tab_Count (View.all, Left_Pane) = 1 and then Path (View.all, Left_Pane) = Tab_Two,
                "Ctrl+W closes the tab: " & Path (View.all, Left_Pane));
         Key (Letter_T, Control => True, Shift => True);
         Wait ("tab restored");
         Check (Tab_Count (View.all, Left_Pane) = 2 and then Path (View.all, Left_Pane) = "@scratch:0/fixture",
                "Ctrl+Shift+T restores it: " & Path (View.all, Left_Pane));
         --  Each tab has a close button: one on another tab closes that tab
         --  and leaves the current one (and its listing) alone.
         declare
            Close_One : constant CuBit.UI.Rect := Tab_Close_Area (View.all, Left_Pane, 1);
            Close_Two : CuBit.UI.Rect;
            Listing_Before : constant Unsigned_64 := Listing_Us (View.all, Left_Pane);
         begin
            Check (Close_One.w > 0, "a tab has a close button");
            Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Press,
                     Close_One.x + Close_One.w / 2, Close_One.y + Close_One.h / 2, 7_000);
            Frame;
            Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Release,
                     Close_One.x + Close_One.w / 2, Close_One.y + Close_One.h / 2, 7_010);
            Frame;
            Check (Tab_Count (View.all, Left_Pane) = 1 and then Current_Tab (View.all, Left_Pane) = 1
                   and then Path (View.all, Left_Pane) = "@scratch:0/fixture"
                   and then Listing_Us (View.all, Left_Pane) = Listing_Before,
                   "x on another tab closes it, the current stays: " & Path (View.all, Left_Pane));
            Key (Letter_T, Control => True, Shift => True);
            Wait ("closed tab restored");
            Check (Tab_Count (View.all, Left_Pane) = 2, "Ctrl+Shift+T brings back an x-closed tab");
            Close_Two := Tab_Close_Area (View.all, Left_Pane, 1);
            --  Pressing x then releasing elsewhere does nothing.
            Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Press,
                     Close_Two.x + Close_Two.w / 2, Close_Two.y + Close_Two.h / 2, 7_100);
            Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Release, 5, 300, 7_110);
            Frame;
            Check (Tab_Count (View.all, Left_Pane) = 2, "x released off the button keeps the tab");
            --  A middle click on a tab closes it.
            Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Press,
                     Close_Two.x - 40, Close_Two.y + Close_Two.h / 2, 7_200, Middle => True);
            Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Release,
                     Close_Two.x - 40, Close_Two.y + Close_Two.h / 2, 7_210, Middle => True);
            Wait ("middle click");
            Check (Tab_Count (View.all, Left_Pane) = 1, "a middle click closes the tab");
            Key (Letter_T, Control => True, Shift => True);
            Wait ("restored again");
         end;
         Key (Letter_W, Control => True);
         Wait ("back to one tab");
         Key (Backspace);
         Wait ("up to the fixture");
      end;

      --  Operations: left the fixture, right an empty folder.
      Close (View.all);
      Initialize (View.all, CAPACITY, NAME_BYTES, "@scratch:0/" & FIXTURE, "@scratch:0/out");
      Wait ("operations");
      declare
         use Ada.Directories;
         Out_Root : constant String := Scratch_Root & "/out";
         function Go_To_Name (Pane : Side; Name : String) return Boolean is
         begin
            if Active (View.all) /= Pane then
               Key (Tab);
            end if;
            Key (Home);
            for N in 1 .. Rows (View.all, Pane) loop
               exit when Cursor_Name (View.all, Pane) = Name;
               Key (Down);
            end loop;
            return Cursor_Name (View.all, Pane) = Name;
         end Go_To_Name;
         procedure Type_Line (Text : String) is
         begin
            Type_Text (Text);
         end Type_Line;
      begin
         --  F7 in the right pane.
         Key (Tab);
         Key (F7);
         Type_Line ("made");
         Key (Enter);
         Wait ("mkdir");
         Check (Exists (Out_Root & "/made") and then Kind (Out_Root & "/made") = Directory, "F7 makes a folder");
         Check (Names (Right_Pane) = "..|made|", "the new folder is listed: " & Names (Right_Pane));
         --  F5: a file, then a folder tree.
         Check (Go_To_Name (Left_Pane, "file2.txt"), "cursor on file2.txt");
         Key (F5);
         Save_PPM (Screen, "build/files-confirm-copy.ppm");
         Key (Enter);
         Wait ("copy file");
         Check (Exists (Out_Root & "/file2.txt") and then Size (Out_Root & "/file2.txt") = 301,
                "F5 copies a file: " & Status_Message (View.all));
         Check (Go_To_Name (Left_Pane, "alpha"), "cursor on alpha");
         Key (F5);
         Key (Enter);
         Wait ("copy tree");
         Check (Exists (Out_Root & "/alpha/inside.txt"), "F5 copies a folder tree: " & Status_Message (View.all));
         --  The same file again: the conflict is asked; K keeps both.
         Check (Go_To_Name (Left_Pane, "file2.txt"), "cursor on file2.txt again");
         Key (F5);
         Key (Enter);
         Wait ("conflict");
         Save_PPM (Screen, "build/files-conflict.ppm");
         Type_Line ("k");
         Wait ("keep both");
         Check (Exists (Out_Root & "/file2 (2).txt"), "keep both makes file2 (2).txt: " & Status_Message (View.all));
         --  Overwrite chosen up front.
         Key (F5);
         Type_Line ("o");
         Key (Enter);
         Wait ("overwrite");
         Check (not Exists (Out_Root & "/file2 (3).txt") and then Size (Out_Root & "/file2.txt") = 301,
                "overwrite replaces: " & Status_Message (View.all));
         --  F6 within a volume renames: Zeta.md leaves the left pane.
         Check (Go_To_Name (Left_Pane, "Zeta.md"), "cursor on Zeta.md");
         Key (F6);
         Key (Enter);
         Wait ("move");
         Check (Exists (Out_Root & "/Zeta.md") and then not Exists (Scratch_Root & "/" & FIXTURE & "/Zeta.md"),
                "F6 moves: " & Status_Message (View.all));
         --  Shift+F6 renames in place.
         Check (Go_To_Name (Right_Pane, "Zeta.md"), "cursor on the moved file");
         Key (F6, Shift => True);
         for N in 1 .. 7 loop
            Key (Backspace);
         end loop;
         Type_Line ("renamed.md");
         Key (Enter);
         Wait ("rename");
         Check (Exists (Out_Root & "/renamed.md"), "Shift+F6 renames: " & Status_Message (View.all));
         --  F8 deletes a tree after confirmation.
         Check (Go_To_Name (Right_Pane, "alpha"), "cursor on the copied alpha");
         Key (F8);
         Save_PPM (Screen, "build/files-confirm-delete.ppm");
         Key (Enter);
         Wait ("delete");
         Check (not Exists (Out_Root & "/alpha"), "F8 deletes a folder tree: " & Status_Message (View.all));
         --  Esc dismisses a prompt without acting.
         Check (Go_To_Name (Right_Pane, "made"), "cursor on made");
         Key (F8);
         Key (Escape);
         Wait ("dismissed");
         Check (Exists (Out_Root & "/made"), "Escape dismisses the delete prompt");
         --  Cancel: a large copy stopped at once leaves no partial file.
         declare
            Big : Ada.Text_IO.File_Type;
         begin
            Ada.Text_IO.Create (Big, Ada.Text_IO.Out_File, Scratch_Root & "/" & FIXTURE & "/big.bin");
            for N in 1 .. 4_000 loop
               Ada.Text_IO.Put (Big, [1 .. 2_000 => 'b']);
            end loop;
            Ada.Text_IO.Close (Big);
         end;
         Key (Tab);
         Key (Letter_R, Control => True);
         Wait ("big file listed");
         Check (Go_To_Name (Left_Pane, "big.bin"), "cursor on big.bin");
         Files_Mock_Service.Set_Latency (20_000);
         Key (F5);
         Key (Enter);
         declare
            Busy, Changed : Boolean;
         begin
            for N in 1 .. 8 loop
               Pump (View.all, 20_000, Now_Us, Busy, Changed);
               delay 0.02;
            end loop;
         end;
         Key (Escape);
         Wait ("cancel");
         Files_Mock_Service.Set_Latency (0);
         Check (not Exists (Out_Root & "/big.bin") and then Ada.Strings.Fixed.Index (Status_Message (View.all), "Cancel") > 0,
                "a cancelled copy leaves no partial file: " & Status_Message (View.all));
         --  Into a read-only place: refused, said so.
         Close (View.all);
         Initialize (View.all, CAPACITY, NAME_BYTES, "@scratch:0/" & FIXTURE, "@host:0/tmp");
         Wait ("read-only target");
         Check (Go_To_Name (Left_Pane, "file10.txt"), "cursor on file10.txt");
         Key (F5);
         Key (Enter);
         Wait ("read-only copy");
         Check (Ada.Strings.Fixed.Index (Status_Message (View.all), "read-only") > 0,
                "copying into a read-only place says so: " & Status_Message (View.all));
      end;

      --  Change watches: another client renames a file in the folder shown;
      --  the pane follows with no input and no polling.
      Close (View.all);
      Initialize (View.all, CAPACITY, NAME_BYTES, "@scratch:0/" & FIXTURE, "@scratch:0/out");
      Wait ("watched");
      declare
         Status : Unsigned_32;
         function Shows (Pane : Side; Name : String) return Boolean is
           (for some Row in 1 .. Rows (View.all, Pane) => Row_Name (View.all, Pane, Row) = Name);
      begin
         Files_Mock_Service.Rename ("@scratch:0/" & FIXTURE & "/file10.txt", "@scratch:0/" & FIXTURE & "/file11.txt",
                                    Status);
         Wait_For_Wake (2_000);
         Wait ("renamed elsewhere");
         Check (Status = CuBit.Filesystems.REPLY_OK and then Shows (Left_Pane, "file11.txt") and then not Shows (Left_Pane, "file10.txt"),
                "a watched folder shows another client's rename");
         Files_Mock_Service.Rename ("@scratch:0/" & FIXTURE & "/file11.txt", "@scratch:0/" & FIXTURE & "/file10.txt",
                                    Status);
         Wait_For_Wake (2_000);
         Wait ("renamed back");
         Check (Shows (Left_Pane, "file10.txt"), "and the rename back");
         --  Free space of the active pane's volume, in the status line.
         Check (Ada.Strings.Fixed.Index (Volume_Space (View.all), "free of") > 0,
                "the status line shows free space: " & Volume_Space (View.all));
      end;

      --  No folders named: the panes start at the granted scopes, which are
      --  also places.
      Close (View.all);
      Initialize (View.all, CAPACITY, NAME_BYTES, "", "");
      Wait ("scopes");
      Check (Path (View.all, Left_Pane) = "@host:0/" and then Path (View.all, Right_Pane) = "@scratch:0/",
             "panes start at the granted scopes: " & Path (View.all, Left_Pane) & " " & Path (View.all, Right_Pane));
      Check (Drawer_Row (View.all, 2) = "@host:0/" and then Drawer_Row (View.all, 3) = "@scratch:0/",
             "each scope is a place: " & Drawer_Row (View.all, 2));

      --  Authority and hostility.
      Key (Tab);
      Initialize (View.all, CAPACITY, NAME_BYTES, "@nowhere:0/", "@host:0/../../etc");
      Wait ("denied");
      Check (Load (View.all, Left_Pane) = Failed and then Load (View.all, Right_Pane) = Failed,
             "unknown places and '..' are refused");
      Save_PPM (Screen, "build/files-denied.ppm");
      Close (View.all);
   end Run;
end Files_View_Tests;
