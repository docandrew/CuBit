with CuBit.Filesystems;
with Ada.Directories;
with Ada.Text_IO;
with CuBit.Appearance;
with CuBit.UI;
with CuBit.UI.Controls;
with CuBit.UI.Icons;
with CuBit.UI.Keys;
with CuBit.UI.Modifier_Reveal;
with Files_Keymaps;
with CuBit.UI_Metrics;
with CuBit.UI.Tables;
with CuBit.UI.State;
with Ada.Strings.Fixed;
with Interfaces; use Interfaces;
with Files_Mock_Service;
with Files_Order;
with Files_View; use Files_View;
with Clipboard_Text;
with Files_Host_Support; use Files_Host_Support;

package body Files_View_Tests is
   package K renames CuBit.UI.Keys;
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
      procedure Key (Name : K.Key; Shift, Control, Alt : Boolean := False) is
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
      Key (K.Down);
      Check (Cursor_Name (View.all, Left_Pane) = "alpha", "Down moves the cursor");
      Key (K.Enter);
      Wait ("enter alpha");
      Check (Path (View.all, Left_Pane) = "@scratch:0/fixture/alpha" and then Names (Left_Pane) = "..|inside.txt|",
             "Enter opens a folder: " & Path (View.all, Left_Pane) & " " & Names (Left_Pane));
      Key (K.Backspace);
      Wait ("back up");
      Check (Path (View.all, Left_Pane) = "@scratch:0/fixture" and then Cursor_Name (View.all, Left_Pane) = "alpha",
             "Backspace goes up and lands on the folder left: " & Cursor_Name (View.all, Left_Pane));
      Key (K.Left, Alt => True);
      Wait ("history back");
      Check (Path (View.all, Left_Pane) = "@scratch:0/fixture/alpha", "Alt+Left goes back");
      Key (K.Right, Alt => True);
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
      Key (K.Backspace);
      Key (K.Backspace);
      Key (K.Backspace);
      Wait ("widen");
      Check (Filter_Text (View.all, Left_Pane) = "fi" and then Names (Left_Pane) = "file2.txt|file10.txt|",
             "Backspace widens the filter");
      Key (K.Escape);
      Wait ("clear");
      Check (Filter_Text (View.all, Left_Pane) = "" and then Rows (View.all, Left_Pane) = 7
             and then Cursor_Name (View.all, Left_Pane) = "file2.txt",
             "Escape clears the filter; the cursor stays on its entry");
      --  Copy (docs/clipboard.md): the cursor's entry, as names or paths.
      declare
         Text_Form, Literal : Clipboard_Text.Builder (4_096);
         Count : Natural;
         Request : Clipboard_Request;
      begin
         Selection_Copy (View.all, Copy_Names, Text_Form, Literal, Count);
         Check (Count = 1 and then Clipboard_Text.Image (Text_Form) = "file2.txt"
                and then Clipboard_Text.Image (Literal) = "(list ""file2.txt"")",
                "copy: the cursor's name, as a List-String: " & Clipboard_Text.Image (Literal));
         Selection_Copy (View.all, Copy_Paths, Text_Form, Literal, Count);
         Check (Count = 1 and then Clipboard_Text.Image (Text_Form) = "@scratch:0/fixture/file2.txt"
                and then Clipboard_Text.Image (Literal) = "(list ""@scratch:0/fixture/file2.txt"")",
                "copy: the cursor's path: " & Clipboard_Text.Image (Text_Form));
         Take_Clipboard_Request (View.all, Request);
         Check (Request = No_Request, "no menu copy pending");
         Check (not Prompt_Takes_Text (View.all), "no prompt takes the paste");
      end;

      --  Marks.
      Key (K.Insert);
      Check (Marked_Count (View.all, Left_Pane) = 1 and then Cursor_Name (View.all, Left_Pane) = "file10.txt",
             "Insert marks and moves down");
      Key (K.Down, Shift => True);
      Check (Marked_Count (View.all, Left_Pane) = 2, "Shift+Down marks the row passed");
      Key (K.A, Control => True);
      Check (Marked_Count (View.all, Left_Pane) = 6, "Ctrl+A marks every entry");
      declare
         Text_Form, Literal : Clipboard_Text.Builder (4_096);
         Count : Natural;
      begin
         Selection_Copy (View.all, Copy_Names, Text_Form, Literal, Count);
         Check (Count = 6 and then Clipboard_Text.Image (Literal) =
                  "(list ""alpha"" ""beta"" ""nested"" ""file2.txt"" ""file10.txt"" ""Zeta.md"")"
                and then Clipboard_Text.Image (Text_Form) =
                  "alpha" & ASCII.LF & "beta" & ASCII.LF & "nested" & ASCII.LF & "file2.txt" & ASCII.LF &
                  "file10.txt" & ASCII.LF & "Zeta.md",
                "copy: marked entries in the pane's order: " & Clipboard_Text.Image (Literal));
      end;
      Key (K.I, Control => True);
      Check (Marked_Count (View.all, Left_Pane) = 0, "Ctrl+I inverts");
      Key (K.Space);
      Check (Marked_Count (View.all, Left_Pane) = 1, "Space marks");
      Save_PPM (Screen, "build/files-marked.ppm");

      --  Sorting keeps the cursor on its entry.
      declare
         Name : constant String := Cursor_Name (View.all, Left_Pane);
      begin
         Key (K.F6, Control => True);
         Wait ("sort by size");
         Check (Rule (View.all, Left_Pane).Key = Files_Order.By_Size, "Ctrl+F6 sorts by size");
         Check (Names (Left_Pane) = "..|alpha|beta|nested|file10.txt|file2.txt|Zeta.md|",
                "by size: " & Names (Left_Pane));
         Check (Cursor_Name (View.all, Left_Pane) = Name, "the cursor follows its entry through a sort");
         Key (K.F6, Control => True);
         Wait ("size descending");
         Check (Names (Left_Pane) = "..|nested|beta|alpha|Zeta.md|file2.txt|file10.txt|",
                "again reverses (folders stay first): " & Names (Left_Pane));
         Key (K.F3, Control => True);
         Wait ("by name");
      end;

      --  Panes.
      Key (K.Tab);
      Check (Active (View.all) = Right_Pane, "Tab switches panes");
      Key (K.End_Key);
      Check (Cursor_Row (View.all, Right_Pane) = Rows (View.all, Right_Pane), "End goes to the last row");
      Key (K.Page_Up);
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
      Key (K.Tab);
      Check (Active (View.all) = Right_Pane, "right pane active for the drag");
      Key (K.Home);
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
      Key (K.Tab);

      --  The quick viewer: Enter on a file reads it through the queue.
      Key (K.Backspace);
      Wait ("back to the fixture");
      Key (K.Home);
      for N in 1 .. 4 loop
         Key (K.Down);
      end loop;
      Check (Cursor_Name (View.all, Left_Pane) = "file2.txt", "cursor on a file: " & Cursor_Name (View.all, Left_Pane));
      Key (K.Enter);
      Wait ("viewer");
      Check (Viewer_Open (View.all) and then Viewer_Line (View.all, 1) = [1 .. 300 => 'x'],
             "Enter on a file shows its text");
      Save_PPM (Screen, "build/files-viewer.ppm");
      Key (K.Escape);
      Check (not Viewer_Open (View.all), "Escape closes the viewer");
      --  A read that misses its deadline ends visibly, and nothing stays stuck.
      Files_Mock_Service.Set_Latency (2_500_000);
      Key (K.Enter);
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
      Key (K.Enter);
      Wait ("viewer again");
      Check (Viewer_Open (View.all) and then Viewer_Line (View.all, 1)'Length = 300, "the viewer works after a timeout");
      Key (K.Escape);

      --  Toolbar, context menu and drawer: the service's places (the mock's
      --  three), then the platform's own shortcuts.
      Add_Place (View.all, "Fixture", "@scratch:0/fixture", CuBit.UI.Icons.Folder);
      Add_Place (View.all, "Synthetic", "@synthetic:500/", CuBit.UI.Icons.Network);
      Frame;
      Check (Drawer_Open (View.all) and then Drawer_Row (View.all, 1) = "Places"
             and then Drawer_Row (View.all, 2) = "@host:0/" and then Drawer_Row (View.all, 5) = "Fixture",
             "the drawer lists places, then shortcuts: " & Drawer_Row (View.all, 2) & " " & Drawer_Row (View.all, 5));
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
         Click (Drawer_Box.x + 40, Drawer_Box.y + 26 + 4 * 24 + 12);
         Wait ("drawer place");
         Check (Path (View.all, Left_Pane) = "@synthetic:500/", "a drawer place opens: " & Path (View.all, Left_Pane));
         Click (Drawer_Box.x + 40, Drawer_Box.y + 26 + 3 * 24 + 12);
         Wait ("back to the fixture");
         Check (Path (View.all, Left_Pane) = "@scratch:0/fixture", "back to the fixture: " & Path (View.all, Left_Pane));
         --  Bookmarks.
         Key (K.D, Control => True);
         Check (Bookmarked (View.all, Left_Pane), "Ctrl+D bookmarks the folder");
         Key (K.B, Control => True);
         Check (not Drawer_Open (View.all), "Ctrl+B hides the drawer");
         Key (K.B, Control => True);
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
            Key (K.Down);
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
                   and then F10_Box.y + F10_Box.h <= HEIGHT
                   and then F10_Box.h <= CuBit.UI_Metrics.Compact_Row_Height,
                   "F9 and F10 are toolkit buttons along the bottom");
            Click (F9_Box.x + F9_Box.w / 2, F9_Box.y + F9_Box.h / 2);
            Check (not Pane_Shown (View.all, Right_Pane), "a click on F9 shows one pane");
            Click (F9_Box.x + F9_Box.w / 2, F9_Box.y + F9_Box.h / 2);
            Check (Pane_Shown (View.all, Right_Pane), "and again shows both");
            Save_PPM (Screen, "build/files-keys.ppm");
            Key (K.F12, Control => True);
            Check (not Keys_Shown (View.all) and then Center (FUNCTION_KEY_FIRST + 8).w = 0,
                   "Ctrl+F12 hides the bar");
            Key (K.F12, Control => True);
            Check (Keys_Shown (View.all) and then Center (FUNCTION_KEY_FIRST + 8).w > 0, "and shows it");
         end;
         --  UI-051 / UI-052: the key bar's captions are whole (F2's Refresh
         --  was cut before), hints come from the keymap, a remap moves
         --  dispatch and every hint at once, held modifiers reveal layers.
         declare
            use type K.Chord, K.Modifier, K.Modifier_Set, K.Key;
            use type Files_Keymaps.Map.Bind_Result;
            Result : Files_Keymaps.Map.Bind_Result;
            Busy, Changed : Boolean;
            T0 : constant Unsigned_64 := 90_000_000;
            HOLD_US : constant Unsigned_64 := CuBit.UI.Modifier_Reveal.HOLD_DELAY_MS * 1_000;
            Ctrl_Only : constant K.Modifier_Set := [K.Ctrl => True, others => False];
            Alt_Only : constant K.Modifier_Set := [K.Alt => True, others => False];
            procedure Release (Name : K.Key; Shift, Control, Alt : Boolean := False) is
            begin
               Handle (View.all, (Kind => Key_Release, Key => Name, Shift => Shift, Control => Control, Alt => Alt,
                                  others => <>), Map.all, Redraw);
               Frame;
            end Release;
            procedure Hold_Until (Us : Unsigned_64) is
            begin
               Pump (View.all, 1_000, Us, Busy, Changed);
               Frame;
            end Hold_Until;
            procedure Bar_Fits (Label : String) is
            begin
               for F in K.Function_Key loop
                  Check (Key_Cell_Needs (View.all, F) > 0
                         and then Key_Cell (View.all, F).w >= Key_Cell_Needs (View.all, F),
                         Label & ": " & F'Image & " shows its caption and keycap whole");
               end loop;
            end Bar_Fits;
         begin
            Bar_Fits ("wide");
            Check (Key_Cell_Caption (View.all, K.F2) = "Refresh"
                   and then Key_Cell (View.all, K.F2).w >= CuBit.UI.UI_Text_Width ("Refresh"),
                   "F2 shows Refresh, measured whole in the real font");
            Check (Key_Bar_Rows (View.all) = 1, "a wide window: one row of keys");
            --  The context menu's keycaps are the keymap's hints.
            Key (K.Menu);
            Check (Menu_Open (View.all), "the Menu key opens the context menu");
            Check (Menu_Row_Hint (View.all, 1) /= K.No_Chord, "its first item shows a keycap");
            Key (K.Escape);

            --  UI-035: the wheel scrolls the pane under the pointer, through
            --  the toolkit's scroll container; elsewhere it scrolls nothing.
            declare
               Rows_Box : constant CuBit.UI.Rect := CuBit.UI.Controls.Bounds (Map.all, ROWS_ID (Left_Pane));
               Right_Box : constant CuBit.UI.Rect := CuBit.UI.Controls.Bounds (Map.all, ROWS_ID (Right_Pane));
               Left_Top : constant Natural := Top_Row (View.all, Left_Pane);
               Right_Top : constant Natural := Top_Row (View.all, Right_Pane);
               procedure Wheel_At (X, Y : Natural; Steps : Integer) is
               begin
                  Handle (View.all, (Kind => Wheel, X => X, Y => Y, Steps => Steps, others => <>), Map.all, Redraw);
                  Frame;
               end Wheel_At;
            begin
               Wheel_At (Rows_Box.x + 20, Rows_Box.y + 20, -2);
               Check (Top_Row (View.all, Left_Pane) = Left_Top + 6
                      and then Top_Row (View.all, Right_Pane) = Right_Top,
                      "the wheel scrolls the left pane under it, three rows a notch:"
                      & Natural'Image (Top_Row (View.all, Left_Pane)));
               Wheel_At (Right_Box.x + 20, Right_Box.y + 20, -1);
               Check (Top_Row (View.all, Right_Pane) = Right_Top + 3, "and the right pane under it");
               Wheel_At (5, 5, -1);
               Check (Top_Row (View.all, Left_Pane) = Left_Top + 6 and then Top_Row (View.all, Right_Pane) = Right_Top + 3,
                      "over the toolbar it scrolls nothing");
               Wheel_At (Rows_Box.x + 20, Rows_Box.y + 20, 2);
               Wheel_At (Right_Box.x + 20, Right_Box.y + 20, 1);
               Check (Top_Row (View.all, Left_Pane) = Left_Top, "back up");
            end;

            --  Held Ctrl: after the hold delay the bar shows the Ctrl layer.
            Set_Clock (View.all, T0);
            Key (K.Control_Key, Control => True);
            Check (Revealed (View.all) = K.No_Modifiers, "nothing is revealed at once");
            Check (Next_Deadline_Us (View.all) <= T0 + HOLD_US, "the view asks to be woken for the reveal");
            Hold_Until (T0 + HOLD_US);
            Check (Revealed (View.all) = Ctrl_Only, "holding Ctrl reveals the Ctrl layer");
            Check (Key_Cell_Caption (View.all, K.F3) = "Name" and then Key_Cell_Caption (View.all, K.F6) = "Size",
                   "Ctrl+F3 .. F6 sort: " & Key_Cell_Caption (View.all, K.F3));
            Bar_Fits ("Ctrl layer");
            Save_PPM (Screen, "build/files-keys-ctrl.ppm");
            Set_Clock (View.all, T0 + 2 * HOLD_US);
            Release (K.Control_Key);
            Check (Revealed (View.all) = K.No_Modifiers and then Key_Cell_Caption (View.all, K.F3) = "View",
                   "releasing Ctrl goes back to the plain keys");
            --  Held Shift: Shift+F6 renames.
            Set_Clock (View.all, T0 + 3 * HOLD_US);
            Key (K.Left_Shift_Key, Shift => True);
            Hold_Until (T0 + 4 * HOLD_US);
            Check (Key_Cell_Caption (View.all, K.F6) = "Rename", "holding Shift shows Shift+F6 Rename");
            Set_Clock (View.all, T0 + 5 * HOLD_US);
            Release (K.Left_Shift_Key);
            --  A quick chord reveals nothing.
            Set_Clock (View.all, T0 + 6 * HOLD_US);
            Key (K.Control_Key, Control => True);
            Key (K.B, Control => True);
            Hold_Until (T0 + 8 * HOLD_US);
            Check (Revealed (View.all) = K.No_Modifiers, "a quick Ctrl+B reveals nothing");
            Release (K.Control_Key);
            Key (K.B, Control => True);
            --  Held Alt: the toolbar's icon-only buttons show their keycaps.
            Set_Clock (View.all, T0 + 9 * HOLD_US);
            Key (K.Alt_Key, Alt => True);
            Hold_Until (T0 + 10 * HOLD_US);
            Check (Revealed (View.all) = Alt_Only, "holding Alt reveals");
            Check (Tool_Revealed (View.all, Tool_Back) and then Tool_Revealed (View.all, Tool_Refresh),
                   "Back [Alt][Left] and Refresh [F2] show under the toolbar");
            Check (not Tool_Revealed (View.all, Tool_Columns), "Columns has no chord: no keycap");
            Save_PPM (Screen, "build/files-alt-toolbar.ppm");
            Set_Clock (View.all, T0 + 11 * HOLD_US);
            Release (K.Alt_Key);
            Check (not Tool_Revealed (View.all, Tool_Back), "releasing Alt hides them");

            --  A remap: dispatch, the key bar, the menu and the tooltip follow.
            Check (Hint (View.all, Files_Keymaps.Refresh) = +K.F2, "Refresh's hint is F2");
            Remap (View.all, Files_Keymaps.Refresh, +K.F2, +K.F11, Result);
            Frame;
            Check (Result = Files_Keymaps.Map.Bound, "F2 -> F11 for Refresh");
            Check (Hint (View.all, Files_Keymaps.Refresh) = +K.F11, "the hint is F11");
            Check (Key_Cell_Caption (View.all, K.F2) = "", "F2's cell is unbound now");
            Key (K.F2);
            Check (Load (View.all, Active (View.all)) = Loaded, "F2 no longer refreshes");
            Key (K.F11);
            Check (Load (View.all, Active (View.all)) in Opening | Reading, "F11 refreshes");
            Wait ("refresh by F11");
            --  The menu's keycaps follow a remap too (Copy on an entry's menu).
            Remap (View.all, Files_Keymaps.Copy, +K.F5, K.Shift + K.F5, Result);
            Check (Result = Files_Keymaps.Map.Bound, "F5 -> Shift+F5 for Copy");
            Key (K.Menu);
            declare
               Found : Boolean := False;
            begin
               for Row in 1 .. 12 loop
                  Found := Found or else Menu_Row_Hint (View.all, Row) = K.Shift + K.F5;
               end loop;
               Check (Found, "the context menu's Copy shows Shift+F5");
            end;
            Key (K.Escape);
            Remap (View.all, Files_Keymaps.Copy, K.Shift + K.F5, +K.F5, Result);
            declare
               Box : constant CuBit.UI.Rect := Center (Tool_ID (Tool_Refresh));
               T1 : constant Unsigned_64 := T0 + 20 * HOLD_US;
            begin
               Set_Clock (View.all, T1);
               Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Move, Box.x + 4, Box.y + 200, 0);
               Frame;
               Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Move, Box.x + 4, Box.y + 4, 0);
               Frame;
               Pump (View.all, 1_000, T1 + 600_000, Busy, Changed);
               Frame;
               Check (Tooltip_Hint (View.all) = +K.F11, "the toolbar tooltip shows F11");
               Key (K.Down);
               Key (K.Up);
            end;
            --  Conflicts are refused when made.
            Remap (View.all, Files_Keymaps.Refresh, +K.F11, +K.F5, Result);
            Check (Result = Files_Keymaps.Map.Taken, "F5 is Copy's: refused");
            Remap (View.all, Files_Keymaps.Copy, +K.F5, K.Ctrl + K.C, Result);
            Check (Result = Files_Keymaps.Map.Reserved_Taken, "Ctrl+C is the clipboard's: refused");
            Remap (View.all, Files_Keymaps.Copy, +K.F5, K.Super + K.C, Result);
            Check (Result = Files_Keymaps.Map.Other_Layer, "Super chords are the Desktop's: refused");
            Remap (View.all, Files_Keymaps.Refresh, +K.F11, +K.F2, Result);
            Frame;
            Check (Key_Cell_Caption (View.all, K.F2) = "Refresh", "remapped back");
         end;
         --  Context menu on a file: View is first; Enter chooses it.
         Key (K.Home);
         for N in 1 .. 4 loop
            Key (K.Down);
         end loop;
         declare
            Rows_Box : constant CuBit.UI.Rect := CuBit.UI.Controls.Bounds (Map.all, ROWS_ID (Left_Pane));
            Y : constant Natural := Rows_Box.y + (Cursor_Row (View.all, Left_Pane) - 1) * 20 + 10;
         begin
            Click (Rows_Box.x + 60, Y, Secondary => True);
            Check (Menu_Open (View.all), "a right-click opens the context menu");
            Save_PPM (Screen, "build/files-menu.ppm");
            Key (K.Down);
            Check (Menu_Selected (View.all) = 1, "Down selects the first item");
            Key (K.Enter);
            Wait ("menu view");
            Check (not Menu_Open (View.all) and then Viewer_Open (View.all), "Enter on View opens the viewer");
            Key (K.Escape);
            --  On empty space: Sort by > Size through the keyboard.
            Click (Rows_Box.x + 60, Rows_Box.y + Rows_Box.h - 10, Secondary => True);
            Check (Menu_Open (View.all), "a right-click on empty space opens a menu");
            for N in 1 .. 4 loop
               Key (K.Down);
            end loop;
            Key (K.Right);
            Save_PPM (Screen, "build/files-submenu.ppm");
            Key (K.End_Key);
            Key (K.Enter);
            Wait ("menu sort");
            Check (Rule (View.all, Left_Pane).Key = Files_Order.By_Size, "Sort by > Size from the submenu");
            Key (K.F3, Control => True);
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
               Key (K.Escape);
            end if;
         end;
      end;

      --  Columns: the header's menu adds one; a header drag moves one.
      declare
         Base : constant CuBit.UI.Controls.Control_ID := COLUMNS_BASE (Left_Pane) + CuBit.UI.Tables.MAX_COLUMNS;
         Name_Cell : constant CuBit.UI.Rect := CuBit.UI.Controls.Bounds (Map.all, Base);
      begin
         Check (Column_Titles (View.all, Left_Pane) = "Name|Size|Modified|", "default columns");
         --  Name takes the width Size and Modified (sized to their text) leave.
         declare
            Size_Cell : constant CuBit.UI.Rect := CuBit.UI.Controls.Bounds (Map.all, Base + 1);
            Modified_Cell : constant CuBit.UI.Rect := CuBit.UI.Controls.Bounds (Map.all, Base + 2);
            Padding : constant Natural := 2 * 5;
         begin
            --  Header cells' click targets leave the 7-pixel edge grab areas out.
            Check (Size_Cell.x = Name_Cell.x + Name_Cell.w + 7 and then Modified_Cell.x = Size_Cell.x + Size_Cell.w + 7,
                   "columns are adjacent");
            Check (Name_Cell.w > Size_Cell.w + Modified_Cell.w,
                   "Name is the flexible column:" & Name_Cell.w'Image & Size_Cell.w'Image & Modified_Cell.w'Image);
            Check (Size_Cell.w + 7 >= CuBit.UI.UI_Text_Width ("1023 MiB") + Padding
                   and then Modified_Cell.w + 3 >= CuBit.UI.UI_Text_Width ("0000-00-00 00:00") + Padding,
                   "Size and Modified fit their text");
            Check (Size_Cell.w < 100 and then Modified_Cell.w < 150, "Size and Modified are not padded out");
            --  The Name|Size edge trades width with Size; Modified stays.
            declare
               Edge : constant CuBit.UI.Rect := CuBit.UI.Controls.Bounds (Map.all, COLUMNS_BASE (Left_Pane));
               Name_Before : constant Natural := Name_Cell.w;
            begin
               Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Press, Edge.x + Edge.w / 2,
                        Edge.y + 5, 6_900);
               Frame;
               Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Move, Edge.x + Edge.w / 2 - 30,
                        Edge.y + 5);
               Frame;
               Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Release, Edge.x + Edge.w / 2 - 30,
                        Edge.y + 5, 6_950);
               Frame;
               Check (CuBit.UI.Controls.Bounds (Map.all, Base).w = Name_Before - 30
                      and then CuBit.UI.Controls.Bounds (Map.all, Base + 1).w = Size_Cell.w + 30
                      and then CuBit.UI."=" (CuBit.UI.Controls.Bounds (Map.all, Base + 2), Modified_Cell),
                      "the Name edge follows the pointer: Size gives way, Modified stays:"
                      & Name_Before'Image & CuBit.UI.Controls.Bounds (Map.all, Base).w'Image
                      & Size_Cell.w'Image & CuBit.UI.Controls.Bounds (Map.all, Base + 1).w'Image
                      & Modified_Cell.x'Image & CuBit.UI.Controls.Bounds (Map.all, Base + 2).x'Image);
            end;
         end;
         Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Press, Name_Cell.x + 20, Name_Cell.y + 5,
                  7_000, Secondary => True);
         Frame;
         Check (Menu_Open (View.all), "a right-click on the header opens the columns menu");
         Save_PPM (Screen, "build/files-columns-menu.ppm");
         for N in 1 .. 6 loop
            Key (K.Down);
         end loop;
         Key (K.Enter);
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
      Key (K.N, Control => True);
      Wait ("third pane");
      Check (Pane_Count (View.all) = 3 and then Active (View.all) = 2, "Ctrl+N adds a pane after the active one");
      Save_PPM (Screen, "build/files-three-panes.ppm");
      Key (K.Tab);
      Check (Active (View.all) = 3, "Tab cycles to the next pane");
      Key (K.Tab);
      Check (Active (View.all) = 1, "and wraps");
      Key (K.Tab, Shift => True);
      Check (Active (View.all) = 3, "Shift+Tab goes back");
      Key (K.F9);
      Check (Pane_Shown (View.all, 3) and then not Pane_Shown (View.all, 1) and then not Pane_Shown (View.all, 2),
             "F9: single-pane mode");
      Save_PPM (Screen, "build/files-single-pane.ppm");
      Key (K.F9);
      Check (Pane_Shown (View.all, 1) and then Pane_Shown (View.all, 2), "F9 again shows them all");
      Key (K.F1, Control => True);
      Check (not Pane_Shown (View.all, 1), "Ctrl+F1 hides the first pane");
      Key (K.F1, Control => True);
      Check (Pane_Shown (View.all, 1), "and shows it again");
      Key (K.W, Control => True, Shift => True);
      Check (Pane_Count (View.all) = 2, "Ctrl+Shift+W closes the active pane");
      if Active (View.all) /= Left_Pane then
         Key (K.Tab);
      end if;

      --  Tabs.
      Key (K.T, Control => True);
      Check (Tab_Count (View.all, Left_Pane) = 2 and then Current_Tab (View.all, Left_Pane) = 2, "Ctrl+T opens a tab");
      Key (K.Down);
      Key (K.Down);
      Key (K.Enter);
      Wait ("tab 2 navigates");
      declare
         Tab_Two : constant String := Path (View.all, Left_Pane);
      begin
         Key (K.Tab, Control => True);
         Wait ("tab 1");
         Check (Current_Tab (View.all, Left_Pane) = 1 and then Path (View.all, Left_Pane) = "@scratch:0/fixture",
                "Ctrl+Tab switches tabs: " & Path (View.all, Left_Pane));
         Save_PPM (Screen, "build/files-tabs.ppm");
         Key (K.Page_Down, Control => True, Shift => True);
         Check (Current_Tab (View.all, Left_Pane) = 2, "Ctrl+Shift+PgDn moves the tab right");
         Key (K.W, Control => True);
         Wait ("tab closed");
         Check (Tab_Count (View.all, Left_Pane) = 1 and then Path (View.all, Left_Pane) = Tab_Two,
                "Ctrl+W closes the tab: " & Path (View.all, Left_Pane));
         Key (K.T, Control => True, Shift => True);
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
            Key (K.T, Control => True, Shift => True);
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
            Key (K.T, Control => True, Shift => True);
            Wait ("restored again");
         end;
         Key (K.W, Control => True);
         Wait ("back to one tab");
         Key (K.Backspace);
         Wait ("up to the fixture");
      end;

         --  A narrow window wraps the bar (F1..F4, then F5..F10) and
         --  never cuts or hides a caption.
         declare
            NARROW_WIDTH : constant := 600;
            Narrow : constant Surface := New_Surface (NARROW_WIDTH, HEIGHT);
         begin
            if Viewer_Open (View.all) then
               Key (K.Escape);
            end if;
            Handle (View.all, (Kind => Resize, X => NARROW_WIDTH, Y => HEIGHT, others => <>), Map.all, Redraw);
            Take_Damage (View.all, Damage);
            Render (View.all, Canvas (Narrow), Bounds (Narrow), UI.all, Map.all);
            Check (Key_Bar_Rows (View.all) = 2, "a narrow window wraps the key bar onto two rows:"
                   & Natural'Image (Key_Bar_Rows (View.all)));
            for F in K.Function_Key loop
               Check (Key_Cell_Needs (View.all, F) > 0
                      and then Key_Cell (View.all, F).w >= Key_Cell_Needs (View.all, F),
                      "narrow: " & F'Image & " shows its caption and keycap whole");
            end loop;
            Check (Key_Cell (View.all, K.F5).y > Key_Cell (View.all, K.F4).y
                   and then Key_Cell (View.all, K.F5).y = Key_Cell (View.all, K.F10).y,
                   "F1..F4 on the first row, F5..F10 on the second");
            for F in K.Function_Key loop
               Check (Key_Cell (View.all, F).x + Key_Cell (View.all, F).w <= NARROW_WIDTH,
                      "narrow: " & F'Image & " inside the window");
            end loop;
            Save_PPM (Narrow, "build/files-keys-narrow.ppm");
            --  The same in the dark scheme (screenshots for review).
            CuBit.UI.Set_Color_Scheme (CuBit.Appearance.Alloy_Dark);
            Handle (View.all, (Kind => Resize, X => NARROW_WIDTH, Y => HEIGHT, others => <>), Map.all, Redraw);
            Render (View.all, Canvas (Narrow), Bounds (Narrow), UI.all, Map.all);
            Save_PPM (Narrow, "build/files-keys-narrow-dark.ppm");
            Handle (View.all, (Kind => Resize, X => WIDTH, Y => HEIGHT, others => <>), Map.all, Redraw);
            Frame;
            Save_PPM (Screen, "build/files-keys-dark.ppm");
            CuBit.UI.Set_Color_Scheme (CuBit.Appearance.Alloy_Light);

            Handle (View.all, (Kind => Resize, X => WIDTH, Y => HEIGHT, others => <>), Map.all, Redraw);
            Frame;
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
               Key (K.Tab);
            end if;
            Key (K.Home);
            for N in 1 .. Rows (View.all, Pane) loop
               exit when Cursor_Name (View.all, Pane) = Name;
               Key (K.Down);
            end loop;
            return Cursor_Name (View.all, Pane) = Name;
         end Go_To_Name;
         procedure Type_Line (Text : String) is
         begin
            Type_Text (Text);
         end Type_Line;
      begin
         --  F7 in the right pane.
         Key (K.Tab);
         Key (K.F7);
         Type_Line ("made");
         Key (K.Enter);
         Wait ("mkdir");
         Check (Exists (Out_Root & "/made") and then Kind (Out_Root & "/made") = Directory, "F7 makes a folder");
         Check (Names (Right_Pane) = "..|made|", "the new folder is listed: " & Names (Right_Pane));
         --  F5: a file, then a folder tree.
         Check (Go_To_Name (Left_Pane, "file2.txt"), "cursor on file2.txt");
         Key (K.F5);
         Save_PPM (Screen, "build/files-confirm-copy.ppm");
         Key (K.Enter);
         Wait ("copy file");
         Check (Exists (Out_Root & "/file2.txt") and then Size (Out_Root & "/file2.txt") = 301,
                "F5 copies a file: " & Status_Message (View.all));
         Check (Go_To_Name (Left_Pane, "alpha"), "cursor on alpha");
         Key (K.F5);
         Key (K.Enter);
         Wait ("copy tree");
         Check (Exists (Out_Root & "/alpha/inside.txt"), "F5 copies a folder tree: " & Status_Message (View.all));
         --  The same file again: the conflict is asked; K keeps both.
         Check (Go_To_Name (Left_Pane, "file2.txt"), "cursor on file2.txt again");
         Key (K.F5);
         Key (K.Enter);
         Wait ("conflict");
         Save_PPM (Screen, "build/files-conflict.ppm");
         --  The conflict's choices are keys (its keymap: K keeps both).
         Key (K.K);
         Wait ("keep both");
         Check (Exists (Out_Root & "/file2 (2).txt"), "keep both makes file2 (2).txt: " & Status_Message (View.all));
         --  Overwrite chosen up front.
         Key (K.F5);
         Key (K.O);
         Key (K.Enter);
         Wait ("overwrite");
         Check (not Exists (Out_Root & "/file2 (3).txt") and then Size (Out_Root & "/file2.txt") = 301,
                "overwrite replaces: " & Status_Message (View.all));
         --  F6 within a volume renames: Zeta.md leaves the left pane.
         Check (Go_To_Name (Left_Pane, "Zeta.md"), "cursor on Zeta.md");
         Key (K.F6);
         Key (K.Enter);
         Wait ("move");
         Check (Exists (Out_Root & "/Zeta.md") and then not Exists (Scratch_Root & "/" & FIXTURE & "/Zeta.md"),
                "F6 moves: " & Status_Message (View.all));
         --  Shift+F6 renames in place.
         Check (Go_To_Name (Right_Pane, "Zeta.md"), "cursor on the moved file");
         Key (K.F6, Shift => True);
         for N in 1 .. 7 loop
            Key (K.Backspace);
         end loop;
         Type_Line ("renamed.md");
         Key (K.Enter);
         Wait ("rename");
         Check (Exists (Out_Root & "/renamed.md"), "Shift+F6 renames: " & Status_Message (View.all));
         --  F8 deletes a tree after confirmation.
         Check (Go_To_Name (Right_Pane, "alpha"), "cursor on the copied alpha");
         Key (K.F8);
         Save_PPM (Screen, "build/files-confirm-delete.ppm");
         Key (K.Enter);
         Wait ("delete");
         Check (not Exists (Out_Root & "/alpha"), "F8 deletes a folder tree: " & Status_Message (View.all));
         --  Esc dismisses a prompt without acting.
         Check (Go_To_Name (Right_Pane, "made"), "cursor on made");
         Key (K.F8);
         Key (K.Escape);
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
         Key (K.Tab);
         Key (K.R, Control => True);
         Wait ("big file listed");
         Check (Go_To_Name (Left_Pane, "big.bin"), "cursor on big.bin");
         Files_Mock_Service.Set_Latency (20_000);
         Key (K.F5);
         Key (K.Enter);
         declare
            Busy, Changed : Boolean;
         begin
            for N in 1 .. 8 loop
               Pump (View.all, 20_000, Now_Us, Busy, Changed);
               delay 0.02;
            end loop;
         end;
         Key (K.Escape);
         Wait ("cancel");
         Files_Mock_Service.Set_Latency (0);
         Check (not Exists (Out_Root & "/big.bin") and then Ada.Strings.Fixed.Index (Status_Message (View.all), "Cancel") > 0,
                "a cancelled copy leaves no partial file: " & Status_Message (View.all));
         --  Into a read-only place: refused, said so.
         Close (View.all);
         Initialize (View.all, CAPACITY, NAME_BYTES, "@scratch:0/" & FIXTURE, "@host:0/tmp");
         Wait ("read-only target");
         Check (Go_To_Name (Left_Pane, "file10.txt"), "cursor on file10.txt");
         Key (K.F5);
         Key (K.Enter);
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

      --  No folders named: both panes start at the first place (the
      --  granted scopes whose volume is present), and each is in the drawer.
      Close (View.all);
      Initialize (View.all, CAPACITY, NAME_BYTES, "", "");
      Wait ("places");
      Check (Path (View.all, Left_Pane) = "@host:0/" and then Path (View.all, Right_Pane) = "@host:0/",
             "panes start at the first place: " & Path (View.all, Left_Pane) & " " & Path (View.all, Right_Pane));
      Check (Drawer_Row (View.all, 2) = "@host:0/" and then Drawer_Row (View.all, 3) = "@scratch:0/",
             "each place is in the drawer: " & Drawer_Row (View.all, 2));

      --  Authority and hostility: a folder from before that is refused (an
      --  unknown place, '..') is not shown; the pane goes to the first place.
      Key (K.Tab);
      Close (View.all);
      Initialize (View.all, CAPACITY, NAME_BYTES, "@nowhere:0/", "@host:0/../../etc");
      Wait ("denied");
      Check (Load (View.all, Left_Pane) = Loaded and then Path (View.all, Left_Pane) = "@host:0/"
             and then Load (View.all, Right_Pane) = Loaded and then Path (View.all, Right_Pane) = "@host:0/",
             "unknown places and '..' are refused: the first place instead: " & Path (View.all, Left_Pane) & " "
             & Path (View.all, Right_Pane));
      Save_PPM (Screen, "build/files-denied.ppm");
      Close (View.all);

      --  The volumes this machine has (docs/files-app.md, "Places"): the
      --  manifest grants @nvme:0/ and @mem:0/ (in that order) everywhere.
      declare
         use Files_Mock_Service;
         function Drawer_Places return String is
            Result : String (1 .. 512);
            Length : Natural := 0;
         begin
            for Row in 2 .. 8 loop
               declare
                  Caption : constant String := Drawer_Row (View.all, Row);
               begin
                  exit when Caption'Length = 0 or else Caption (Caption'First .. Caption'First) = "B";
                  Result (Length + 1 .. Length + Caption'Length + 1) := Caption & "|";
                  Length := Length + Caption'Length + 1;
               end;
            end loop;
            return Result (1 .. Length);
         end Drawer_Places;
         procedure Start (Volumes : Volume_Grants; Left, Right : String; Label : String) is
         begin
            Grant_Volumes (Volumes);
            Initialize (View.all, CAPACITY, NAME_BYTES, Left, Right);
            Wait (Label);
            Frame;
         end Start;
      begin
         Ada.Directories.Create_Path (Volume_Folder ("mem:0") & "/kept");
         Ada.Directories.Create_Path (Volume_Folder ("nvme:0") & "/lost+found");

         --  The live image: no NVMe driver, so no NVMe place.
         Start ([Volume ("nvme:0", Present => False), Volume ("mem:0", Present => True)], "", "", "live image");
         Check (Drawer_Places = "Live workspace|", "absent volume not listed: " & Drawer_Places);
         Check (Path (View.all, Left_Pane) = "@mem:0/" and then Path (View.all, Right_Pane) = "@mem:0/"
                and then Load (View.all, Left_Pane) = Loaded and then Load (View.all, Right_Pane) = Loaded,
                "only @mem:0: both panes at the live workspace: " & Path (View.all, Left_Pane));
         Check (Status_Message (View.all) = "", "no message at start: " & Status_Message (View.all));
         Save_PPM (Screen, "build/files-live-image.ppm");
         Close (View.all);

         --  An installed system without a live workspace.
         Start ([Volume ("nvme:0", Present => True), Volume ("mem:0", Present => False)], "", "", "installed");
         Check (Drawer_Places = "NVMe volume 0|", "only @nvme:0 listed: " & Drawer_Places);
         Check (Path (View.all, Left_Pane) = "@nvme:0/" and then Path (View.all, Right_Pane) = "@nvme:0/"
                and then Names (Left_Pane) = "lost+found|",
                "only @nvme:0: both panes at the NVMe volume: " & Path (View.all, Left_Pane) & " " & Names (Left_Pane));
         Close (View.all);

         --  Both: the first in the manifest's order.
         Start ([Volume ("nvme:0", Present => True), Volume ("mem:0", Present => True)], "", "", "both");
         Check (Drawer_Places = "NVMe volume 0|Live workspace|", "both listed, in order: " & Drawer_Places);
         Check (Path (View.all, Left_Pane) = "@nvme:0/" and then Path (View.all, Right_Pane) = "@nvme:0/",
                "both: the panes start at the first place");
         Close (View.all);

         --  The folders shown last come back only while they exist.
         Start ([Volume ("nvme:0", Present => True), Volume ("mem:0", Present => True)],
                "@mem:0/kept", "@mem:0/gone", "restore");
         Check (Path (View.all, Left_Pane) = "@mem:0/kept" and then Load (View.all, Left_Pane) = Loaded,
                "a folder that still exists is restored: " & Path (View.all, Left_Pane));
         Check (Path (View.all, Right_Pane) = "@nvme:0/" and then Load (View.all, Right_Pane) = Loaded,
                "a folder that is gone is not: the first place: " & Path (View.all, Right_Pane));
         Close (View.all);
         Start ([Volume ("nvme:0", Present => False), Volume ("mem:0", Present => True)],
                "@nvme:0/lost+found", "@mem:0/kept", "restore on the live image");
         Check (Path (View.all, Left_Pane) = "@mem:0/" and then Load (View.all, Left_Pane) = Loaded,
                "a folder on a volume that is absent now: the first place: " & Path (View.all, Left_Pane));
         Check (Path (View.all, Right_Pane) = "@mem:0/kept", "the other pane's folder is restored");
         Close (View.all);

         --  Nothing granted is present: the panes say so, muted, in words.
         Start ([Volume ("nvme:0", Present => False)], "", "", "no volume");
         Check (Load (View.all, Left_Pane) = Failed and then Path (View.all, Left_Pane) = "",
                "no volume: the pane says so");
         Save_PPM (Screen, "build/files-no-volume.ppm");
         Close (View.all);
         Grant_Defaults;
      end;
   end Run;
end Files_View_Tests;
