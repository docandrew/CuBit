with Ada.Command_Line;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Appearance;
with CuBit.UI;
with CuBit.UI.Menus;
with Text_Commands; use Text_Commands;
with Text_Cursors;
with Text_Documents; use Text_Documents;
with Text_Editors;
with Text_Limits; use Text_Limits;
with Text_Marks;
with Text_Syntax;
with Text_Shell; use Text_Shell;
with Text_Host_Files; use Text_Host_Files;
with Text_Host_Support; use Text_Host_Support;
with Text_Input;
with CuBit.UI.Input;
with Text_Test_Support; use Text_Test_Support;

--  The Text app through its shell (docs/text-editor.md): real files through
--  the host stand-in, keys and pointer events, rendering into memory.
--  Checks state and repaint counts; with --screens it also writes the
--  screenshot set (build/screen-*.ppm; run.sh converts them to PNG).
procedure Text_View_Tests is
   use type Text_Syntax.Language;
   REPO : constant String := "/home/doc/git/cubit";
   SCRATCH : constant String := "/home/doc/cubit-build-tmp/text-editor/files";
   WIDTH : constant := 1400;
   HEIGHT : constant := 860;
   Screens : constant Boolean :=
     Ada.Command_Line.Argument_Count >= 1 and then Ada.Command_Line.Argument (1) = "--screens";

   Host : aliased Host_Files;
   Target : constant Surface := New_Surface (WIDTH, HEIGHT);
   App : Shell;
   Ok : Boolean;

   procedure Shot (Name : String) is
   begin
      Frame (App, Target);
      if Screens then
         Save_PPM (Target, "build/screen-" & Name & ".ppm");
         Note ("screenshot " & Name);
      end if;
   end Shot;

   procedure Write_File (Path, Text : String) is
      use Ada.Streams.Stream_IO;
      F : Ada.Streams.Stream_IO.File_Type;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Text'Length))
        with Import, Address => Text'Address;
   begin
      Create (F, Out_File, Path);
      Write (F, Buffer);
      Close (F);
   end Write_File;

   function Read_File (Path : String) return String is
      use Ada.Streams.Stream_IO;
      F : Ada.Streams.Stream_IO.File_Type;
   begin
      Open (F, In_File, Path);
      declare
         Result : String (1 .. Natural (Size (F)));
         Buffer : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Result'Length))
           with Import, Address => Result'Address;
         Last : Ada.Streams.Stream_Element_Offset;
      begin
         Read (F, Buffer, Last);
         Close (F);
         return Result;
      end;
   end Read_File;

   procedure Hit (Item : Text_Commands.Key; Control, Shift, Alt : Boolean := False) is
   begin
      Set_Clock (App, Now_Us);
      Handle (App, Text_Shell.Key (Item, Control, Shift, Alt));
   end Hit;

   procedure Type_Text (Text : String) is
   begin
      for C of Text loop
         Set_Clock (App, Now_Us);
         Handle (App, Typed ([1 => C]));
      end loop;
   end Type_Text;

   procedure Click (X, Y : Natural; Time_Ms : Unsigned_64; Control, Shift, Alt : Boolean := False) is
   begin
      Handle (App, (Kind => Pointer_Event, Action => Text_Shell.Press, X => X, Y => Y, Time_Ms => Time_Ms,
                    Control => Control, Shift => Shift, Alt => Alt, others => <>));
      Handle (App, (Kind => Pointer_Event, Action => Text_Shell.Release, X => X, Y => Y, Time_Ms => Time_Ms,
                    others => <>));
   end Click;

   function D return Document_Access is (Active_Document (App));

   function Line_Of_Primary return Line_Number is (Line_Of (D.all, Primary (D.all).Head));

   procedure Open_And_Wait (Name : String) is
   begin
      Open (App, Name);
      Check (Settle (App, Target), "open " & Name & ": settles");
   end Open_And_Wait;

   function Count_Lines (Text : String) return Natural is
      N : Natural := 1;
   begin
      for C of Text loop
         if C = ASCII.LF then
            N := N + 1;
         end if;
      end loop;
      return N;
   end Count_Lines;

   SAMPLE_ADA : constant String := REPO & "/userspace/lib/text/text_pieces.adb";

   --  Every pixel of the target, hashed (damage-only frames must leave the
   --  same picture as a full repaint).
   function Picture return Unsigned_64 is
      H : Unsigned_64 := 16#CBF2_9CE4_8422_2325#;
   begin
      for Y in 0 .. HEIGHT - 1 loop
         for X in 0 .. WIDTH - 1 loop
            H := (H xor Unsigned_64 (Pixel (Target, X, Y))) * 16#0000_0100_0000_01B3#;
         end loop;
      end loop;
      return H;
   end Picture;
begin
   Put_Line ("text view tests" & (if Screens then " (with screenshots)" else ""));
   Ada.Directories.Create_Path (SCRATCH);
   Install_Themes (CuBit.Appearance.Alloy_Light);
   Grant (Host, REPO, Writable => False, Label => "repo");
   Grant (Host, SCRATCH, Writable => True, Label => "scratch");
   Offer_Files (Host, Depth => 3, Limit => 4_000);
   Initialize (App, Host'Unchecked_Access, Dark => False, Edit_Bytes => 64 * 1_048_576);
   Frame (App, Target);

   Section ("opening a real Ada unit");
   Open_And_Wait (SAMPLE_ADA);
   Check (Tabs (App) = 1 and then not Loading (App), "one tab, loaded");
   Check (Content (D.all) = Read_File (SAMPLE_ADA), "content equals the file");
   Check (Natural (Line_Total (D.all)) = Count_Lines (Read_File (SAMPLE_ADA)), "line count");
   Check (Status_Message (App)'Length > 0, "status: " & Status_Message (App));
   Execute_Call (App, "(text.goto-line 58)", Ok);
   Check (Ok and then Line_Of_Primary = 58, "CCL call (text.goto-line 58) moves to line 58");
   Shot ("ada-light");

   Section ("typing repaints its own row");
   declare
      Before : constant String := Content (D.all);
   begin
      Hit (End_Key);
      Frame (App, Target);
      Type_Text ("x");
      Frame (App, Target);
      Check (Rows_Painted (App) <= 2, "one typed character repaints" & Natural'Image (Rows_Painted (App)) & " row(s)");
      Hit (Down);
      Frame (App, Target);
      Check (Rows_Painted (App) <= 2, "a cursor move repaints" & Natural'Image (Rows_Painted (App)) & " rows");
      Hit (Key_Z, Control => True);
      Check (Content (D.all) = Before, "Ctrl+Z restores the file");
      Check (not Modified (D.all), "unmodified again after undo");
      --  Rows repaint only as far as their old or new content reaches:
      --  a line that grows then shrinks, and pages of other lines, leave
      --  exactly what a full repaint draws.
      Frame (App, Target);
      Hit (Up);
      Hit (End_Key);
      Type_Text ("--  a long comment typed at the end of the line, then undone");
      Frame (App, Target);
      for Step in 1 .. 100 loop
         exit when not Modified (D.all);
         Hit (Key_Z, Control => True);
      end loop;
      Frame (App, Target);
      Hit (Page_Down);
      Frame (App, Target);
      Hit (Page_Down);
      Frame (App, Target);
      Hit (Page_Up);
      Frame (App, Target);
      declare
         Damage_Only : constant Unsigned_64 := Picture;
      begin
         Text_Editors.Invalidate (Active_Editor (App).all);
         Frame (App, Target);
         Check (Picture = Damage_Only, "damage-only frames equal a full repaint");
      end;
      Check (Content (D.all) = Before, "the file is unchanged");
   end;

   Section ("frames into rotating buffers (as CuBit's protected frames)");
   declare
      --  Two buffers presented in turn. Each frame draws into the buffer
      --  that is two frames old, clipped to this frame's damage and the
      --  damage it missed (the repair a buffer-age host hands over). The
      --  newest buffer must always equal a full repaint.
      Buffers : constant array (0 .. 1) of Surface := [New_Surface (WIDTH, HEIGHT), New_Surface (WIDTH, HEIGHT)];
      Reference : constant Surface := New_Surface (WIDTH, HEIGHT);
      Next : Natural := 0;
      Missed : array (0 .. 1) of CuBit.UI.Rect := [others => Bounds (Target)];
      Wrong_Frames : Natural := 0;
      Frames_Drawn : Natural := 0;
      Before : constant String := Content (D.all);

      procedure Rotating_Frame is
         Damage : CuBit.UI.Rect;
         Repair : CuBit.UI.Rect;
      begin
         Text_Shell.Set_Clock (App, Now_Us);
         Text_Shell.Take_Damage (App, Damage);
         Repair := CuBit.UI.Union_Rect (Damage, Missed (Next));
         if not CuBit.UI.Is_Empty (Repair) then
            Text_Shell.Render (App, Canvas (Buffers (Next), Repair), Bounds (Target));
         end if;
         Missed (Next) := (others => 0);
         Missed (1 - Next) := CuBit.UI.Union_Rect (Missed (1 - Next), Damage);
         --  The same state painted whole, into a buffer of its own (its
         --  record is new, so everything is drawn).
         Text_Shell.Render (App, Canvas (Reference), Bounds (Target));
         Frames_Drawn := Frames_Drawn + 1;
         for Y in 0 .. HEIGHT - 1 loop
            declare
               Bad : Boolean := False;
            begin
               for X in 0 .. WIDTH - 1 loop
                  if Pixel (Buffers (Next), X, Y) /= Pixel (Reference, X, Y) then
                     Bad := True;
                     exit;
                  end if;
               end loop;
               if Bad then
                  Wrong_Frames := Wrong_Frames + 1;
                  exit;
               end if;
            end;
         end loop;
         Next := 1 - Next;
      end Rotating_Frame;
   begin
      Text_Editors.Invalidate (Active_Editor (App).all);
      Rotating_Frame;
      Rotating_Frame;
      for Step in 1 .. 12 loop
         case Step mod 4 is
            when 0 => Hit (Down);
            when 1 => Type_Text ("ab");
            when 2 => Hit (Page_Down);
            when others => Hit (Up);
         end case;
         Rotating_Frame;
      end loop;
      for Step in 1 .. 100 loop
         exit when not Modified (D.all);
         Hit (Key_Z, Control => True);
      end loop;
      Rotating_Frame;
      Rotating_Frame;
      --  A menu opening, moving along the bar and closing over the text.
      for Step in 1 .. 9 loop
         case Step mod 3 is
            when 1 => Hit (F10);
            when 2 => Hit (Right);
            when others => Hit (Escape);
         end case;
         Rotating_Frame;
      end loop;
      Rotating_Frame;
      Check (Wrong_Frames = 0, "every frame equals a full repaint (" & Natural'Image (Wrong_Frames) & " of"
             & Natural'Image (Frames_Drawn) & " differ)");
      Check (Content (D.all) = Before, "the file is unchanged after the rotation");
      Text_Editors.Invalidate (Active_Editor (App).all);
      Frame (App, Target);
   end;

   Section ("a click takes its own modifiers (no stuck Ctrl)");
   declare
      use CuBit.UI.Input;
      --  Ctrl+S: Ctrl's press reaches Text, its release goes elsewhere (a
      --  dialog, the Apps menu). Then two plain clicks: one cursor.
      --  The desktop's KEYMOD_* bits (CuBit.UI.App) and Ctrl's scan code.
      SHIFT_BIT : constant Unsigned_64 := 1;
      CTRL_BIT : constant Unsigned_64 := 2;
      ALT_BIT : constant Unsigned_64 := 4;
      CTRL_SCAN_CODE : constant Unsigned_64 := 16#1D#;
      PRIMARY_BUTTON : constant Unsigned_64 := 1;
      Ctrl_Down : constant Input_Event := (kind => INPUT_KEY_DOWN, serial => 0, payload0 => CTRL_SCAN_CODE,
                                           payload1 => CTRL_BIT);
      Plain_Click : constant Input_Event :=
        (kind => INPUT_POINTER_DOWN, serial => 0, payload0 => 0, payload1 => PRIMARY_BUTTON);
      Ctrl_Click : constant Input_Event :=
        (kind => INPUT_POINTER_DOWN, serial => 0, payload0 => 0,
         payload1 => PRIMARY_BUTTON or Shift_Left (CTRL_BIT, POINTER_MODIFIERS_SHIFT));
      X, Y0 : Natural := 0;
      Shown : Boolean;

      procedure Click_With (Event : Input_Event; Row : Natural; Time_Ms : Unsigned_64) is
         M : constant Unsigned_64 := Text_Input.Modifiers_Of (Event);
      begin
         for Action in Text_Shell.Press .. Text_Shell.Release loop
            Handle (App, (Kind => Pointer_Event, Action => Action, X => X, Y => Y0 + Row * Text_Editors.ROW_HEIGHT,
                          Time_Ms => Time_Ms, Control => (M and CTRL_BIT) /= 0,
                          Shift => (M and SHIFT_BIT) /= 0, Alt => (M and ALT_BIT) /= 0, others => <>));
         end loop;
         Frame (App, Target);
      end Click_With;
   begin
      Hit (Escape);
      Execute_Call (App, "goto-line 20", Ok);
      Frame (App, Target);
      Text_Editors.Caret_Position (Active_Editor (App).all, Line_Start (D.all, 20), X, Y0, Shown);
      X := X + 2 * Text_Editors.CELL_WIDTH;
      Y0 := Y0 + 5;
      Check (Text_Input.Modifiers_Of (Ctrl_Down) = CTRL_BIT, "a key event carries Ctrl");
      Check (Text_Input.Modifiers_Of (Plain_Click) = 0, "a plain click carries no modifiers");
      Hit (Key_S, Control => True);
      Click_With (Plain_Click, 2, 10_000);
      Click_With (Plain_Click, 4, 11_000);
      Check (Selection_Count (D.all) = 1, "Ctrl+S, then two plain clicks: one cursor ("
             & Natural'Image (Selection_Count (D.all)) & ")");
      Click_With (Ctrl_Click, 6, 12_000);
      Check (Selection_Count (D.all) = 2, "a Ctrl+click adds a cursor");
      Click_With (Plain_Click, 3, 13_000);
      Check (Selection_Count (D.all) = 1, "a plain click after it leaves one");
      --  Two quick clicks far apart are two single clicks, not a double
      --  click (which would select a word).
      Click_With (Plain_Click, 2, 14_000);
      Click_With (Plain_Click, 5, 14_100);
      Check (Selection_Count (D.all) = 1 and then Text_Cursors.Is_Empty (Primary (D.all)),
             "quick clicks on different lines: one empty cursor");
   end;

   Section ("cached glyph cells equal blended glyphs");
   declare
      --  Every printable character, in inks over backgrounds that collide
      --  in the toolkit's small-cell cache many times over: the cached
      --  opaque path must paint exactly what blending over a filled
      --  background paints.
      Cells : constant Surface := New_Surface (960, 40);
      Blended : constant Surface := New_Surface (960, 40);
      Inks : constant array (1 .. 6) of CuBit.UI.Color :=
        [16#FF20_2020#, 16#FF0B_5FC6#, 16#FFB0_3A2E#, 16#FF2E_7D32#, 16#FFFF_FFFF#, 16#FF80_8080#];
      Grounds : constant array (1 .. 5) of CuBit.UI.Color :=
        [16#FFFF_FFFF#, 16#FFFF_F8D8#, 16#FFB8_D4F8#, 16#FF20_2428#, 16#FFE0_E0E0#];
      Text : String (1 .. 95);
      Wrong : Natural := 0;
   begin
      for K in Text'Range loop
         Text (K) := Character'Val (31 + K);
      end loop;
      for Round in 1 .. 2 loop
         for I in Inks'Range loop
            for G in Grounds'Range loop
               CuBit.UI.Fill_Rect (Canvas (Cells), Bounds (Cells), Grounds (G));
               CuBit.UI.Draw_Code_Text (Canvas (Cells), 4, 10, Text, Inks (I), Grounds (G));
               CuBit.UI.Fill_Rect (Canvas (Blended), Bounds (Blended), Grounds (G));
               CuBit.UI.Draw_Code_Text_Transparent (Canvas (Blended), 4, 10, Text, Inks (I));
               for Y in 0 .. 39 loop
                  for X in 0 .. 959 loop
                     if Pixel (Cells, X, Y) /= Pixel (Blended, X, Y) then
                        Wrong := Wrong + 1;
                     end if;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
      Check (Wrong = 0, "2 x 30 ink and background pairs, 95 characters:" & Natural'Image (Wrong) & " pixels differ");
   end;

   Section ("find");
   Hit (Key_F, Control => True);
   Type_Text ("Piece");
   Check (Find_Open (App), "find bar open");
   Check (Settle (App, Target), "count settles");
   Check (Search_Pattern (D.all) = "Piece", "pattern set as typed");
   declare
      P : constant Text_Cursors.Selection := Primary (D.all);
   begin
      Check (Slice (D.all, Text_Cursors.First_Of (P), Text_Cursors.Last_Of (P)) = "Piece",
             "incremental search selects the first match");
   end;
   Hit (Enter);
   Hit (Enter);
   Shot ("find");
   Hit (Escape);
   Check (not Find_Open (App), "Esc closes the find bar");

   Section ("palette");
   Hit (Key_P, Control => True, Shift => True);
   declare
      Wanted : Natural := 0;
      Hidden_Listed : Boolean := False;
   begin
      for C in Real_Command loop
         if In_Palette (C) then
            Wanted := Wanted + 1;
         end if;
      end loop;
      for Row in 1 .. Palette_Rows (App) loop
         for C in Real_Command loop
            declare
               Text : constant String := Palette_Row (App, Row);
               Code : constant String := "|" & Qualified_Name (C) & "|";
            begin
               for K in Text'First .. Text'Last - Code'Length + 1 loop
                  if Text (K .. K + Code'Length - 1) = Code and then not In_Palette (C) then
                     Hidden_Listed := True;
                  end if;
               end loop;
            end;
         end loop;
      end loop;
      Check (Palette_Open (App) and then Palette_Rows (App) = Wanted,
             "palette lists the commands a person looks for:" & Natural'Image (Palette_Rows (App)) & " of"
             & Natural'Image (Real_Command'Range_Length));
      Check (not Hidden_Listed, "no cursor motion, selection extension or scrolling in the palette");
   end;
   Shot ("palette-all");
   Hit (Escape);
   Hit (Key_P, Control => True, Shift => True);
   Type_Text ("left");
   Check (Palette_Rows (App) = 0 or else Palette_Row (App, 1) /= "Move left one character|text.left|Left",
          "text.left is a key, not a palette row");
   Hit (Escape);
   Execute_Call (App, "(text.palette)", Ok);
   Check (Palette_Open (App), "the palette opens by CCL");
   Hit (Escape);
   Hit (Key_P, Control => True, Shift => True);
   Type_Text ("goto");
   Check (Palette_Rows (App) in 1 .. 4 and then Palette_Row (App, 1) = "Go to a line number|text.goto-line|Ctrl+G",
          "filtered to a few rows, goto-line first:" & Natural'Image (Palette_Rows (App)));
   Shot ("palette");
   Hit (Escape);
   Hit (Key_P, Control => True, Shift => True);
   Type_Text ("(text.goto-line 120)");
   Hit (Enter);
   Check (not Palette_Open (App) and then Line_Of_Primary = 120, "a typed CCL call in the palette runs");
   Hit (Key_P, Control => True);
   Type_Text ("@Insert");
   Check (Palette_Rows (App) >= 1, "go to symbol finds Insert: " & (if Palette_Rows (App) >= 1 then Palette_Row (App, 1) else ""));
   Hit (Enter);
   Check (Slice (D.all, Line_Start (D.all, Line_Of_Primary), Line_End (D.all, Line_Of_Primary))'Length > 0,
          "go to symbol jumps to line" & Line_Of_Primary'Image);

   Section ("menu bar (from the command table)");
   declare
      use CuBit.UI.Menus;
      Bar : constant Model := Menu_Bar (App);
      Seen : array (Command) of Natural := [others => 0];
      Wrong_Menu, Wrong_Keys : Boolean := False;
   begin
      Check (Valid (Bar) and then Bar.Menu_Count = Menu_Name'Range_Length,
             "six menus:" & Natural'Image (Bar.Menu_Count));
      for K in 1 .. Bar.Item_Count loop
         if not Bar.Items (K).Separator then
            declare
               C : constant Command := Command'Val (Bar.Items (K).Command);
               Keys : constant String := Chord_Text (Binding (C));
            begin
               Seen (C) := Seen (C) + 1;
               Wrong_Menu := Wrong_Menu or else not In_Menu (C)
                 or else Bar.Items (K).Parent /= Menu_Name'Pos (Menu_Of (C)) + 1;
               Wrong_Keys := Wrong_Keys or else
                 (if Keys'Length = 0 then Bar.Items (K).Shortcut /= null
                  else Bar.Items (K).Shortcut = null or else Bar.Items (K).Shortcut.all /= Keys);
            end;
         end if;
      end loop;
      Check ((for all C in Real_Command => Seen (C) = (if In_Menu (C) then 1 else 0)),
             "every menu command once, no key-only command (" & Natural'Image (Bar.Item_Count) & " rows)");
      Check (not Wrong_Menu, "each item in its command's menu");
      Check (not Wrong_Keys, "each item shows its command's keys");
      for K in 1 .. Bar.Item_Count loop
         if not Bar.Items (K).Separator and then Command'Val (Bar.Items (K).Command) = Toggle_Wrap then
            Check (Bar.Items (K).Checked = Text_Editors.Wrap_On (Active_Editor (App).all), "Soft Wrap's checkmark");
         elsif not Bar.Items (K).Separator and then Command'Val (Bar.Items (K).Command) = Undo then
            Check (Bar.Items (K).Enabled = Can_Undo (D.all), "Undo disabled with nothing to undo");
         elsif not Bar.Items (K).Separator and then Command'Val (Bar.Items (K).Command) = Other_View then
            Check (not Bar.Items (K).Enabled, "Other View disabled without a split");
         end if;
      end loop;
   end;
   Hit (F10);
   Check (Open_Menu (App) = 1, "F10 opens File");
   Shot ("menu-file");
   Hit (Right);
   Hit (Right);
   Check (Open_Menu (App) = 3, "Right moves to Selection");
   Hit (Escape);
   Check (Open_Menu (App) = 0, "Esc closes the menu");
   declare
      Wrap_Before : constant Boolean := Text_Editors.Wrap_On (Active_Editor (App).all);
   begin
      Hit (Key_V, Alt => True);
      Check (Open_Menu (App) = 4, "Alt+V opens View");
      Frame (App, Target);
      Shot ("menu-view");
      Type_Text ("o");
      Check (Open_Menu (App) = 0 and then Text_Editors.Wrap_On (Active_Editor (App).all) /= Wrap_Before,
             "its mnemonic runs Soft Wrap and closes the menu");
      Execute_Call (App, "toggle-wrap", Ok);
   end;
   declare
      Line_Before : constant Line_Number := Line_Of_Primary;
      Edit_X : constant Natural := CuBit.UI.Menu_Title_Width ("File") + 4;
   begin
      Click (Edit_X, 10, 50_000);
      Check (Open_Menu (App) = 2, "a click on Edit opens it");
      Frame (App, Target);
      Click (700, 500, 51_000);
      Check (Open_Menu (App) = 0 and then Line_Of_Primary = Line_Before, "a click outside closes it, nothing else");
      Frame (App, Target);
      declare
         Damage_Only : constant Unsigned_64 := Picture;
      begin
         Text_Editors.Invalidate (Active_Editor (App).all);
         Frame (App, Target);
         Check (Picture = Damage_Only, "after the popup closes, damage-only frames equal a full repaint");
      end;
   end;

   Section ("toolbar tooltips");
   declare
      --  The first tool (New): below the menu bar, inside the toolbar.
      Tool_X : constant Natural := 10;
      Tool_Y : constant Natural := 24 + 12;
   begin
      Frame (App, Target);
      declare
         --  The pointer comes to rest at T0; once the occurrence pass
         --  (250 ms) is done, the next deadline is the tooltip's.
         T0 : constant Unsigned_64 := Now_Us;
         Delay_Us : constant Unsigned_64 := 500 * 1_000;
         Rest_Us : constant Unsigned_64 := 300 * 1_000;
         Budget : constant := 100_000;
         Busy, Changed : Boolean;
      begin
         Set_Clock (App, T0);
         Handle (App, (Kind => Pointer_Event, Action => Text_Shell.Move, X => Tool_X, Y => Tool_Y, others => <>));
         Set_Clock (App, T0 + Rest_Us);
         Pump (App, Budget, Busy, Changed);
         Check (Tooltip_Text (App) = "", "no tooltip before the delay");
         Check (Next_Deadline_Us (App) = (T0 / 1_000) * 1_000 + Delay_Us,
                "the delay is the deadline of the loop's wait:" & Next_Deadline_Us (App)'Image & " vs"
                & Unsigned_64'Image ((T0 / 1_000) * 1_000 + Delay_Us));
      end;
      delay 0.6;
      Check (Settle (App, Target), "settles");
      Check (Tooltip_Text (App) = "New", "after resting, the tool's tooltip: " & Tooltip_Text (App));
      Shot ("tooltip");
      Hit (Escape);
      Check (Tooltip_Text (App) = "", "a key hides it");
      Frame (App, Target);
      declare
         Damage_Only : constant Unsigned_64 := Picture;
      begin
         Text_Editors.Invalidate (Active_Editor (App).all);
         Frame (App, Target);
         Check (Picture = Damage_Only, "after the tooltip hides, damage-only frames equal a full repaint");
      end;
   end;

   Section ("multiple cursors");
   Execute_Call (App, "goto-line 10", Ok);
   Hit (End_Key);
   for K in 1 .. 5 loop
      Hit (Down, Alt => True, Shift => True);
   end loop;
   Check (Selection_Count (D.all) = 6, "Alt+Shift+Down adds cursors: " & Natural'Image (Selection_Count (D.all)));
   Shot ("multicursor");
   Type_Text (" -- edited");
   Check (Selection_Count (D.all) = 6, "typing keeps every cursor");
   --  Typing coalesces by word: " --" and " edited" are two steps.
   Hit (Key_Z, Control => True);
   Check (Modified (D.all), "one undo takes back the last word only");
   Hit (Key_Z, Control => True);
   Hit (Escape);
   Check (Selection_Count (D.all) = 1 and then not Modified (D.all), "undo and Esc: one cursor, unmodified");
   --  Ctrl+D: the word, then its next occurrences.
   Execute_Call (App, "goto-line 30", Ok);
   Execute_Call (App, "find ""Table""", Ok);
   Hit (Escape);
   Hit (Key_D, Control => True);
   Hit (Key_D, Control => True);
   Hit (Key_D, Control => True);
   Check (Selection_Count (D.all) = 4, "with a match selected, three Ctrl+D add three more: "
          & Natural'Image (Selection_Count (D.all)));
   Shot ("ctrl-d");
   Hit (Escape);
   Hit (Escape);

   Section ("a scratch file: edit, save atomically, reopen");
   Write_File (SCRATCH & "/demo.adb", "procedure Demo is" & ASCII.LF & "begin" & ASCII.LF & "   null;" & ASCII.LF
               & "end Demo;" & ASCII.LF);
   Open_And_Wait (SCRATCH & "/demo.adb");
   Check (Tabs (App) = 2, "a second tab");
   Execute_Call (App, "goto-line 3", Ok);
   Hit (End_Key);
   Hit (Enter);
   Type_Text ("Put_Line (""saved by CuBit Text"");");
   Check (Modified (D.all), "modified");
   Hit (Key_S, Control => True);
   Check (Settle (App, Target), "save settles");
   Check (not Modified (D.all), "saved: " & Status_Message (App));
   Check (Read_File (SCRATCH & "/demo.adb") = Content (D.all), "the file on disk is the document");
   Check (not Ada.Directories.Exists (SCRATCH & "/.demo.adb.text-save"), "no temporary left behind");
   --  A read-only place refuses before any request.
   Hit (Page_Up, Control => True);
   Check (Name (D.all) = SAMPLE_ADA, "Ctrl+PgUp: the previous tab");
   Type_Text ("z");
   Hit (Key_S, Control => True);
   Check (Status_Message (App) = "@repo:0/userspace/lib/text/text_pieces.adb: read-only place",
          "saving under a read-only grant, shown by its granted name: "
          & Status_Message (App));
   Hit (Key_Z, Control => True);

   Section ("diagnostics, annotations and a generated region (embedding hooks)");
   declare
      Added : Boolean;
      L : constant Line_Number := 61;
      Start : constant Byte_Offset := Line_Start (D.all, L);
   begin
      Execute_Call (App, "goto-line 60", Ok);
      Add_Mark (D.all, (Kind => Text_Marks.Diagnostic, First => Start + 6, Last => Start + 18,
                        Level => Text_Marks.Error, Id => 1, others => <>), Added);
      Add_Mark (D.all, (Kind => Text_Marks.Diagnostic, First => Line_Start (D.all, 64) + 6,
                        Last => Line_Start (D.all, 64) + 14, Level => Text_Marks.Warning, Id => 2, others => <>),
                Added);
      Add_Mark (D.all, (Kind => Text_Marks.Annotation, First => Line_Start (D.all, 66), Last => Line_Start (D.all, 66),
                        Glyph => Text_Marks.Breakpoint, Id => 3, others => <>), Added);
      Add_Mark (D.all, (Kind => Text_Marks.Generated, First => Line_Start (D.all, 70), Last => Line_Start (D.all, 72),
                        Id => 4, others => <>), Added);
      Execute_Call (App, "goto-line 71", Ok);
      Type_Text ("x");
      Check (Status_Message (App) = "That text is protected (read-only or generated)",
             "typing in a generated region is refused: " & Status_Message (App));
      Check (not Modified (D.all), "nothing changed");
      Execute_Call (App, "goto-line 60", Ok);
      Shot ("diagnostics");
      Remove_Marks (D.all, 1);
      Remove_Marks (D.all, 2);
      Remove_Marks (D.all, 3);
      Remove_Marks (D.all, 4);
   end;

   Section ("modal keys");
   Hit (F9);
   Check (Modal_On (App), "F9: modal on");
   Execute_Call (App, "goto-line 5", Ok);
   declare
      Before : constant String := Content (D.all);
      Lines_Before : constant Line_Number := Line_Total (D.all);
   begin
      Type_Text ("dd");
      Check (Line_Total (D.all) = Lines_Before - 1, "dd deletes the line");
      Type_Text ("u");
      Check (Content (D.all) = Before, "u undoes it");
      Type_Text ("3j");
      Check (Line_Of_Primary = 8, "3j moves three lines down (on" & Line_Of_Primary'Image & ")");
      Type_Text ("ihi");
      Hit (Escape);
      Check (Modified (D.all), "i, text, Esc: inserted");
      Type_Text ("u");
      Check (Content (D.all) = Before, "u again");
      Type_Text ("gg");
      Check (Line_Of_Primary = 1, "gg: the first line");
      Type_Text ("40G");
      Check (Line_Of_Primary = 40, "40G: line 40");
   end;
   Shot ("modal");
   Hit (F9);

   Section ("describe a key");
   Hit (F1);
   Hit (Key_S, Control => True);
   Check (Status_Message (App) = "Ctrl+S: text.save - Save (atomic replace)", "F1 then Ctrl+S: " & Status_Message (App));

   Section ("other languages, dark, split, minimap, wrap");
   Open_And_Wait (REPO & "/docs/text-editor.md");
   Check (Text_Documents.Language (D.all) = Text_Syntax.Markdown, "markdown by extension");
   Execute_Call (App, "toggle-wrap", Ok);
   Execute_Call (App, "goto-line 220", Ok);
   Shot ("markdown-wrap");
   Execute_Call (App, "toggle-wrap", Ok);
   Write_File (SCRATCH & "/sample.rs",
               "/// A raw string and lifetimes: Rust's lexer state crosses lines." & ASCII.LF
               & "#[derive(Debug)]" & ASCII.LF
               & "pub struct Piece<'a> {" & ASCII.LF
               & "    first: usize, /* nested /* comment */ still comment */" & ASCII.LF
               & "    text: &'a str," & ASCII.LF
               & "}" & ASCII.LF & ASCII.LF
               & "fn main() {" & ASCII.LF
               & "    let raw = r#""a ""quoted"" raw string""#;" & ASCII.LF
               & "    let c = 'x';" & ASCII.LF
               & "    println!(""{} {}"", raw, c);" & ASCII.LF
               & "}" & ASCII.LF);
   Open_And_Wait (SCRATCH & "/sample.rs");
   Write_File (SCRATCH & "/sample.c",
               "#include <stdint.h>" & ASCII.LF
               & "/* A block comment" & ASCII.LF & "   across lines */" & ASCII.LF
               & "static uint32_t count_lines(const char *text, size_t n) {" & ASCII.LF
               & "    uint32_t lines = 1;" & ASCII.LF
               & "    for (size_t i = 0; i < n; i++) if (text[i] == '\n') lines++;" & ASCII.LF
               & "    return lines; // done" & ASCII.LF & "}" & ASCII.LF);
   Open_And_Wait (SCRATCH & "/sample.c");
   Open_And_Wait (REPO & "/system.ccl");
   Execute_Call (App, "goto-line 18", Ok);
   Shot ("ccl");
   Hit (Page_Up, Control => True);
   Shot ("c");
   Hit (Page_Up, Control => True);
   Shot ("rust");
   Hit (Page_Up, Control => True);
   Hit (Page_Up, Control => True);
   Hit (Page_Up, Control => True);
   Check (Name (D.all) = SAMPLE_ADA, "back on the Ada unit");
   Execute_Call (App, "toggle-theme", Ok);
   Execute_Call (App, "split", Ok);
   Execute_Call (App, "toggle-minimap", Ok);
   Execute_Call (App, "goto-line 140", Ok);
   Check (Split (App), "split on");
   Shot ("split-dark-minimap");
   Execute_Call (App, "split", Ok);
   Execute_Call (App, "toggle-minimap", Ok);
   Execute_Call (App, "toggle-theme", Ok);

   Section ("column selection with the pointer (Alt+drag)");
   declare
      E : constant Text_Editors.Editor_Access := Active_Editor (App);
      A : constant CuBit.UI.Rect := Text_Editors.Area (E.all);
      X, Y : Natural;
      Shown : Boolean;
   begin
      Execute_Call (App, "goto-line 20", Ok);
      Frame (App, Target);
      Text_Editors.Caret_Position (E.all, Line_Start (D.all, 20), X, Y, Shown);
      Check (Shown, "line 20 is shown");
      Handle (App, (Kind => Pointer_Event, Action => Text_Shell.Press, X => X + 3 * Text_Editors.CELL_WIDTH,
                    Y => Y + 5, Time_Ms => 10_000, Alt => True, others => <>));
      Handle (App, (Kind => Pointer_Event, Action => Text_Shell.Move, X => X + 12 * Text_Editors.CELL_WIDTH,
                    Y => Y + 5 + 6 * Text_Editors.ROW_HEIGHT, others => <>));
      Handle (App, (Kind => Pointer_Event, Action => Text_Shell.Release, X => 0, Y => 0, others => <>));
      Check (Selection_Count (D.all) = 7, "a box over seven lines: " & Natural'Image (Selection_Count (D.all)));
      Shot ("column-box");
      Hit (Escape);
      Hit (Escape);
      --  Double click selects a word.
      --  Line 20 is "   --  Recount the table's ...": column 9 is in "Recount".
      Click (X + 9 * Text_Editors.CELL_WIDTH + 2, Y + 5, 20_000);
      Click (X + 9 * Text_Editors.CELL_WIDTH + 2, Y + 5, 20_100);
      Check (Slice (D.all, Text_Cursors.First_Of (Primary (D.all)), Text_Cursors.Last_Of (Primary (D.all))) = "Recount",
             "double click selects a word: "
             & Slice (D.all, Text_Cursors.First_Of (Primary (D.all)), Text_Cursors.Last_Of (Primary (D.all))));
      pragma Unreferenced (A);
   end;

   Section ("occurrences of the identifier under a resting cursor");
   Hit (Page_Up, Control => True);
   while Name (D.all) /= SAMPLE_ADA loop
      Hit (Page_Up, Control => True);
   end loop;
   Execute_Call (App, "goto-line 37", Ok);
   Hit (End_Key);
   Hit (Left, Control => True);
   Hit (Left, Control => True);
   Frame (App, Target);
   delay 0.3;
   Check (Settle (App, Target), "settles after the cursor rests");
   Shot ("occurrences");

   Section ("a 1 MiB single line stays bounded");
   declare
      Line : String (1 .. 1_048_576);
   begin
      for K in Line'Range loop
         Line (K) := (if K mod 9 = 0 then ' ' else Character'Val (Character'Pos ('a') + K mod 26));
      end loop;
      Write_File (SCRATCH & "/long-line.txt", Line);
   end;
   Open_And_Wait (SCRATCH & "/long-line.txt");
   Check (Line_Total (D.all) = 1, "one line");
   Execute_Call (App, "goto-line 1", Ok);
   for K in 1 .. 40 loop
      Hit (Down);
   end loop;
   Check (Primary (D.all).Head > 0, "Down moves through the wrapped rows of one line");
   Hit (End_Key, Control => True);
   Check (Primary (D.all).Head = Length (D.all), "Ctrl+End: the end of the line");
   Shot ("long-line");

   Summary;
end Text_View_Tests;
