------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Hosted tests of the toolkit's keys (UI-049, UI-051, UI-052): decoding,
--  chords, the keymap (a remap moves dispatch and every hint; conflicts,
--  the other layer, the secure attention chord and menu mnemonics are
--  refused), the modifier reveal's timing, the button bar (keycaps,
--  icon-only buttons, reveals, never truncating, wrapping) and the menu
--  bar's keys through Handle_Chord. Measured with the real font.
------------------------------------------------------------------------------
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.Button_Bars;
with CuBit.UI.Controls;
with CuBit.UI.Icons;
with CuBit.UI.Keycaps;
with CuBit.UI.Keymaps;
with CuBit.UI.Keys;
with CuBit.UI.Menus;
with CuBit.UI.Modifier_Reveal;
with CuBit.UI.Popup_Menus;
with CuBit.UI.Tooltips;

procedure Keys_Tests is
   package Keys renames CuBit.UI.Keys;
   package Controls renames CuBit.UI.Controls;
   use type Keys.Chord, Keys.Modifier, Keys.Modifier_Set, Keys.Key;

   Failures, Checks : Natural := 0;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Name);
      end if;
   end Check;

   WIDTH : constant := 900;
   HEIGHT : constant := 300;
   BYTES_PER_PIXEL : constant := 4;
   Pixels : aliased array (0 .. WIDTH * HEIGHT - 1) of Unsigned_32 := [others => 0];
   Screen : constant Canvas := (addr => Pixels'Address, width => WIDTH, height => HEIGHT,
                                pitch => WIDTH * BYTES_PER_PIXEL, others => <>);
   Window : constant Rect := (0, 0, WIDTH, HEIGHT);
   Colors : constant Theme := CuBit_Alloy;

   --  An app's commands, defaults declared in one place.
   type Command is (Refresh, Copy, Delete, Sort_Name, Help, Back, Save, Quit);
   package Map_Of is new CuBit.UI.Keymaps (Command);
   use type Map_Of.Bind_Result;
   DEFAULTS : constant Map_Of.Binding_List :=
     [(+Keys.F2, Refresh), (Keys.Ctrl + Keys.R, Refresh), (+Keys.F5, Copy), (+Keys.F8, Delete),
      (+Keys.Delete, Delete), (Keys.Ctrl + Keys.F3, Sort_Name), (+Keys.F1, Help),
      (Keys.Alt + Keys.Left, Back), (Keys.Ctrl + Keys.S, Save), (+Keys.F10, Quit)];

   function Caption (Item : Command) return String is
     (case Item is
         when Refresh => "Refresh", when Copy => "Copy", when Delete => "Delete", when Sort_Name => "Name",
         when Help => "Help", when Back => "Back", when Save => "Save", when Quit => "Quit");
   function Description (Item : Command) return String is ("Run " & Caption (Item));
   function Enabled (Item : Command) return Boolean is (Item /= Help);
   function Checked (Item : Command) return Boolean is (Item = Quit and then False);
   function Shade (Item : Command) return CuBit.UI.Keycaps.Tone is
     (if Item = Delete then CuBit.UI.Keycaps.Danger else CuBit.UI.Keycaps.Accent);
   function Picture (Item : Command) return CuBit.UI.Icons.Icon is
     (if Item = Save then CuBit.UI.Icons.Refresh else CuBit.UI.Icons.Copy);
   package Bars is new CuBit.UI.Button_Bars
     (Map_Of, Caption, Description, Enabled, Checked, Shade, Picture);

   Map : Map_Of.Keymap;
   Refused : Natural;
   Result : Map_Of.Bind_Result;
   Found : Boolean;
   Target : Command;

   procedure Decoding is
   begin
      for K in Keys.Key loop
         if K /= Keys.Unknown then
            Check (Keys.Key_Of (Keys.Scancode_Of (K)) = K, "decode round trip " & K'Image);
         end if;
      end loop;
      Check (Keys.Key_Of (16#13#) = Keys.R, "0x13 is R");
      Check (Keys.Key_Of (16#3F#) = Keys.F5, "0x3F is F5");
      Check (Keys.Key_Of (16#58#) = Keys.F12, "0x58 is F12");
      Check (Keys.Key_Of (16#7F#) = Keys.Unknown and then Keys.Key_Of (16#1_0000#) = Keys.Unknown,
             "unassigned codes are Unknown");
      Check (Keys.Chord_Of (16#13#, 2) = Keys.Ctrl + Keys.R, "a wire key event decodes to Ctrl+R");
      Check (Keys.Chord_Of (16#2E#, 3) = Keys.Ctrl + Keys.Shift + Keys.C, "Ctrl+Shift+C from the wire");
      Check (Keys.Ctrl + Keys.Shift + Keys.Z = Keys.Shift + Keys.Ctrl + Keys.Z, "modifier order does not matter");
      Check ((Keys.Ctrl + Keys.Ctrl) + Keys.Z = Keys.Ctrl + Keys.Z, "adding a modifier twice is once");
      Check (Keys.Z + Keys.Ctrl = Keys.Ctrl + Keys.Z, "Key + Modifier = Modifier + Key");
      Check ((Keys.Ctrl + Keys.Z) + Keys.Shift = Keys.Ctrl + Keys.Shift + Keys.Z, "Chord + Modifier");
      Check (Keys.Image (Keys.Ctrl + Keys.Shift + Keys.C) = "Ctrl+Shift+C", "accessibility text of a chord");
      Check (Keys.Image (Keys.Super + Keys.N) = "Super+N", "an environment chord's text");
      Check (not Keys.Is_Chord (+Keys.Control_Key), "a modifier key alone is no chord");
   end Decoding;

   procedure Keymap is
   begin
      Map_Of.Load (Map, DEFAULTS, Refused);
      Check (Refused = 0, "the defaults load without conflicts");
      Map_Of.Find (Map, Keys.Ctrl + Keys.R, Found, Target);
      Check (Found and then Target = Refresh, "Ctrl+R dispatches Refresh");
      Map_Of.Find (Map, +Keys.F2, Found, Target);
      Check (Found and then Target = Refresh, "F2 also dispatches Refresh");
      Check (Map_Of.Hint (Map, Refresh) = +Keys.F2, "the first default is the hint");
      Check (Map_Of.Hint (Map, Delete) = +Keys.F8, "Delete's hint is F8");

      --  Conflicts are refused when made, and change nothing.
      Map_Of.Bind (Map, Keys.Ctrl + Keys.R, Copy, Result);
      Check (Result = Map_Of.Taken, "binding another command's chord is refused");
      Map_Of.Find (Map, Keys.Ctrl + Keys.R, Found, Target);
      Check (Found and then Target = Refresh, "the refused bind left Ctrl+R alone");
      Map_Of.Bind (Map, Keys.Super + Keys.C, Copy, Result);
      Check (Result = Map_Of.Other_Layer, "an app can't bind a Super chord");
      Map_Of.Bind (Map, Keys.Secure_Attention, Quit, Result);
      Check (Result = Map_Of.Reserved_Chord, "nobody binds the secure attention chord");
      Map_Of.Bind (Map, +Keys.Alt_Key, Quit, Result);
      Check (Result = Map_Of.Not_A_Chord, "a modifier key alone can't be bound");

      --  Remap one chord: dispatch and the hint move at once.
      Map_Of.Remap (Map, Refresh, +Keys.F2, +Keys.F11, Result);
      Check (Result = Map_Of.Bound, "F2 -> F11 for Refresh");
      Map_Of.Find (Map, +Keys.F2, Found, Target);
      Check (not Found, "F2 no longer dispatches");
      Map_Of.Find (Map, +Keys.F11, Found, Target);
      Check (Found and then Target = Refresh, "F11 dispatches Refresh");
      Check (Map_Of.Hint (Map, Refresh) = +Keys.F11, "Refresh's hint is now F11");
      Map_Of.Remap (Map, Copy, +Keys.F5, Keys.Ctrl + Keys.R, Result);
      Check (Result = Map_Of.Taken, "a remap onto another command's chord is refused");
      Check (Map_Of.Hint (Map, Copy) = +Keys.F5, "and Copy keeps F5");

      --  Unbinding the last chord leaves no hint (no keycap).
      Map_Of.Unbind (Map, Keys.Ctrl + Keys.S, Save, Result);
      Check (Result = Map_Of.Bound and then Map_Of.Hint (Map, Save) = Keys.No_Chord,
             "a command without a chord has no hint");
      Map_Of.Unbind (Map, +Keys.F8, Delete, Result);
      Check (Map_Of.Hint (Map, Delete) = +Keys.Delete, "the hint falls to the command's other chord");

      --  Menu mnemonics are part of the check, both ways.
      Map_Of.Reserve_Mnemonic (Map, Keys.F, Result);
      Check (Result = Map_Of.Bound, "Alt+F reserved for the File menu");
      Map_Of.Bind (Map, Keys.Alt + Keys.F, Copy, Result);
      Check (Result = Map_Of.Reserved_Taken, "an app command can't take a mnemonic's Alt+F");
      Map_Of.Remap (Map, Back, Keys.Alt + Keys.Left, Keys.Alt + Keys.F, Result);
      Check (Result = Map_Of.Reserved_Taken, "a remap can't take a mnemonic either");
      Map_Of.Find (Map, Keys.Alt + Keys.Left, Found, Target);
      Check (Found and then Target = Back, "Alt+Left stays an app command");
      Map_Of.Reserve_Mnemonic (Map, Keys.Left, Result);
      Check (Result = Map_Of.Taken, "a mnemonic can't take an app command's chord");

      --  User remaps as a typed list over the defaults.
      Map_Of.Load (Map, DEFAULTS, Refused);
      Map_Of.Apply (Map, [(Save, Keys.Ctrl + Keys.S, Keys.Ctrl + Keys.Shift + Keys.S),
                          (Copy, +Keys.F5, +Keys.F8)], Refused);
      Check (Refused = 1, "one of two remaps conflicts and is refused");
      Check (Map_Of.Hint (Map, Save) = Keys.Ctrl + Keys.Shift + Keys.S, "the other applied");
   end Keymap;

   procedure Environment is
      package Env is new CuBit.UI.Keymaps (Command, Keys.Environment_Layer);
      E : Env.Keymap;
      R : Env.Bind_Result;
      use type Env.Bind_Result;
   begin
      Env.Clear (E);
      Env.Bind (E, Keys.Super + Keys.V, Copy, R);
      Check (R = Env.Bound, "the environment binds Super+V");
      Env.Bind (E, Keys.Ctrl + Keys.V, Copy, R);
      Check (R = Env.Other_Layer, "the environment can't bind an app's Ctrl chord");
   end Environment;

   procedure Reveal is
      package MR renames CuBit.UI.Modifier_Reveal;
      S : MR.State;
      Ctrl_Set : constant Keys.Modifier_Set := [Keys.Ctrl => True, others => False];
      T0 : constant Unsigned_64 := 10_000;
   begin
      MR.Key_Event (S, Keys.Control_Key, True, Ctrl_Set, T0);
      Check (not MR.Is_Revealed (S, T0 + MR.HOLD_DELAY_MS - 1), "no reveal before the hold delay");
      Check (MR.Next_Deadline (S) = T0 + MR.HOLD_DELAY_MS, "the delay is the deadline");
      Check (MR.Revealed (S, T0 + MR.HOLD_DELAY_MS) = Ctrl_Set, "holding Ctrl reveals the Ctrl layer");
      MR.Key_Event (S, Keys.Left_Shift_Key, True, Ctrl_Set + Keys.Shift, T0 + 2 * MR.HOLD_DELAY_MS);
      Check (MR.Revealed (S, T0 + 2 * MR.HOLD_DELAY_MS) = Ctrl_Set + Keys.Shift,
             "Shift added to a revealed Ctrl switches to Ctrl+Shift at once");
      MR.Key_Event (S, Keys.Left_Shift_Key, False, Ctrl_Set, T0 + 3 * MR.HOLD_DELAY_MS);
      MR.Key_Event (S, Keys.Control_Key, False, Keys.No_Modifiers, T0 + 3 * MR.HOLD_DELAY_MS);
      Check (not MR.Is_Revealed (S, T0 + 4 * MR.HOLD_DELAY_MS), "release hides it");

      --  A quick Ctrl+S: nothing shows, even if Ctrl stays down a while.
      MR.Key_Event (S, Keys.Control_Key, True, Ctrl_Set, T0 + 5_000);
      MR.Key_Event (S, Keys.S, True, Ctrl_Set, T0 + 5_050);
      Check (not MR.Is_Revealed (S, T0 + 5_000 + 2 * MR.HOLD_DELAY_MS), "a quick chord reveals nothing");
      Check (MR.Next_Deadline (S) = MR.NEVER, "and nothing is pending");
      MR.Key_Event (S, Keys.Control_Key, False, Keys.No_Modifiers, T0 + 6_000);
      Check (not MR.Chord_Used (S), "release forgets the chord");

      --  A chord after the reveal hides it until release.
      MR.Key_Event (S, Keys.Alt_Key, True, [Keys.Alt => True, others => False], T0 + 7_000);
      Check (MR.Is_Revealed (S, T0 + 7_000 + MR.HOLD_DELAY_MS), "Alt held reveals");
      MR.Key_Event (S, Keys.Left, True, [Keys.Alt => True, others => False], T0 + 8_000);
      Check (not MR.Is_Revealed (S, T0 + 8_001), "Alt+Left completes the chord: hidden");
   end Reveal;

   procedure Bar_Tests is
      B : Bars.Bar;
      Tips : CuBit.UI.Tooltips.Tip_Table;
      Tip : CuBit.UI.Tooltips.Tooltip;
      Controls_Map : Controls.Control_Map;
      Pointer : constant Pointer_State := (enabled => False, others => <>);
      Ctrl_Set : constant Keys.Modifier_Set := [Keys.Ctrl => True, others => False];
      Alt_Set : constant Keys.Modifier_Set := [Keys.Alt => True, others => False];
      Damage : Rect;
      FIRST_ID : constant := 40;
      Wide : constant Rect := (0, 0, WIDTH, 60);
      procedure Frame (Area : Rect; Revealed : Keys.Modifier_Set) is
      begin
         Controls.Clear (Controls_Map);
         CuBit.UI.Tooltips.Clear (Tips);
         Bars.Draw (Screen, Controls_Map, Tips, B, Map, Area, Colors, Pointer, Revealed, 0);
      end Frame;
   begin
      Map_Of.Load (Map, DEFAULTS, Refused);
      --  A default button, an icon-only one, a function-key cell.
      Bars.Clear (B, FIRST_ID, Bars.Toolbar_Style);
      Bars.Add_Command (B, Copy);
      Bars.Add_Command (B, Save, Bars.Icon_Only);
      Bars.Add_Key (B, Keys.F3, Starts_Cluster => True);
      Frame (Wide, Keys.No_Modifiers);
      Check (Bars.Shows_Keycap (B, Map, 1, Keys.No_Modifiers), "a default button shows its keycap");
      Check (not Bars.Shows_Keycap (B, Map, 2, Keys.No_Modifiers), "an icon-only button doesn't");
      Check (Controls.Shortcut (Controls_Map, Bars.Button_ID (B, 1)) = +Keys.F5
             and then Controls.Shortcut (Controls_Map, Bars.Button_ID (B, 2)) = Keys.Ctrl + Keys.S,
             "both carry their shortcut as accessibility data");
      for K in 1 .. 2 loop
         declare
            Box : constant Rect := Bars.Button_Area (B, K);
         begin
            CuBit.UI.Tooltips.Pointer_At (Tip, Tips, Controls_Map, Box.x + 2, Box.y + 2, 1_000, Damage);
            CuBit.UI.Tooltips.Tick (Tip, Tips, 100_000, Damage);
            Check (CuBit.UI.Tooltips.Hint (Tip, Tips) = (if K = 1 then +Keys.F5 else Keys.Ctrl + Keys.S),
                   "button" & K'Image & "'s tooltip shows its keycap");
            CuBit.UI.Tooltips.Dismiss (Tip, Damage);
         end;
      end loop;
      Check (Bars.Revealed_At (B, Map, 2, Alt_Set) and then not Bars.Revealed_At (B, Map, 1, Alt_Set),
             "holding Alt reveals the icon-only button's keycap (the default one already shows it)");
      Check (Bars.Revealed_At (B, Map, 2, Ctrl_Set), "holding Ctrl reveals Save [Ctrl][S]");
      Check (not Bars.Revealed_At (B, Map, 2, Keys.No_Modifiers), "nothing held, nothing revealed");
      Check (not Is_Empty (Bars.Reveal_Area (B, Map, Alt_Set, Window)), "the reveal has an area to draw");
      Check (Bars.Shown_Caption (B, Map, 3, Keys.No_Modifiers) = "", "F3 runs nothing in the base layer");
      Check (Bars.Shown_Caption (B, Map, 3, Ctrl_Set) = "Name", "while Ctrl is held F3 shows Ctrl+F3's Name");

      --  A remap changes the button's hint, the tooltip and the menu at once.
      Map_Of.Remap (Map, Copy, +Keys.F5, Keys.Ctrl + Keys.Shift + Keys.C, Result);
      Frame (Wide, Keys.No_Modifiers);
      Check (Bars.Shown_Chord (B, Map, 1, Keys.No_Modifiers) = Keys.Ctrl + Keys.Shift + Keys.C,
             "the remap shows on the button");
      Check (Controls.Shortcut (Controls_Map, Bars.Button_ID (B, 1)) = Keys.Ctrl + Keys.Shift + Keys.C,
             "and in its accessibility data");
      declare
         Menu : CuBit.UI.Popup_Menus.Model;
      begin
         CuBit.UI.Popup_Menus.Clear (Menu);
         CuBit.UI.Popup_Menus.Add (Menu, CuBit.UI.Popup_Menus.ROOT, "Copy", 1, Map_Of.Hint (Map, Copy));
         Check (CuBit.UI.Popup_Menus.Hint (Menu, CuBit.UI.Popup_Menus.ROOT, 1) = Keys.Ctrl + Keys.Shift + Keys.C,
                "and in the menu built from the keymap");
      end;
      Map_Of.Load (Map, DEFAULTS, Refused);

      --  Flash: a keyboard press flashes its button, briefly.
      Frame (Wide, Keys.No_Modifiers);
      Bars.Flash (B, Map, +Keys.F5, 500, Damage);
      Check (Bars.Flashing (B, 1, 500) and then Damage = Bars.Button_Area (B, 1), "F5 flashes Copy");
      Bars.Tick (B, 500 + Bars.FLASH_MS, Damage);
      Check (not Bars.Flashing (B, 1, 500 + Bars.FLASH_MS), "the flash ends");

      --  The Files F1..F10 bar: never truncated, wraps when narrow.
      Bars.Clear (B, FIRST_ID, Bars.Key_Bar_Style);
      for K in Keys.Key range Keys.F1 .. Keys.F10 loop
         Bars.Add_Key (B, K, Starts_Cluster => K in Keys.F5 | Keys.F9);
      end loop;
      Frame (Wide, Keys.No_Modifiers);
      Check (Bars.Rows (B) = 1, "a wide window: one row");
      for K in 1 .. Bars.Count (B) loop
         declare
            Text : constant String := Bars.Shown_Caption (B, Map, K, Keys.No_Modifiers);
            Box : constant Rect := Bars.Button_Area (B, K);
         begin
            Check (Box.w >= UI_Text_Width (Text) + CuBit.UI.Keycaps.Width (Bars.Shown_Chord (B, Map, K, Keys.No_Modifiers))
                   + 2 * Bars.BUTTON_INSET,
                   "cell" & K'Image & " fits its caption and keycap whole");
            Check (Box.x + Box.w <= WIDTH, "cell" & K'Image & " is inside the bar");
         end;
      end loop;
      Check (UI_Text_Width ("Refresh") > 0, "the real font measures Refresh");
      declare
         Narrow : constant Rect := (0, 0, 320, 120);
      begin
         Frame (Narrow, Keys.No_Modifiers);
         Check (Bars.Rows (B) >= 2, "a narrow window wraps the bar");
         for K in 1 .. Bars.Count (B) loop
            Check (Bars.Button_Area (B, K).w = Bars.Needed_Width (B, Map, K, Keys.No_Modifiers),
                   "narrow: cell" & K'Image & " keeps its width");
            Check (Bars.Button_Area (B, K).x + Bars.Button_Area (B, K).w <= Narrow.w,
                   "narrow: cell" & K'Image & " inside the bar");
         end loop;
         Check (Bars.Button_Area (B, 5).x = Bars.BAR_PADDING, "F5..F8 start the second row together");
         Check (Bars.Height (B, Map, Narrow.w, Keys.No_Modifiers) > Bars.Height (B, Map, WIDTH, Keys.No_Modifiers),
                "the wrapped bar is taller");
      end;
   end Bar_Tests;

   procedure Band_And_Layers is
      B : Bars.Bar;
      Tips : CuBit.UI.Tooltips.Tip_Table;
      Controls_Map : Controls.Control_Map;
      Pointer : constant Pointer_State := (enabled => False, others => <>);
      Alt_Set : constant Keys.Modifier_Set := [Keys.Alt => True, others => False];
      Ctrl_Set : constant Keys.Modifier_Set := [Keys.Ctrl => True, others => False];
      Bar_Area : constant Rect := (0, 0, 400, 36);
      Band : Rect;
   begin
      Map_Of.Load (Map, DEFAULTS, Refused);
      --  Six icon-only buttons side by side, three with long chords.
      Bars.Clear (B, 60, Bars.Toolbar_Style);
      declare
         type Command_List is array (Positive range <>) of Command;
         Row : constant Command_List := [Back, Save, Back, Refresh, Copy, Save];
      begin
         for Item of Row loop
            Bars.Add_Command (B, Item, Bars.Icon_Only);
         end loop;
      end;
      Controls.Clear (Controls_Map);
      Bars.Draw (Screen, Controls_Map, Tips, B, Map, Bar_Area, Colors, Pointer, Keys.No_Modifiers, 0);
      Band := Bars.Reveal_Area (B, Map, Alt_Set, Window);
      Check (Band.y = Bar_Area.y + Bar_Area.h and then Band.x = Bar_Area.x, "the reveal band sits right under the bar");
      Check (Band.h <= 2 * (Natural'Max (UI_Text_Height, CuBit.UI.Keycaps.Height) + 2) + 4,
             "two lanes at most");
      for J in 1 .. Bars.Count (B) loop
         declare
            L : constant Rect := Bars.Reveal_Label (B, Map, J, Alt_Set, Window);
         begin
            Check (not Is_Empty (L), "label" & J'Image & " shows");
            Check (L.x >= Band.x and then L.x + L.w <= Band.x + Band.w and then L.y >= Band.y
                   and then L.y + L.h <= Band.y + Band.h, "label" & J'Image & " is inside the band");
            for I in 1 .. J - 1 loop
               declare
                  O : constant Rect := Bars.Reveal_Label (B, Map, I, Alt_Set, Window);
               begin
                  Check (O.y /= L.y or else O.x + O.w <= L.x or else L.x + L.w <= O.x,
                         "labels" & I'Image & " and" & J'Image & " don't overlap");
               end;
            end loop;
         end;
      end loop;
      Check (CuBit.UI.Keycaps.Width (Keys.Alt + Keys.Left) < CuBit.UI.Keycaps.Width (Keys.Alt + Keys.F10),
             "an arrow cap is a glyph, narrower than a word");

      --  A key bar's Ctrl layer: one indicator, cells as wide as the plain layer at least.
      Bars.Clear (B, 70, Bars.Key_Bar_Style);
      for K in Keys.Key range Keys.F1 .. Keys.F10 loop
         Bars.Add_Key (B, K, Starts_Cluster => K in Keys.F5 | Keys.F9);
      end loop;
      for K in 1 .. Bars.Count (B) loop
         Check (Bars.Needed_Width (B, Map, K, Ctrl_Set) >= Bars.Needed_Width (B, Map, K, Keys.No_Modifiers),
                "the Ctrl layer keeps cell" & K'Image & "'s plain width at least");
      end loop;
      Controls.Clear (Controls_Map);
      Bars.Draw (Screen, Controls_Map, Tips, B, Map, (0, 0, 300, 120), Colors, Pointer, Ctrl_Set, 0);
      Check (Bars.Button_Area (B, 1).x > Bars.BAR_PADDING, "the layer's [Ctrl] comes first");
      for K in 1 .. Bars.Count (B) loop
         Check (Bars.Button_Area (B, K).x + Bars.Button_Area (B, K).w <= 300,
                "the Ctrl layer wraps: cell" & K'Image & " stays on screen");
      end loop;

      --  Status lines drop whole hints, never cut one.
      declare
         Line : constant String := "Tab: other pane   Enter: open   Backspace: up";
         Two : constant String := "Tab: other pane   Enter: open";
      begin
         Check (Fit_UI_Parts (Line, UI_Text_Width (Line)) = Line, "a line that fits stays whole");
         Check (Fit_UI_Parts (Line, UI_Text_Width (Two) + 5) = Two, "too narrow: the last hint goes, whole");
         Check (Fit_UI_Parts (Line, UI_Text_Width ("Tab: other pane") + 1) = "Tab: other pane", "down to the first");
      end;
   end Band_And_Layers;

   procedure Menu_Keys is
      package Menus renames CuBit.UI.Menus;
      File_Caption : aliased constant String := "File";
      Edit_Caption : aliased constant String := "Edit";
      Open_Caption : aliased constant String := "Open";
      Paste_Caption : aliased constant String := "Paste";
      Def : Menus.Model;
      State : Menus.Menu_State;
      Chosen : Natural;
      Handled : Boolean;
   begin
      Def.Menu_Count := 2;
      Def.Menus (1) := (Caption => File_Caption'Unchecked_Access, Mnemonic => 'f');
      Def.Menus (2) := (Caption => Edit_Caption'Unchecked_Access, Mnemonic => 'e');
      Def.Item_Count := 2;
      Def.Items (1) := (Parent => 1, Caption => Open_Caption'Unchecked_Access, Mnemonic => 'o', Command => 10,
                        Hint => Keys.Ctrl + Keys.O, others => <>);
      Def.Items (2) := (Parent => 2, Caption => Paste_Caption'Unchecked_Access, Mnemonic => 'p', Command => 20,
                        others => <>);
      --  The keymap refuses the mnemonics' chords.
      Map_Of.Load (Map, DEFAULTS, Refused);
      for M in 1 .. Def.Menu_Count loop
         Map_Of.Reserve_Mnemonic (Map, Menus.Mnemonic_Key (Def.Menus (M).Mnemonic), Result);
         Check (Result = Map_Of.Bound, "mnemonic" & M'Image & " reserved");
      end loop;
      Map_Of.Bind (Map, Keys.Alt + Keys.E, Copy, Result);
      Check (Result = Map_Of.Reserved_Taken, "Alt+E is the Edit menu's, not a command's");

      Menus.Handle_Chord (State, Def, Keys.Alt + Keys.E, Chosen, Handled);
      Check (Handled and then Menus.Open_Menu (State) = 2, "Alt+E opens the Edit menu");
      Menus.Handle_Chord (State, Def, +Keys.P, Chosen, Handled);
      Check (Handled and then Chosen = 20 and then not Menus.Is_Open (State), "P in the open menu pastes");
      Menus.Handle_Chord (State, Def, +Keys.F10, Chosen, Handled);
      Check (Handled and then Menus.Open_Menu (State) = 1, "F10 activates the bar");
      Menus.Handle_Chord (State, Def, +Keys.Right, Chosen, Handled);
      Check (Menus.Open_Menu (State) = 2, "Right moves to the next menu");
      Menus.Handle_Chord (State, Def, +Keys.Escape, Chosen, Handled);
      Check (not Menus.Is_Open (State), "Escape closes it");
      Menus.Handle_Chord (State, Def, Keys.Ctrl + Keys.R, Chosen, Handled);
      Check (not Handled, "a closed bar leaves the app's chords to its keymap");
      Menus.Handle_Chord (State, Def, Keys.Alt + Keys.Left, Chosen, Handled);
      Check (not Handled, "Alt+Left is not a mnemonic: the app's Back");
      Menus.Alt_Tapped (State, Def);
      Check (Menus.Is_Open (State), "a lone Alt tap activates the bar");
   end Menu_Keys;

   procedure Draws is
      Before : constant Unsigned_32 := Pixels (0);
   begin
      Fill_Rect (Screen, Window, Colors.face);
      CuBit.UI.Keycaps.Draw (Screen, 10, 10, Keys.Ctrl + Keys.Shift + Keys.C, Colors, CuBit.UI.Keycaps.Accent);
      Check (CuBit.UI.Keycaps.Width (Keys.Ctrl + Keys.Shift + Keys.C)
             > CuBit.UI.Keycaps.Width (+Keys.C) + 2 * CuBit.UI.Keycaps.CAP_GAP,
             "a chord is caps side by side");
      Check (CuBit.UI.Keycaps.Width (Keys.No_Chord) = 0, "no chord, no cap");
      Check (Pixels (11 * WIDTH + 12) /= Unsigned_32 (Colors.face) or else Before /= Pixels (0),
             "the cap drew");
   end Draws;
begin
   Decoding;
   Keymap;
   Environment;
   Reveal;
   Bar_Tests;
   Band_And_Layers;
   Menu_Keys;
   Draws;
   if Failures = 0 then
      Ada.Text_IO.Put_Line ("PASS: " & Natural'Image (Checks) & " checks");
   else
      Ada.Text_IO.Put_Line ("FAIL:" & Natural'Image (Failures) & " of" & Natural'Image (Checks) & " checks");
   end if;
end Keys_Tests;
