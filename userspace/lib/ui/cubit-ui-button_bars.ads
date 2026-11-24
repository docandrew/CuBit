------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Button bars (UI-051): a row of ordinary toolkit buttons, each bound to
--  a command of the app's keymap, showing its caption and its current
--  keycap. Function-key bars (Files' F1..F10), toolbars and dialog button
--  rows are all this one widget.
--
--  @description
--  A button is either a command (its hint is the keymap's chord for it) or
--  a key (a function-key bar's cell: it shows whatever command that key
--  runs in the layer shown). The default face shows the caption with the
--  keycap; an icon-only button (toolbar style) opts out of the visible
--  keycap, per button. Its shortcut still shows in its tooltip, in its
--  accessibility data (Controls.Shortcut) and in the reveal.
--
--  Layout (concept D): every button is as wide as its caption and keycap
--  need, never narrowed, truncated or ellipsized; clusters (F1..F4,
--  F5..F8, F9..F12) have wider gaps between them, as on a keyboard; a bar
--  too narrow for one row wraps (Button_Bar_Layout, proved), keeping
--  clusters together where they fit. A key with no command in the layer
--  shown, or a disabled command, stays visible: dimmed, with a dashed cap.
--
--  Modifiers (CuBit.UI.Modifier_Reveal, one model): while Ctrl or Shift is
--  held past the hold delay, a key bar shows that layer's real bindings
--  (Ctrl+F3 sorts by name), with the layer's caps once at the start
--  ([Ctrl]) and each cell keeping its plain width where the caption fits;
--  icon-only buttons whose chord uses exactly the held modifiers show a
--  reveal label ("Refresh [Ctrl][R]"); holding Alt reveals every icon-only
--  button's keycaps. Reveal labels sit in a band directly under the bar,
--  centred under their buttons in one lane, two at most. A key pressed on
--  the keyboard flashes its button briefly.
--
--  The bar draws, registers its buttons (Controls) and their tooltips; the
--  app routes clicks back with Take_Chosen and key presses through its
--  keymap (Find), then calls Flash. No allocation, callbacks or IPC.
------------------------------------------------------------------------------
with Interfaces;
with CuBit.UI.Button_Bar_Layout;
with CuBit.UI.Controls;
with CuBit.UI.Icons;
with CuBit.UI.Keycaps;
with CuBit.UI.Keymaps;
with CuBit.UI.Keys;
with CuBit.UI.Tooltips;
with CuBit.UI_Metrics;

generic
   with package Commands is new CuBit.UI.Keymaps (<>);
   --  What a command is called on its button and what its tooltip says.
   with function Caption (Item : Commands.Command) return String;
   with function Description (Item : Commands.Command) return String;
   --  Live state: whether it can run now, and whether it is on (a toggle).
   with function Enabled (Item : Commands.Command) return Boolean;
   with function Checked (Item : Commands.Command) return Boolean;
   --  Its keycap's colour (Danger for destructive ones) and its icon.
   with function Shade (Item : Commands.Command) return CuBit.UI.Keycaps.Tone;
   with function Picture (Item : Commands.Command) return CuBit.UI.Icons.Icon;
package CuBit.UI.Button_Bars is
   use Interfaces;
   package Keys renames CuBit.UI.Keys;
   package Layout renames CuBit.UI.Button_Bar_Layout;
   subtype Command is Commands.Command;
   subtype Button_Count is Layout.Button_Count;
   subtype Button_Index is Layout.Button_Index;
   MAXIMUM_BUTTONS : constant := Layout.MAXIMUM_BUTTONS;

   type Face is (Caption_With_Keycap, Icon_Only);
   type Keycap_Side is (Keycap_Before, Keycap_After);
   type Alignment is (Align_Left, Align_Right);
   --  Key_Bar_Style: a panel with a rule above (a function-key bar);
   --  Toolbar_Style: the toolbar face, separators between clusters;
   --  Plain_Style: no background (a dialog's buttons).
   type Bar_Style is (Key_Bar_Style, Toolbar_Style, Plain_Style);

   --  Geometry (logical pixels, from the em).
   CAPTION_BUTTON_HEIGHT : constant := CuBit.UI_Metrics.Compact_Row_Height;
   ICON_BUTTON_SIZE : constant := CuBit.UI_Metrics.Control_Height;
   BUTTON_INSET : constant := CuBit.UI_Metrics.Space_S;
   CAPTION_GAP : constant := CuBit.UI_Metrics.Space_S;
   BUTTON_GAP : constant := CuBit.UI_Metrics.Space_XS + 1;
   CLUSTER_GAP : constant := CuBit.UI_Metrics.Space_M + CuBit.UI_Metrics.Space_S;
   BAR_PADDING : constant := 2;
   ROW_GAP : constant := 2;
   --  How long a key's press flash lasts.
   FLASH_MS : constant Unsigned_64 := 140;
   NEVER : constant Unsigned_64 := Unsigned_64'Last;

   type Bar is private;
   --  A bar's buttons take the control IDs First_ID .. First_ID +
   --  MAXIMUM_BUTTONS - 1.
   procedure Clear
     (B : out Bar; First_ID : Controls.Control_ID; Style : Bar_Style := Key_Bar_Style;
      Align : Alignment := Align_Left; Side : Keycap_Side := Keycap_Before);
   procedure Add_Command
     (B : in out Bar; Item : Command; Look : Face := Caption_With_Keycap; Starts_Cluster : Boolean := False);
   procedure Add_Key (B : in out Bar; Item : Keys.Key; Starts_Cluster : Boolean := False);
   function Count (B : Bar) return Button_Count;
   function Button_ID (B : Bar; K : Button_Index) return Controls.Control_ID;

   --  The layer a bar shows for the revealed modifiers (Modifier_Reveal):
   --  Revealed if any of its key buttons has a binding there, else none.
   function Layer_Shown (B : Bar; Map : Commands.Keymap; Revealed : Keys.Modifier_Set) return Keys.Modifier_Set;
   --  What button K runs in Layer (key buttons) or always (command ones).
   procedure Command_Of
     (B : Bar; Map : Commands.Keymap; K : Button_Index; Layer : Keys.Modifier_Set;
      Found : out Boolean; Item : out Command);
   --  The chord button K shows (No_Chord for none) and its caption.
   function Shown_Chord (B : Bar; Map : Commands.Keymap; K : Button_Index; Revealed : Keys.Modifier_Set)
     return Keys.Chord;
   function Shown_Caption (B : Bar; Map : Commands.Keymap; K : Button_Index; Revealed : Keys.Modifier_Set)
     return String;
   --  The keycap shows on the button itself (not icon-only, and a chord).
   function Shows_Keycap (B : Bar; Map : Commands.Keymap; K : Button_Index; Revealed : Keys.Modifier_Set)
     return Boolean;
   --  The width button K needs to show its caption and keycap whole.
   function Needed_Width (B : Bar; Map : Commands.Keymap; K : Button_Index; Revealed : Keys.Modifier_Set)
     return Natural;

   --  The height the bar takes at Width (its rows), for the app's layout.
   function Height (B : Bar; Map : Commands.Keymap; Width : Natural; Revealed : Keys.Modifier_Set) return Natural;

   --  Draw into Area and register the buttons and their tooltips. Revealed:
   --  Modifier_Reveal.Revealed now. Call Draw_Reveal after everything else.
   procedure Draw
     (C : Canvas; Map : in out Controls.Control_Map; Tips : in out Tooltips.Tip_Table; B : in out Bar;
      Keymap : Commands.Keymap; Area : Rect; Colors : Theme; Pointer : Pointer_State;
      Revealed : Keys.Modifier_Set; Now_Ms : Unsigned_64);
   --  The reveal labels under icon-only buttons (see above), kept inside
   --  Window; what they cover is Reveal_Area (damage it when the revealed
   --  set changes).
   procedure Draw_Reveal
     (C : Canvas; B : Bar; Keymap : Commands.Keymap; Colors : Theme; Revealed : Keys.Modifier_Set;
      Window : Rect);
   function Reveal_Area (B : Bar; Keymap : Commands.Keymap; Revealed : Keys.Modifier_Set; Window : Rect)
     return Rect;
   --  Where button K's reveal label is (empty: none), for tests.
   function Reveal_Label (B : Bar; Keymap : Commands.Keymap; K : Button_Index; Revealed : Keys.Modifier_Set;
                          Window : Rect) return Rect;
   --  Which buttons get a reveal label for Revealed.
   function Revealed_At (B : Bar; Keymap : Commands.Keymap; K : Button_Index; Revealed : Keys.Modifier_Set)
     return Boolean;

   --  After the app's pointer dispatch: a button clicked since the last
   --  call, and what it runs.
   procedure Take_Chosen
     (B : Bar; Map : in out Controls.Control_Map; Keymap : Commands.Keymap; Found : out Boolean;
      Item : out Command);

   --  A key went down: flash the button showing it. Damage: what to repaint.
   procedure Flash (B : in out Bar; Keymap : Commands.Keymap; Pressed : Keys.Chord; Now_Ms : Unsigned_64;
                    Damage : out Rect);
   function Flashing (B : Bar; K : Button_Index; Now_Ms : Unsigned_64) return Boolean;
   procedure Tick (B : in out Bar; Now_Ms : Unsigned_64; Damage : out Rect);
   function Next_Deadline (B : Bar) return Unsigned_64;

   --  Where button K was drawn, the bar's area and its rows (last Draw).
   function Button_Area (B : Bar; K : Button_Index) return Rect;
   function Area_Of (B : Bar) return Rect;
   function Rows (B : Bar) return Natural;
private
   type Source is (Command_Source, Key_Source);
   type Button is record
      From : Source := Command_Source;
      Item : Command := Command'First;
      Key : Keys.Key := Keys.Unknown;
      Look : Face := Caption_With_Keycap;
      Starts_Cluster : Boolean := False;
   end record;
   type Button_Table is array (Button_Index) of Button;
   type Rect_Table is array (Button_Index) of Rect;
   type Bar is record
      Buttons : Button_Table;
      Count : Button_Count := 0;
      First_ID : Controls.Control_ID := Controls.NO_CONTROL;
      Style : Bar_Style := Key_Bar_Style;
      Align : Alignment := Align_Left;
      Side : Keycap_Side := Keycap_Before;
      --  The last Draw: where each button went, the bar, its rows and the
      --  layer it showed.
      Areas : Rect_Table := [others => (others => 0)];
      Area : Rect := (others => 0);
      Row_Count : Natural := 0;
      Layer : Keys.Modifier_Set := Keys.No_Modifiers;
      --  The flashing button (0: none) and until when.
      Flash_Button : Button_Count := 0;
      Flash_Until : Unsigned_64 := 0;
   end record;
end CuBit.UI.Button_Bars;
