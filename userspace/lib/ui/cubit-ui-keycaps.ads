------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The keycap shortcut hint (UI-051): one look for every shortcut in the
--  environment, menus, tooltips, buttons, the button bar and the reveals.
--
--  @description
--  A keycap is a small rounded cap: a one-pixel border with a thicker
--  bottom edge and a short bold label (the emphasis face at the small
--  size). A chord is its caps side by side, modifiers first in display
--  order (Super, Ctrl, Alt, Shift): [Ctrl] [Shift] [C], never the string
--  "Ctrl+Shift+C". Colours come from the theme only: the accent for an
--  ordinary action, Danger for a destructive one, the muted ink for a
--  neutral one. An unbound key is a dimmed cap with a dashed border.
--  Callers pass typed chords (from a keymap's Hint), never text.
------------------------------------------------------------------------------
with CuBit.UI.Keys;
with CuBit.UI_Metrics;

package CuBit.UI.Keycaps is
   package Keys renames CuBit.UI.Keys;

   --  What the action is, for colour.
   type Tone is (Neutral, Accent, Danger);
   --  How the cap sits: on an ordinary surface, on a selected row (menus),
   --  greyed (a disabled action), or unbound (a dashed, empty-looking cap).
   type Look is (Normal, On_Selection, Disabled, Unbound);

   --  Geometry (logical pixels). The label is the emphasis face at the
   --  small size; the cap adds its border, a thicker bottom edge, and side
   --  padding; caps of a chord are CAP_GAP apart.
   BORDER : constant := 1;
   BOTTOM_BORDER : constant := 2;
   SIDE_PADDING : constant := CuBit.UI_Metrics.Space_XS + 2;
   TOP_PADDING : constant := 0;
   CAP_GAP : constant := CuBit.UI_Metrics.Space_XS;
   --  A cap is never narrower than it is tall minus its bottom edge, so a
   --  one-letter cap is square.
   function Cap_Height return Natural;
   function Cap_Width (Label : String) return Natural;

   --  A chord's caps, side by side (0 for No_Chord or a non-chord).
   function Width (Item : Keys.Chord) return Natural;
   function Height return Natural renames Cap_Height;

   --  Draw at (X, Y), top left; nothing for a non-chord.
   procedure Draw
     (C : Canvas; X, Y : Natural; Item : Keys.Chord; Colors : Theme;
      Shade : Tone := Neutral; Style : Look := Normal);
   --  One cap for a key alone (the button bar's keys, an unbound key).
   procedure Draw_Key
     (C : Canvas; X, Y : Natural; Item : Keys.Key; Colors : Theme;
      Shade : Tone := Neutral; Style : Look := Normal);
   function Key_Width (Item : Keys.Key) return Natural;

   --  The reveal label (UI-052): what holding a modifier shows anchored to
   --  a control: an optional caption and the chord's caps in a small flat
   --  box ("Save [Ctrl][S]"). Never a tooltip, never moving the layout:
   --  drawn last, over whatever is below the control. The Desktop's
   --  hold-Super hints and the toolkit's held-modifier reveals are this one
   --  label.
   LABEL_PADDING : constant := CuBit.UI_Metrics.Space_XS + 1;
   LABEL_GAP : constant := CuBit.UI_Metrics.Space_XS + 1;
   function Label_Width (Caption : String; Item : Keys.Chord) return Natural;
   function Label_Height return Natural;
   procedure Draw_Label
     (C : Canvas; X, Y : Natural; Caption : String; Item : Keys.Chord; Colors : Theme;
      Shade : Tone := Neutral);

   --  The same caption and caps without the box, on a band the caller
   --  paints (the toolkit's reveal band under a toolbar).
   function Plain_Label_Width (Caption : String; Item : Keys.Chord) return Natural;
   procedure Draw_Plain_Label
     (C : Canvas; X, Y : Natural; Caption : String; Item : Keys.Chord; Colors : Theme;
      Shade : Tone := Neutral);

   --  The caps of a set of modifiers alone (a key bar's layer indicator:
   --  [Ctrl] while the Ctrl layer shows).
   function Modifiers_Width (Set : Keys.Modifier_Set) return Natural;
   procedure Draw_Modifiers (C : Canvas; X, Y : Natural; Set : Keys.Modifier_Set; Colors : Theme);

   --  Always-visible keycaps (buttons, button bar cells, dialog buttons,
   --  menu items, the status line) show only while this is True: the one
   --  decision point a later appearance setting ("Hide key shortcuts",
   --  default shown) switches. Tooltips, held-modifier reveals and the
   --  controls' accessibility data (Controls.Shortcut) always carry the
   --  shortcut, whatever it says.
   function Persistent_Shown return Boolean is (True);
   --  A chord as an always-visible hint: Item, or No_Chord while hidden.
   function Persistent (Item : Keys.Chord) return Keys.Chord is
     (if Persistent_Shown then Item else Keys.No_Chord);

   --  The ink a tone draws in on the theme (border and label).
   function Ink (Colors : Theme; Shade : Tone; Style : Look) return Color;
end CuBit.UI.Keycaps;
