------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The toolkit's standard chords (UI-049, UI-053): conventions every app
--  gets without declaring them, as typed chords for hints and dispatch.
--
--  @description
--  The clipboard's chords are the ones the Desktop issues clipboard tickets
--  for (Clipboard_Chords, proved; docs/clipboard.md): they are the
--  environment's gesture, so an app reserves them in its keymap
--  (Reserve_All) rather than binding them, and its menus show them from
--  here. They move to Super+C/X/V in a later slice (UI-052); until then
--  they stay Ctrl. tests/ui-keys checks they agree with Clipboard_Chords.
--
--  Accept and Cancel are what a focused dialog's default and cancel
--  buttons answer to ("OK [Enter]", "Cancel [Esc]").
------------------------------------------------------------------------------
with CuBit.UI.Keys;
with CuBit.UI.Keymaps;

package CuBit.UI.Standard_Keys is
   package Keys renames CuBit.UI.Keys;
   use type Keys.Modifier, Keys.Modifier_Set;

   Copy : constant Keys.Chord := Keys.Ctrl + Keys.C;
   Copy_Insert : constant Keys.Chord := Keys.Ctrl + Keys.Insert;
   Cut : constant Keys.Chord := Keys.Ctrl + Keys.X;
   Cut_Delete : constant Keys.Chord := Keys.Shift + Keys.Delete;
   Paste : constant Keys.Chord := Keys.Ctrl + Keys.V;
   Paste_Insert : constant Keys.Chord := Keys.Shift + Keys.Insert;
   --  Files copies full paths with Shift added (Copy + Shift).
   Copy_Shifted : constant Keys.Chord := Keys.Ctrl + Keys.Shift + Keys.C;
   Accept_Default : constant Keys.Chord := (Modifiers => Keys.No_Modifiers, Pressed => Keys.Enter);
   Cancel : constant Keys.Chord := (Modifiers => Keys.No_Modifiers, Pressed => Keys.Escape);

   type Chord_List is array (Positive range <>) of Keys.Chord;
   CLIPBOARD : constant Chord_List :=
     [Copy, Copy_Insert, Cut, Cut_Delete, Paste, Paste_Insert, Copy_Shifted];

   --  Reserve every clipboard chord in an app's keymap; Refused counts the
   --  ones an app command already held (tests assert none).
   generic
      with package Map_Of is new CuBit.UI.Keymaps (<>);
   procedure Reserve_Clipboard (Map : in out Map_Of.Keymap; Refused : out Natural);
end CuBit.UI.Standard_Keys;
