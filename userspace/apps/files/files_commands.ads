------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  CuBit Files' commands and their default chords, in one place (UI-052).
--
--  @description
--  Every key, menu item, toolbar button and key bar cell names one of
--  these commands; the chords are the keymaps' (Files_Keymaps), so a remap
--  changes dispatch and every hint at once. Three contexts each have a
--  keymap: the panes, the quick viewer and a prompt (the same F3 opens the
--  viewer in the panes and closes it in the viewer).
--  The clipboard's chords are the toolkit's (CuBit.UI.Standard_Keys):
--  reserved here, not bound.
------------------------------------------------------------------------------
with CuBit.UI.Icons;
with CuBit.UI.Keycaps;
with CuBit.UI.Keys;
with Files_Keymaps;

package Files_Commands is
   package Keys renames CuBit.UI.Keys;
   subtype Command is Files_Keymaps.Command;
   use all type Files_Keymaps.Command;

   --  What a button and a menu item are called ("Refresh"), and what the
   --  tooltip says ("Refresh the folder").
   function Caption (Item : Command) return String;
   function Description (Item : Command) return String;
   --  The keycap's colour: Danger for destructive commands, the accent
   --  for file actions, neutral for the rest.
   function Shade (Item : Command) return CuBit.UI.Keycaps.Tone;
   function Picture (Item : Command) return CuBit.UI.Icons.Icon;

   --  The defaults (the first chord of a command is its hint).
   function Panes return Files_Keymaps.Map.Binding_List;
   function Viewer return Files_Keymaps.Map.Binding_List;
   function Prompt return Files_Keymaps.Map.Binding_List;

   --  The column toggles (the Columns menu), in Column_Kind order
   --  after the Name column.
   subtype Column_Command is Command range Show_Extension .. Show_Owner;
end Files_Commands;
