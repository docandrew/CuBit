------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The environment's layer (UI-052): Desktop's Super chords as typed
--  commands in a keymap of the environment layer, the one source of the
--  dispatch and of the hold-Super hints over the Desktop's controls.
--
--  @description
--  Super chords belong to Desktop: apps never see them (key_repeat's
--  Super_Chord route) and cannot bind them (an app's keymap refuses the
--  environment's layer). Holding Super past the hold delay
--  (CuBit.UI.Modifier_Reveal) shows each command's keycaps over the control
--  it works; a quick tap still opens the Apps menu.
------------------------------------------------------------------------------
with CuBit.UI.Keys;
with CuBit.UI.Keymaps;

package Desktop_Super_Keys is
   type Command is (Clipboard_History, Deskbar_Panel, Taskbar_Focus);
   package Map is new CuBit.UI.Keymaps (Command, CuBit.UI.Keys.Environment_Layer);

   --  The defaults: Super+V, Super+N, Super+T.
   function Defaults return Map.Binding_List;
   --  What a hint calls the command.
   function Caption (Item : Command) return String;
end Desktop_Super_Keys;
