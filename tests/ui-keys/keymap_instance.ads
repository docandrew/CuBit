------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The keymaps the proof analyses (generics are proved per instance): an
--  application's and the environment's, over a small command set.
------------------------------------------------------------------------------
with CuBit.UI.Keys;
with CuBit.UI.Keymaps;

package Keymap_Instance with SPARK_Mode is
   type Sample_Command is (Open, Refresh, Delete, Sort_By_Name, Quit);
   package App_Map is new CuBit.UI.Keymaps (Sample_Command);
   package Environment_Map is new CuBit.UI.Keymaps (Sample_Command, CuBit.UI.Keys.Environment_Layer);
end Keymap_Instance;
