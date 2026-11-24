------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
package body Desktop_Super_Keys is
   package Keys renames CuBit.UI.Keys;
   use type Keys.Modifier, Keys.Key;

   function Defaults return Map.Binding_List is
     [(Keys.Super + Keys.V, Clipboard_History),
      (Keys.Super + Keys.N, Deskbar_Panel),
      (Keys.Super + Keys.T, Taskbar_Focus)];

   function Caption (Item : Command) return String is
     (case Item is
         when Clipboard_History => "Clipboard",
         when Deskbar_Panel => "Status",
         when Taskbar_Focus => "Taskbar");
end Desktop_Super_Keys;
