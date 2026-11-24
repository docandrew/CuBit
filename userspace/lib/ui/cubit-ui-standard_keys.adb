------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
package body CuBit.UI.Standard_Keys is
   procedure Reserve_Clipboard (Map : in out Map_Of.Keymap; Refused : out Natural) is
      use type Map_Of.Bind_Result;
      Result : Map_Of.Bind_Result;
   begin
      Refused := 0;
      for Item of CLIPBOARD loop
         Map_Of.Reserve (Map, Item, Result);
         if Result /= Map_Of.Bound then
            Refused := Refused + 1;
         end if;
      end loop;
   end Reserve_Clipboard;
end CuBit.UI.Standard_Keys;
