------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
package body CuBit.Settings is
   function To_Text (Item : String) return Text is
      Result : Text;
   begin
      Result.Length := Item'Length;
      Result.Data (1 .. Item'Length) := Item;
      return Result;
   end To_Text;
end CuBit.Settings;
