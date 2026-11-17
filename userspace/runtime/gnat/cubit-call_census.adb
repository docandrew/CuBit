------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Call_Census is

   procedure Note (Call : Unsigned_64) is
   begin
      if Enabled then
         declare
            Slot : constant Number :=
              (if Call > Last_Number then Last_Number else Number (Call));
         begin
            Census (Slot) := Census (Slot) + 1;
         end;
      end if;
   end Note;

end CuBit.Call_Census;
